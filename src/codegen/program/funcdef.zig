const std = @import("std");
const analyser = @import("../../analyser.zig");
const Ast = @import("../../ast.zig");
const bbcTypes = @import("../../types.zig");
const errors = @import("../../errors.zig");

const Inst = @import("../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../compiler.zig").Compiler;

const codegen = @import("codegenprog.zig");
const gc = @import("gc.zig");

/// Replace characters that are invalid in NASM identifiers with underscores.
fn sanitizeLabelName(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, name.len);
    for (name, 0..) |c, i| {
        buf[i] = switch (c) {
            '<', '>', ',', ' ', '!' => '_',
            else => c,
        };
    }
    return buf;
}

pub fn generateFuncUID(funcname: []const u8, compiler: *Compiler) ![]const u8 {
    // Main can't have a different ID than 'main'
    if (std.mem.eql(u8, funcname, "main"))
        return funcname;

    const safe_name = try sanitizeLabelName(funcname, compiler.allocator);
    const func_num = try compiler.f_table.getOrPutValue(safe_name, 0);

    const id = try std.fmt.allocPrint(compiler.allocator, "{s}_{d}", .{ safe_name, func_num.value_ptr.* });
    try compiler.f_table.put(safe_name, func_num.value_ptr.* + 1);
    return id;
}

pub fn codegenFuncdef(func: *const analyser.functionVersion, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Reset per-function state.
    compiler.stack_size = 0;
    compiler.frame_is_fixed = false;
    compiler.frame_slot_cursor = 0;
    compiler.current_func_propagates_errors = func.signature.retype.err;

    const function_uid = compiler.getUid(func.*);
    try compiler.addInstruction(Instruction{ .label = .{ .name = function_uid } });

    const function_def = cctx.getFunction(func.name);
    const is_method = function_def.parent != null;
    const total_frame_args = func.signature.argtypes.items.len + @as(usize, if (is_method) 1 else 0);
    const arg_regs = compiler.arch.argument_regs;
    const args_in_regs = @min(total_frame_args, arg_regs.len);

    // ── Prologue ───────────────────────────────────────────────────────────────
    try compiler.addInstruction(.{ .frame_enter = .{} });

    // Assign stack slots to args using stack_size as a temporary cursor.
    // These slot numbers ([rbp - slot*8]) are stable for the lifetime of the call.
    if (is_method) {
        compiler.incrementStackOffset();
        try function_def.ctx.?.putVariableStackIndex("self", compiler.stack_size);
        try function_def.code.ctx.putVariableStackIndex("self", compiler.stack_size);
    }
    for (0..@min(function_def.arguments.items.len, arg_regs.len)) |i| {
        const arg = function_def.arguments.items[i];
        compiler.incrementStackOffset();
        try function_def.ctx.?.putVariableStackIndex(arg.name, compiler.stack_size);
        try function_def.code.ctx.putVariableStackIndex(arg.name, compiler.stack_size);
    }
    for (@min(function_def.arguments.items.len, arg_regs.len)..function_def.arguments.items.len) |i| {
        const arg = function_def.arguments.items[i];
        const idx: i64 = @intCast(i);
        const sub_reg_idx: i64 = @intCast(arg_regs.len);
        const stack_idx = -((idx - sub_reg_idx) + 3);
        try function_def.ctx.?.putVariableStackIndex(arg.name, stack_idx);
        try function_def.code.ctx.putVariableStackIndex(arg.name, stack_idx);
    }
    // Round arg count to even so the frame is 16-byte aligned.
    if (@mod(compiler.stack_size, 2) != 0) compiler.incrementStackOffset();
    const args_aligned: i64 = compiler.stack_size;

    // Pre-compute total body slots (all nested scopes included, conservative sum).
    const body_slots: usize = codegen.scope.countSlotsInScope(function_def.code);
    const total_slots_raw: usize = @as(usize, @intCast(args_aligned)) + body_slots;
    // Round total to even so sub rsp is a multiple of 16.
    const aligned_total_slots: usize = if (total_slots_raw % 2 != 0) total_slots_raw + 1 else total_slots_raw;
    const aligned_total_bytes: usize = aligned_total_slots * 8;

    // Single prologue sub rsp: covers args + all body/nested locals.
    if (aligned_total_bytes > 0) {
        try compiler.addInstruction(.{ .minus = .{
            .lhs = .RSP,
            .rhs = .{ .immediate = @intCast(aligned_total_bytes) },
        } });
    }

    // Spill register args to their pre-assigned slots.
    for (0..args_in_regs) |i| {
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .register = arg_regs[i] },
            .to = .{ .registerOffset = .{
                .register = .RBP,
                .offset = -@as(i64, @intCast((i + 1) * 8)),
            } },
        } });
    }

    // Enter fixed-frame mode: stack_size is now the total frame depth (fixed).
    // frame_slot_cursor starts after arg slots so body locals don't collide.
    compiler.stack_size = @intCast(aligned_total_slots);
    compiler.frame_is_fixed = true;
    compiler.frame_slot_cursor = args_aligned;

    // GC Section A: gc_inc every heap-typed register argument.
    for (function_def.arguments.items[0..@min(function_def.arguments.items.len, arg_regs.len)]) |arg| {
        const arg_bbc_type = bbcTypes.Type{ .decided = arg._type };
        if (!gc.shouldGcDecVar(arg_bbc_type, cctx)) continue;
        const slot = function_def.ctx.?.variable_stack_loc.get(arg.name) orelse continue;
        if (slot <= 0) continue;
        const tmp_idx = try compiler.registerTable.allocate(compiler);
        const tmp_reg = try compiler.registerTable.getValue(tmp_idx, compiler);
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .register = .RBP, .offset = -slot * 8 } },
            .to = tmp_reg,
        } });
        try gc.emitGcInc(tmp_reg, compiler);
        try compiler.registerTable.free(tmp_idx);
    }

    // Generate the error-exit label.
    const func_error_exit_lbl = try compiler.generateLabel("func_error_exit");
    compiler.error_exit_label = func_error_exit_lbl.name;

    // Generate the function body. Fixed-frame mode: no sub/add rsp inside.
    try codegen.scope.codegenScope(function_def.code, compiler, true);

    compiler.error_exit_label = null;

    // After the body scope, stack_size is still aligned_total_slots (fixed).
    // No re-reservation needed — the frame was pre-allocated.

    // ── GC Section C: return-value protection + scope teardown ────────────────
    //
    // 1. gc_inc the return value (protect from scope teardown).
    // 2. Push return register (survives GC calls).
    // 3. GC dec all heap-typed body locals.
    // 4. GC dec all heap-typed register args.
    // 5. Pop return value, move to rax.
    // 6. gc_dec_no_check (undo protective inc; caller gets refcount=0).

    const ret_bbc_type = bbcTypes.Type{ .decided = func.signature.retype };
    const ret_is_heap = gc.shouldGcDecVar(ret_bbc_type, cctx);

    const ret_idx_opt: ?usize = compiler.registerTable.lastReg();

    if (ret_idx_opt) |ret_idx| {
        const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
        if (ret_is_heap) try gc.emitGcInc(ret_reg, compiler);
        try compiler.addInstruction(.{ .push = .{ .reg = ret_reg } });
        compiler.incrementStackOffset();
        compiler.registerTable.last_used = ret_idx;
    }

    const return_var_name: ?[]const u8 = codegen.scope.returnVarName(function_def.code);

    try codegen.scope.emitScopeGcExit(function_def.code, compiler, return_var_name);

    for (function_def.arguments.items) |arg| {
        const arg_bbc_type = bbcTypes.Type{ .decided = arg._type };
        if (!gc.shouldGcDecVar(arg_bbc_type, cctx)) continue;
        const slot = function_def.ctx.?.variable_stack_loc.get(arg.name) orelse continue;
        if (slot <= 0) continue;
        const tmp_idx = try compiler.registerTable.allocate(compiler);
        const tmp_reg = try compiler.registerTable.getValue(tmp_idx, compiler);
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .register = .RBP, .offset = -slot * 8 } },
            .to = tmp_reg,
        } });
        const free_fn = try gc.getFreeFnLabel(arg_bbc_type, cctx, compiler.allocator);
        try gc.emitGcDec(tmp_reg, free_fn, compiler);
        try compiler.registerTable.free(tmp_idx);
    }

    if (ret_idx_opt) |ret_idx| {
        const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
        try compiler.addInstruction(.{ .pop = .{ .reg = ret_reg } });
        compiler.decrementStackOffset();
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = ret_reg }, .to = .R0 } });
        if (ret_is_heap) try gc.emitGcDecNoCheck(.R0, compiler);
        if (ret_is_heap and return_var_name != null) {
            try gc.emitGcDecNoCheck(.R0, compiler);
        }
        try compiler.registerTable.free(ret_idx);
    }

    if (!compiler.registerTable.isCorrectlyFreed()) {
        std.debug.print("All the registers are not freed !! 🚨🚨🚨🚨", .{});
        unreachable;
    }

    // ── Normal epilogue ────────────────────────────────────────────────────────
    const skip_err_exit_lbl = try compiler.generateLabel("skip_func_error_exit");
    try compiler.addInstruction(.{ .jmp = .{ .where = skip_err_exit_lbl.name } });

    // ── Error-exit block ───────────────────────────────────────────────────────
    // Entered via jmp from emitEarlyReturnOnError. rax = ErrorObj*.
    // RSP may be anywhere inside nested scopes; restore via rbp.
    try compiler.addInstruction(.{ .label = func_error_exit_lbl });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .RBP }, .to = .RSP } });
    // Reserve full frame + 1 slot for saving rax (ErrorObj*).
    // All arg + body local slots live at [rbp - slot*8] within aligned_total_slots.
    const err_reserve: i64 = @intCast(aligned_total_slots + 1);
    try compiler.addInstruction(.{ .minus = .{
        .lhs = .RSP,
        .rhs = .{ .immediate = err_reserve * 8 },
    } });
    compiler.stack_size = @intCast(err_reserve);
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .register = .R0 },
        .to = .{ .registerOffset = .{ .register = .RSP, .offset = 0 } },
    } });
    try codegen.scope.emitScopeGcExit(function_def.code, compiler, null);
    for (function_def.arguments.items) |arg| {
        const arg_bbc_type_err = bbcTypes.Type{ .decided = arg._type };
        if (!gc.shouldGcDecVar(arg_bbc_type_err, cctx)) continue;
        const slot = function_def.ctx.?.variable_stack_loc.get(arg.name) orelse continue;
        if (slot <= 0) continue;
        const tmp_idx2 = try compiler.registerTable.allocate(compiler);
        const tmp_reg2 = try compiler.registerTable.getValue(tmp_idx2, compiler);
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .register = .RBP, .offset = -slot * 8 } },
            .to = tmp_reg2,
        } });
        const free_fn2 = try gc.getFreeFnLabel(arg_bbc_type_err, cctx, compiler.allocator);
        try gc.emitGcDec(tmp_reg2, free_fn2, compiler);
        try compiler.registerTable.free(tmp_idx2);
    }
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .registerOffset = .{ .register = .RSP, .offset = 0 } },
        .to = .R0,
    } });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .RBP }, .to = .RSP } });
    if (compiler.arch.target == .arm64)
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = .R5 } });
    try compiler.addInstruction(.{ .frame_leave = .{} });

    try compiler.addInstruction(.{ .label = skip_err_exit_lbl });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .RBP }, .to = .RSP } });
    if (compiler.arch.target == .arm64)
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = .R5 } });
    try compiler.addInstruction(.{ .frame_leave = .{} });

    // Reset fixed-frame mode for the next function.
    compiler.frame_is_fixed = false;
    compiler.frame_slot_cursor = 0;
}
