const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;
const codegen = @import("../codegenprog.zig");

/// Emit codegen for an inbuilt (non-user-defined) function call.
/// `func_name` is the fully-qualified name (e.g. "min", "strings.truncate").
pub fn codegenInbuiltFuncall(
    func_name: []const u8,
    f: *Ast.Funcall,
    compiler: *Compiler,
    cctx: *analyser.Context,
) !void {
    if (std.mem.eql(u8, func_name, "min")) {
        try codegenMinMax(false, f.args.items, compiler, cctx);
        return;
    }
    if (std.mem.eql(u8, func_name, "max")) {
        try codegenMinMax(true, f.args.items, compiler, cctx);
        return;
    }
    if (std.mem.eql(u8, func_name, "zeroOf")) {
        try codegenZeroOrUnit(false, f.args.items[0], compiler, cctx);
        return;
    }
    if (std.mem.eql(u8, func_name, "unitOf")) {
        try codegenZeroOrUnit(true, f.args.items[0], compiler, cctx);
        return;
    }
    if (std.mem.eql(u8, func_name, "error")) {
        try codegenErrorCall(f.args.items, compiler, cctx);
        return;
    }
    // Namespace inbuilt (e.g. "strings.truncate") — call the corresponding C helper.
    try codegenNamespaceInbuilt(func_name, f.args.items, compiler, cctx);
}

// ---------------------------------------------------------------------------
// min / max
// ---------------------------------------------------------------------------
// Evaluates all args, iteratively selecting the min (want_max=false) or max
// (want_max=true) using signed integer comparison.
fn codegenMinMax(
    want_max: bool,
    args: []*Ast.Value,
    compiler: *Compiler,
    cctx: *analyser.Context,
) !void {
    if (args.len == 0) {
        // Should not happen after analysis — produce 0 as a safe fallback.
        const r = try compiler.registerTable.allocate(compiler);
        const reg = try compiler.registerTable.getValue(r, compiler);
        try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = reg } });
        return;
    }

    // Evaluate the first argument — this becomes the running result.
    try codegen.value.codegenValue(args[0], compiler, cctx);
    const result_idx = compiler.registerTable.lastReg().?;

    for (args[1..]) |arg| {
        try codegen.value.codegenValue(arg, compiler, cctx);
        const tmp_idx = compiler.registerTable.lastReg().?;
        const result_reg = try compiler.registerTable.getValue(result_idx, compiler);
        const tmp_reg = try compiler.registerTable.getValue(tmp_idx, compiler);

        // skip_label: jump here when result already holds the correct extreme.
        const skip_label = try compiler.generateLabel("minmax_skip");

        // cmp result, tmp  →  flags based on (result - tmp)
        try compiler.addInstruction(.{ .cmp = .{
            .val1 = .{ .register = result_reg },
            .val2 = .{ .register = tmp_reg },
        } });

        // For min: skip update if result <= tmp  (JLE means "result is already ≤ tmp, it IS the min")
        // For max: skip update if result >= tmp  (JGE means "result is already ≥ tmp, it IS the max")
        const skip_cc: Inst.CodeCondition = if (want_max) .GE else .LE;
        try compiler.addInstruction(.{ .jcond = .{ .cc = skip_cc, .where = skip_label.name } });

        // tmp is the new extreme — move it into result.
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .register = tmp_reg },
            .to = result_reg,
        } });

        try compiler.addInstruction(.{ .label = skip_label });

        try compiler.registerTable.free(tmp_idx);
    }
    // result_idx is already set as the last used register via lastReg above,
    // but we need to ensure last_used points to it for downstream consumers.
    compiler.registerTable.last_used = result_idx;
}

// ---------------------------------------------------------------------------
// zeroOf / unitOf
// ---------------------------------------------------------------------------
// Evaluates the argument to obtain its runtime type, discards the value, then
// emits the appropriate zero (false/0/'\0') or unit (true/1/'\x01') constant.
fn codegenZeroOrUnit(
    want_unit: bool,
    arg: *Ast.Value,
    compiler: *Compiler,
    cctx: *analyser.Context,
) !void {
    // Evaluate to determine type; release heap ownership before discarding.
    try codegen.value.codegenValue(arg, compiler, cctx);
    const val_idx = compiler.registerTable.lastReg().?;
    {
        const gc_mod = @import("../gc.zig");
        const val_type = try bbcTypes.getTypeOfValue(arg, cctx, compiler.allocator);
        if (gc_mod.shouldGcDecVar(val_type, cctx)) {
            const val_reg = try compiler.registerTable.getValue(val_idx, compiler);
            const free_fn = try gc_mod.getFreeFnLabel(val_type, cctx, compiler.allocator);
            try gc_mod.emitGcDec(val_reg, free_fn, compiler);
        }
    }
    try compiler.registerTable.free(val_idx);

    const imm: i64 = if (want_unit) 1 else 0;
    const r = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = imm }, .to = reg } });
}

// ---------------------------------------------------------------------------
// error("message")
// ---------------------------------------------------------------------------
// Creates an error value (error_code = 0, ignores the message in codegen),
// sets bbc_error_flag, returns the ErrorObj pointer in a register.
// If the current function propagates errors, also emits an early return.
fn codegenErrorCall(
    args: []*Ast.Value,
    compiler: *Compiler,
    cctx: *analyser.Context,
) !void {
    // Evaluate the message string (needed for GC, even though message is unused in codegen).
    if (args.len > 0) {
        try codegen.value.codegenValue(args[0], compiler, cctx);
        const msg_idx = compiler.registerTable.lastReg().?;
        // GC dec the string arg — error() doesn't store it.
        const gc = @import("../gc.zig");
        const msg_reg = try compiler.registerTable.getValue(msg_idx, compiler);
        const msg_free_fn = try gc.getFreeFnLabel(
            bbcTypes.Type{ .decided = blk: {
                const t = try compiler.allocator.create(Ast.Type);
                t.* = .{ .base = .{ .name = "String" }, .err = false, .references = 0 };
                break :blk t;
            } },
            cctx,
            compiler.allocator,
        );
        try gc.emitGcDec(msg_reg, msg_free_fn, compiler);
        try compiler.registerTable.free(msg_idx);
    }

    // Call bbc_make_error_fn(0) — error code 0 for user-raised errors.
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = .R5 } }); // rdi = 0
    try compiler.emitAlignedCall("bbc_make_error_fn", compiler.stack_size);

    // Set bbc_error_flag = 1.
    try compiler.addInstruction(.{ .store = .{ .from = .{ .immediate = 1 }, .to = .{ .rip_memory = "bbc_error_flag" } } });

    // Move ErrorObj ptr (rax) into a register.
    const r = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = reg } });

    if (compiler.in_error_check_expr) {
        // Inside ? err: leave the register live so errorcheck.zig can read it
        // via inner_last_used / lastReg() on the ok path (which never executes
        // at runtime for error(), but must compile cleanly).
    } else {
        // Not inside ? err: emit early return for error-propagating functions,
        // then free the result register so isCorrectlyFreed() passes for
        // non-final statement positions.
        if (compiler.current_func_propagates_errors) {
            const ok_label = try compiler.generateLabel("error_call_ok");
            try compiler.emitEarlyReturnOnError(ok_label);
        }
        try compiler.registerTable.free(r);
        compiler.registerTable.last_used = null;
    }
}

// ---------------------------------------------------------------------------
// Namespace inbuilt calls (e.g. strings.truncate)
// ---------------------------------------------------------------------------
// Routes to a corresponding C runtime helper: `bbc_<lib>_<func>`.
fn codegenNamespaceInbuilt(
    func_name: []const u8,
    args: []*Ast.Value,
    compiler: *Compiler,
    cctx: *analyser.Context,
) !void {
    // Build the C runtime label: "strings.truncate" → "bbc_strings_truncate"
    const dot = std.mem.indexOf(u8, func_name, ".") orelse {
        std.debug.print("codegenNamespaceInbuilt: no dot in '{s}'\n", .{func_name});
        return;
    };
    const lib = func_name[0..dot];
    const fname = func_name[dot + 1 ..];
    const c_label = try std.fmt.allocPrint(compiler.allocator, "bbc_{s}_{s}", .{ lib, fname });

    // Push caller-saved live registers.
    var saved_live = std.ArrayList(Inst.Register).init(compiler.allocator);
    defer saved_live.deinit();
    const live_snap = try compiler.allocator.dupe(usize, compiler.registerTable.live.items);
    defer compiler.allocator.free(live_snap);
    for (live_snap) |li| {
        const rs = &compiler.registerTable.states.items[li];
        if (rs.freed or rs.spilled) continue;
        var is_caller = false;
        for (Inst.CALLER_SAVED) |cr| {
            if (cr == rs.reg.?) { is_caller = true; break; }
        }
        if (!is_caller) continue;
        try compiler.addInstruction(.{ .push = .{ .reg = rs.reg.? } });
        compiler.incrementStackOffset();
        try saved_live.append(rs.reg.?);
    }

    // Push args in reverse order, then pop into argument registers.
    const n = args.len;
    for (0..n) |ri| {
        const ai = n - 1 - ri;
        try codegen.value.codegenValue(args[ai], compiler, cctx);
        const a_idx = compiler.registerTable.lastReg().?;
        const a_reg = try compiler.registerTable.getValue(a_idx, compiler);
        try compiler.addInstruction(.{ .push = .{ .reg = a_reg } });
        compiler.incrementStackOffset();
        try compiler.registerTable.free(a_idx);
    }
    const n_regs = @min(n, Inst.ARGUMENT_REGS.len);
    for (0..n_regs) |i| {
        try compiler.addInstruction(.{ .pop = .{ .reg = Inst.ARGUMENT_REGS[i] } });
        compiler.decrementStackOffset();
    }

    // For each register-passed arg that is heap-typed: bump its refcount and push
    // a copy to the stack so we can gc_dec after the call.  The C helper borrows
    // but does not own the argument, so we must release our borrow when it returns.
    const gc_mod = @import("../gc.zig");
    var heap_arg_count: usize = 0;
    for (0..n_regs) |i| {
        const a_type = try bbcTypes.getTypeOfValue(args[i], cctx, compiler.allocator);
        if (!gc_mod.shouldGcDecVar(a_type, cctx)) continue;
        try gc_mod.emitGcInc(Inst.ARGUMENT_REGS[i], compiler);
        try compiler.addInstruction(.{ .push = .{ .reg = Inst.ARGUMENT_REGS[i] } });
        compiler.incrementStackOffset();
        heap_arg_count += 1;
    }

    try compiler.emitAlignedCall(c_label, compiler.stack_size);

    // gc_dec each saved heap arg copy.  R0/rax holds the call result and must be
    // preserved — push it first so the offset-based loads can still find the heap
    // arg copies, then pop R0 back after all gc_dec calls.
    //
    // Stack layout after pushing R0:
    //   [rsp+0]              = R0 (call result)
    //   [rsp+8]              = last heap arg copy   (pushed last in Phase C)
    //   [rsp+8+(n-1)*8]     = first heap arg copy  (pushed first in Phase C)
    if (heap_arg_count > 0) {
        try compiler.addInstruction(.{ .push = .{ .reg = .R0 } });
        compiler.incrementStackOffset();

        var pending: usize = heap_arg_count;
        var si = n_regs;
        while (si > 0 and pending > 0) {
            si -= 1;
            const a_type = try bbcTypes.getTypeOfValue(args[si], cctx, compiler.allocator);
            if (!gc_mod.shouldGcDecVar(a_type, cctx)) continue;
            pending -= 1;
            // +8 to skip past the pushed R0; further offset into the heap arg copies.
            const stack_offset: i64 = @intCast(8 + (heap_arg_count - 1 - pending) * 8);
            const tmp = try compiler.registerTable.allocate(compiler);
            const tmp_reg = try compiler.registerTable.getValue(tmp, compiler);
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .register = .RSP, .offset = stack_offset } },
                .to = tmp_reg,
            } });
            const free_fn = try gc_mod.getFreeFnLabel(a_type, cctx, compiler.allocator);
            try gc_mod.emitGcDec(tmp_reg, free_fn, compiler);
            try compiler.registerTable.free(tmp);
        }

        // Restore R0 (call result).
        try compiler.addInstruction(.{ .pop = .{ .reg = .R0 } });
        compiler.decrementStackOffset();
        // Clean up heap arg copies from the stack.
        try compiler.addInstruction(.{ .plus = .{
            .lhs = .RSP,
            .rhs = .{ .immediate = @intCast(heap_arg_count * 8) },
        } });
        for (0..heap_arg_count) |_| compiler.decrementStackOffset();
    }

    // Restore live registers.
    var ri = saved_live.items.len;
    while (ri > 0) {
        ri -= 1;
        try compiler.addInstruction(.{ .pop = .{ .reg = saved_live.items[ri] } });
        compiler.decrementStackOffset();
    }

    // Check error flag from the callee (early return if error propagates).
    if (compiler.current_func_propagates_errors and !compiler.in_error_check_expr) {
        const ok_label = try compiler.generateLabel("ns_inbuilt_ok");
        try compiler.emitEarlyReturnOnError(ok_label);
    }

    // Move return value (rax) into a fresh register.
    const r = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = reg } });
}
