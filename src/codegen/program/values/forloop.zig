const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");
const gc = @import("../gc.zig");

pub fn codegenForLoop(fl: *Ast.ForLoop, compiler: *Compiler, cctx: *analyser.Context) !void {
    const cond_label = try compiler.generateLabel("for_cond");
    const end_label = try compiler.generateLabel("for_end");

    // Get iterator type so the synthetic variable can be resolved by codegenFuncall.
    const iter_type = try bbcTypes.getTypeOfValue(fl.iterable, cctx, compiler.allocator);

    // Unique name for the iterator slot; use the label counter as suffix.
    const iter_var_name = try std.fmt.allocPrint(
        compiler.allocator,
        "_for_iter_{s}",
        .{cond_label.name},
    );

    // Claim one slot for the iterator object.
    // Fixed-frame mode: advance frame_slot_cursor (RSP is pre-allocated, unchanged).
    // Non-fixed mode: advance stack_size and emit sub rsp, 8.
    const iter_slot: i64 = blk: {
        if (compiler.frame_is_fixed) {
            compiler.frame_slot_cursor += 1;
            break :blk compiler.frame_slot_cursor;
        } else {
            compiler.incrementStackOffset();
            try compiler.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
            break :blk compiler.stack_size;
        }
    };
    try cctx.createVariable(iter_var_name, iter_type, fl.reference);
    try cctx.putVariableStackIndex(iter_var_name, iter_slot);

    // Evaluate the iterable expression and store the iterator object in the slot.
    try codegen.value.codegenValue(fl.iterable, compiler, cctx);
    const iter_idx = compiler.registerTable.lastReg().?;
    const iter_reg = try compiler.registerTable.getValue(iter_idx, compiler);
    // gc_inc if heap-typed: iterator arrives at refcount=0 (new creation model);
    // the slot takes ownership so we must increment before the scope-exit gc_dec.
    if (gc.shouldGcDecVar(iter_type, cctx)) {
        try gc.emitGcInc(iter_reg, compiler);
    }
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .register = iter_reg },
        .to = .{ .registerOffset = .{ .register = .RBP, .offset = -iter_slot * 8 } },
    } });
    try compiler.registerTable.free(iter_idx);

    // ── Loop start ────────────────────────────────────────────────────────────
    try compiler.addInstruction(.{ .label = cond_label });

    // Call iter.isLast() — synthetic method call on the stored iterator.
    {
        var iter_val = Ast.Value{ .identifier = .{ .name = iter_var_name, .reference = fl.reference } };
        var uop = Ast.UnaryOperatorRight{
            .operator = .{ .pointAttr = "isLast" },
            .expr = &iter_val,
            .reference = fl.reference,
        };
        var uop_val = Ast.Value{ .unaryOperatorRight = &uop };
        var args = std.ArrayList(*Ast.Value).init(compiler.allocator);
        defer args.deinit();
        var call = Ast.Funcall{ .func = &uop_val, .args = args };
        var call_val = Ast.Value{ .funcall = &call };
        try codegen.value.codegenValue(&call_val, compiler, cctx);
    }

    const is_last_idx = compiler.registerTable.lastReg().?;
    const is_last_reg = try compiler.registerTable.getValue(is_last_idx, compiler);
    try compiler.addInstruction(.{ .cmp = .{
        .val1 = .{ .register = is_last_reg },
        .val2 = .{ .immediate = 0 },
    } });
    try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = end_label.name } });
    try compiler.registerTable.free(is_last_idx);

    // Call iter.next() and store result in the loop variable's stack slot.
    {
        var iter_val = Ast.Value{ .identifier = .{ .name = iter_var_name, .reference = fl.reference } };
        var uop = Ast.UnaryOperatorRight{
            .operator = .{ .pointAttr = "next" },
            .expr = &iter_val,
            .reference = fl.reference,
        };
        var uop_val = Ast.Value{ .unaryOperatorRight = &uop };
        var args = std.ArrayList(*Ast.Value).init(compiler.allocator);
        defer args.deinit();
        var call = Ast.Funcall{ .func = &uop_val, .args = args };
        var call_val = Ast.Value{ .funcall = &call };
        try codegen.value.codegenValue(&call_val, compiler, cctx);
    }

    const next_idx = compiler.registerTable.lastReg().?;
    const next_reg = try compiler.registerTable.getValue(next_idx, compiler);
    const var_slot: i64 = @intCast(cctx.getVariableStackIndex(fl.var_name));
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .register = next_reg },
        .to = .{ .registerOffset = .{ .register = .RBP, .offset = -var_slot * 8 } },
    } });
    try compiler.registerTable.free(next_idx);

    // Execute the loop body.
    try codegen.value.codegenValue(fl.exec, compiler, cctx);
    if (compiler.registerTable.lastReg()) |body_idx| {
        try compiler.registerTable.free(body_idx);
    }

    try compiler.addInstruction(.{ .jmp = .{ .where = cond_label.name } });
    try compiler.addInstruction(.{ .label = end_label });

    // Release the iterator slot (non-fixed mode only; fixed frame never adjusts RSP).
    if (!compiler.frame_is_fixed) {
        try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
        compiler.decrementStackOffset();
    }
}
