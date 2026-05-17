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

pub fn codegenAssignement(assign: *Ast.Assignement, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Evaluate RHS first so any function calls in the RHS cannot clobber the LHS address register.
    // LHS address computation is always a pure LEA (no calls), so it is safe to do after RHS.
    try codegen.value.codegenValue(assign.rhs, compiler, cctx);
    const rhs_idx = compiler.registerTable.lastReg().?;

    try codegen.value.codegenValueAdr(assign.lhs, compiler, cctx);
    const lhs_idx = compiler.registerTable.lastReg().?;

    const rhs_reg = try compiler.registerTable.getValue(rhs_idx, compiler);
    const lhs_reg = try compiler.registerTable.getValue(lhs_idx, compiler);

    // GC invariant: heap objects are created at refcount=0.
    // Every assignment (including first/varDec) gc_inc's the new value to take ownership.
    // Non-first assignments also gc_dec the old value to release the previous ownership.
    const is_first_assign = assign.lhs.* == .varDec;
    const rhs_type = try bbcTypes.getTypeOfValue(assign.rhs, cctx, compiler.allocator);

    if (gc.shouldGcDecVar(rhs_type, cctx)) {
        if (is_first_assign) {
            // First assignment: new value starts at refcount=0; gc_inc to take ownership.
            // No old value in this slot — do not attempt to load or gc_dec it.
            try gc.emitGcInc(rhs_reg, compiler);
            try compiler.addInstruction(.{ .store = .{
                .from = .{ .register = rhs_reg },
                .to = .{ .registerOffset = .{ .offset = 0, .register = lhs_reg } },
            } });
            try compiler.registerTable.free(rhs_idx);
            try compiler.registerTable.free(lhs_idx);
            return;
        } else {
            // Reassignment (plain stack var, struct field, or buffer index):
            // Load old value, gc_inc new, store new, gc_dec old.
            const old_idx = try compiler.registerTable.allocate(compiler);
            const old_reg = try compiler.registerTable.getValue(old_idx, compiler);
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .register = lhs_reg, .offset = 0 } },
                .to = old_reg,
            } });

            try gc.emitGcInc(rhs_reg, compiler);

            try compiler.addInstruction(.{ .store = .{
                .from = .{ .register = rhs_reg },
                .to = .{ .registerOffset = .{ .offset = 0, .register = lhs_reg } },
            } });

            // Free rhs/lhs registers before the gc_dec call (which may call global.object.free
            // and clobber caller-saved registers).
            try compiler.registerTable.free(rhs_idx);
            try compiler.registerTable.free(lhs_idx);

            const free_fn = try gc.getFreeFnLabel(rhs_type, cctx, compiler.allocator);
            try gc.emitGcDec(old_reg, free_fn, compiler);
            try compiler.registerTable.free(old_idx);
            return;
        }
    }

    // Non-heap type: plain store, no GC needed.
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .register = rhs_reg },
        .to = .{ .registerOffset = .{
            .offset = 0,
            .register = lhs_reg,
        } },
    } });

    try compiler.registerTable.free(rhs_idx);
    try compiler.registerTable.free(lhs_idx);
}
