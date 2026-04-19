const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenAssignement(assign: *Ast.Assignement, compiler: *Compiler, cctx: *analyser.Context) !void {
    try codegen.value.codegenValueAdr(assign.lhs, compiler, cctx);
    const lhs_idx = compiler.registerTable.lastReg().?;

    try codegen.value.codegenValue(assign.rhs, compiler, cctx);
    const rhs_idx = compiler.registerTable.lastReg().?;

    const rhs_reg = try compiler.registerTable.getValue(rhs_idx, compiler);
    const lhs_reg = try compiler.registerTable.getValue(lhs_idx, compiler);

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
