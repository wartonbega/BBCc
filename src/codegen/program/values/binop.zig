const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenBinop(binop: *Ast.binaryOperation, compiler: *Compiler, cctx: *analyser.Context) !void {
    try codegen.value.codegenValue(binop.lhs, compiler, cctx);
    const lhs_idx = compiler.registerTable.lastReg().?;

    try codegen.value.codegenValue(binop.rhs, compiler, cctx);
    const rhs_idx = compiler.registerTable.lastReg().?;

    const rhs_reg = try compiler.registerTable.getValue(rhs_idx, compiler);
    const lhs_reg = try compiler.registerTable.getValue(lhs_idx, compiler);

    switch (binop.operator) {
        .Plus => {
            try compiler.addInstruction(.{ .plus = .{ .lhs = lhs_reg, .rhs = .{ .register = rhs_reg } } });
        },
        .Minus => {
            try compiler.addInstruction(.{ .minus = .{ .lhs = lhs_reg, .rhs = .{ .register = rhs_reg } } });
        },
        else => unreachable,
    }

    try compiler.registerTable.free(rhs_idx);

    compiler.registerTable.last_used = lhs_idx;
}
