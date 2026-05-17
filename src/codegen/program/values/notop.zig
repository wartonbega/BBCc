const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenNotOp(not: *Ast.NotOp, compiler: *Compiler, cctx: *analyser.Context) !void {
    try codegen.value.codegenValue(not.expr, compiler, cctx);
    const r_idx = compiler.registerTable.lastReg().?;
    const reg = try compiler.registerTable.getValue(r_idx, compiler);
    try compiler.addInstruction(.{ .xor = .{ .lhs = reg, .rhs = .{ .immediate = 1 } } });
    compiler.registerTable.last_used = r_idx;
}
