const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenVardecAdr(vardec: *Ast.VarDeclaration, compiler: *Compiler, cctx: *analyser.Context) !void {
    const r_idx = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r_idx, compiler);

    const decal: i64 = @intCast(cctx.getVariableStackIndex(vardec.name));
    try compiler.addInstruction(.{ .lea = .{
        .adr = .{ .registerOffset = .{
            .register = .RBP,
            .offset = -decal * 8,
        } },
        .to = reg,
    } });
    compiler.registerTable.last_used = r_idx;
}
