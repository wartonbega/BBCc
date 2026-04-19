const std = @import("std");
const Parser = @import("../../../parser.zig");

const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenIdentifierAdr(ident: anytype, compiler: *Compiler, cctx: *analyser.Context) !void {
    const r_idx = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r_idx, compiler);

    if (cctx.variableExist(ident.name)) {
        const decal: i64 = @intCast(cctx.getVariableStackIndex(ident.name));
        try compiler.addInstruction(.{ .lea = .{
            .adr = .{ .registerOffset = .{
                .register = .RBP,
                .offset = -decal * 8,
            } },
            .to = reg,
        } });
    }
    compiler.registerTable.last_used = r_idx;
}

pub fn codegenIdentifier(ident: anytype, compiler: *Compiler, cctx: *analyser.Context) !void {
    const r_idx = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r_idx, compiler);

    if (cctx.functionExist(ident.name)) {
        // It's a function
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .immediate = 0 },
            .to = reg,
        } });
    } else if (cctx.variableExist(ident.name)) {
        const decal: i64 = @intCast(cctx.getVariableStackIndex(ident.name));
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{
                .register = .RBP,
                .offset = -decal * 8,
            } },
            .to = reg,
        } });
    }
    compiler.registerTable.last_used = r_idx;
}
