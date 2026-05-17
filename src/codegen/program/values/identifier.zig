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

fn getFuncLabel(name: []const u8, compiler: *Compiler, cctx: *analyser.Context) ![]const u8 {
    const func_def = cctx.getFunction(name);
    const sig = try analyser.createFunctionSignature(func_def, compiler.allocator);
    const vers = std.StringHashMap(bbcTypes.Type).init(compiler.allocator);
    return compiler.getUid(analyser.functionVersion{
        .name = sig.base.function.fname,
        .signature = sig.base.function,
        .version = vers,
    });
}

pub fn codegenFunctionValue(func: *Ast.funcDef, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = cctx;
    const r_idx = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r_idx, compiler);
    const sig = try analyser.createFunctionSignature(func, compiler.allocator);
    const vers = std.StringHashMap(bbcTypes.Type).init(compiler.allocator);
    const f_uid = compiler.getUid(analyser.functionVersion{
        .name = sig.base.function.fname,
        .signature = sig.base.function,
        .version = vers,
    });
    try compiler.addInstruction(.{ .lea = .{
        .adr = .{ .label = f_uid },
        .to = reg,
    } });
    compiler.registerTable.last_used = r_idx;
}

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

    if (cctx.variableExist(ident.name)) {
        if (cctx.getVariableStackIndexOpt(ident.name)) |decal| {
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{
                    .register = .RBP,
                    .offset = -decal * 8,
                } },
                .to = reg,
            } });
        } else {
            // Function-type entry in the variables map — emit LEA to its label.
            const f_uid = try getFuncLabel(ident.name, compiler, cctx);
            try compiler.addInstruction(.{ .lea = .{
                .adr = .{ .label = f_uid },
                .to = reg,
            } });
        }
    } else if (cctx.functionExist(ident.name)) {
        const f_uid = try getFuncLabel(ident.name, compiler, cctx);
        try compiler.addInstruction(.{ .lea = .{
            .adr = .{ .label = f_uid },
            .to = reg,
        } });
    }
    compiler.registerTable.last_used = r_idx;
}
