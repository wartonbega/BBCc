const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenWhileLoop(wl: *Ast.WhileLoop, compiler: *Compiler, cctx: *analyser.Context) !void {
    const cond_label = try compiler.generateLabel("while_cond");
    const end_label = try compiler.generateLabel("while_end");

    try compiler.addInstruction(.{ .label = cond_label });

    try codegen.value.codegenValue(wl.condition, compiler, cctx);
    const cond_idx = compiler.registerTable.lastReg().?;
    const cond_reg = try compiler.registerTable.getValue(cond_idx, compiler);

    try compiler.addInstruction(.{ .cmp = .{
        .val1 = .{ .register = cond_reg },
        .val2 = .{ .immediate = 0 },
    } });
    try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = end_label.name } });
    try compiler.registerTable.free(cond_idx);

    try codegen.value.codegenValue(wl.exec, compiler, cctx);
    if (compiler.registerTable.lastReg()) |body_idx| {
        try compiler.registerTable.free(body_idx);
    }

    try compiler.addInstruction(.{ .jmp = .{ .where = cond_label.name } });
    try compiler.addInstruction(.{ .label = end_label });
}
