const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");

const Compiler = @import("../../compiler.zig").Compiler;

pub fn codegenCharlit(value: u8, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = cctx;
    const r_idx = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(r_idx, compiler);
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .immediate = @intCast(value) },
        .to = reg,
    } });
    compiler.registerTable.last_used = r_idx;
}
