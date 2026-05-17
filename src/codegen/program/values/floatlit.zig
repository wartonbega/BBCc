const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Compiler = @import("../../compiler.zig").Compiler;

pub fn codegenFloatLit(value: f64, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = cctx;
    const bits: u64 = @bitCast(value);
    const label = try std.fmt.allocPrint(
        compiler.allocator,
        "bbc_fconst_{d}",
        .{compiler.float_constants.items.len},
    );
    try compiler.float_constants.append(.{ .label = label, .bits = bits });
    const idx = try compiler.registerTable.allocate(compiler);
    const dst = try compiler.registerTable.getValue(idx, compiler);
    try compiler.addInstruction(.{ .float_lit = .{ .dst = dst, .label = label } });
}
