const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;

// "hello"  →  malloc((n+2)*8), _count=1, _size=n, chars stored as 8-byte words
pub fn codegenStringLit(value: []const u8, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = cctx;
    const n = value.len;
    const byte_count: i64 = @intCast((n + 2) * 8);

    // Save all caller-saved registers that are currently in use.
    for (Inst.CALLER_SAVED) |reg| {
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .push = .{ .reg = reg } });
            compiler.incrementStackOffset();
        }
    }

    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = byte_count }, .to = .R5 } });
    try compiler.emitAlignedCall("_malloc", compiler.stack_size);

    // Restore caller-saved registers in reverse order.
    var ri = Inst.CALLER_SAVED.len;
    while (ri > 0) {
        ri -= 1;
        const reg = Inst.CALLER_SAVED[ri];
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .pop = .{ .reg = reg } });
            compiler.decrementStackOffset();
        }
    }

    const res_idx = try compiler.registerTable.allocate(compiler);
    const res_reg = try compiler.registerTable.getValue(res_idx, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = res_reg } });

    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = 0 },
        .to = .{ .registerOffset = .{ .register = res_reg, .offset = 0 } },
    } });
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = @intCast(n) },
        .to = .{ .registerOffset = .{ .register = res_reg, .offset = 8 } },
    } });

    for (value, 0..) |ch, i| {
        const offset: i64 = @intCast((2 + i) * 8);
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .immediate = @intCast(ch) },
            .to = .{ .registerOffset = .{ .register = res_reg, .offset = offset } },
        } });
    }

    compiler.registerTable.last_used = res_idx;
}
