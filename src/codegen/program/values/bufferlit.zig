const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");
const gc = @import("../gc.zig");

// @[elem0, elem1, ...]  →  malloc((n+2)*8), _count=1, _size=n, then store each element
pub fn codegenBufferLit(bl: *Ast.BufferLit, compiler: *Compiler, cctx: *analyser.Context) !void {
    const n = bl.elements.items.len;

    // Save all in-use caller-saved registers before the malloc call.
    for (Inst.CALLER_SAVED) |reg| {
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .push = .{ .reg = reg } });
            compiler.incrementStackOffset();
        }
    }

    // malloc((n + 2) * 8) — rdi is R5, must be free at this point (all live regs saved above)
    const byte_count: i64 = @intCast((n + 2) * 8);
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

    // _count = 0 at offset 0 (caller's gc_inc will bring it to 1)
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = 0 },
        .to = .{ .registerOffset = .{ .register = res_reg, .offset = 0 } },
    } });
    // _size = n at offset 8
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = @intCast(n) },
        .to = .{ .registerOffset = .{ .register = res_reg, .offset = 8 } },
    } });

    // Store each element at offset (2 + i) * 8.
    // GC: heap-typed elements are gc_inc'd so the buffer owns a reference.
    // The matching dec is in the buffer's destructor (global.Buffer_*.free).
    for (bl.elements.items, 0..) |elem, i| {
        try codegen.value.codegenValue(elem, compiler, cctx);
        const elem_idx = compiler.registerTable.lastReg().?;
        const elem_reg = try compiler.registerTable.getValue(elem_idx, compiler);
        const offset: i64 = @intCast((2 + i) * 8);
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .register = elem_reg },
            .to = .{ .registerOffset = .{ .register = res_reg, .offset = offset } },
        } });
        const elem_type = try bbcTypes.getTypeOfValue(elem, cctx, compiler.allocator);
        if (gc.shouldGcDecVar(elem_type, cctx)) {
            try gc.emitGcInc(elem_reg, compiler);
        }
        try compiler.registerTable.free(elem_idx);
    }

    compiler.registerTable.last_used = res_idx;
}
