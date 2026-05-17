const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

// @[Type](n)  →  malloc((n + 2) * 8), write _count=1, _size=n, return pointer
pub fn codegenBufferAlloc(ba: *Ast.BufferAlloc, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Evaluate the size expression
    try codegen.value.codegenValue(ba.size, compiler, cctx);
    const size_idx = compiler.registerTable.lastReg().?;
    const size_reg = try compiler.registerTable.getValue(size_idx, compiler);

    // Save the original size to the stack BEFORE the calloc argument setup.
    // size_reg may be R5 (rdi), which would be clobbered by the `mov rdi, 1`
    // (nelem) instruction below.  A plain push/pop around the whole calloc
    // sequence preserves the original value independently of register table
    // save/restore, which only snapshots registers at the point of the push.
    try compiler.addInstruction(.{ .push = .{ .reg = size_reg } });
    compiler.incrementStackOffset();

    // Compute byte count: (size + 2) * 8 → shl size, 3 then add 16.
    // Use calloc(1, total_bytes) so slots are zero-initialized — avoids
    // GC-dec of garbage when a slot is first assigned.
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = size_reg }, .to = .R4 } }); // rsi = size
    try compiler.addInstruction(.{ .shl = .{ .lhs = .R4, .imm = 3 } });
    try compiler.addInstruction(.{ .plus = .{ .lhs = .R4, .rhs = .{ .immediate = 16 } } });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = .R5 } }); // rdi = 1 (nmemb)

    // Save in-use caller-saved registers before calloc.
    for (Inst.CALLER_SAVED) |reg| {
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .push = .{ .reg = reg } });
            compiler.incrementStackOffset();
        }
    }

    try compiler.emitAlignedCall("_calloc", compiler.stack_size);

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

    // rax now holds the allocated pointer.
    const res_idx = try compiler.registerTable.allocate(compiler);
    const res_reg = try compiler.registerTable.getValue(res_idx, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = res_reg } });

    // Pop the original size that we saved before the calloc setup.
    const saved_size_idx = try compiler.registerTable.allocate(compiler);
    const saved_size_reg = try compiler.registerTable.getValue(saved_size_idx, compiler);
    try compiler.addInstruction(.{ .pop = .{ .reg = saved_size_reg } });
    compiler.decrementStackOffset();

    // _count = 0 at offset 0 (caller's gc_inc will bring it to 1)
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = 0 },
        .to = .{ .registerOffset = .{ .register = res_reg, .offset = 0 } },
    } });
    // _size = n at offset 8 — use the saved original size, not size_reg which
    // may have been clobbered by the calloc argument setup.
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .register = saved_size_reg },
        .to = .{ .registerOffset = .{ .register = res_reg, .offset = 8 } },
    } });

    try compiler.registerTable.free(saved_size_idx);
    try compiler.registerTable.free(size_idx);
    compiler.registerTable.last_used = res_idx;
}
