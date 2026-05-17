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
const gc = @import("../gc.zig");

pub fn codegenFuncall(f: *Ast.Funcall, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Check whether this is a call to an inbuilt function (plain or namespace-qualified).
    // resolveInbuiltFunc handles both `min(...)` and `strings.truncate(...)`.
    if (analyser.resolveInbuiltFunc(f.func, cctx, compiler.allocator)) |indef| {
        try codegen.inbuilt.codegenInbuiltFuncall(indef.name, f, compiler, cctx);
        return;
    }

    const raw_f_base = (try bbcTypes.getTypeOfValue(f.func, cctx, compiler.allocator)).decided.base;
    const f_type: Ast.TypeFunc = switch (raw_f_base) {
        .function => |ft| ft,
        .bound_method => |ft| ft,
        else => unreachable,
    };
    const vers = try bbcTypes.getFuncallVersion(f, f_type, cctx, compiler.allocator);
    const f_uid = compiler.getUid(analyser.functionVersion{ .name = f_type.fname, .signature = f_type, .version = vers });

    // Detect bound method variable call: f.func is an identifier with .bound_method type.
    // The receiver is stored at [bm_ptr + 8]; we load it and pass as self.
    var is_bound_method_var = false;
    if (raw_f_base == .bound_method) {
        is_bound_method_var = true;
    }

    // Detect direct method call: f.func is a field access on a concrete object (non-namespace).
    var is_method = is_bound_method_var;
    if (!is_bound_method_var and f.func.* == .unaryOperatorRight) {
        const base_type = try bbcTypes.getTypeOfValue(f.func.unaryOperatorRight.expr, cctx, compiler.allocator);
        if (base_type == .decided) {
            is_method = base_type.decided.base != .import_ns;
        }
    }

    // Section F: for bound-method calls, evaluate the bm pointer and load the receiver BEFORE
    // the live-reg snapshot so both survive the call via the caller-save push/pop mechanism.
    //
    // Two sub-cases:
    //   (a) Temporary bm — f.func is `t.method` (unaryOperatorRight): codegenUopRight allocates a
    //       24-byte bm heap object at _count=0.  We gc_inc it here to take ownership and gc_dec
    //       it after the call so the destructor runs and releases the receiver reference.
    //   (b) Variable bm — f.func is an identifier: the bm object is owned by the stack variable;
    //       scope exit will gc_dec it.  We only need to load the receiver here.
    var pre_bm_reg_idx: ?usize = null;   // set for case (a); kept live across call for post-call gc_dec
    var pre_recv_reg_idx: ?usize = null; // receiver register pre-loaded for both (a) and (b)

    if (is_bound_method_var) {
        try codegen.value.codegenValue(f.func, compiler, cctx);
        const bm_idx = compiler.registerTable.lastReg().?;
        const bm_r = try compiler.registerTable.getValue(bm_idx, compiler);

        const is_temp_bm = f.func.* == .unaryOperatorRight;
        if (is_temp_bm) {
            // Take ownership so gc_dec after the call can free it.
            try gc.emitGcInc(bm_r, compiler);
            pre_bm_reg_idx = bm_idx; // kept live; will be preserved by caller-save snapshot below
        } else {
            // Variable bm: only need the receiver pointer, not the bm itself.
            try compiler.registerTable.free(bm_idx);
        }

        // Load receiver from [bm_ptr + 8] into a fresh register.
        // This register is kept live so the caller-save snapshot preserves it across the call;
        // it is freed in the is_method push block below (right after being pushed as self).
        const recv_idx = try compiler.registerTable.allocate(compiler);
        const recv_r = try compiler.registerTable.getValue(recv_idx, compiler);
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .register = bm_r, .offset = 8 } },
            .to = recv_r,
        } });
        pre_recv_reg_idx = recv_idx;
    }

    const explicit_args = f.args.items.len;
    // For callwithargs: self counts as a positional arg slot (it maps to rdi for method calls).
    const total_args = if (is_method) explicit_args + 1 else explicit_args;
    const arg_regs = compiler.arch.argument_regs;
    const used_arg_regs = @min(total_args, arg_regs.len);

    // Push all currently live (non-spilled) register-table values before touching the stack.
    // All scratch registers are caller-saved; without this the callee clobbers live values
    // held by the caller (e.g. lhs in `fib(n-1) + fib(n-2)` is overwritten by fib(n-2)).
    // bm_ptr (pre_bm_reg_idx) and recv (pre_recv_reg_idx) are included here when live.
    var extra_live_regs = std.ArrayList(Inst.Register).init(compiler.allocator);
    defer extra_live_regs.deinit();
    const live_snapshot = try compiler.allocator.dupe(usize, compiler.registerTable.live.items);
    defer compiler.allocator.free(live_snapshot);
    for (live_snapshot) |live_idx| {
        const rs = &compiler.registerTable.states.items[live_idx];
        if (!rs.freed and !rs.spilled) {
            const reg = rs.reg.?;
            try compiler.addInstruction(.{ .push = .{ .reg = reg } });
            try extra_live_regs.append(reg);
        }
    }

    // Save argument registers that are currently in use (will be clobbered by args).
    var saved_regs = std.ArrayList(Inst.Register).init(compiler.allocator);
    defer saved_regs.deinit();
    for (arg_regs[0..used_arg_regs]) |reg| {
        try compiler.addInstruction(.{ .push = .{ .reg = reg } });
        try saved_regs.append(reg);
    }

    const stack_args = if (total_args > arg_regs.len) total_args - arg_regs.len else 0;
    const total_stack = compiler.arch.push_size + stack_args * compiler.arch.push_size;
    // ARM64: each push is 16 bytes so total_stack is always a multiple of 16 — no padding needed.
    const needs_padding = if (compiler.arch.target == .arm64) false else (total_stack % 16) != 8;
    if (needs_padding) {
        try compiler.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
    }

    // Push explicit args in reverse order (last arg first).
    for (0..explicit_args) |r_arg_idx| {
        const arg_idx = explicit_args - 1 - r_arg_idx;
        const arg = f.args.items[arg_idx];
        try codegen.value.codegenValue(arg, compiler, cctx);
        const arg_reg_idx = compiler.registerTable.lastReg().?;
        const arg_reg = try compiler.registerTable.getValue(arg_reg_idx, compiler);
        try compiler.addInstruction(.{ .push = .{ .reg = arg_reg } });
        try compiler.registerTable.free(arg_reg_idx);
    }

    // For method calls, push the receiver (self) as the last positional arg.
    // Stack layout after all pushes: [dummy][self/arg0][arg1]...[argN-1]
    // For free/namespace calls, only explicit args are pushed (no self slot).
    if (is_method) {
        if (is_bound_method_var) {
            // Receiver was pre-loaded into pre_recv_reg_idx before the live-reg snapshot.
            const recv_r = try compiler.registerTable.getValue(pre_recv_reg_idx.?, compiler);
            try compiler.addInstruction(.{ .push = .{ .reg = recv_r } });
            try compiler.registerTable.free(pre_recv_reg_idx.?);
        } else {
            try codegen.value.codegenValue(f.func.unaryOperatorRight.expr, compiler, cctx);
            const self_reg_idx = compiler.registerTable.lastReg().?;
            const self_reg = try compiler.registerTable.getValue(self_reg_idx, compiler);
            try compiler.addInstruction(.{ .push = .{ .reg = self_reg } });
            try compiler.registerTable.free(self_reg_idx);
        }
    }

    // Push dummy 0 — sits at [rsp] so arg[0]/self is at [rsp+8], arg[1] at [rsp+16], etc.
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = .R2 } });
    try compiler.addInstruction(.{ .push = .{ .reg = .R2 } });

    // Load in-register args from stack into argument registers.
    const ps = compiler.arch.push_size;
    for (0..used_arg_regs) |i| {
        const offset: i64 = @intCast((i + 1) * ps);
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .register = .RSP, .offset = offset } },
            .to = arg_regs[i],
        } });
    }
    // Pop in-reg args + dummy off the stack.
    try compiler.addInstruction(.{ .plus = .{
        .lhs = .RSP,
        .rhs = .{ .immediate = @intCast((used_arg_regs + 1) * ps) },
    } });
    // Reserve stack space for alignment without clobbering any argument register.
    // Using .minus (sub sp/rsp, N) instead of a push avoids zeroing arg_regs[2] (R2/x2/rdx).
    try compiler.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = @intCast(compiler.arch.push_size) } } });

    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = f_uid } } });

    // Remove dummy + any stack-passed args + optional alignment padding.
    const cleanup: usize = ps + stack_args * ps + if (needs_padding) ps else 0;
    if (cleanup > 0) {
        try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = @intCast(cleanup) } } });
    }

    // Restore saved argument registers in reverse order.
    var i = saved_regs.items.len;
    while (i > 0) {
        i -= 1;
        try compiler.addInstruction(.{ .pop = .{ .reg = saved_regs.items[i] } });
    }

    // Restore extra live registers (pop in reverse push order).
    var ei = extra_live_regs.items.len;
    while (ei > 0) {
        ei -= 1;
        try compiler.addInstruction(.{ .pop = .{ .reg = extra_live_regs.items[ei] } });
    }

    // Section F: gc_dec the temporary bound method heap object now that all registers
    // are restored.  pre_bm_reg_idx holds bm_ptr (preserved by the caller-save push/pop
    // above).  The destructor (global.BoundMethod.free) gc_decs the receiver and _frees
    // the bm allocation, so the receiver refcount stays balanced.
    //
    // global.object.free (called by gc_dec) clobbers rax (R0) — save it before the call
    // and restore it so the return-value move below still reads the correct value.
    if (pre_bm_reg_idx) |bm_idx| {
        const bm_r = try compiler.registerTable.getValue(bm_idx, compiler);
        try compiler.addInstruction(.{ .push = .{ .reg = .R0 } });
        compiler.incrementStackOffset();
        try gc.emitGcDec(bm_r, "global.BoundMethod.free", compiler);
        try compiler.addInstruction(.{ .pop = .{ .reg = .R0 } });
        compiler.decrementStackOffset();
        try compiler.registerTable.free(bm_idx);
    }

    const ret_idx = try compiler.registerTable.allocate(compiler);
    const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .register = .R0 },
        .to = ret_reg,
    } });
}
