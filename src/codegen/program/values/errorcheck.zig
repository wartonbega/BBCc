const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;
const codegen = @import("../codegenprog.zig");

// value ? err_name fallback
//
// Stack discipline (RSP = R0 on entry):
//   push val            → RSP = R0-8   (both paths start here)
//   [flag check, no branch]
//
//   ERROR PATH:
//     add rsp,8         → RSP = R0     discard invalid val
//     sub rsp,8         → RSP = R0-8   err_name slot
//     evaluate fallback → result in register
//     add rsp,8         → RSP = R0     free err_name slot  ← BEFORE push
//     push result       → RSP = R0-8
//     jmp end
//
//   OK PATH:
//     (val still on stack at R0-8, no RSP changes)
//
//   end_label:
//     pop result        → RSP = R0
//
// Invariant: exactly one word on the stack at end_label in both paths.
pub fn codegenErrorCheck(errcheck: *Ast.ErrCheck, compiler: *Compiler, cctx: *analyser.Context) !void {
    const ok_label = try compiler.generateLabel("errcheck_ok");
    const end_label = try compiler.generateLabel("errcheck_end");

    // Evaluate the expression that may produce an error.
    // Suppress inner early-returns so the ? err handler below can catch any error.
    const prev_in_err = compiler.in_error_check_expr;
    compiler.in_error_check_expr = true;
    try codegen.value.codegenValue(errcheck.value, compiler, cctx);
    compiler.in_error_check_expr = prev_in_err;

    // Save last_used BEFORE allocating the flag register — allocate() will overwrite
    // last_used, and we need to restore it so the ok path can read the inner result.
    const inner_last_used = compiler.registerTable.last_used;

    // Read and test the error flag.
    const flag_idx = try compiler.registerTable.allocate(compiler);
    const flag_reg = try compiler.registerTable.getValue(flag_idx, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .rip_memory = "bbc_error_flag" }, .to = flag_reg } });
    try compiler.addInstruction(.{ .cmp = .{
        .val1 = .{ .register = flag_reg },
        .val2 = .{ .immediate = 0 },
    } });
    try compiler.registerTable.free(flag_idx);

    // Restore last_used so the ok path below can call lastReg() correctly.
    compiler.registerTable.last_used = inner_last_used;

    try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = ok_label.name } });

    // ----- Error path -----
    // rax (R0) always holds the ErrorObj* when bbc_error_flag=1: either set directly
    // by bbc_make_error_fn, or propagated from a callee's rax.  We read from rax
    // directly rather than from the inner expression's result register because:
    //   a) for multi-statement scopes the error may have fired in a non-final statement,
    //      leaving the inner register freed/stale, and
    //   b) rax is never clobbered on the error-exit path (no C calls on that path).
    // Consume (discard) inner_last_used to keep the register table clean.
    compiler.registerTable.last_used = null;
    const errptr_idx = try compiler.registerTable.allocate(compiler);
    const errptr_reg = try compiler.registerTable.getValue(errptr_idx, compiler);
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = errptr_reg } });

    const errcode_idx = try compiler.registerTable.allocate(compiler);
    const errcode_reg = try compiler.registerTable.getValue(errcode_idx, compiler);
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .registerOffset = .{ .register = errptr_reg, .offset = 8 } },
        .to = errcode_reg,
    } });

    // Free the ErrorObj. It has no heap-typed fields, so _free suffices.
    // bbc_make_error_fn sets _count=1; the ? err handler is the sole consumer.
    // Save all live caller-saved registers across the _free call.
    for (Inst.CALLER_SAVED) |reg| {
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .push = .{ .reg = reg } });
            compiler.incrementStackOffset();
        }
    }
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = errptr_reg }, .to = .R5 } });
    try compiler.emitAlignedCall("_free", compiler.stack_size);
    var errfree_ri = Inst.CALLER_SAVED.len;
    while (errfree_ri > 0) {
        errfree_ri -= 1;
        const reg = Inst.CALLER_SAVED[errfree_ri];
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .pop = .{ .reg = reg } });
            compiler.decrementStackOffset();
        }
    }

    try compiler.registerTable.free(errptr_idx);

    try compiler.addInstruction(.{ .store = .{ .from = .{ .immediate = 0 }, .to = .{ .rip_memory = "bbc_error_flag" } } });

    // Allocate a stack slot for err_name, bind it to the error code.
    // In fixed-frame mode the slot is already pre-reserved by countSlotsInValue (+1 for errorCheck).
    // Just advance frame_slot_cursor so the store address is correct; don't emit sub sp.
    // In non-fixed mode, pushStackSlot emits sub rsp/sp and tracks the slot.
    if (compiler.frame_is_fixed) {
        compiler.frame_slot_cursor += 1;
    } else {
        try compiler.pushStackSlot();
    }
    const err_slot = if (compiler.frame_is_fixed) compiler.frame_slot_cursor else compiler.stack_size;
    const err_type = try bbcTypes.CreateTypeError(compiler.allocator, false);
    try cctx.createVariable(errcheck.err, err_type, errcheck.reference);
    try cctx.putVariableStackIndex(errcheck.err, err_slot);
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .register = errcode_reg },
        .to = .{ .registerOffset = .{ .register = .RBP, .offset = -@as(i64, err_slot) * 8 } },
    } });
    try compiler.registerTable.free(errcode_idx);

    // Evaluate the fallback — result lands in a register.
    try codegen.value.codegenValue(errcheck.scope, compiler, cctx);
    const fallback_idx = compiler.registerTable.lastReg();

    // Release the err_name slot (only in non-fixed mode; fixed frame cursor stays).
    if (!compiler.frame_is_fixed) {
        try compiler.popStackSlot();
    }

    if (fallback_idx) |fi| {
        const fallback_reg = try compiler.registerTable.getValue(fi, compiler);
        try compiler.addInstruction(.{ .push = .{ .reg = fallback_reg } });
        try compiler.registerTable.free(fi);
    }
    try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });

    // ----- Ok path -----
    // The inner expression returned normally; its result is in the register
    // referenced by inner_last_used.  The error-path code gen above called
    // lastReg() (for the fallback expression) which cleared last_used to null,
    // so we must restore inner_last_used here before calling lastReg().
    try compiler.addInstruction(.{ .label = ok_label });
    compiler.registerTable.last_used = inner_last_used;
    const val_idx = compiler.registerTable.lastReg().?;
    const val_reg = try compiler.registerTable.getValue(val_idx, compiler);
    try compiler.addInstruction(.{ .push = .{ .reg = val_reg } });
    try compiler.registerTable.free(val_idx);

    // ----- Merge -----
    try compiler.addInstruction(.{ .label = end_label });
    const result_idx = try compiler.registerTable.allocate(compiler);
    const result_reg = try compiler.registerTable.getValue(result_idx, compiler);
    try compiler.addInstruction(.{ .pop = .{ .reg = result_reg } });
    compiler.registerTable.last_used = result_idx;
}
