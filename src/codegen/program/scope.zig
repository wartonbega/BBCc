const std = @import("std");
const Ast = @import("../../ast.zig");
const bbcTypes = @import("../../types.zig");
const errors = @import("../../errors.zig");

const Inst = @import("../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../compiler.zig").Compiler;

const codegen = @import("codegenprog.zig");
const gc = @import("gc.zig");

/// Emit gc_dec for every heap-typed variable in `scope.ctx` at scope exit.
/// `skip_name`: the name of the scope-local whose gc_dec must be skipped
/// because the caller inherits its reference (the return value). Pass null
/// when all locals should be decremented (non-function scopes).
/// Must be called BEFORE `add rsp, size*8` so that [rbp - slot*8] is still
/// within the current frame.
pub fn emitScopeGcExit(scope: *const Ast.Scope, compiler: *Compiler, skip_name: ?[]const u8) !void {
    if (scope.code.items.len == 0) return;

    // Save `last_used` before the gc_dec loop so we can restore it afterward.
    // Each `allocate` call inside the loop overwrites `last_used`; without
    // this save/restore the caller would see a freed temp register as the
    // scope's return value.
    const saved_last_used = compiler.registerTable.last_used;

    // Decrement heap-typed vars (skip the protected return variable).
    var var_it = scope.ctx.variables.iterator();
    while (var_it.next()) |variable| {
        const var_name = variable.key_ptr.*;
        const var_type = variable.value_ptr.*;

        if (!gc.shouldGcDecVar(var_type, scope.ctx)) continue;
        if (skip_name) |rv| {
            if (std.mem.eql(u8, var_name, rv)) continue;
        }

        // Only dec variables in the current frame (positive slot = local).
        const slot = scope.ctx.variable_stack_loc.get(var_name) orelse continue;
        if (slot <= 0) continue;

        const tmp_idx = try compiler.registerTable.allocate(compiler);
        const tmp_reg = try compiler.registerTable.getValue(tmp_idx, compiler);
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .register = .RBP, .offset = -slot * 8 } },
            .to = tmp_reg,
        } });
        // scope.ctx walks the parent chain so typeDefExist resolves to root —
        // no need to pass an external cctx for destructor label resolution.
        const free_fn = try gc.getFreeFnLabel(var_type, scope.ctx, compiler.allocator);
        try gc.emitGcDec(tmp_reg, free_fn, compiler);
        try compiler.registerTable.free(tmp_idx);
    }

    // If there was a protected return variable, reload its register from the
    // stack slot — gc_dec calls (C calls internally) may have clobbered it.
    if (skip_name) |rv| {
        if (saved_last_used) |ret_idx| {
            if (scope.ctx.variable_stack_loc.get(rv)) |slot| {
                if (slot > 0) {
                    const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
                    try compiler.addInstruction(.{ .load = .{
                        .from = .{ .registerOffset = .{ .register = .RBP, .offset = -slot * 8 } },
                        .to = ret_reg,
                    } });
                }
            }
        }
    }

    // Restore last_used so the caller sees the scope's actual return register.
    compiler.registerTable.last_used = saved_last_used;
}

/// Compute which scope-local variable name (if any) is the return value and
/// should be skipped by emitScopeGcExit. Returns the name if the last
/// statement is a bare heap-typed identifier declared in this scope's ctx.
pub fn returnVarName(scope: *const Ast.Scope) ?[]const u8 {
    if (scope.code.items.len == 0) return null;
    // Unwrap parentheses so `(x)` is treated the same as `x`.
    var last = scope.code.items[scope.code.items.len - 1];
    while (last.* == .parenthesis) last = last.parenthesis;
    return switch (last.*) {
        .identifier => |id| blk: {
            if (scope.ctx.variables.get(id.name)) |id_type| {
                if (gc.shouldGcDecVar(id_type, scope.ctx)) break :blk id.name;
            }
            break :blk null;
        },
        else => null,
    };
}

/// Count the total number of local variable slots needed by a value and all its
/// nested scopes (recursive). For-loop values add 1 extra for the synthetic
/// iterator slot that forloop.zig allocates at runtime.
fn countSlotsInValue(val: *const Ast.Value) usize {
    return switch (val.*) {
        .scope => |s| countSlotsInScope(s),
        .If => |ifstmt| blk: {
            var s: usize = 0;
            for (ifstmt.scopes.items) |branch| s += countSlotsInValue(branch);
            if (ifstmt.elsescope) |es| s += countSlotsInValue(es);
            break :blk s;
        },
        .While => |wl| countSlotsInValue(wl.exec),
        .For => |fl| 1 + countSlotsInValue(fl.exec), // +1 for _for_iter_ synthetic slot
        .errorCheck => |ec| 1 + countSlotsInValue(ec.value) + countSlotsInValue(ec.scope), // +1 for err_name slot
        // Assignments and parentheses may wrap scopes — recurse into rhs / inner value.
        .assignement => |a| countSlotsInValue(a.rhs),
        .parenthesis => |p| countSlotsInValue(p),
        else => 0,
    };
}

/// Total local slots needed by this scope and all transitively nested scopes.
/// Used by funcdef.zig to pre-compute the full frame size before the prologue.
pub fn countSlotsInScope(scope: *const Ast.Scope) usize {
    var total: usize = scope.ctx.variables.count();
    for (scope.code.items) |val| total += countSlotsInValue(val);
    return total;
}

/// `skip_gc_exit` — pass true for function body scopes (called from codegenFuncdef)
/// where argument ownership and return-value protection are handled separately.
/// Pass false for control-flow body scopes (while/for/if branches) so that
/// heap-typed locals are decremented before the frame is released.
pub fn codegenScope(scope: *const Ast.Scope, compiler: *Compiler, skip_gc_exit: bool) !void {
    try compiler.addInstruction(.{ .comment = "Begin of scope" });

    // In fixed-frame mode, RSP is never adjusted — all locals live in the pre-allocated
    // frame. We only save stack_size for non-fixed mode where sub/add rsp track RSP.
    const pre_scope_stack_size = compiler.stack_size;

    // Pre-allocate stack slots for all variables declared in this scope.
    // Fixed frame: advance frame_slot_cursor (RSP unchanged).
    // Non-fixed:   advance stack_size and emit sub rsp.
    var var_it = scope.ctx.variables.iterator();
    var size: i64 = 0;
    while (var_it.next()) |variable| {
        if (compiler.frame_is_fixed) {
            compiler.frame_slot_cursor += 1;
            try scope.ctx.putVariableStackIndex(variable.key_ptr.*, compiler.frame_slot_cursor);
        } else {
            compiler.incrementStackOffset();
            try scope.ctx.putVariableStackIndex(variable.key_ptr.*, compiler.stack_size);
        }
        size += 1;
    }
    if (!compiler.frame_is_fixed) {
        try compiler.addInstruction(.{ .minus = .{
            .lhs = .RSP,
            .rhs = .{ .immediate = size * 8 },
        } });
    }

    // When this scope is the expression inside `value ? err fallback`, we must
    // NOT exit the function early on error (that would bypass the handler).
    // Instead, when an error is detected after a non-final statement:
    //   - save rax (the ErrorObj* set by bbc_make_error_fn) into a fresh register
    //   - jump past the remaining statements and the GC exit directly to cleanup
    // The scope then "returns" the ErrorObj* as its value, and codegenErrorCheck
    // reads the flag, pops it, and evaluates the fallback.
    //
    // This structure is only needed when:
    //   a) we're inside a ? err expression (in_error_check_expr = true), AND
    //   b) the scope has >1 statements (single-statement scopes return the
    //      error directly without any inter-statement checks).
    const needs_err_exit = compiler.in_error_check_expr and scope.code.items.len > 1;
    const scope_err_exit_label = if (needs_err_exit)
        try compiler.generateLabel("scope_err_exit")
    else
        @as(?Inst.LABEL, null);
    const scope_normal_exit_label = if (needs_err_exit)
        try compiler.generateLabel("scope_normal_exit")
    else
        @as(?Inst.LABEL, null);
    const scope_cleanup_label = if (needs_err_exit)
        try compiler.generateLabel("scope_cleanup")
    else
        @as(?Inst.LABEL, null);

    for (scope.code.items, 0..) |val, i| {
        try codegen.value.codegenValue(val, compiler, scope.ctx);
        if (i != scope.code.items.len - 1) {
            // Discard the result register if the statement produced one.
            // For heap-typed results, gc_dec before freeing so the destructor
            // runs and the allocation is not leaked (e.g. discarded foo() calls).
            if (compiler.registerTable.lastReg()) |disc_idx| {
                const disc_type = try bbcTypes.getTypeOfValue(val, scope.ctx, compiler.allocator);
                // Cache the decided type in the register so callers that only
                // have the register index (not the original AST node) can query
                // it via registerTable.getType() without re-querying cctx.
                if (disc_type == .decided) {
                    compiler.registerTable.setType(disc_idx, disc_type.decided);
                }
                if (gc.shouldGcDecVar(disc_type, scope.ctx)) {
                    const disc_reg = try compiler.registerTable.getValue(disc_idx, compiler);
                    const free_fn = try gc.getFreeFnLabel(disc_type, scope.ctx, compiler.allocator);
                    try gc.emitGcDec(disc_reg, free_fn, compiler);
                }
                try compiler.registerTable.free(disc_idx);
            }
            if (!compiler.registerTable.isCorrectlyFreed()) {
                std.debug.print("Not all registers are freed!\n", .{});
                std.debug.print("Free registers ({d}/{d}):\n", .{
                    compiler.registerTable.free_regs.items.len,
                    compiler.registerTable.initial_reg_count,
                });
                for (compiler.registerTable.free_regs.items) |r| {
                    std.debug.print("  {}\n", .{r});
                }
                unreachable;
            }
            if (needs_err_exit) {
                // Inside a ? err block: don't exit the function — jump to the
                // error-exit block so the ? err handler can catch the error.
                try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .rip_memory = "bbc_error_flag" }, .val2 = .{ .immediate = 0 } } });
                try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = scope_err_exit_label.?.name } });
            } else if (compiler.current_func_propagates_errors) {
                // Outside any ? err: exit the function immediately on error.
                const ok = try compiler.generateLabel("err_propagate_ok");
                try compiler.emitEarlyReturnOnError(ok);
            }
        }
    }

    // Normal path: jump over the error-exit block to the GC exit.
    if (needs_err_exit) {
        try compiler.addInstruction(.{ .jmp = .{ .where = scope_normal_exit_label.?.name } });

        // Error-exit block: all registers are freed at the point each inter-statement
        // check jumps here. rax (R0) still holds the ErrorObj* set by bbc_make_error_fn
        // — nothing on this path calls a C function that would clobber it.
        // GC exit is skipped (same trade-off as emitEarlyReturnOnError: memory leak on
        // the error path, but no crash and the ? err handler can catch the error).
        // codegenErrorCheck reads rax directly on the error path, so no register
        // allocation is needed here.
        try compiler.addInstruction(.{ .label = scope_err_exit_label.? });
        try compiler.addInstruction(.{ .jmp = .{ .where = scope_cleanup_label.?.name } });

        // Normal-exit label: GC exit runs here for the non-error path.
        try compiler.addInstruction(.{ .label = scope_normal_exit_label.? });
    }

    // GC scope exit: dec heap-typed locals before releasing the frame.
    // For control-flow scopes the return variable (last heap identifier in this
    // scope) must be skipped so the parent inherits the reference.
    if (!skip_gc_exit) {
        try emitScopeGcExit(scope, compiler, returnVarName(scope));
    }

    // Both paths converge here for RSP cleanup.
    if (needs_err_exit) {
        try compiler.addInstruction(.{ .label = scope_cleanup_label.? });
    }

    // In non-fixed mode, undo the sub rsp and restore the stack_size counter.
    // In fixed-frame mode, RSP was never adjusted; slot cursor is NOT restored
    // so that sequential scopes claim fresh (non-overlapping) slots.
    if (!compiler.frame_is_fixed) {
        try compiler.addInstruction(.{ .plus = .{
            .lhs = .RSP,
            .rhs = .{ .immediate = size * 8 },
        } });
        compiler.stack_size = pre_scope_stack_size;
    }

    try compiler.addInstruction(.{ .comment = "End of scope" });

}
