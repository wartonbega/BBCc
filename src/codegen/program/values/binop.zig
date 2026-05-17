const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const Inst = @import("../../instructions.zig");
const bbcTypes = @import("../../../types.zig");

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");
const gc = @import("../gc.zig");

fn isFloat(value: *const Ast.Value, cctx: *analyser.Context, allocator: std.mem.Allocator) bool {
    const t = bbcTypes.getTypeOfValue(@constCast(value), cctx, allocator) catch return false;
    if (t != .decided) return false;
    return t.decided.base == .name and std.mem.eql(u8, t.decided.base.name, "Float");
}

pub fn codegenBinop(binop: *Ast.binaryOperation, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Evaluate both operands first (non-short-circuit; And/Or check both sides).
    try codegen.value.codegenValue(binop.lhs, compiler, cctx);
    const lhs_idx = compiler.registerTable.lastReg().?;

    try codegen.value.codegenValue(binop.rhs, compiler, cctx);
    const rhs_idx = compiler.registerTable.lastReg().?;

    const rhs_reg = try compiler.registerTable.getValue(rhs_idx, compiler);
    const lhs_reg = try compiler.registerTable.getValue(lhs_idx, compiler);

    const lhs_is_float = isFloat(binop.lhs, cctx, compiler.allocator);

    if (lhs_is_float) {
        switch (binop.operator) {
            .Plus  => try compiler.addInstruction(.{ .float_binop = .{ .op = .Add, .lhs = lhs_reg, .rhs = rhs_reg } }),
            .Minus => try compiler.addInstruction(.{ .float_binop = .{ .op = .Sub, .lhs = lhs_reg, .rhs = rhs_reg } }),
            .Times => try compiler.addInstruction(.{ .float_binop = .{ .op = .Mul, .lhs = lhs_reg, .rhs = rhs_reg } }),
            .Div   => try compiler.addInstruction(.{ .float_binop = .{ .op = .Div, .lhs = lhs_reg, .rhs = rhs_reg } }),
            .Lt, .Gt, .Le, .Ge, .Equal, .NotEqual => {
                const true_label = try compiler.generateLabel("fcmp_true");
                const end_label  = try compiler.generateLabel("fcmp_end");
                // ucomisd sets CF/ZF; use unsigned condition codes.
                const cc: Inst.CodeCondition = switch (binop.operator) {
                    .Lt       => .B,
                    .Gt       => .A,
                    .Le       => .BE,
                    .Ge       => .AE,
                    .Equal    => .E,
                    .NotEqual => .NE,
                    else      => unreachable,
                };
                try compiler.addInstruction(.{ .float_cmp_flags = .{ .lhs = lhs_reg, .rhs = rhs_reg } });
                try compiler.addInstruction(.{ .jcond = .{ .cc = cc, .where = true_label.name } });
                try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = lhs_reg } });
                try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });
                try compiler.addInstruction(.{ .label = true_label });
                try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = lhs_reg } });
                try compiler.addInstruction(.{ .label = end_label });
            },
            else => unreachable,
        }
        try compiler.registerTable.free(rhs_idx);
        compiler.registerTable.last_used = lhs_idx;
        return;
    }

    switch (binop.operator) {
        .Plus => {
            const lhs_type = try bbcTypes.getTypeOfValue(binop.lhs, cctx, compiler.allocator);
            const is_string_concat = lhs_type == .decided and
                lhs_type.decided.base == .name and
                std.mem.eql(u8, lhs_type.decided.base.name, "String");

            if (is_string_concat) {
                // String + Char: call bbc_string_append_char(rdi=string_ptr, rsi=char)
                // bbc_string_append_char always allocates a NEW string without freeing the
                // old one.  Bump the old string's refcount now; after the call we release
                // that reference.  lhs_reg is caller-saved so the call may clobber it —
                // push a copy to the stack before the call and pop it after for gc_dec.
                try gc.emitGcInc(lhs_reg, compiler);

                // Save all live non-spilled caller-saved regs except lhs and rhs.
                const live_snapshot = try compiler.allocator.dupe(usize, compiler.registerTable.live.items);
                defer compiler.allocator.free(live_snapshot);
                var saved_regs = std.ArrayList(Inst.Register).init(compiler.allocator);
                defer saved_regs.deinit();
                for (live_snapshot) |live_idx| {
                    const rs = &compiler.registerTable.states.items[live_idx];
                    if (rs.freed or rs.spilled) continue;
                    const reg = rs.reg.?;
                    if (reg == lhs_reg or reg == rhs_reg) continue;
                    var is_caller_saved = false;
                    for (Inst.CALLER_SAVED) |cr| {
                        if (cr == reg) { is_caller_saved = true; break; }
                    }
                    if (!is_caller_saved) continue;
                    try compiler.addInstruction(.{ .push = .{ .reg = reg } });
                    compiler.incrementStackOffset();
                    try saved_regs.append(reg);
                }

                // Push an extra copy of lhs_reg so we have the old pointer available
                // for gc_dec after the call (bbc_string_append_char may clobber lhs_reg).
                try compiler.addInstruction(.{ .push = .{ .reg = lhs_reg } });
                compiler.incrementStackOffset();

                // Push args: rhs first (char), then lhs (string ptr).
                // Stack after: [rsp]=lhs, [rsp+ps]=rhs  →  pop R5=lhs(arg0), pop R4=rhs(arg1).
                try compiler.addInstruction(.{ .push = .{ .reg = rhs_reg } });
                compiler.incrementStackOffset();
                try compiler.addInstruction(.{ .push = .{ .reg = lhs_reg } });
                compiler.incrementStackOffset();
                try compiler.addInstruction(.{ .pop = .{ .reg = .R5 } }); // arg0 = string ptr
                compiler.decrementStackOffset();
                try compiler.addInstruction(.{ .pop = .{ .reg = .R4 } }); // arg1 = char
                compiler.decrementStackOffset();

                try compiler.emitAlignedCall("bbc_string_append_char", compiler.stack_size);

                // Pop the saved old-lhs pointer, save R0 (new string) around gc_dec
                // (the C call inside gc_dec clobbers R0), then load R0 into lhs_reg.
                const old_tmp_idx = try compiler.registerTable.allocate(compiler);
                const old_tmp_reg = try compiler.registerTable.getValue(old_tmp_idx, compiler);
                try compiler.addInstruction(.{ .pop = .{ .reg = old_tmp_reg } }); // old lhs ptr
                compiler.decrementStackOffset();
                try compiler.addInstruction(.{ .push = .{ .reg = .R0 } }); // save new string
                compiler.incrementStackOffset();
                try gc.emitGcDec(old_tmp_reg, "global.String.free", compiler);
                try compiler.registerTable.free(old_tmp_idx);
                try compiler.addInstruction(.{ .pop = .{ .reg = lhs_reg } }); // restore R0
                compiler.decrementStackOffset();

                // Restore saved registers in reverse order.
                var ri = saved_regs.items.len;
                while (ri > 0) {
                    ri -= 1;
                    try compiler.addInstruction(.{ .pop = .{ .reg = saved_regs.items[ri] } });
                    compiler.decrementStackOffset();
                }
            } else {
                try compiler.addInstruction(.{ .plus = .{ .lhs = lhs_reg, .rhs = .{ .register = rhs_reg } } });
            }
        },
        .Minus => {
            try compiler.addInstruction(.{ .minus = .{ .lhs = lhs_reg, .rhs = .{ .register = rhs_reg } } });
        },
        .Times => {
            try compiler.addInstruction(.{ .imul = .{ .lhs = lhs_reg, .rhs = .{ .register = rhs_reg } } });
        },
        .Div => {
            const div_ok_label = try compiler.generateLabel("div_ok");
            const div_end_label = try compiler.generateLabel("div_end");
            // Check divisor before pushing — avoids a QWORD-sized memory comparison.
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = rhs_reg }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = div_ok_label.name } });
            // Error path: divisor == 0. Nothing has been pushed yet so no cleanup.
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = .R5 } }); // mov rdi, 1
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "bbc_make_error_fn" } } });
            try compiler.addInstruction(.{ .store = .{ .from = .{ .immediate = 1 }, .to = .{ .rip_memory = "bbc_error_flag" } } });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .jmp = .{ .where = div_end_label.name } });
            // Ok path: divide.
            try compiler.addInstruction(.{ .label = div_ok_label });
            if (compiler.arch.target == .arm64) {
                try compiler.addInstruction(.{ .idiv = .{ .lhs = lhs_reg, .rhs = rhs_reg, .is_modulo = false } });
            } else {
                try compiler.addInstruction(.{ .push = .{ .reg = rhs_reg } });
                try compiler.addInstruction(.{ .load = .{ .from = .{ .register = lhs_reg }, .to = .R0 } });
                try compiler.addInstruction(.{ .cqo = .{} });
                try compiler.addInstruction(.{ .idiv_mem = .{} });
                try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
                try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = lhs_reg } });
            }
            try compiler.addInstruction(.{ .label = div_end_label });
            if (compiler.current_func_propagates_errors and !compiler.in_error_check_expr) {
                const ok = try compiler.generateLabel("div_propagate_ok");
                try compiler.emitEarlyReturnOnError(ok);
            }
        },
        .Modulus => {
            const mod_ok_label = try compiler.generateLabel("mod_ok");
            const mod_end_label = try compiler.generateLabel("mod_end");
            // Check divisor before pushing — same rationale as Div.
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = rhs_reg }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = mod_ok_label.name } });
            // Error path.
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = .R5 } });
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "bbc_make_error_fn" } } });
            try compiler.addInstruction(.{ .store = .{ .from = .{ .immediate = 1 }, .to = .{ .rip_memory = "bbc_error_flag" } } });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .jmp = .{ .where = mod_end_label.name } });
            // Ok path.
            try compiler.addInstruction(.{ .label = mod_ok_label });
            if (compiler.arch.target == .arm64) {
                // ARM64: IDIV with is_modulo=true leaves result in R3 (x3).
                try compiler.addInstruction(.{ .idiv = .{ .lhs = lhs_reg, .rhs = rhs_reg, .is_modulo = true } });
                try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R3 }, .to = lhs_reg } });
            } else {
                try compiler.addInstruction(.{ .push = .{ .reg = rhs_reg } });
                try compiler.addInstruction(.{ .load = .{ .from = .{ .register = lhs_reg }, .to = .R0 } });
                try compiler.addInstruction(.{ .cqo = .{} });
                try compiler.addInstruction(.{ .idiv_mem = .{} });
                try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
                // Modulus result is in rdx (R3).
                try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R3 }, .to = lhs_reg } });
            }
            try compiler.addInstruction(.{ .label = mod_end_label });
            if (compiler.current_func_propagates_errors and !compiler.in_error_check_expr) {
                const ok = try compiler.generateLabel("mod_propagate_ok");
                try compiler.emitEarlyReturnOnError(ok);
            }
        },
        // Comparisons: cmp lhs, rhs; branch to produce 0 or 1 in lhs_reg.
        .Lt, .Gt, .Le, .Ge, .Equal, .NotEqual => {
            const true_label = try compiler.generateLabel("cmp_true");
            const end_label = try compiler.generateLabel("cmp_end");
            const cc: Inst.CodeCondition = switch (binop.operator) {
                .Lt => .L,
                .Gt => .G,
                .Le => .LE,
                .Ge => .GE,
                .Equal => .E,
                .NotEqual => .NE,
                else => unreachable,
            };
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = lhs_reg }, .val2 = .{ .register = rhs_reg } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = cc, .where = true_label.name } });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });
            try compiler.addInstruction(.{ .label = true_label });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .label = end_label });
        },
        .And => {
            const false_label = try compiler.generateLabel("and_false");
            const end_label = try compiler.generateLabel("and_end");
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = lhs_reg }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = false_label.name } });
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = rhs_reg }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = false_label.name } });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });
            try compiler.addInstruction(.{ .label = false_label });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .label = end_label });
        },
        .Or => {
            const true_label = try compiler.generateLabel("or_true");
            const end_label = try compiler.generateLabel("or_end");
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = lhs_reg }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = true_label.name } });
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = rhs_reg }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = true_label.name } });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });
            try compiler.addInstruction(.{ .label = true_label });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = lhs_reg } });
            try compiler.addInstruction(.{ .label = end_label });
        },
    }

    try compiler.registerTable.free(rhs_idx);
    compiler.registerTable.last_used = lhs_idx;
}
