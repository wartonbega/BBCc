const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;
const gc = @import("../gc.zig");

const codegen = @import("../codegenprog.zig");

/// Thin wrapper around compiler.emitAlignedCall kept for call-site readability.
fn emitAlignedCall(label: []const u8, stack_size: i64, compiler: *Compiler) !void {
    try compiler.emitAlignedCall(label, stack_size);
}

// Emits a printf/putchar call for a single value.
// Saves/restores rdi (R5) around the call.
fn printOne(arg: *Ast.Value, ln: bool, compiler: *Compiler, cctx: *analyser.Context) !void {
    const arg_type = try bbcTypes.getTypeOfValue(arg, cctx, compiler.allocator);

    try codegen.value.codegenValue(arg, compiler, cctx);
    const val_idx = compiler.registerTable.lastReg().?;
    const val_reg = try compiler.registerTable.getValue(val_idx, compiler);

    const type_name: []const u8 = switch (arg_type) {
        .decided => |t| switch (t.base) {
            .name => |n| n,
            .buffer => "buffer",
            else => "Int",
        },
        .undecided => "Int",
    };

    const is_buffer = switch (arg_type) {
        .decided => |t| switch (t.base) {
            .buffer => true,
            else => false,
        },
        .undecided => false,
    };
    // For heap-typed args: gc_inc before use, gc_dec after.
    // Named variables (count >= 1) end up net-zero; temporaries (count = 0)
    // get freed after the print.  This mirrors the interpreter's checkReference.
    const is_heap_arg = gc.shouldGcDecVar(arg_type, cctx);
    if (is_heap_arg) try gc.emitGcInc(val_reg, compiler);

    if (is_buffer) {
        const elem_type = arg_type.decided.base.buffer;
        const elem_name: []const u8 = switch (elem_type.base) {
            .name => |n| n,
            else => "Int",
        };
        const loop_label = try compiler.generateLabel("buf_print_loop");
        const end_label = try compiler.generateLabel("buf_print_end");
        // r12, r13, rbx pushed below → stack depth increases by 3 for inner calls
        const inner_stack = compiler.stack_size + 3;

        // push r12, r13, rbx; mov r12, val_reg; mov r13, [r12+8]; xor rbx, rbx
        try compiler.addInstruction(.{ .push = .{ .reg = .R10 } });
        try compiler.addInstruction(.{ .push = .{ .reg = .R11 } });
        try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = val_reg }, .to = .R10 } });
        try compiler.addInstruction(.{ .load = .{ .from = .{ .registerOffset = .{ .register = .R10, .offset = 8 } }, .to = .R11 } });
        try compiler.addInstruction(.{ .xor = .{ .lhs = .R1, .rhs = .{ .register = .R1 } } });

        // Print "["
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_buf_open" }, .to = .R5 } });
        try emitAlignedCall("_printf", inner_stack, compiler);

        try compiler.addInstruction(.{ .label = loop_label });
        try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R1 }, .val2 = .{ .register = .R11 } } });
        try compiler.addInstruction(.{ .jcond = .{ .cc = .GE, .where = end_label.name } });

        // Load element into rsi (second printf arg): mov rsi, [r12 + rbx*8 + 16]
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .sib = .{ .base = .R10, .index = .R1, .scale = 8, .disp = 16 } },
            .to = .R4,
        } });

        if (std.mem.eql(u8, elem_name, "Char")) {
            if (compiler.arch.target == .arm64) {
                // Use putchar on ARM64: move element (R4=x1) into R5 (x0), call putchar.
                try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R4 }, .to = .R5 } });
                try emitAlignedCall("_putchar", inner_stack, compiler);
            } else {
                try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_char" }, .to = .R5 } });
                try emitAlignedCall("_printf", inner_stack, compiler);
            }
        } else if (std.mem.eql(u8, elem_name, "Bool")) {
            const bool_true_lbl = try compiler.generateLabel("buf_elem_bool_true");
            const bool_end_lbl = try compiler.generateLabel("buf_elem_bool_end");
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R4 }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = bool_true_lbl.name } });
            try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_bool_f" }, .to = .R5 } });
            try emitAlignedCall("_printf", inner_stack, compiler);
            try compiler.addInstruction(.{ .jmp = .{ .where = bool_end_lbl.name } });
            try compiler.addInstruction(.{ .label = bool_true_lbl });
            try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_bool_t" }, .to = .R5 } });
            try emitAlignedCall("_printf", inner_stack, compiler);
            try compiler.addInstruction(.{ .label = bool_end_lbl });
        } else {
            // Int (default): value is in R4 (rsi on x86-64, x1 on ARM64)
            try compiler.emitPrintfInt(.R4, "bbc_fmt_int", inner_stack);
        }

        // Print ", " separator unless this is the last element
        // lea rax, [rbx + 1]; cmp rax, r13
        const sep_skip_lbl = try compiler.generateLabel("buf_sep_skip");
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R1 }, .to = .R0 } });
        try compiler.addInstruction(.{ .inc = .{ .lhs = .R0 } });
        try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R0 }, .val2 = .{ .register = .R11 } } });
        try compiler.addInstruction(.{ .jcond = .{ .cc = .GE, .where = sep_skip_lbl.name } });
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_buf_sep" }, .to = .R5 } });
        try emitAlignedCall("_printf", inner_stack, compiler);
        try compiler.addInstruction(.{ .label = sep_skip_lbl });

        try compiler.addInstruction(.{ .inc = .{ .lhs = .R1 } });
        try compiler.addInstruction(.{ .jmp = .{ .where = loop_label.name } });

        try compiler.addInstruction(.{ .label = end_label });
        // pop rbx, r13; then gc_dec R10 while it still holds the Buffer ptr; then pop r12
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R11 } });
        if (is_heap_arg) {
            const free_fn = try gc.getFreeFnLabel(arg_type, cctx, compiler.allocator);
            try gc.emitGcDec(.R10, free_fn, compiler);
        }
        try compiler.addInstruction(.{ .pop = .{ .reg = .R10 } });

        // Print "]"
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_buf_close" }, .to = .R5 } });
        try emitAlignedCall("_printf", compiler.stack_size, compiler);

        if (ln) {
            try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_newline" }, .to = .R5 } });
            try emitAlignedCall("_printf", compiler.stack_size, compiler);
        }
    } else if (std.mem.eql(u8, type_name, "String")) {
        // Print each character in the string (layout: [_count][_size][ch0][ch1]...).
        // Move string pointer to r12 (callee-saved) so it survives printf calls.
        // Use r13 for _size, rbx for loop index.
        const loop_label = try compiler.generateLabel("str_print_loop");
        const end_label = try compiler.generateLabel("str_print_end");
        // After pushing r12, r13, rbx the effective depth increases by 3 for alignment.
        const inner_stack = compiler.stack_size + 3;

        // push r12, r13, rbx; mov r12, val_reg; mov r13, [r12+8]; xor rbx, rbx
        try compiler.addInstruction(.{ .push = .{ .reg = .R10 } });
        try compiler.addInstruction(.{ .push = .{ .reg = .R11 } });
        try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = val_reg }, .to = .R10 } });
        try compiler.addInstruction(.{ .load = .{ .from = .{ .registerOffset = .{ .register = .R10, .offset = 8 } }, .to = .R11 } });
        try compiler.addInstruction(.{ .xor = .{ .lhs = .R1, .rhs = .{ .register = .R1 } } });

        try compiler.addInstruction(.{ .label = loop_label });
        try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R1 }, .val2 = .{ .register = .R11 } } });
        try compiler.addInstruction(.{ .jcond = .{ .cc = .GE, .where = end_label.name } });
        // mov rdi, [r12 + rbx*8 + 16]
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .sib = .{ .base = .R10, .index = .R1, .scale = 8, .disp = 16 } },
            .to = .R5,
        } });
        try emitAlignedCall("_putchar", inner_stack, compiler);
        try compiler.addInstruction(.{ .inc = .{ .lhs = .R1 } });
        try compiler.addInstruction(.{ .jmp = .{ .where = loop_label.name } });
        try compiler.addInstruction(.{ .label = end_label });
        // pop rbx, r13; then gc_dec R10 while it still holds the String ptr; then pop r12
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R11 } });
        if (is_heap_arg) {
            const free_fn = try gc.getFreeFnLabel(arg_type, cctx, compiler.allocator);
            try gc.emitGcDec(.R10, free_fn, compiler);
        }
        try compiler.addInstruction(.{ .pop = .{ .reg = .R10 } });

        if (ln) {
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 10 }, .to = .R5 } });
            try emitAlignedCall("_putchar", compiler.stack_size, compiler);
        }
    } else if (std.mem.eql(u8, type_name, "Bool")) {
        // printf(bool_fmt_string)  — value selects which label
        const true_label = try compiler.generateLabel("print_bool_true");
        const end_label = try compiler.generateLabel("print_bool_end");

        const fmt_true = if (ln) "bbc_fmt_bool_t_ln" else "bbc_fmt_bool_t";
        const fmt_false = if (ln) "bbc_fmt_bool_f_ln" else "bbc_fmt_bool_f";

        try compiler.addInstruction(.{ .cmp = .{
            .val1 = .{ .register = val_reg },
            .val2 = .{ .immediate = 0 },
        } });
        try compiler.addInstruction(.{ .jcond = .{ .cc = .NZ, .where = true_label.name } });

        // false branch
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = fmt_false }, .to = .R5 } });
        try emitAlignedCall("_printf", compiler.stack_size, compiler);
        try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });

        try compiler.addInstruction(.{ .label = true_label });
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = fmt_true }, .to = .R5 } });
        try emitAlignedCall("_printf", compiler.stack_size, compiler);

        try compiler.addInstruction(.{ .label = end_label });
    } else if (std.mem.eql(u8, type_name, "Float")) {
        const fmt = if (ln) "bbc_fmt_float_ln" else "bbc_fmt_float";
        try compiler.addInstruction(.{ .float_print = .{
            .val = val_reg,
            .fmt_label = fmt,
            .stack_size = @intCast(compiler.stack_size),
        } });
    } else if (std.mem.eql(u8, type_name, "Char")) {
        // Use putchar instead of printf("%c") to safely handle null chars (printf asserts on macOS).
        // Null char (0) means no output; still emit the newline if ln.
        const skip_label = try compiler.generateLabel("putchar_skip");
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = val_reg }, .to = .R5 } }); // rdi = char val
        try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R5 }, .val2 = .{ .immediate = 0 } } });
        try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = skip_label.name } });
        try emitAlignedCall("_putchar", compiler.stack_size, compiler);
        try compiler.addInstruction(.{ .label = skip_label });
        if (ln) {
            try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 10 }, .to = .R5 } });
            try emitAlignedCall("_putchar", compiler.stack_size, compiler);
        }
    } else {
        // Int / default: printf("%ld", val)
        const fmt = if (ln) "bbc_fmt_int_ln" else "bbc_fmt_int";
        try compiler.emitPrintfInt(val_reg, fmt, compiler.stack_size);
    }

    try compiler.registerTable.free(val_idx);
}

pub fn codegenPrint(print: anytype, compiler: *Compiler, cctx: *analyser.Context) !void {
    const n = print.args.items.len;
    for (print.args.items, 0..) |arg, i| {
        const is_last = i == n - 1;
        try printOne(arg, print.ln and is_last, compiler, cctx);
    }
    if (n == 0 and print.ln) {
        // println with no args: just emit a newline
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_fmt_newline" }, .to = .R5 } });
        try emitAlignedCall("_printf", compiler.stack_size, compiler);
    }
}
