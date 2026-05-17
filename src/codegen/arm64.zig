const std = @import("std");
const inst = @import("instructions.zig");
const Compiler = @import("compiler.zig").Compiler;

pub fn dumpAssemblyArm64(compiler: *Compiler, entry_point: []const u8) !void {
    const file = try std.fs.cwd().createFile(
        "output.asm",
        .{ .read = false, .truncate = true },
    );
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const writer = file.writer();

    _ = try writer.print(
        \\.section __TEXT,__text,regular,pure_instructions
        \\.globl {s}
        \\
    , .{entry_point});

    for (compiler.program.items) |instruction| {
        try instruction.toAsm(writer, alloc, compiler.arch);
    }

    // Runtime data section
    _ = try writer.print(
        \\
        \\.section __DATA,__data
        \\bbc_fmt_int_ln:    .asciz "%ld\n"
        \\bbc_fmt_int:       .asciz "%ld"
        \\bbc_fmt_float_ln:  .asciz "%g\n"
        \\bbc_fmt_float:     .asciz "%g"
        \\bbc_fmt_bool_t_ln: .asciz "true\n"
        \\bbc_fmt_bool_t:    .asciz "true"
        \\bbc_fmt_bool_f_ln: .asciz "false\n"
        \\bbc_fmt_bool_f:    .asciz "false"
        \\bbc_fmt_char_ln:   .asciz "%c\n"
        \\bbc_fmt_char:      .asciz "%c"
        \\bbc_fmt_newline:   .asciz "\n"
        \\bbc_fmt_buf_open:  .asciz "["
        \\bbc_fmt_buf_close: .asciz "]"
        \\bbc_fmt_buf_sep:   .asciz ", "
        \\bbc_error_flag:    .quad 0
        \\bbc_uncaught_error_msg: .asciz "Uncaught error\n"
        \\
    , .{});

    // Float literal constants
    for (compiler.float_constants.items) |fc| {
        try writer.print("{s}: .quad {d}\n", .{ fc.label, fc.bits });
    }

    _ = try writer.print(
        \\
        \\.section __TEXT,__text
        \\
        \\// global.object.free — generic GC destructor dispatcher.
        \\// Calling convention (stack-based, same as x86-64 variant):
        \\//   [sp+0]  = object ptr  (pushed second, lower address)
        \\//   [sp+16] = free_fn ptr (pushed first, higher address)
        \\// After the liberate call, sp is restored by the caller (add sp, sp, #32).
        \\global.object.free:
        \\    stp x19, x30, [sp, #-16]!
        \\    ldr x19, [sp, #16]          // object ptr
        \\    cbz x19, global_object_free_null
        \\    ldr x16, [sp, #32]          // free_fn ptr
        \\global.object.free.liberate:
        \\    // Layout: [_count@0][payload@8...] (ptr_size=8 per field)
        \\    ldr x0, [x19]               // x0 = refcount
        \\    subs x0, x0, #1
        \\    str x0, [x19]
        \\    b.ne global_object_free_done
        \\    // refcount hit 0 — call destructor: free_fn(object_ptr)
        \\    // free_fn address is already in x16
        \\    mov x0, x19
        \\    blr x16
        \\global_object_free_done:
        \\global_object_free_null:
        \\    ldp x19, x30, [sp], #16
        \\    ret
        \\
        \\// global.BoundMethod.free — destructor for a bound method heap object.
        \\// Layout: [_count@0][receiver@8][receiver_free_fn@16]  (24 bytes)
        \\// Called via blr from global.object.free with x0 = self (BM ptr).
        \\global.BoundMethod.free:
        \\    stp x19, x30, [sp, #-16]!
        \\    mov x19, x0                    // x19 = self (BM ptr), passed in x0
        \\    ldr x0, [x19, #8]              // x0 = receiver ptr
        \\    cbz x0, bbc_bm_free_self
        \\    ldr x16, [x19, #16]            // x16 = receiver_free_fn
        \\    str x16, [sp, #-16]!           // push free_fn
        \\    str x0, [sp, #-16]!            // push receiver
        \\    bl global.object.free
        \\    add sp, sp, #32
        \\bbc_bm_free_self:
        \\    mov x0, x19                    // x0 = self
        \\    bl _free
        \\    ldp x19, x30, [sp], #16
        \\    ret
        \\
        \\// bbc_make_error_fn(x0=error_code) -> x0=*ErrorObj
        \\// Layout: [_count @0][error_code @8]  (16 bytes)
        \\bbc_make_error_fn:
        \\    stp x19, x30, [sp, #-16]!
        \\    mov x19, x0                    // save error_code
        \\    mov x0, #16
        \\    bl _malloc
        \\    mov x1, #1
        \\    str x1, [x0]                   // _count = 1
        \\    str x19, [x0, #8]              // error_code
        \\    ldp x19, x30, [sp], #16
        \\    ret
        \\
        \\// bbc_string_append_char(x0=*StringObj, x1=char_val) -> x0=*StringObj
        \\// StringObj layout: [_count@0][_size@8][c0@16][c1@24]...
        \\bbc_string_append_char:
        \\    stp x19, x30, [sp, #-32]!     // x19, x30 + 16 bytes padding
        \\    stp x20, x21, [sp, #16]
        \\    mov x19, x0                    // save old string ptr
        \\    mov x20, x1                    // save char value
        \\    ldr x21, [x0, #8]             // x21 = old _size
        \\    // alloc (old_size + 3) * 8 bytes
        \\    add x0, x21, #3
        \\    lsl x0, x0, #3
        \\    bl _malloc
        \\    // x0 = new string ptr
        \\    mov x2, #0
        \\    str x2, [x0]                   // _count = 0
        \\    add x2, x21, #1
        \\    str x2, [x0, #8]               // _size = old_size + 1
        \\    // copy old chars
        \\    mov x3, #0
        \\bbc_str_concat_loop:
        \\    cmp x3, x21
        \\    b.ge bbc_str_concat_end
        \\    add x4, x19, x3, lsl #3
        \\    ldr x5, [x4, #16]
        \\    add x4, x0, x3, lsl #3
        \\    str x5, [x4, #16]
        \\    add x3, x3, #1
        \\    b bbc_str_concat_loop
        \\bbc_str_concat_end:
        \\    // store new char at index old_size
        \\    add x4, x0, x21, lsl #3
        \\    str x20, [x4, #16]
        \\    ldp x20, x21, [sp, #16]
        \\    ldp x19, x30, [sp], #32
        \\    ret
        \\
        \\// global.Buffer_Buffer_.free — destructor for Buffer<Buffer<T>>.
        \\global.Buffer_Buffer_.free:
        \\    stp x29, x30, [sp, #-16]!
        \\    mov x29, sp
        \\    str x19, [sp, #-16]!
        \\    str x20, [sp, #-16]!
        \\    str x21, [sp, #-16]!
        \\    mov x19, x0                   // x19 = outer buffer ptr (self arrives in x0)
        \\    mov x20, #0                   // x20 = loop index
        \\bbc_bbb_loop:
        \\    ldr x21, [x19, #8]            // x21 = _size
        \\    cmp x20, x21
        \\    b.ge bbc_bbb_done
        \\    add x0, x19, x20, lsl #3
        \\    ldr x0, [x0, #16]             // inner buffer ptr
        \\    cbz x0, bbc_bbb_next
        \\    adrp x1, global.Buffer.free@PAGE
        \\    add x1, x1, global.Buffer.free@PAGEOFF
        \\    str x1, [sp, #-16]!
        \\    str x0, [sp, #-16]!
        \\    bl global.object.free
        \\    add sp, sp, #32
        \\bbc_bbb_next:
        \\    add x20, x20, #1
        \\    b bbc_bbb_loop
        \\bbc_bbb_done:
        \\    mov x0, x19
        \\    bl _free
        \\    ldr x21, [sp], #16
        \\    ldr x20, [sp], #16
        \\    ldr x19, [sp], #16
        \\    ldp x29, x30, [sp], #16
        \\    ret
        \\
        \\// bbc_strings_truncate(x0=*StringObj, x1=from_idx, x2=to_idx) -> x0=*StringObj (or ErrorObj)
        \\bbc_strings_truncate:
        \\    stp x19, x30, [sp, #-48]!     // 48 = 16-byte multiple >= 6*8
        \\    stp x20, x21, [sp, #16]
        \\    stp x22, x23, [sp, #32]
        \\    mov x19, x0                    // src ptr
        \\    mov x20, x1                    // from
        \\    mov x21, x2                    // to
        \\    // validate: from <= to
        \\    cmp x20, x21
        \\    b.le bbc_trunc_check_bounds
        \\    mov x0, #1
        \\    bl bbc_make_error_fn
        \\    adrp x16, bbc_error_flag@PAGE
        \\    mov x1, #1
        \\    str x1, [x16, bbc_error_flag@PAGEOFF]
        \\    ldp x22, x23, [sp, #32]
        \\    ldp x20, x21, [sp, #16]
        \\    ldp x19, x30, [sp], #48
        \\    ret
        \\bbc_trunc_check_bounds:
        \\    ldr x22, [x19, #8]             // x22 = src._size
        \\    cmp x21, x22
        \\    b.le bbc_trunc_alloc
        \\    mov x0, #1
        \\    bl bbc_make_error_fn
        \\    adrp x16, bbc_error_flag@PAGE
        \\    mov x1, #1
        \\    str x1, [x16, bbc_error_flag@PAGEOFF]
        \\    ldp x22, x23, [sp, #32]
        \\    ldp x20, x21, [sp, #16]
        \\    ldp x19, x30, [sp], #48
        \\    ret
        \\bbc_trunc_alloc:
        \\    sub x22, x21, x20              // x22 = new_size = to - from
        \\    add x0, x22, #2
        \\    lsl x0, x0, #3
        \\    bl _malloc
        \\    mov x23, x0                    // x23 = new string ptr
        \\    mov x1, #0
        \\    str x1, [x0]                   // _count = 0
        \\    str x22, [x0, #8]              // _size = new_size
        \\    mov x2, #0
        \\bbc_trunc_copy:
        \\    cmp x2, x22
        \\    b.ge bbc_trunc_done
        \\    add x3, x20, x2               // from + i
        \\    add x4, x19, x3, lsl #3
        \\    ldr x5, [x4, #16]
        \\    add x4, x23, x2, lsl #3
        \\    str x5, [x4, #16]
        \\    add x2, x2, #1
        \\    b bbc_trunc_copy
        \\bbc_trunc_done:
        \\    mov x0, x23
        \\    ldp x22, x23, [sp, #32]
        \\    ldp x20, x21, [sp, #16]
        \\    ldp x19, x30, [sp], #48
        \\    ret
        \\
    , .{});
}
