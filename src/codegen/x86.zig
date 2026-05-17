const std = @import("std");

const inst = @import("instructions.zig");
const Compiler = @import("compiler.zig").Compiler;

const Allocator = std.mem.Allocator;

const FileWriter = std.io.Writer(std.fs.File, std.fs.File.WriteError, std.fs.File.write);

pub fn dumpAssemblyX86(compiler: *Compiler, entry_point: []const u8) !void {
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
        \\default rel
        \\    section .text align=16
        \\    global {s} ;; entry point
        \\extern _malloc
        \\extern _calloc
        \\extern _free
        \\extern _printf
        \\extern _putchar
        \\
    , .{entry_point});
    for (compiler.program.items) |instruction| {
        try instruction.toAsm(writer, alloc, compiler.arch);
    }

    // Runtime data: printf format strings and error flag.
    _ = try writer.print(
        \\
        \\section .data
        \\bbc_fmt_int_ln:    db "%ld", 10, 0
        \\bbc_fmt_int:       db "%ld", 0
        \\bbc_fmt_float_ln:  db "%g", 10, 0
        \\bbc_fmt_float:     db "%g", 0
        \\bbc_fmt_bool_t_ln: db "true", 10, 0
        \\bbc_fmt_bool_t:    db "true", 0
        \\bbc_fmt_bool_f_ln: db "false", 10, 0
        \\bbc_fmt_bool_f:    db "false", 0
        \\bbc_fmt_char_ln:   db "%c", 10, 0
        \\bbc_fmt_char:      db "%c", 0
        \\bbc_fmt_newline:   db 10, 0
        \\bbc_fmt_buf_open:  db "[", 0
        \\bbc_fmt_buf_close: db "]", 0
        \\bbc_fmt_buf_sep:   db ", ", 0
        \\bbc_error_flag:    dq 0
        \\bbc_uncaught_error_msg: db "Uncaught error", 10, 0
        \\
    , .{});

    // Float literal constants: each stored as a raw IEEE-754 double-word.
    for (compiler.float_constants.items) |fc| {
        try writer.print("{s}: dq {d}\n", .{ fc.label, fc.bits });
    }

    _ = try writer.print(
        \\
        \\section .text
        \\; global.BoundMethod.free — destructor for a bound method heap object.
        \\; Layout: [_count@0][receiver@8][receiver_free_fn@16]  (24 bytes)
        \\; Calling convention (via global.object.free.liberate): self at [rsp+32] after push rbx.
        \\global.BoundMethod.free:
        \\    push rbx
        \\    mov rax, [rsp+32]              ; rax = self (bound method ptr)
        \\    mov rcx, [rax+8]               ; rcx = receiver ptr
        \\    test rcx, rcx
        \\    jz bbc_bm_free_self            ; null receiver — skip gc_dec
        \\    mov rdx, [rax+16]              ; rdx = receiver_free_fn absolute address
        \\    push rdx                       ; push free_fn first (higher address)
        \\    push rcx                       ; push receiver second (lower address)
        \\    call global.object.free
        \\    add rsp, 16
        \\bbc_bm_free_self:
        \\    mov rdi, [rsp+32]              ; reload self (rsp+32 after add rsp,16 / after push rbx)
        \\    call _free
        \\    pop rbx
        \\    ret
        \\
        \\; bbc_make_error_fn(rdi=error_code) -> rax=*ErrorObj
        \\; Layout: [_count @0][error_code @8]  (16 bytes)
        \\; Handles arbitrary call-site stack alignment via rbp.
        \\bbc_make_error_fn:
        \\    push rbp
        \\    mov rbp, rsp
        \\    and rsp, -16
        \\    sub rsp, 16
        \\    mov [rsp], rdi
        \\    mov rdi, 16
        \\    call _malloc
        \\    mov rdi, [rsp]
        \\    mov rsp, rbp
        \\    mov QWORD [rax], 1
        \\    mov QWORD [rax + 8], rdi
        \\    pop rbp
        \\    ret
        \\
        \\; bbc_string_append_char(rdi=*StringObj, rsi=char_val) -> rax=*StringObj
        \\; StringObj layout: [_count@0][_size@8][c0@16][c1@24]...
        \\; Always allocates a new StringObj with _count=0 (GC takes ownership on assignment).
        \\bbc_string_append_char:
        \\    push rbp
        \\    mov rbp, rsp
        \\    and rsp, -16
        \\    sub rsp, 32
        \\    mov [rsp], rdi          ; save old string ptr
        \\    mov [rsp+8], rsi        ; save char value
        \\    mov rax, [rdi+8]        ; rax = old _size
        \\    mov [rsp+16], rax       ; save old_size
        \\    ; alloc (old_size + 3) * 8 bytes for new string
        \\    add rax, 3
        \\    imul rax, rax, 8
        \\    mov rdi, rax
        \\    call _malloc
        \\    ; rax = new string ptr; reload saved values
        \\    mov r8, [rsp+16]        ; old_size
        \\    mov r9, [rsp]           ; old string ptr
        \\    mov r10, [rsp+8]        ; char value
        \\    ; init header
        \\    mov QWORD [rax], 0      ; _count = 0
        \\    lea r11, [r8+1]
        \\    mov [rax+8], r11        ; _size = old_size + 1
        \\    ; copy old chars
        \\    xor rcx, rcx
        \\bbc_str_concat_loop:
        \\    cmp rcx, r8
        \\    jge bbc_str_concat_end
        \\    mov rdx, [r9 + rcx*8 + 16]
        \\    mov [rax + rcx*8 + 16], rdx
        \\    inc rcx
        \\    jmp bbc_str_concat_loop
        \\bbc_str_concat_end:
        \\    ; store new char at index old_size
        \\    mov [rax + r8*8 + 16], r10
        \\    mov rsp, rbp
        \\    pop rbp
        \\    ret
        \\
        \\; global.Buffer_Buffer.free — destructor for Buffer<Buffer<T>>.
        \\; Iterates over inner buffer elements calling global.object.free(elem, global.Buffer.free),
        \\; then frees the outer block.
        \\; Calling convention: same as all T.free destructors (via global.object.free.liberate).
        \\; After this function's own push rbx: self (outer Buffer ptr) is at [rsp+32].
        \\global.Buffer_Buffer.free:
        \\    push rbx
        \\    mov rax, [rsp+32]             ; rax = outer buffer ptr
        \\    xor rbx, rbx                  ; rbx = loop index = 0
        \\bbc_bbb_loop:
        \\    cmp rbx, QWORD [rax+8]        ; cmp index vs _size
        \\    jge bbc_bbb_done
        \\    mov rdi, [rax + rbx*8 + 16]   ; rdi = inner buffer ptr
        \\    test rdi, rdi
        \\    jz bbc_bbb_next
        \\    lea rdx, [global.Buffer.free]
        \\    push rdx
        \\    push rdi
        \\    call global.object.free
        \\    add rsp, 16
        \\    mov rax, [rsp+32]             ; reload outer ptr (rax clobbered by call)
        \\bbc_bbb_next:
        \\    inc rbx
        \\    jmp bbc_bbb_loop
        \\bbc_bbb_done:
        \\    mov rdi, [rsp+32]
        \\    call _free
        \\    pop rbx
        \\    ret
        \\
        \\; bbc_strings_truncate(rdi=*StringObj, rsi=from_idx, rdx=to_idx) -> rax=*StringObj (or ErrorObj on error)
        \\; StringObj layout: [_count@0][_size@8][c0@16][c1@24]...
        \\; Returns a new StringObj with chars [from..to). Sets bbc_error_flag on OOB.
        \\bbc_strings_truncate:
        \\    push rbp
        \\    mov rbp, rsp
        \\    and rsp, -16
        \\    sub rsp, 48
        \\    mov [rsp],    rdi          ; save src string ptr
        \\    mov [rsp+8],  rsi          ; save from
        \\    mov [rsp+16], rdx          ; save to
        \\    ; validate: from <= to
        \\    cmp rsi, rdx
        \\    jle bbc_trunc_check_bounds
        \\    ; error: from > to
        \\    mov rdi, 1
        \\    call bbc_make_error_fn
        \\    mov QWORD [rel bbc_error_flag], 1
        \\    mov rsp, rbp
        \\    pop rbp
        \\    ret
        \\bbc_trunc_check_bounds:
        \\    ; validate: to <= _size
        \\    mov r8, [rdi+8]            ; r8 = src._size
        \\    cmp rdx, r8
        \\    jle bbc_trunc_alloc
        \\    ; error: to > _size
        \\    mov rdi, 1
        \\    call bbc_make_error_fn
        \\    mov QWORD [rel bbc_error_flag], 1
        \\    mov rsp, rbp
        \\    pop rbp
        \\    ret
        \\bbc_trunc_alloc:
        \\    ; new_size = to - from
        \\    mov rax, [rsp+16]          ; to
        \\    sub rax, [rsp+8]           ; rax = to - from
        \\    mov [rsp+24], rax          ; save new_size
        \\    ; alloc (new_size + 2) * 8 bytes
        \\    lea rdi, [rax+2]
        \\    imul rdi, rdi, 8
        \\    call _malloc
        \\    mov [rsp+32], rax          ; save new string ptr
        \\    ; init header: _count=0, _size=new_size
        \\    mov QWORD [rax], 0
        \\    mov rcx, [rsp+24]          ; new_size
        \\    mov [rax+8], rcx
        \\    ; copy chars: src[from..to) -> dst[0..new_size)
        \\    mov r8, [rsp]              ; src ptr
        \\    mov r9, [rsp+8]            ; from
        \\    xor rcx, rcx
        \\bbc_trunc_copy:
        \\    cmp rcx, [rsp+24]
        \\    jge bbc_trunc_done
        \\    lea r11, [r9+rcx]          ; r11 = from + i
        \\    mov rdx, [r8 + r11*8 + 16] ; src[from+i]
        \\    mov r10, [rsp+32]
        \\    mov [r10 + rcx*8 + 16], rdx
        \\    inc rcx
        \\    jmp bbc_trunc_copy
        \\bbc_trunc_done:
        \\    mov rax, [rsp+32]
        \\    mov rsp, rbp
        \\    pop rbp
        \\    ret
        \\
    , .{});
}
