const std = @import("std");
const analyser = @import("analyser.zig");
const Ast = @import("ast.zig");
const bbcTypes = @import("types.zig");
const errors = @import("errors.zig");
const Inst = @import("codegen/instructions.zig");
const Compiler = @import("codegen/compiler.zig").Compiler;

const codegen = @import("codegen/program/codegenprog.zig");

/// Replace characters invalid in NASM identifiers (from generic type names) with underscores.
fn sanitize(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, name.len);
    for (name, 0..) |c, i| {
        buf[i] = switch (c) {
            '<', '>', ',', ' ', '!' => '_',
            else => c,
        };
    }
    return buf;
}

/// Strip `!` from inside generic type-parameter brackets, e.g.
/// `Foo<!Bar>` → `Foo<Bar>`.  `!` outside brackets (error-union return
/// type marker) is left untouched.  Returns a slice owned by `allocator`.
fn stripErrFromGenericParams(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var out = try std.ArrayList(u8).initCapacity(allocator, name.len);
    var depth: usize = 0;
    for (name) |c| {
        switch (c) {
            '<' => { depth += 1; try out.append(c); },
            '>' => { depth -|= 1; try out.append(c); },
            '!' => if (depth == 0) try out.append(c), // keep ! outside <>
            else => try out.append(c),
        }
    }
    return out.toOwnedSlice();
}

/// Register a specialized Buffer-of-T free function in `needed` if the
/// element type T is heap-managed.  `needed` maps the buffer-free label to
/// the element-free label.
fn collectBufElemFree(
    elem: *Ast.Type,
    cctx: *analyser.Context,
    needed: *std.StringHashMap([]const u8),
    alloc: std.mem.Allocator,
) !void {
    switch (elem.base) {
        .name => |name| {
            if (cctx.typeDefExist(name) or std.mem.eql(u8, name, "String")) {
                const safe = try sanitize(name, alloc);
                const buf_lbl = try std.fmt.allocPrint(alloc, "global.Buffer_{s}_.free", .{safe});
                const elem_lbl = try std.fmt.allocPrint(alloc, "global.{s}.free", .{safe});
                try needed.put(buf_lbl, elem_lbl);
            }
        },
        .generic => |g| {
            const safe = try sanitize(g.name, alloc);
            const buf_lbl = try std.fmt.allocPrint(alloc, "global.Buffer_{s}_.free", .{safe});
            const elem_lbl = try std.fmt.allocPrint(alloc, "global.{s}.free", .{safe});
            try needed.put(buf_lbl, elem_lbl);
        },
        .buffer => {
            try needed.put("global.Buffer_Buffer_.free", "global.Buffer.free");
        },
        else => {},
    }
}

pub fn generateProgram(prog: *Ast.Program, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = prog;

    // Compile all functions, handling dynamic discovery of new specializations.
    //
    // Problem with the original two-loop approach:
    //   Loop 1 registered UIDs; Loop 2 compiled. During Loop 2, codegenFuncdef
    //   could call addFunctionToCompile, appending to functions_to_compile while
    //   the slice-based for loop was still iterating → use-after-free / segfault.
    //   Newly added functions also never got UIDs → [INTERN ERROR].
    //
    // Fix: index-based loops. `uid_frontier` tracks how many UIDs have been
    // registered. Before compiling each function, we flush the frontier so every
    // currently-known function has its UID registered (a callee's UID must exist
    // before the caller is compiled, even if the callee is compiled later).
    var uid_frontier: usize = 0;
    var fi: usize = 0;
    while (fi < cctx.functions_to_compile.items.len) : (fi += 1) {
        // Register UIDs for all functions discovered so far (including ones just
        // added by previous codegenFuncdef calls) before compiling index fi.
        // Error-union variants (e.g. Foo<!T>) normalise to their base (Foo<T>):
        // they produce identical code, so they reuse that function's UID.
        while (uid_frontier < cctx.functions_to_compile.items.len) : (uid_frontier += 1) {
            const f = cctx.functions_to_compile.items[uid_frontier];
            const norm = try stripErrFromGenericParams(f.name, compiler.allocator);
            // If a non-error version is already registered, reuse its UID.
            const uid = blk: {
                if (!std.mem.eql(u8, norm, f.name)) {
                    for (compiler.func_uid_list.items) |entry| {
                        if (std.mem.eql(u8, entry.version.name, norm)) {
                            break :blk entry.uid;
                        }
                    }
                }
                break :blk try codegen.funcdef.generateFuncUID(f.name, compiler);
            };
            try compiler.func_uid_list.append(.{ .uid = uid, .version = f });
        }
        // Skip compilation for error-union variants — their normalised base was
        // already compiled (or will be) and the UID redirect above handles calls.
        const cur = cctx.functions_to_compile.items[fi];
        const norm_cur = try stripErrFromGenericParams(cur.name, compiler.allocator);
        const is_err_variant = !std.mem.eql(u8, norm_cur, cur.name);
        const base_ctx_missing = cctx.getFunction(cur.name).ctx == null;
        if (is_err_variant or base_ctx_missing) continue;
        try codegen.funcdef.codegenFuncdef(&cctx.functions_to_compile.items[fi], compiler, cctx);
    }

    // MAIN WRAPPER — macOS C runtime entry point (_main).
    // Calls BBC `main`. If the error flag is set on return, print "Uncaught error" and exit 1.
    try compiler.addInstruction(.{ .label = .{ .name = compiler.getMainWrapper() } });
    try compiler.addInstruction(.{ .frame_enter = .{} });
    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "main" } } });
    // Check for uncaught error from main.
    try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .rip_memory = "bbc_error_flag" }, .val2 = .{ .immediate = 0 } } });
    try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = "bbc_main_ok" } });
    // Print "Uncaught error\n" and exit with code 1.
    try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = "bbc_uncaught_error_msg" }, .to = .R5 } });
    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_printf" } } });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = .R0 } });
    // ARM64: C runtime reads return value from x0 (R5/ABI), but BBC uses x9 (R0). Copy R0→R5.
    if (compiler.arch.target == .arm64)
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = .R5 } });
    try compiler.addInstruction(.{ .frame_leave = .{ .emit_ret = true } });
    try compiler.addInstruction(.{ .label = .{ .name = "bbc_main_ok" } });
    try compiler.addInstruction(.{ .xor = .{ .lhs = .R0, .rhs = .{ .register = .R0 } } }); // return 0 (success)
    // ARM64: C runtime reads return value from x0 (R5/ABI), but BBC uses x9 (R0). Copy R0→R5.
    if (compiler.arch.target == .arm64)
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = .R5 } });
    try compiler.addInstruction(.{ .frame_leave = .{ .emit_ret = true } });

    // Generating the free functions for the objects
    // Stack layout inside any global.{T}.free, after its own `push R1`:
    //   [rsp+0]  = saved R1 (this destructor's push R1)
    //   [rsp+8]  = ret addr back to global.object.free.liberate
    //   [rsp+16] = saved R1 from global.object.free
    //   [rsp+24] = ret addr from the bbc caller to global.object.free
    //   [rsp+32] = self (the object pointer)
    var struct_it = cctx.typedef.iterator();
    while (struct_it.next()) |stdef| {
        const s_name = stdef.key_ptr.*;
        // String is handled by the explicit global.String.free emitter below.
        if (std.mem.eql(u8, s_name, "String")) continue;
        const safe_s_name = try sanitize(s_name, compiler.allocator);
        const label_name = try std.fmt.allocPrint(
            compiler.allocator,
            "global.{s}.{s}",
            .{ safe_s_name, "free" },
        );
        try compiler.addInstruction(.{ .label = .{ .name = label_name } });
        if (compiler.arch.target == .arm64) {
            // ARM64: use frame_enter (saves x29+x30), then save self (x0/R5) into R1 (x19).
            try compiler.addInstruction(.{ .frame_enter = .{} });
            try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R5 }, .to = .R1 } });
        } else {
            try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });
        }

        for (stdef.value_ptr.*.fields.items) |field| {
            const field_type = stdef.value_ptr.*.getHabitant(field);
            const child_free_function: ?[]const u8 = switch (field_type.base) {
                .name => |name| blk: {
                    if (cctx.typeDefExist(name) or std.mem.eql(u8, name, "String"))
                        break :blk try std.fmt.allocPrint(compiler.allocator, "global.{s}.free", .{try sanitize(name, compiler.allocator)});
                    break :blk null;
                },
                .generic => |g| blk: {
                    if (cctx.typeDefExist(g.name))
                        break :blk try std.fmt.allocPrint(compiler.allocator, "global.{s}.free", .{try sanitize(g.name, compiler.allocator)});
                    break :blk null;
                },
                .buffer => |elem| blk: {
                    const buf_free: []const u8 = switch (elem.base) {
                        .name => |name| if (cctx.typeDefExist(name) or std.mem.eql(u8, name, "String"))
                            try std.fmt.allocPrint(compiler.allocator, "global.Buffer_{s}_.free", .{try sanitize(name, compiler.allocator)})
                        else
                            "global.Buffer.free",
                        .generic => |g| try std.fmt.allocPrint(compiler.allocator, "global.Buffer_{s}_.free", .{try sanitize(g.name, compiler.allocator)}),
                        .buffer => "global.Buffer_Buffer_.free",
                        else => "global.Buffer.free",
                    };
                    break :blk buf_free;
                },
                else => null,
            };
            if (child_free_function) |free_fn| {
                if (compiler.arch.target == .arm64) {
                    // ARM64: R1 (x19) holds self. Load field from [R1 + field_offset].
                    const field_offset: i64 = @intCast(8 * stdef.value_ptr.*.getHabitantIndex(field));
                    try compiler.addInstruction(.{ .load = .{
                        .from = .{ .registerOffset = .{ .offset = field_offset, .register = .R1 } },
                        .to = .R0,
                    } });
                    // Push free_fn FIRST (higher address), field_ptr (R0) SECOND (lower/top).
                    try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = free_fn }, .to = .R5 } });
                    try compiler.addInstruction(.{ .push = .{ .reg = .R5 } }); // free_fn first
                    try compiler.addInstruction(.{ .push = .{ .reg = .R0 } }); // field_ptr second
                    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "global.object.free" } } });
                    try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 32 } } });
                    // Restore R1 (x19 was saved by global.object.free via stp, so it's intact,
                    // but reload self from R1 in case it was clobbered — it wasn't; x19 is callee-saved).
                } else {
                    // x86-64: Load self from [rsp+32] into R1, then load the field ptr into R0.
                    try compiler.addInstruction(.{ .load = .{
                        .from = .{ .registerOffset = .{ .offset = 32, .register = .RSP } },
                        .to = .R1,
                    } });
                    try compiler.addInstruction(.{ .load = .{
                        .from = .{
                            .registerOffset = .{
                                .offset = @intCast(8 * stdef.value_ptr.*.getHabitantIndex(field)),
                                .register = .R1,
                            },
                        },
                        .to = .R0,
                    } });

                    // Push free_fn FIRST (higher address), field_ptr SECOND (lower/top).
                    // global.object.free reads: self=[rsp+16], free_fn=[rsp+24] after its push R1.
                    try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = free_fn }, .to = .R1 } });
                    try compiler.addInstruction(.{ .push = .{ .reg = .R1 } }); // free_fn first
                    try compiler.addInstruction(.{ .push = .{ .reg = .R0 } }); // field_ptr second

                    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "global.object.free" } } });
                    try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 16 } } });
                }
            }
        }

        if (compiler.arch.target == .arm64) {
            // ARM64: restore self from R1 (x19) into R5 (x0), call _free, then frame_leave.
            try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R1 }, .to = .R5 } });
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
            try compiler.addInstruction(.{ .frame_leave = .{ .emit_ret = true } });
        } else {
            // x86-64: Free the struct memory itself. Self is at [rsp+32].
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .offset = 32, .register = .RSP } },
                .to = .R5,
            } });
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
            try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
            try compiler.addInstruction(.{ .ret = .{} });
        }
    }

    // global.String.free: String is a flat malloc'd block (_count, _size, chars inline).
    // No nested pointers — just free the allocation itself.
    // Same calling convention as struct destructors: self at [rsp+32] after push R1.
    try compiler.addInstruction(.{ .label = .{ .name = "global.String.free" } });
    if (compiler.arch.target == .arm64) {
        // ARM64: self arrives in x0 (R5); just call _free(x0) and return.
        try compiler.addInstruction(.{ .frame_enter = .{} });
        try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
        try compiler.addInstruction(.{ .frame_leave = .{ .emit_ret = true } });
    } else {
        try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .offset = 32, .register = .RSP } },
            .to = .R5,
        } });
        try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .ret = .{} });
    }

    // Collect which buffer element types need specialized per-element destructors.
    var needed_buf_frees = std.StringHashMap([]const u8).init(compiler.allocator);
    defer needed_buf_frees.deinit();

    // Scan struct fields.
    var scan_struct_it = cctx.typedef.iterator();
    while (scan_struct_it.next()) |stdef| {
        for (stdef.value_ptr.*.fields.items) |field| {
            const ft = stdef.value_ptr.*.getHabitant(field);
            if (ft.base == .buffer) {
                try collectBufElemFree(ft.base.buffer, cctx, &needed_buf_frees, compiler.allocator);
            }
        }
    }
    // Scan function signatures (args + return types).
    for (cctx.functions_to_compile.items) |func| {
        if (func.signature.retype.base == .buffer) {
            try collectBufElemFree(func.signature.retype.base.buffer, cctx, &needed_buf_frees, compiler.allocator);
        }
        for (func.signature.argtypes.items) |argtype| {
            if (argtype.base == .buffer) {
                try collectBufElemFree(argtype.base.buffer, cctx, &needed_buf_frees, compiler.allocator);
            }
        }
    }
    // Scan function body variable types.
    for (cctx.functions_to_compile.items) |func| {
        const fdef = cctx.getFunction(func.name);
        var bv_it = fdef.code.ctx.variables.iterator();
        while (bv_it.next()) |bv| {
            const vt = bv.value_ptr.*;
            if (vt != .decided) continue;
            if (vt.decided.base == .buffer) {
                try collectBufElemFree(vt.decided.base.buffer, cctx, &needed_buf_frees, compiler.allocator);
            }
        }
    }

    // Emit one specialized "global.Buffer_T_.free" destructor per collected entry.
    // Calling convention: same as all type destructors — after `push R1`, self is at [rsp+32].
    // The loop additionally pushes R10, R11, R12 (callee-saved), so self shifts to [rsp+56].
    // Uses R10=r12 (self), R11=r13 (count), R12=r14 (loop counter).
    // GC_DEC clobbers rbx (R1) but global.object.free restores it (callee-saved), so R1
    // is safe to use as a scratch by GC_DEC internally; the loop counter is in R12/r14.
    var buf_free_it = needed_buf_frees.iterator();
    while (buf_free_it.next()) |entry| {
        const buf_free_label = entry.key_ptr.*;
        const elem_free_label = entry.value_ptr.*;

        // ARM64: arm64.zig already provides global.Buffer_Buffer_.free; skip IR generation.
        if (compiler.arch.target == .arm64 and std.mem.eql(u8, buf_free_label, "global.Buffer_Buffer_.free")) continue;

        const loop_lbl = try compiler.generateLabel("buf_elem_free_loop");
        const next_lbl = try compiler.generateLabel("buf_elem_free_next");
        const done_lbl = try compiler.generateLabel("buf_elem_free_done");

        try compiler.addInstruction(.{ .label = .{ .name = buf_free_label } });

        if (compiler.arch.target == .arm64) {
            // ARM64: use frame_enter (saves x29+x30), then save self (x0/R5) into R1 (x19).
            // Callee-saved loop registers: R10 (x20), R11 (x21), R12 (x22).
            try compiler.addInstruction(.{ .frame_enter = .{} });
            try compiler.addInstruction(.{ .push = .{ .reg = .R10 } });
            try compiler.addInstruction(.{ .push = .{ .reg = .R11 } });
            try compiler.addInstruction(.{ .push = .{ .reg = .R12 } });
            // Save self (arrives in x0/R5) into R1 (x19).
            try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R5 }, .to = .R1 } });
            // Load count from [R1 + 8].
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .register = .R1, .offset = 8 } },
                .to = .R11,
            } });
            // Init loop counter R12 = 0.
            try compiler.addInstruction(.{ .xor = .{ .lhs = .R12, .rhs = .{ .register = .R12 } } });

            try compiler.addInstruction(.{ .label = loop_lbl });
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R12 }, .val2 = .{ .register = .R11 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .GE, .where = done_lbl.name } });

            // Load element = [R1 + R12*8 + 16]
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .sib = .{ .base = .R1, .index = .R12, .scale = 8, .disp = 16 } },
                .to = .R0,
            } });
            // Skip null elements.
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R0 }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .E, .where = next_lbl.name } });

            // call global.object.free(elem, elem_free_fn) — stack convention.
            try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = elem_free_label }, .to = .R5 } });
            try compiler.addInstruction(.{ .push = .{ .reg = .R5 } }); // free_fn first
            try compiler.addInstruction(.{ .push = .{ .reg = .R0 } }); // elem second
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "global.object.free" } } });
            try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 32 } } });

            try compiler.addInstruction(.{ .label = next_lbl });
            try compiler.addInstruction(.{ .inc = .{ .lhs = .R12 } });
            try compiler.addInstruction(.{ .jmp = .{ .where = loop_lbl.name } });

            try compiler.addInstruction(.{ .label = done_lbl });
            try compiler.addInstruction(.{ .pop = .{ .reg = .R12 } });
            try compiler.addInstruction(.{ .pop = .{ .reg = .R11 } });
            try compiler.addInstruction(.{ .pop = .{ .reg = .R10 } });
            // Restore self from R1 into R5 (x0) for _free call.
            try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R1 }, .to = .R5 } });
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
            try compiler.addInstruction(.{ .frame_leave = .{ .emit_ret = true } });
        } else {
            try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });   // destructor convention (save rbx)
            try compiler.addInstruction(.{ .push = .{ .reg = .R10 } });  // save r12
            try compiler.addInstruction(.{ .push = .{ .reg = .R11 } });  // save r13
            try compiler.addInstruction(.{ .push = .{ .reg = .R12 } });  // save r14

            // self at [rsp+56] (4 pushes × 8 + original [rsp+32-offset] = 32+24=56)
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .register = .RSP, .offset = 56 } },
                .to = .R10,
            } });
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .register = .R10, .offset = 8 } },
                .to = .R11,
            } });
            try compiler.addInstruction(.{ .xor = .{ .lhs = .R12, .rhs = .{ .register = .R12 } } });

            try compiler.addInstruction(.{ .label = loop_lbl });
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R12 }, .val2 = .{ .register = .R11 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .GE, .where = done_lbl.name } });

            // Load element = [R10 + R12*8 + 16]
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .sib = .{ .base = .R10, .index = .R12, .scale = 8, .disp = 16 } },
                .to = .R0,
            } });
            // Skip null/uninitialized elements
            try compiler.addInstruction(.{ .cmp = .{ .val1 = .{ .register = .R0 }, .val2 = .{ .immediate = 0 } } });
            try compiler.addInstruction(.{ .jcond = .{ .cc = .E, .where = next_lbl.name } });

            // call global.object.free(elem, elem_free_fn)
            try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = elem_free_label }, .to = .R1 } });
            try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });  // free_fn first
            try compiler.addInstruction(.{ .push = .{ .reg = .R0 } });  // elem second
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "global.object.free" } } });
            try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 16 } } });

            try compiler.addInstruction(.{ .label = next_lbl });
            try compiler.addInstruction(.{ .inc = .{ .lhs = .R12 } });
            try compiler.addInstruction(.{ .jmp = .{ .where = loop_lbl.name } });

            try compiler.addInstruction(.{ .label = done_lbl });
            try compiler.addInstruction(.{ .pop = .{ .reg = .R12 } });  // restore r14
            try compiler.addInstruction(.{ .pop = .{ .reg = .R11 } });  // restore r13
            try compiler.addInstruction(.{ .pop = .{ .reg = .R10 } });  // restore r12
            // self is now back at [rsp+32]
            try compiler.addInstruction(.{ .load = .{
                .from = .{ .registerOffset = .{ .register = .RSP, .offset = 32 } },
                .to = .R5,
            } });
            try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
            try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
            try compiler.addInstruction(.{ .ret = .{} });
        }
    }

    // global.Buffer.free: Buffer is a flat malloc'd block (_count, _size, elem0, elem1, ...).
    // Elements are primitives or unmanaged pointers at this ABI level — just free the block.
    // Same calling convention: self at [rsp+32] after push R1.
    try compiler.addInstruction(.{ .label = .{ .name = "global.Buffer.free" } });
    if (compiler.arch.target == .arm64) {
        // ARM64: self arrives in x0 (R5); just call _free(x0) and return.
        try compiler.addInstruction(.{ .frame_enter = .{} });
        try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
        try compiler.addInstruction(.{ .frame_leave = .{ .emit_ret = true } });
    } else {
        try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .load = .{
            .from = .{ .registerOffset = .{ .offset = 32, .register = .RSP } },
            .to = .R5,
        } });
        try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "_free" } } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .ret = .{} });
    }

    // global.object.free — generic GC destructor dispatcher.
    // ARM64: arm64.zig provides this via raw assembly; skip IR generation to avoid duplicate symbols.
    if (compiler.arch.target != .arm64) {
        // global.free.object
        // ; (rsp + 8) : R1 saved
        // ; (rsp + 16) : self
        // ; (rsp + 24) : function
        // R0 : self
        // R1 : count
        try compiler.addInstruction(.{ .label = .{ .name = Compiler.getGlobalFreeObject() } });
        try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .load = .{
            .from = .{
                .registerOffset = .{
                    .register = .RSP,
                    .offset = 16,
                },
            },
            .to = .R0,
        } });
        // Null guard: zero-initialized buffer slots contain 0; skip dec/free.
        try compiler.addInstruction(.{ .cmp = .{
            .val1 = .{ .register = .R0 },
            .val2 = .{ .immediate = 0 },
        } });
        try compiler.addInstruction(.{ .jcond = .{
            .cc = .E,
            .where = "global.object.free.null_exit",
        } });
        try compiler.addInstruction(.{ .load = .{
            .from = .{
                .registerOffset = .{
                    .register = .R0,
                    .offset = 0,
                },
            },
            .to = .R1,
        } });
        try compiler.addInstruction(.{ .dec = .{ .lhs = .R1 } });
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .register = .R1 },
            .to = .{
                .registerOffset = .{ .register = .R0, .offset = 0 },
            },
        } });
        try compiler.addInstruction(.{ .cmp = .{
            .val1 = .{ .register = .R1 },
            .val2 = .{ .immediate = 0 },
        } });
        try compiler.addInstruction(.{ .jcond = .{
            .cc = .E,
            .where = "global.object.free.liberate",
        } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .ret = .{} });
        try compiler.addInstruction(.{ .label = .{ .name = "global.object.free.null_exit" } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .ret = .{} });
        try compiler.addInstruction(.{ .label = .{ .name = "global.object.free.liberate" } });
        try compiler.addInstruction(.{ .call = .{ .value = .{
            .registerOffset = .{
                .offset = 24,
                .register = .RSP,
            },
        } } });
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .ret = .{} });
    }
}
