const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

// Dispatch: Buffer/String → bounds-checked raw load; user struct → index method call.
pub fn codegenBufferIndex(bi: *Ast.BufferIndex, compiler: *Compiler, cctx: *analyser.Context) !void {
    const buf_type = try bbcTypes.getTypeOfValue(bi.buffer, cctx, compiler.allocator);
    if (buf_type == .decided) {
        switch (buf_type.decided.base) {
            .name => |name| {
                if (!std.mem.eql(u8, name, "String") and cctx.typeDefExist(name)) {
                    try codegenSubscriptionIndex(bi, compiler, cctx, name);
                    return;
                }
            },
            .generic => |gen| {
                const spec_name = try ensureSpecName(gen.name, bi, compiler, cctx);
                if (spec_name) |s| {
                    try codegenSubscriptionIndex(bi, compiler, cctx, s);
                    return;
                }
            },
            else => {},
        }
    }
    try codegenRawBufIndex(bi, compiler, cctx);
}

// Raw bounds-checked load for Buffer and String types.
// Layout: [_count @+0][_size @+8][elem0 @+16][elem1 @+24]...
// If idx >= _size: bbc_error_flag=1, result=0.
fn codegenRawBufIndex(bi: *Ast.BufferIndex, compiler: *Compiler, cctx: *analyser.Context) !void {
    try codegen.value.codegenValue(bi.buffer, compiler, cctx);
    const buf_idx = compiler.registerTable.lastReg().?;
    const buf_reg = try compiler.registerTable.getValue(buf_idx, compiler);

    try codegen.value.codegenValue(bi.index, compiler, cctx);
    const idx_idx = compiler.registerTable.lastReg().?;
    const idx_reg = try compiler.registerTable.getValue(idx_idx, compiler);

    const res_idx = try compiler.registerTable.allocate(compiler);
    const res_reg = try compiler.registerTable.getValue(res_idx, compiler);

    const oob_label = try compiler.generateLabel("buf_oob");
    const end_label = try compiler.generateLabel("buf_idx_end");

    // Unsigned comparison: catches negative indices and true OOB.
    try compiler.addInstruction(.{ .cmp = .{
        .val1 = .{ .register = idx_reg },
        .val2 = .{ .registerOffset = .{ .register = buf_reg, .offset = 8 } },
    } });
    try compiler.addInstruction(.{ .jcond = .{ .cc = .AE, .where = oob_label.name } });

    // OK path: compute element address and load.
    try compiler.addInstruction(.{ .shl = .{ .lhs = idx_reg, .imm = 3 } });
    try compiler.addInstruction(.{ .plus = .{ .lhs = idx_reg, .rhs = .{ .immediate = 16 } } });
    try compiler.addInstruction(.{ .plus = .{ .lhs = idx_reg, .rhs = .{ .register = buf_reg } } });
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .registerOffset = .{ .register = idx_reg, .offset = 0 } },
        .to = res_reg,
    } });
    try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });

    // OOB path: allocate ErrorObj(code=2) → rax; put ptr in res_reg.
    try compiler.addInstruction(.{ .label = oob_label });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 2 }, .to = .R5 } });
    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "bbc_make_error_fn" } } });
    try compiler.addInstruction(.{ .store = .{ .from = .{ .immediate = 1 }, .to = .{ .rip_memory = "bbc_error_flag" } } });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = res_reg } });

    try compiler.addInstruction(.{ .label = end_label });
    try compiler.registerTable.free(buf_idx);
    try compiler.registerTable.free(idx_idx);
    compiler.registerTable.last_used = res_idx;

    if (compiler.current_func_propagates_errors and !compiler.in_error_check_expr) {
        const ok = try compiler.generateLabel("buf_idx_propagate_ok");
        try compiler.emitEarlyReturnOnError(ok);
    }
}

// Method call path: emit self.index(idx) for types implementing Subscription.
// Follows the same callwithargs(2) protocol as funcall.zig method calls.
fn codegenSubscriptionIndex(bi: *Ast.BufferIndex, compiler: *Compiler, cctx: *analyser.Context, type_name: []const u8) !void {
    const method_name = try std.fmt.allocPrint(compiler.allocator, "{s}.index", .{type_name});
    var method_uid: ?[]const u8 = null;
    for (compiler.func_uid_list.items) |func| {
        if (std.mem.eql(u8, func.version.name, method_name)) {
            method_uid = func.uid;
            break;
        }
    }
    if (method_uid == null) {
        std.debug.print("[CODEGEN ERROR] No compiled index method for type '{s}'\n", .{type_name});
        return;
    }

    // Evaluate self (the struct pointer) and idx.
    try codegen.value.codegenValue(bi.buffer, compiler, cctx);
    const self_idx = compiler.registerTable.lastReg().?;
    const self_reg = try compiler.registerTable.getValue(self_idx, compiler);

    try codegen.value.codegenValue(bi.index, compiler, cctx);
    const idx_idx = compiler.registerTable.lastReg().?;
    const idx_reg = try compiler.registerTable.getValue(idx_idx, compiler);

    // Save the two argument registers (rdi, rsi) that callwithargs will clobber.
    // This follows the same pattern as funcall.zig.
    const used_arg_regs = 2; // self + 1 explicit arg
    var saved_regs = std.ArrayList(Inst.Register).init(compiler.allocator);
    defer saved_regs.deinit();
    for (Inst.ARGUMENT_REGS[0..used_arg_regs]) |reg| {
        try compiler.addInstruction(.{ .push = .{ .reg = reg } });
        try saved_regs.append(reg);
    }

    // Stack layout: [dummy][self][idx]
    // dummy at [rsp], self at [rsp+8], idx at [rsp+16].
    try compiler.addInstruction(.{ .push = .{ .reg = idx_reg } });
    try compiler.addInstruction(.{ .push = .{ .reg = self_reg } });
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = .R2 } });
    try compiler.addInstruction(.{ .push = .{ .reg = .R2 } });

    // Load self → rdi, idx → rsi from stack; pop all three + dummy.
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .registerOffset = .{ .register = .RSP, .offset = 8 } },
        .to = Inst.ARGUMENT_REGS[0], // rdi = self
    } });
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .registerOffset = .{ .register = .RSP, .offset = 16 } },
        .to = Inst.ARGUMENT_REGS[1], // rsi = idx
    } });
    try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 24 } } }); // pop self+idx+dummy
    // Push dummy 0 back for 16-byte alignment before call.
    try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = .R2 } });
    try compiler.addInstruction(.{ .push = .{ .reg = .R2 } });

    try compiler.addInstruction(.{ .call = .{ .value = .{ .label = method_uid.? } } });

    try compiler.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } }); // pop dummy

    // Restore saved argument registers in reverse order.
    var i = saved_regs.items.len;
    while (i > 0) {
        i -= 1;
        try compiler.addInstruction(.{ .pop = .{ .reg = saved_regs.items[i] } });
    }

    try compiler.registerTable.free(self_idx);
    try compiler.registerTable.free(idx_idx);

    // Load return value (rax) into a fresh result register.
    const res_idx = try compiler.registerTable.allocate(compiler);
    const res_reg = try compiler.registerTable.getValue(res_idx, compiler);
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .register = .R0 },
        .to = res_reg,
    } });
    compiler.registerTable.last_used = res_idx;

    if (compiler.current_func_propagates_errors and !compiler.in_error_check_expr) {
        const ok = try compiler.generateLabel("sub_idx_propagate_ok");
        try compiler.emitEarlyReturnOnError(ok);
    }
}

// If the generic base type has a known specialization for this subscript context,
// return the specialization name; otherwise return null.
fn ensureSpecName(base_name: []const u8, bi: *Ast.BufferIndex, compiler: *Compiler, cctx: *analyser.Context) !?[]const u8 {
    _ = bi;
    // Walk func_uid_list looking for any "{base_name}<...>.index" function.
    for (compiler.func_uid_list.items) |func| {
        if (std.mem.startsWith(u8, func.version.name, base_name) and
            std.mem.endsWith(u8, func.version.name, ".index"))
        {
            const full = func.version.name;
            // Extract the specialization name (everything before ".index").
            return full[0 .. full.len - ".index".len];
        }
    }
    _ = cctx;
    return null;
}

// &buf[idx]  — address of the element (for assignment buf[i] = v).
// Only valid for Buffer/String types; no dispatch for user structs.
pub fn codegenBufferIndexAdr(bi: *Ast.BufferIndex, compiler: *Compiler, cctx: *analyser.Context) !void {
    try codegen.value.codegenValue(bi.buffer, compiler, cctx);
    const buf_idx = compiler.registerTable.lastReg().?;
    const buf_reg = try compiler.registerTable.getValue(buf_idx, compiler);

    try codegen.value.codegenValue(bi.index, compiler, cctx);
    const idx_idx = compiler.registerTable.lastReg().?;
    const idx_reg = try compiler.registerTable.getValue(idx_idx, compiler);

    try compiler.addInstruction(.{ .shl = .{ .lhs = idx_reg, .imm = 3 } });
    try compiler.addInstruction(.{ .plus = .{ .lhs = idx_reg, .rhs = .{ .immediate = 16 } } });
    try compiler.addInstruction(.{ .plus = .{ .lhs = idx_reg, .rhs = .{ .register = buf_reg } } });

    try compiler.registerTable.free(buf_idx);
    compiler.registerTable.last_used = idx_idx;
}
