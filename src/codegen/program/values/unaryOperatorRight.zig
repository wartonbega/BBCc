const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");
const gc = @import("../gc.zig");

// Returns the byte offset of `field` within the object pointed to by the base expression.
// Buffer layout (hardcoded): [_count @ 0][_size @ 8][elem0 @ 16]...
// Struct layout: determined by getStructHabitantIndex — fields array is
//   [_count, _size, userField0, userField1, ...], so offset = index * 8.
fn getFieldOffset(expr: *const Ast.Value, field: []const u8, cctx: *analyser.Context, allocator: std.mem.Allocator) !i64 {
    // Buffer-only pseudo-fields — no struct def exists for [T] types.
    if (std.mem.eql(u8, field, "_count")) return 0;
    if (std.mem.eql(u8, field, "_size")) return 8;

    // User-defined struct fields: look up offset via the type of the identifier.
    if (expr.* == .identifier) {
        const t = cctx.getVariable(expr.identifier.name);
        switch (t) {
            .decided => |at| switch (at.base) {
                .name => |type_name| {
                    const idx = cctx.getStructHabitantIndex(type_name, field);
                    return idx * 8;
                },
                .generic => |gen| {
                    const spec_name = try analyser.ensureGenericSpecialization(gen, cctx, allocator);
                    const idx = cctx.getStructHabitantIndex(spec_name, field);
                    return idx * 8;
                },
                else => {},
            },
            .undecided => {},
        }
    }
    return 0;
}

/// Load a struct / buffer field: `expr.field` → result register.
/// Special case: if the attribute is a method, create a bound method heap object
/// {_count @ 0, receiver @ 8, receiver_free_fn @ 16} and return a pointer to it.
pub fn codegenUopRight(uopr: *Ast.UnaryOperatorRight, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Detect method access: the type of the whole expression is .bound_method.
    var uopr_val = Ast.Value{ .unaryOperatorRight = uopr };
    const uopr_type = try bbcTypes.getTypeOfValue(&uopr_val, cctx, compiler.allocator);

    if (uopr_type == .decided and uopr_type.decided.base == .bound_method) {
        // Allocate the bound method heap object: 24 bytes.
        // Save/restore caller-saved regs around malloc (same pattern as structinit).
        for (Inst.CALLER_SAVED) |reg| {
            if (compiler.registerTable.isRegisterInUse(reg)) {
                try compiler.addInstruction(.{ .push = .{ .reg = reg } });
                compiler.incrementStackOffset();
            }
        }
        try compiler.addInstruction(.{ .alloc = .{
            .size = 24,
            .stack_size = @intCast(compiler.stack_size),
        } });
        for (0..Inst.CALLER_SAVED.len) |i| {
            const reg = Inst.CALLER_SAVED[Inst.CALLER_SAVED.len - 1 - i];
            if (compiler.registerTable.isRegisterInUse(reg)) {
                try compiler.addInstruction(.{ .pop = .{ .reg = reg } });
                compiler.decrementStackOffset();
            }
        }

        const bm_reg_idx = try compiler.registerTable.allocate(compiler);
        const bm_reg = try compiler.registerTable.getValue(bm_reg_idx, compiler);
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = bm_reg } });

        // _count = 0; assignment GC will gc_inc to 1 when bound method is stored.
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .immediate = 0 },
            .to = .{ .registerOffset = .{ .register = bm_reg, .offset = 0 } },
        } });

        // Evaluate receiver expression → recv_reg.
        try codegen.value.codegenValue(uopr.expr, compiler, cctx);
        const recv_reg_idx = compiler.registerTable.lastReg().?;
        const recv_reg = try compiler.registerTable.getValue(recv_reg_idx, compiler);

        // gc_inc receiver: bound method owns a reference.
        try gc.emitGcInc(recv_reg, compiler);

        // Store receiver pointer at offset 8.
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .register = recv_reg },
            .to = .{ .registerOffset = .{ .register = bm_reg, .offset = 8 } },
        } });

        // Determine receiver type's destructor label and store at offset 16.
        const recv_type = try bbcTypes.getTypeOfValue(uopr.expr, cctx, compiler.allocator);
        const free_fn_label = try gc.getFreeFnLabel(recv_type, cctx, compiler.allocator);

        const fn_reg_idx = try compiler.registerTable.allocate(compiler);
        const fn_reg = try compiler.registerTable.getValue(fn_reg_idx, compiler);
        try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = free_fn_label }, .to = fn_reg } });
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .register = fn_reg },
            .to = .{ .registerOffset = .{ .register = bm_reg, .offset = 16 } },
        } });

        try compiler.registerTable.free(fn_reg_idx);
        try compiler.registerTable.free(recv_reg_idx);
        compiler.registerTable.last_used = bm_reg_idx;
        return;
    }

    try codegen.value.codegenValue(uopr.expr, compiler, cctx);
    const base_idx = compiler.registerTable.lastReg().?;
    const base_reg = try compiler.registerTable.getValue(base_idx, compiler);

    const offset = try getFieldOffset(uopr.expr, uopr.operator.pointAttr, cctx, compiler.allocator);

    const res_idx = try compiler.registerTable.allocate(compiler);
    const res_reg = try compiler.registerTable.getValue(res_idx, compiler);

    try compiler.addInstruction(.{ .load = .{
        .from = .{ .registerOffset = .{ .register = base_reg, .offset = offset } },
        .to = res_reg,
    } });

    try compiler.registerTable.free(base_idx);
    compiler.registerTable.last_used = res_idx;
}

/// Produce the address of a struct field: `expr.field` → address register.
/// Used as the LHS of an assignment like `p.x = val`.
pub fn codegenUopRightAdr(uopr: *Ast.UnaryOperatorRight, compiler: *Compiler, cctx: *analyser.Context) !void {
    try codegen.value.codegenValue(uopr.expr, compiler, cctx);
    const base_idx = compiler.registerTable.lastReg().?;
    const base_reg = try compiler.registerTable.getValue(base_idx, compiler);

    const offset = try getFieldOffset(uopr.expr, uopr.operator.pointAttr, cctx, compiler.allocator);

    if (offset != 0) {
        try compiler.addInstruction(.{ .plus = .{
            .lhs = base_reg,
            .rhs = .{ .immediate = offset },
        } });
    }

    compiler.registerTable.last_used = base_idx;
}
