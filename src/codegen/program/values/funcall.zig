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

pub fn codegenFuncall(f: *Ast.Funcall, compiler: *Compiler, cctx: *analyser.Context) !void {
    const f_type = (try bbcTypes.getTypeOfValue(f.func, cctx, compiler.allocator)).decided.base.function;
    const vers = try bbcTypes.getFuncallVersion(f, f_type, cctx, compiler.allocator);
    const f_uid = compiler.getUid(analyser.functionVersion{ .name = f_type.fname, .signature = f_type, .version = vers });

    const used_arg_regs = @min(f.args.items.len, Inst.ARGUMENT_REGS.len);

    var saved_regs = std.ArrayList(Inst.Register).init(compiler.allocator);
    defer saved_regs.deinit();

    for (Inst.ARGUMENT_REGS[0..used_arg_regs]) |reg| {
        try compiler.addInstruction(.{ .push = .{ .reg = reg } });
        try saved_regs.append(reg);
    }

    // First the padding
    const real_args = f.args.items.len;
    const stack_args = if (real_args > Inst.ARGUMENT_REGS.len)
        real_args - Inst.ARGUMENT_REGS.len
    else
        0;
    const total_stack = 8 + stack_args * @as(usize, 8);
    const needs_padding = (total_stack % 16) != 8;
    if (needs_padding) {
        try compiler.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
    }

    for (0..f.args.items.len) |r_arg_idx| {
        const arg_idx = f.args.items.len - 1 - r_arg_idx;
        const arg = f.args.items[arg_idx];

        try codegen.value.codegenValue(arg, compiler, cctx);
        const arg_reg_idx = compiler.registerTable.lastReg().?;
        const arg_reg = try compiler.registerTable.getValue(arg_reg_idx, compiler);

        try compiler.addInstruction(.{ .push = .{ .reg = arg_reg } });
        try compiler.registerTable.free(arg_reg_idx);
    }

    try codegen.value.codegenValue(f.func, compiler, cctx);
    const self_reg_idx = compiler.registerTable.lastReg().?;
    const self_reg = try compiler.registerTable.getValue(self_reg_idx, compiler);
    try compiler.addInstruction(.{ .push = .{ .reg = self_reg } });
    try compiler.registerTable.free(self_reg_idx);

    try compiler.addInstruction(.{ .callwithargs = .{
        .value = .{ .label = f_uid },
        .argnum = f.args.items.len,
    } });

    var i = saved_regs.items.len;
    while (i > 0) {
        i -= 1;
        try compiler.addInstruction(.{ .pop = .{ .reg = saved_regs.items[i] } });
    }

    const ret_idx = try compiler.registerTable.allocate(compiler);
    const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
    try compiler.addInstruction(.{ .load = .{
        .from = .{ .register = .R0 },
        .to = ret_reg,
    } });
}
