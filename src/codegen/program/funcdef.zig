const std = @import("std");
const analyser = @import("../../analyser.zig");
const Ast = @import("../../ast.zig");
const bbcTypes = @import("../../types.zig");
const errors = @import("../../errors.zig");

const Inst = @import("../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../compiler.zig").Compiler;

const codegen = @import("codegenprog.zig");

pub fn generateFuncUID(funcname: []const u8, compiler: *Compiler) ![]const u8 {
    // Main can't have a different ID than 'main'
    if (std.mem.eql(u8, funcname, "main"))
        return funcname;

    const func_num = try compiler.f_table.getOrPutValue(funcname, 0);

    const id = try std.fmt.allocPrint(compiler.allocator, "{s}@{d}", .{ funcname, func_num.value_ptr.* });
    try compiler.f_table.put(funcname, func_num.value_ptr.* + 1);
    return id;
}

pub fn codegenFuncdef(func: *const analyser.functionVersion, compiler: *Compiler, cctx: *analyser.Context) !void {
    const function_uid = compiler.getUid(func.*);

    std.log.info("Generating function {s}", .{function_uid});
    try compiler.addInstruction(Instruction{ .label = .{ .name = function_uid } });

    const function_def = cctx.getFunction(func.name);

    try compiler.addInstruction(.{ .generateFunctionFrame = .{ .argnum = func.signature.argtypes.items.len } });

    for (0..@min(function_def.arguments.items.len, Inst.ARGUMENT_REGS.len)) |i| {
        const arg = function_def.arguments.items[i];
        try function_def.ctx.?.putVariableStackIndex(arg.name, compiler.stack_size);
        compiler.incrementStackOffset();
    }
    for (@min(function_def.arguments.items.len, Inst.ARGUMENT_REGS.len)..function_def.arguments.items.len) |i| {
        const arg = function_def.arguments.items[i];
        const idx: i64 = @intCast(i);
        const sub_reg_idx: i64 = @intCast(Inst.ARGUMENT_REGS.len);
        try function_def.ctx.?.putVariableStackIndex(arg.name, -((idx - sub_reg_idx) + 3));
    }

    try codegen.scope.codegenScope(function_def.code, compiler, function_def.ctx.?);

    if (compiler.registerTable.lastReg()) |reg| {
        const val = try compiler.registerTable.getValue(reg, compiler);
        try compiler.addInstruction(.{ .load = .{ .from = .{ .register = val }, .to = .R0 } });
        try compiler.registerTable.free(reg);
    }

    // Security to avoid register leaks
    if (!compiler.registerTable.isCorrectlyFreed()) {
        std.debug.print("All the registers are not freed !! 🚨🚨🚨🚨", .{});
        unreachable;
    }

    // Function epilogue
    try compiler.addInstruction(.{ .load = .{ .from = .{ .register = .RBP }, .to = .RSP } });
    try compiler.addInstruction(.{ .pop = .{ .reg = .RBP } });

    try compiler.addInstruction(Instruction{ .ret = .{} });
}
