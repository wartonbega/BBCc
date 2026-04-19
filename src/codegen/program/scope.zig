const std = @import("std");
const analyser = @import("../../analyser.zig");
const Ast = @import("../../ast.zig");
const bbcTypes = @import("../../types.zig");
const errors = @import("../../errors.zig");

const Inst = @import("../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../compiler.zig").Compiler;

const codegen = @import("codegenprog.zig");

pub fn codegenScope(scope: *const Ast.Scope, compiler: *Compiler, cctx: *analyser.Context) !void {
    try compiler.addInstruction(.{ .comment = "Begin of scope" });

    // We can define where all variables are in this scope
    var var_it = scope.ctx.variables.iterator();
    var size: i64 = @intCast(0);
    while (var_it.next()) |variable| {
        compiler.incrementStackOffset();
        size += 1;
        try scope.ctx.putVariableStackIndex(variable.key_ptr.*, compiler.stack_size);
    }
    try compiler.addInstruction(.{ .minus = .{
        .lhs = .RSP,
        .rhs = .{ .immediate = size * 8 },
    } });

    for (scope.code.items, 0..) |val, i| {
        try codegen.value.codegenValue(val, compiler, scope.ctx);
        if (i != scope.code.items.len - 1) { // Also fixed: should be len - 1
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
        }
    }

    try compiler.addInstruction(.{ .plus = .{
        .lhs = .RSP,
        .rhs = .{ .immediate = size * 8 },
    } });

    try compiler.addInstruction(.{ .comment = "End of scope" });

    _ = cctx;
}
