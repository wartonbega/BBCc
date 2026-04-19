const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenIfStatement(_if: *Ast.IfStmt, compiler: *Compiler, cctx: *analyser.Context) !void {
    const end_label = try compiler.generateLabel("end_if");

    var found_value = false;

    for (_if.conditions.items, _if.scopes.items) |cond, scope| {
        const next_if = try compiler.generateLabel("elif");

        try codegen.value.codegenValue(cond, compiler, cctx);
        const val_idx = compiler.registerTable.lastReg().?;
        const val_reg = try compiler.registerTable.getValue(val_idx, compiler);

        try compiler.addInstruction(.{ .cmp = .{
            .val1 = .{ .register = val_reg },
            .val2 = .{ .immediate = 0 },
        } });
        try compiler.addInstruction(.{ .jcond = .{ .cc = .Z, .where = next_if.name } });

        try compiler.registerTable.free(val_idx);

        try codegen.value.codegenValue(scope, compiler, cctx);
        if (compiler.registerTable.lastReg()) |ret_idx| {
            found_value = true;
            const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
            try compiler.addInstruction(.{ .push = .{ .reg = ret_reg } });
            try compiler.registerTable.free(ret_idx);
        }

        try compiler.addInstruction(.{ .jmp = .{ .where = end_label.name } });

        try compiler.addInstruction(.{ .label = next_if });
    }

    if (_if.elsescope) |elsescope| {
        try codegen.value.codegenValue(elsescope, compiler, cctx);
        if (compiler.registerTable.lastReg()) |ret_idx| {
            found_value = true;
            const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
            try compiler.addInstruction(.{ .push = .{ .reg = ret_reg } });
            try compiler.registerTable.free(ret_idx);
        }
    }
    try compiler.addInstruction(.{ .label = end_label });

    if (found_value) {
        const ret_idx = try compiler.registerTable.allocate(compiler);
        const ret_reg = try compiler.registerTable.getValue(ret_idx, compiler);
        try compiler.addInstruction(.{ .pop = .{ .reg = ret_reg } });
    }
}
