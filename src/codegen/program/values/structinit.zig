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

pub fn codegenStructinit(stcinit: *Ast.StructInit, compiler: *Compiler, cctx: *analyser.Context) !void {
    const alloc_size = @as(usize, stcinit.habitants.count()) + 2;

    const ret_reg_idx = try compiler.registerTable.allocate(compiler);
    const ret_reg = try compiler.registerTable.getValue(ret_reg_idx, compiler);

    // Caller saved registers
    for (Inst.CALLER_SAVED) |reg| {
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .push = .{ .reg = reg } });
            compiler.incrementStackOffset();
        }
    }
    try compiler.addInstruction(.{ .malloc = .{
        .size = alloc_size * 8,
        .stack_size = @intCast(compiler.stack_size),
    } });
    for (0..Inst.CALLER_SAVED.len) |i| {
        const reg = Inst.CALLER_SAVED[Inst.CALLER_SAVED.len - 1 - i];
        if (compiler.registerTable.isRegisterInUse(reg)) {
            try compiler.addInstruction(.{ .pop = .{ .reg = reg } });
            compiler.decrementStackOffset();
        }
    }

    try compiler.addInstruction(.{ .load = .{
        .from = .{ .register = .R0 },
        .to = ret_reg,
    } });

    const stc_def = cctx.getTypeDef(stcinit.name);

    // Initializing _size, _count
    const _size_idx = stc_def.getHabitantIndex("_size");
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = 0 },
        .to = .{
            .registerOffset = .{
                .offset = @intCast((_size_idx) * 8),
                .register = ret_reg,
            },
        },
    } });
    const _count_idx = stc_def.getHabitantIndex("_count");
    try compiler.addInstruction(.{ .store = .{
        .from = .{ .immediate = 0 },
        .to = .{
            .registerOffset = .{
                .offset = @intCast((_count_idx) * 8),
                .register = ret_reg,
            },
        },
    } });

    var hab_it = stcinit.habitants.iterator();
    while (hab_it.next()) |hab| {
        try codegen.value.codegenValue(hab.value_ptr.*, compiler, cctx);
        const hab_idx_reg = compiler.registerTable.lastReg().?;
        const hab_reg = try compiler.registerTable.getValue(hab_idx_reg, compiler);
        const hab_idx = stc_def.getHabitantIndex(hab.key_ptr.*);
        try compiler.addInstruction(.{ .store = .{
            .from = .{ .register = hab_reg },
            .to = .{
                .registerOffset = .{
                    .offset = @intCast((hab_idx) * 8),
                    .register = ret_reg,
                },
            },
        } });
        try compiler.registerTable.free(hab_idx_reg);
    }

    compiler.registerTable.last_used = ret_reg_idx;
}
