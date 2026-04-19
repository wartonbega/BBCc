const std = @import("std");
const analyser = @import("analyser.zig");
const Ast = @import("ast.zig");
const bbcTypes = @import("types.zig");
const errors = @import("errors.zig");
const Inst = @import("codegen/instructions.zig");
const Compiler = @import("codegen/compiler.zig").Compiler;

const codegen = @import("codegen/program/codegenprog.zig");

pub fn generateProgram(prog: *Ast.Program, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = prog;

    // Generating the functions
    for (cctx.functions_to_compile.items) |func| {
        // usefull (necessary)
        const function_uid = try codegen.funcdef.generateFuncUID(func.name, compiler);
        try compiler.func_uid_list.append(.{
            .uid = function_uid,
            .version = func,
        });
    }
    for (cctx.functions_to_compile.items) |func| {
        try codegen.funcdef.codegenFuncdef(&func, compiler, cctx);
    }

    // MAIN WRAPPER
    try compiler.addInstruction(.{ .label = .{ .name = Compiler.getMainWrapper() } });
    try compiler.addInstruction(.{ .lea = .{
        .adr = .{ .label = "main" },
        .to = .R0,
    } });
    try compiler.addInstruction(.{ .call = .{ .value = .{ .register = .R0 } } }); // The output of main is in R0
    try compiler.addInstruction(.{ .exit = .{ .code = .{ .register = .R0 } } });

    // Generating the free functions for the objects
    var struct_it = cctx.typedef.iterator();
    while (struct_it.next()) |stdef| {
        const s_name = stdef.key_ptr.*;
        const label_name = try std.fmt.allocPrint(
            compiler.allocator,
            "global.{s}.{s}",
            .{ s_name, "free" },
        );
        try compiler.addInstruction(.{ .label = .{ .name = label_name } });
        try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });

        for (stdef.value_ptr.*.fields.items) |field| {
            const field_type = stdef.value_ptr.*.getHabitant(field);
            if (field_type.base == .name and cctx.typeDefExist(field_type.base.name)) {
                // The first arguement
                try compiler.addInstruction(.{ .load = .{
                    .from = .{
                        .registerOffset = .{
                            .offset = 16,
                            .register = .RSP,
                        },
                    },
                    .to = .R1,
                } });

                // The field pointer
                try compiler.addInstruction(.{ .load = .{
                    .from = .{
                        .registerOffset = .{
                            .offset = @intCast(8 * stdef.value_ptr.*.getHabitantIndex(field)),
                            .register = .R1,
                        },
                    },
                    .to = .R1,
                } });
                try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });

                // The free function global.{Object}.free
                const child_free_function = try std.fmt.allocPrint(compiler.allocator, "global.{s}.free", .{field_type.base.name});
                try compiler.addInstruction(.{ .lea = .{ .adr = .{ .label = child_free_function }, .to = .R1 } });
                try compiler.addInstruction(.{ .push = .{ .reg = .R1 } });

                // Then we can call the said function
                try compiler.addInstruction(.{ .call = .{ .value = .{ .label = "global.object.free" } } });

                // And we can decrement the stack
                try compiler.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = 16 } } });
            }
        }
        try compiler.addInstruction(.{ .pop = .{ .reg = .R1 } });
        try compiler.addInstruction(.{ .ret = .{} });
    }

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
