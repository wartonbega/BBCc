//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const analyser = @import("analyser.zig");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");
const imports = @import("imports.zig");
const codegen = @import("codegen.zig");
const interpretor = @import("interpretor/interpretor.zig");
const x86 = @import("codegen/x86.zig");
const arm64 = @import("codegen/arm64.zig");
const Compiler = @import("codegen/compiler.zig").Compiler;
const arch_mod = @import("codegen/arch.zig");
const build_options = @import("build_options");

const std = @import("std");
const Io = std.io;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var argsIterator = try std.process.ArgIterator.initWithAllocator(arena.allocator());
    defer argsIterator.deinit();
    _ = argsIterator.next();

    var interprete_execute = false;

    var filename: []const u8 = "bbc-examples/basic_test.bbc";
    while (argsIterator.next()) |arg| {
        if (std.mem.eql(u8, arg, "-X")) {
            interprete_execute = true;
            std.log.info("Executing the program with the intepretor", .{});
        } else {
            filename = arg;
        }
    }

    const tokens = try parser.parse(filename, &arena);
    defer tokens.deinit();
    const alloc = arena.allocator();
    const prog = try lexer.lexeProgram(tokens, alloc, filename);
    try imports.resolveAllImports(prog, filename, &arena);
    const ctx = try analyser.Context.init(arena.allocator());
    defer ctx.deinit();
    try analyser.analyse(prog, ctx, arena.allocator());

    if (interprete_execute) {
        std.debug.print("------- Output of programme -------\n", .{});
        try interpretor.interpreteProgram(prog, ctx, arena.allocator());
    } else {
        const arch = if (std.mem.eql(u8, build_options.target_arch, "arm64"))
            arch_mod.ArchConfig.forArm64()
        else
            arch_mod.ArchConfig.forX86_64();
        const compiler = try Compiler.init(alloc, arch);
        std.log.info("Compiling the program", .{});
        try codegen.generateProgram(prog, compiler, ctx);
        switch (arch.target) {
            .x86_64 => {
                std.log.info("Generating assembly for x86_64", .{});
                try x86.dumpAssemblyX86(compiler, compiler.getMainWrapper());
            },
            .arm64 => {
                std.log.info("Generating assembly for arm64", .{});
                try arm64.dumpAssemblyArm64(compiler, compiler.getMainWrapper());
            },
        }
    }
}
