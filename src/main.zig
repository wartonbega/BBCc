//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const analyser = @import("analyser.zig");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");
const codegen = @import("codegen.zig");
const interpretor = @import("interpretor/interpretor.zig");
const x86 = @import("codegen/x86.zig");

const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var argsIterator = try std.process.ArgIterator.initWithAllocator(arena.allocator());
    defer argsIterator.deinit();
    _ = argsIterator.next();

    var filename: []const u8 = "bbc-examples/basic_test.bbc";
    while (argsIterator.next()) |arg| {
        if (std.mem.eql(u8, arg, "-X")) {
            std.debug.print("Argument {s}\n", .{arg});
        } else {
            filename = arg;
        }
    }

    const tokens = try parser.parse(filename, &arena);
    const prog = try lexer.lexeProgram(tokens, arena.allocator());
    const ctx = try analyser.Context.init(arena.allocator());
    try analyser.analyse(prog, ctx, arena.allocator());

    // const builder = try codegen.generateProgram(prog, ctx, arena.allocator());
    // try x86.dumpAssemblyX86(&builder, &codegen.main_function_wrapper_name.*);

    try interpretor.interpreteProgram(prog, ctx, arena.allocator());
}
