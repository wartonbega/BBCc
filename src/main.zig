//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const analyser = @import("analyser.zig");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");
const codegen = @import("codegen.zig");
const x86 = @import("codegen/x86.zig");

const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const tokens = try parser.parse("bbc-examples/basic_test.bbc", &arena);
    const prog = try lexer.lexeProgram(tokens, arena.allocator());
    const ctx = try analyser.Context.init(arena.allocator());
    try analyser.analyse(prog, ctx, arena.allocator());

    const builder = try codegen.generateProgram(prog, ctx, arena.allocator());

    try x86.dumpAssemblyX86(&builder);
}
