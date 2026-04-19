const std = @import("std");

const inst = @import("instructions.zig");
const Compiler = @import("compiler.zig").Compiler;

const Allocator = std.mem.Allocator;

const FileWriter = std.io.Writer(std.fs.File, std.fs.File.WriteError, std.fs.File.write);

pub fn dumpAssemblyX86(compiler: *const Compiler, entry_point: []const u8) !void {
    const file = try std.fs.cwd().createFile(
        "output.asm",
        .{ .read = false, .truncate = true },
    );
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const writer = file.writer();

    _ = try writer.print(
        \\default rel
        \\    section .text align=16
        \\    global {s} ;; entry point
        \\extern _malloc
        \\extern _free
        \\
    , .{entry_point});
    for (compiler.program.items) |instruction| {
        try instruction.toAsm(writer, alloc);
    }
}
