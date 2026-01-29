const std = @import("std");
const lsp = @import("lsp/server.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var server = try lsp.Server.init(allocator);
    defer server.deinit();

    try server.run();
}
