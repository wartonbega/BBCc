const std = @import("std");
const protocol = @import("protocol.zig");

const Allocator = std.mem.Allocator;

pub const Transport = struct {
    stdin: std.fs.File.Reader,
    stdout: std.fs.File.Writer,
    allocator: Allocator,

    pub fn init(allocator: Allocator) Transport {
        return .{
            .stdin = std.io.getStdIn().reader(),
            .stdout = std.io.getStdOut().writer(),
            .allocator = allocator,
        };
    }

    /// Read a message from stdin (LSP uses Content-Length header)
    pub fn readMessage(self: *Transport, arena: Allocator) ![]const u8 {
        var content_length: ?usize = null;
        std.log.info("Waiting for message...", .{});
        // Read headers
        while (true) {
            var line_buf: [256]u8 = undefined;
            const line = try self.stdin.readUntilDelimiter(&line_buf, '\n');

            // Trim \r if present (Windows line endings)
            const trimmed = std.mem.trimRight(u8, line, "\r");

            // Empty line means end of headers
            if (trimmed.len == 0) break;

            // Parse Content-Length header
            if (std.mem.startsWith(u8, trimmed, "Content-Length: ")) {
                const len_str = trimmed["Content-Length: ".len..];
                content_length = try std.fmt.parseInt(usize, len_str, 10);
            }
        }

        if (content_length == null) {
            return error.MissingContentLength;
        }

        // Read content
        const content = try arena.alloc(u8, content_length.?);
        try self.stdin.readNoEof(content);

        return content;
    }

    /// Write a message to stdout
    pub fn writeMessage(self: *Transport, content: []const u8) !void {
        try self.stdout.print("Content-Length: {d}\r\n\r\n", .{content.len});
        try self.stdout.writeAll(content);
    }

    /// Send a response
    pub fn sendResponse(self: *Transport, response: protocol.Response, arena: Allocator) !void {
        var string = std.ArrayList(u8).init(arena);
        try std.json.stringify(response, .{}, string.writer());
        try self.writeMessage(string.items);
    }

    /// Send a notification
    pub fn sendNotification(self: *Transport, notification: protocol.Notification, arena: Allocator) !void {
        var string = std.ArrayList(u8).init(arena);
        try std.json.stringify(notification, .{}, string.writer());
        std.log.info("sending {s}", .{string.items});
        try self.writeMessage(string.items);
    }
};
