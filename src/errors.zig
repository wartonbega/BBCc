const std = @import("std");

var stderr = std.io.getStdErr().writer();
const exit = std.process.exit;

pub fn bbcError(comptime val: []const u8, args: anytype, pos: []const u8) void {
    stderr.print("Error at {s}:\n", .{pos}) catch {
        return;
    };
    stderr.print(val, args) catch {
        return;
    };
}

pub fn bbcErrorExit(comptime val: []const u8, args: anytype, pos: []const u8) void {
    stderr.print("Error at {s}:\n", .{pos}) catch {
        return;
    };
    stderr.print(val, args) catch {
        return;
    };
    exit(0);
}
