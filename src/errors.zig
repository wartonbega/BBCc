const std = @import("std");
const Parser = @import("parser.zig");

var stderr = std.io.getStdErr().writer();
const exit = std.process.exit;

pub fn bbcError(comptime val: []const u8, args: anytype, pos: Parser.Location) void {
    stderr.print("Error at {s}:\n", .{pos.toString()}) catch {
        return;
    };
    stderr.print(val, args) catch {
        return;
    };
}

pub fn bbcErrorExit(comptime val: []const u8, args: anytype, pos: Parser.Location) void {
    stderr.print("Error at {s}:\n", .{pos.toString()}) catch {
        return;
    };
    stderr.print(val, args) catch {
        return;
    };
    stderr.print("\n", .{}) catch {};
    exit(0);
}

pub fn bbcRuntimeError(comptime val: []const u8, args: anytype, pos: Parser.Location) void {
    stderr.print("Runtime error at {s}:\n", .{pos.toString()}) catch {
        return;
    };
    stderr.print(val, args) catch {
        return;
    };
    stderr.print("\n", .{}) catch {};
    exit(0);
}
