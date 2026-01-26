const std = @import("std");
const Values = @import("values.zig");

pub fn print(val: Values.Value) void {
    const stdout = std.io.getStdOut().writer();
    switch (val) {
        .Int => |i| {
            stdout.print("{d}", .{i}) catch {};
        },
        .Bool => |b| {
            stdout.print("{s}", .{if (b) "true" else "false"}) catch {};
        },
        .Object => |o| {
            stdout.print("Object({s})", .{o.name}) catch {};
        },
        .Null => {
            stdout.print("Null", .{}) catch {};
        },
        .Function => |f| {
            stdout.print("Function({s})", .{f.func.name}) catch {};
        },
        .Error => |e| {
            stdout.print("ErrorUnion: {s} at {s}", .{ e.message, e.reference.toString() }) catch {};
        },
        .Char => |c| {
            stdout.print("{c}", .{c}) catch {};
        },
        .String => |s| {
            stdout.print("{s}", .{s.*.content.items}) catch {};
        },
    }
}

pub fn println(val: Values.Value) void {
    const stdout = std.io.getStdOut().writer();
    print(val);
    stdout.print("\n", .{}) catch {};
}
