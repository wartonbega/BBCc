const std = @import("std");
const Values = @import("values.zig");

pub fn print(val: Values.Value) void {
    const stdout = std.io.getStdOut().writer();
    switch (val) {
        .Int => |i| {
            stdout.print("{d}", .{i}) catch {};
        },
        .Float => |f| {
            stdout.print("{d}", .{f}) catch {};
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
        .BuiltinFunction => |name| {
            stdout.print("BuiltinFunction({s})", .{name}) catch {};
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
        .Buffer => |b| {
            _ = stdout.write("[") catch {};
            for (b.content, 0..) |o, i| {
                if (o) |v| {
                    print(v);
                } else _ = stdout.write("?") catch {};

                if (i != b.content.len - 1)
                    stdout.print(", ", .{}) catch {};
            }
            _ = stdout.write("]") catch {};
        },
        .Namespace => |ns| {
            stdout.print("Namespace({s})", .{ns.name}) catch {};
        },
    }
}

pub fn println(val: Values.Value) void {
    const stdout = std.io.getStdOut().writer();
    print(val);
    stdout.print("\n", .{}) catch {};
}
