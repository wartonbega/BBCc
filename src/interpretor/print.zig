const std = @import("std");
const Values = @import("values.zig");

pub fn print(val: Values.Value) void {
    switch (val) {
        .Int => |i| {
            std.debug.print("{d}", .{i});
        },
        .Bool => |b| {
            std.debug.print("{s}", .{if (b) "true" else "false"});
        },
        .Object => |o| {
            std.debug.print("Object({s})", .{o.name});
        },
        .Null => {
            std.debug.print("Null", .{});
        },
        .Function => |f| {
            std.debug.print("Function({s})", .{f.name});
        },
        .Error => |e| {
            std.debug.print("ErrorUnion: {s} at {s}", .{ e.message, e.reference });
        },
        .Char => |c| {
            std.debug.print("{c}", .{c});
        },
    }
}

pub fn println(val: Values.Value) void {
    print(val);
    std.debug.print("\n", .{});
}
