const std = @import("std");

pub const registry = std.StaticStringMap([]const u8).initComptime(.{
    .{ "math", @embedFile("inbuilt/math.config") },
    .{ "os", @embedFile("inbuilt/os.config") },
    .{ "strings", @embedFile("inbuilt/strings.config") },
    .{ "cast", @embedFile("inbuilt/cast.config") },
});
