const std = @import("std");

pub const registry = std.StaticStringMap([]const u8).initComptime(.{
    .{ "math", @embedFile("interpretor/inbuilt/math.config") },
    .{ "os", @embedFile("interpretor/inbuilt/os.config") },
});
