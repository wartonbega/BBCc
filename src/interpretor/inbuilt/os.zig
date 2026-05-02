const std = @import("std");
const Values = @import("../values.zig");
const Itpr = @import("../interpretor.zig");
const Parser = @import("../../parser.zig");

pub fn dispatch(name: []const u8, args: []Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    if (std.mem.eql(u8, name, "plateforme"))
        return try Values.makeString(ctx.heap, "Darwin");

    _ = args;
    return try Values.makeError(ctx.heap, ref, "Unknown os function '{s}'", .{name});
}
