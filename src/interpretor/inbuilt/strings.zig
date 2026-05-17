const std = @import("std");
const Values = @import("../values.zig");
const Itpr = @import("../interpretor.zig");
const Parser = @import("../../parser.zig");

pub fn dispatch(name: []const u8, args: []Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    if (std.mem.eql(u8, name, "truncate")) {
        const string = args[0].String;
        const a = args[1].Int;
        const b = args[2].Int;
        if (a > string.content.items.len or a < 0 or b > string.content.items.len or b < 0 or b < a)
            return try Values.makeError(ctx.heap, ref, "Can't trucate String with index {d}:{d}", .{ a, b });
        return try Values.makeString(ctx.heap, string.content.items[@intCast(a)..@intCast(b)]);
    }

    return try Values.makeError(ctx.heap, ref, "Unknown os function '{s}'", .{name});
}
