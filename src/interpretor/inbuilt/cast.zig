const std = @import("std");
const Values = @import("../values.zig");
const Itpr = @import("../interpretor.zig");
const Parser = @import("../../parser.zig");

fn intCast(val: Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    return switch (val) {
        .Int => |_| val,
        .Char => |c| Values.Value{ .Int = c },
        .Bool => |b| Values.Value{ .Int = @intFromBool(b) },
        .Float => |f| Values.Value{ .Int = @intFromFloat(f) },
        else => try Values.makeError(ctx.heap, ref, "Can't cast type '{s}' to type 'Int'", .{val.getType()}),
    };
}

fn floatCast(val: Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    return switch (val) {
        .Int => |i| Values.Value{ .Float = @floatFromInt(i) },
        .Char => |c| Values.Value{ .Float = @floatFromInt(c) },
        .Bool => |b| Values.Value{ .Float = @floatFromInt(@intFromBool(b)) },
        .Float => |_| val,
        else => try Values.makeError(ctx.heap, ref, "Can't cast type '{s}' to type 'Float'", .{val.getType()}),
    };
}

fn boolCast(val: Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    return switch (val) {
        .Int => |i| Values.Value{ .Bool = if (i >= 1) true else false },
        .Char => |c| Values.Value{ .Bool = if (c >= 1) true else false },
        .Bool => |_| val,
        .Float => |f| Values.Value{ .Bool = if (f >= 1.0) true else false },
        else => try Values.makeError(ctx.heap, ref, "Can't cast type '{s}' to type 'Bool'", .{val.getType()}),
    };
}

fn stringCast(val: Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    _ = ref;
    return Values.valueToString(val, ctx.heap);
}

fn charCast(val: Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    return switch (val) {
        .Char => val,
        .Int => |i| Values.Value{ .Char = @intCast(i & 0xFF) },
        else => try Values.makeError(ctx.heap, ref, "Can't cast type '{s}' to type 'Char'", .{val.getType()}),
    };
}

pub fn dispatch(name: []const u8, args: []Values.Value, ctx: *Itpr.Context, ref: Parser.Location) !Values.Value {
    if (std.mem.eql(u8, name, "int")) return try intCast(args[0], ctx, ref);
    if (std.mem.eql(u8, name, "float")) return try floatCast(args[0], ctx, ref);
    if (std.mem.eql(u8, name, "bool")) return try boolCast(args[0], ctx, ref);
    if (std.mem.eql(u8, name, "string")) return try stringCast(args[0], ctx, ref);
    if (std.mem.eql(u8, name, "char")) return try charCast(args[0], ctx, ref);
    return try Values.makeError(ctx.heap, ref, "Unknown cast function '{s}'", .{name});
}
