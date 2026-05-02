const std = @import("std");
const Values = @import("../values.zig");
const Itpr = @import("../interpretor.zig");
const Parser = @import("../../parser.zig");

pub fn dispatch(
    name: []const u8,
    args: []Values.Value,
    ctx: *Itpr.Context,
    ref: Parser.Location,
) !Values.Value {
    if (std.mem.eql(u8, name, "cos")) return .{ .Float = @cos(args[0].Float) };
    if (std.mem.eql(u8, name, "sin")) return .{ .Float = @sin(args[0].Float) };
    if (std.mem.eql(u8, name, "tan")) return .{ .Float = @tan(args[0].Float) };
    if (std.mem.eql(u8, name, "sqrt")) return .{ .Float = @sqrt(args[0].Float) };
    if (std.mem.eql(u8, name, "abs")) return .{ .Float = @abs(args[0].Float) };
    if (std.mem.eql(u8, name, "pow")) return .{ .Float = std.math.pow(f64, args[0].Float, args[1].Float) };
    if (std.mem.eql(u8, name, "floor")) return .{ .Float = @floor(args[0].Float) };
    if (std.mem.eql(u8, name, "ceil")) return .{ .Float = @ceil(args[0].Float) };
    if (std.mem.eql(u8, name, "min")) return .{ .Float = @min(args[0].Float, args[1].Float) };
    if (std.mem.eql(u8, name, "max")) return .{ .Float = @max(args[0].Float, args[1].Float) };
    if (std.mem.eql(u8, name, "log")) return .{ .Float = @log(args[0].Float) };
    if (std.mem.eql(u8, name, "log2")) return .{ .Float = @log2(args[0].Float) };
    return try Values.makeError(ctx.heap, ref, "Unknown math function '{s}'", .{name});
}
