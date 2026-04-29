const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("print.zig");
const Values = @import("values.zig");
const Itpr = @import("interpretor.zig");
const Parser = @import("../parser.zig");

const Context = Itpr.Context;
const Value = Values.Value;

pub fn dispatchBuiltin(name: []const u8, args: std.ArrayList(Values.Value), ctx: *Itpr.Context, reference: Parser.Location) !Values.Value {
    if (std.mem.eql(u8, name, "input")) {
        if (args.items.len > 0) Print.print(args.items[0]);
        const stdin = std.io.getStdIn().reader();
        var buf = std.ArrayList(u8).init(ctx.heap);
        stdin.streamUntilDelimiter(buf.writer(), '\n', null) catch {};
        if (buf.items.len > 0 and buf.items[buf.items.len - 1] == '\r')
            _ = buf.pop();
        const s_obj = try ctx.heap.create(Values.StringObj);
        s_obj.* = .{ .content = buf, .references = 0 };
        return Values.Value{ .String = s_obj };
        //
    } else if (std.mem.eql(u8, name, "read")) {
        const filename = args.items[0].String.content.items;
        var file = std.fs.cwd().openFile(filename, .{}) catch |err| {
            return try Values.makeError(ctx.heap, reference, "Cannot open '{s}': {s}", .{ filename, @errorName(err) });
        };
        defer file.close();

        const stat = file.stat() catch |err| {
            return try Values.makeError(ctx.heap, reference, "Cannot stat '{s}': {s}", .{ filename, @errorName(err) });
        };

        const buf = file.readToEndAlloc(ctx.heap, stat.size) catch |err| {
            return try Values.makeError(ctx.heap, reference, "Cannot read '{s}': {s}", .{ filename, @errorName(err) });
        };
        defer ctx.heap.free(buf);
        var s_content = std.ArrayList(u8).init(ctx.heap);
        try s_content.appendSlice(buf);

        const s_obj = try ctx.heap.create(Values.StringObj);
        s_obj.* = .{ .content = s_content, .references = 0 };
        return Values.Value{ .String = s_obj };
    } else if (std.mem.eql(u8, name, "error")) {
        const message = args.items[0].String.content.items;
        return try Values.makeError(ctx.heap, reference, "{s}", .{message});
    } else if (std.mem.eql(u8, name, "unitOf")) {
        return switch (args.items[0]) {
            .Bool => Value{ .Bool = true },
            .Char => Value{ .Char = 1 },
            .Int => Value{ .Int = 1 },
            else => try Values.makeError(ctx.heap, reference, "Can't make the unit of runtime type {s}", .{@tagName(args.items[0])}),
        };
    } else if (std.mem.eql(u8, name, "zeroOf")) {
        return switch (args.items[0]) {
            .Bool => Value{ .Bool = false },
            .Char => Value{ .Char = 0 },
            .Int => Value{ .Int = 0 },
            else => try Values.makeError(
                ctx.heap,
                reference,
                "Can't make the zero of runtime type {s}",
                .{@tagName(args.items[0])},
            ),
        };
    } else {
        return try Values.makeError(ctx.heap, reference, "Unknown builtin function '{s}'", .{name});
    }
}
