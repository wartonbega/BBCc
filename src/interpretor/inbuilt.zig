const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("print.zig");
const Values = @import("values.zig");
const Itpr = @import("interpretor.zig");
const Parser = @import("../parser.zig");

const math_lib = @import("inbuilt/math.zig");
const os_lib = @import("inbuilt/os.zig");
const cast_lib = @import("inbuilt/cast.zig");
const strings_lib = @import("inbuilt/strings.zig");

const Context = Itpr.Context;
const Value = Values.Value;

pub fn dispatchBuiltin(name: []const u8, args: std.ArrayList(Values.Value), ctx: *Itpr.Context, reference: Parser.Location) !Values.Value {
    // Route namespaced calls (e.g. "math.cos") to the appropriate library dispatcher.
    if (std.mem.indexOf(u8, name, ".")) |dot| {
        const lib = name[0..dot];
        const func = name[dot + 1 ..];
        if (std.mem.eql(u8, lib, "math"))
            return math_lib.dispatch(func, args.items, ctx, reference);
        if (std.mem.eql(u8, lib, "os"))
            return os_lib.dispatch(func, args.items, ctx, reference);
        if (std.mem.eql(u8, lib, "cast"))
            return cast_lib.dispatch(func, args.items, ctx, reference);
        if (std.mem.eql(u8, lib, "strings"))
            return strings_lib.dispatch(func, args.items, ctx, reference);
        return try Values.makeError(ctx.heap, reference, "Unknown builtin library '{s}'", .{lib});
    }
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
    } else if (std.mem.eql(u8, name, "min")) {
        if (args.items.len == 0)
            return try Values.makeError(ctx.heap, reference, "min expects at least 1 argument", .{});
        return switch (args.items[0]) {
            .Int => |first| blk: {
                var result = first;
                for (args.items[1..]) |arg| {
                    if (arg != .Int)
                        break :blk try Values.makeError(ctx.heap, reference, "min: type mismatch", .{});
                    result = @min(result, arg.Int);
                }
                break :blk Value{ .Int = result };
            },
            .Char => |first| blk: {
                var result = first;
                for (args.items[1..]) |arg| {
                    if (arg != .Char)
                        break :blk try Values.makeError(ctx.heap, reference, "min: type mismatch", .{});
                    result = @min(result, arg.Char);
                }
                break :blk Value{ .Char = result };
            },
            .Float => |first| blk: {
                var result = first;
                for (args.items[1..]) |arg| {
                    if (arg != .Float)
                        break :blk try Values.makeError(ctx.heap, reference, "min: type mismatch", .{});
                    result = @min(result, arg.Float);
                }
                break :blk Value{ .Float = result };
            },
            .Buffer => |buf| blk: {
                if (args.items.len != 1)
                    break :blk try Values.makeError(ctx.heap, reference, "min: buffer overload takes exactly 1 argument", .{});
                if (buf.size == 0)
                    break :blk try Values.makeError(ctx.heap, reference, "min: cannot find min of empty buffer", .{});
                const first = buf.content[0] orelse break :blk try Values.makeError(ctx.heap, reference, "min: buffer has uninitialized elements", .{});
                switch (first) {
                    .Int => {
                        var result = first.Int;
                        for (1..buf.size) |i| {
                            const elem = buf.content[i] orelse break :blk try Values.makeError(ctx.heap, reference, "min: buffer has uninitialized elements", .{});
                            if (elem != .Int) break :blk try Values.makeError(ctx.heap, reference, "min: mixed types in buffer", .{});
                            result = @min(result, elem.Int);
                        }
                        break :blk Value{ .Int = result };
                    },
                    .Char => {
                        var result = first.Char;
                        for (1..buf.size) |i| {
                            const elem = buf.content[i] orelse break :blk try Values.makeError(ctx.heap, reference, "min: buffer has uninitialized elements", .{});
                            if (elem != .Char) break :blk try Values.makeError(ctx.heap, reference, "min: mixed types in buffer", .{});
                            result = @min(result, elem.Char);
                        }
                        break :blk Value{ .Char = result };
                    },
                    else => break :blk try Values.makeError(ctx.heap, reference, "min: unsupported element type {s}", .{@tagName(first)}),
                }
            },
            else => try Values.makeError(ctx.heap, reference, "min: unsupported type {s}", .{@tagName(args.items[0])}),
        };
    } else if (std.mem.eql(u8, name, "max")) {
        if (args.items.len == 0)
            return try Values.makeError(ctx.heap, reference, "max expects at least 1 argument", .{});
        return switch (args.items[0]) {
            .Int => |first| blk: {
                var result = first;
                for (args.items[1..]) |arg| {
                    if (arg != .Int)
                        break :blk try Values.makeError(ctx.heap, reference, "max: type mismatch", .{});
                    result = @max(result, arg.Int);
                }
                break :blk Value{ .Int = result };
            },
            .Char => |first| blk: {
                var result = first;
                for (args.items[1..]) |arg| {
                    if (arg != .Char)
                        break :blk try Values.makeError(ctx.heap, reference, "max: type mismatch", .{});
                    result = @max(result, arg.Char);
                }
                break :blk Value{ .Char = result };
            },
            .Float => |first| blk: {
                var result = first;
                for (args.items[1..]) |arg| {
                    if (arg != .Float)
                        break :blk try Values.makeError(ctx.heap, reference, "max: type mismatch", .{});
                    result = @max(result, arg.Float);
                }
                break :blk Value{ .Float = result };
            },
            .Buffer => |buf| blk: {
                if (args.items.len != 1)
                    break :blk try Values.makeError(ctx.heap, reference, "max: buffer overload takes exactly 1 argument", .{});
                if (buf.size == 0)
                    break :blk try Values.makeError(ctx.heap, reference, "max: cannot find max of empty buffer", .{});
                const first = buf.content[0] orelse break :blk try Values.makeError(ctx.heap, reference, "max: buffer has uninitialized elements", .{});
                switch (first) {
                    .Int => {
                        var result = first.Int;
                        for (1..buf.size) |i| {
                            const elem = buf.content[i] orelse break :blk try Values.makeError(ctx.heap, reference, "max: buffer has uninitialized elements", .{});
                            if (elem != .Int) break :blk try Values.makeError(ctx.heap, reference, "max: mixed types in buffer", .{});
                            result = @max(result, elem.Int);
                        }
                        break :blk Value{ .Int = result };
                    },
                    .Char => {
                        var result = first.Char;
                        for (1..buf.size) |i| {
                            const elem = buf.content[i] orelse break :blk try Values.makeError(ctx.heap, reference, "max: buffer has uninitialized elements", .{});
                            if (elem != .Char) break :blk try Values.makeError(ctx.heap, reference, "max: mixed types in buffer", .{});
                            result = @max(result, elem.Char);
                        }
                        break :blk Value{ .Char = result };
                    },
                    else => break :blk try Values.makeError(ctx.heap, reference, "max: unsupported element type {s}", .{@tagName(first)}),
                }
            },
            else => try Values.makeError(ctx.heap, reference, "max: unsupported type {s}", .{@tagName(args.items[0])}),
        };
    }
    return try Values.makeError(ctx.heap, reference, "Unknown builtin function '{s}'", .{name});
}
