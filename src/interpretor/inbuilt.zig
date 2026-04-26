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

fn dispatchBuiltin(name: []const u8, args: std.ArrayList(Values.Value), ctx: *Itpr.Context, reference: Parser.Location) !Values.Value {
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
    } else if (std.mem.eql(u8, name, "read")) {
        // Read and returns the content of a file

        var file = std.fs.cwd().openFile(args.items[0].String.content.items, .{}) catch {
            return Values.Value{ .Error = .{ .message = "Error while reading the file", .reference = reference } };
        };
        defer file.close();

        const stat = file.stat() catch {
            return Values.Value{ .Error = .{ .message = "Error while reading the file", .reference = reference } };
        };

        const buf = file.readToEndAlloc(ctx.heap, stat.size) catch {
            return Values.Value{ .Error = .{ .message = "Error while reading the file", .reference = reference } };
        };
        defer ctx.heap.free(buf);
        var s_content = std.ArrayList(u8).init(ctx.heap);
        s_content.appendSlice(buf) catch {
            return Values.Value{ .Error = .{ .message = "Error while reading the file", .reference = reference } };
        };

        const s_obj = ctx.heap.create(Values.StringObj) catch {
            return Values.Value{ .Error = .{ .message = "Error while reading the file", .reference = reference } };
        };
        s_obj.* = .{
            .content = s_content,
            .references = 0,
        };

        return Values.Value{ .String = s_obj };
    } else {
        return Values.Value{ .Error = .{
            .message = "Unable to dispatch the inbuilt function",
            .reference = reference,
        } };
    }
}
