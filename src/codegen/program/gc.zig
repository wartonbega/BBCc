const std = @import("std");
const analyser = @import("../../analyser.zig");
const bbcTypes = @import("../../types.zig");
const Inst = @import("../instructions.zig");
const Compiler = @import("../compiler.zig").Compiler;

/// Returns true if this type should be GC-decremented at scope exit.
/// Conservative: only handles named types with auto-generated free functions
/// (user structs and String). Buffer/generic/Error are skipped until their
/// destructors are properly generated.
pub fn shouldGcDecVar(t: bbcTypes.Type, cctx: *analyser.Context) bool {
    if (t != .decided) return false;
    return switch (t.decided.base) {
        .name => |name| cctx.typeDefExist(name) or std.mem.eql(u8, name, "String"),
        .generic => |g| cctx.typeDefExist(g.name),
        .buffer => true,
        .bound_method => true,
        else => false,
    };
}

/// Replace NASM-invalid characters (from generic type names) with underscores.
fn sanitize(name: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const buf = try allocator.alloc(u8, name.len);
    for (name, 0..) |c, i| {
        buf[i] = switch (c) {
            '<', '>', ',', ' ', '!' => '_',
            else => c,
        };
    }
    return buf;
}

/// Returns the label of the type-specific destructor for global.object.free.
pub fn getFreeFnLabel(t: bbcTypes.Type, cctx: *analyser.Context, allocator: std.mem.Allocator) ![]const u8 {
    return switch (t.decided.base) {
        .name => |name| try std.fmt.allocPrint(allocator, "global.{s}.free", .{try sanitize(name, allocator)}),
        .generic => |g| try std.fmt.allocPrint(allocator, "global.{s}.free", .{try sanitize(g.name, allocator)}),
        .bound_method => "global.BoundMethod.free",
        .buffer => |elem| blk: {
            const elem_free: []const u8 = switch (elem.base) {
                .name => |name| if (cctx.typeDefExist(name) or std.mem.eql(u8, name, "String"))
                    try std.fmt.allocPrint(allocator, "global.Buffer_{s}_.free", .{try sanitize(name, allocator)})
                else
                    "global.Buffer.free",
                .generic => |g| try std.fmt.allocPrint(allocator, "global.Buffer_{s}_.free", .{try sanitize(g.name, allocator)}),
                .buffer => "global.Buffer_Buffer_.free",
                else => "global.Buffer.free",
            };
            break :blk elem_free;
        },
        else => unreachable,
    };
}

/// Inline refcount increment: [ptr_reg + 0] += 1.
pub fn emitGcInc(ptr_reg: Inst.Register, compiler: *Compiler) !void {
    try compiler.addInstruction(.{ .gc_inc = .{ .ptr = ptr_reg } });
}

/// Inline refcount decrement WITHOUT calling the destructor.
/// Used to undo a protective gc_inc so the caller takes ownership at refcount=1.
pub fn emitGcDecNoCheck(ptr_reg: Inst.Register, compiler: *Compiler) !void {
    try compiler.addInstruction(.{ .gc_dec_no_check = .{ .ptr = ptr_reg } });
}

/// Decrement refcount via global.object.free (calls type destructor if count hits 0).
/// Bakes the current stack depth for 16-byte alignment; net RSP change is zero.
pub fn emitGcDec(ptr_reg: Inst.Register, free_fn: []const u8, compiler: *Compiler) !void {
    try compiler.addInstruction(.{ .gc_dec = .{
        .ptr = ptr_reg,
        .free_fn = free_fn,
        .stack_size = @intCast(compiler.stack_size),
    } });
}
