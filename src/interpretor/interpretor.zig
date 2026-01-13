const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("../interpretor/print.zig");
const Values = @import("../interpretor/values.zig");
const ValueInterpretor = @import("valueInterpretor.zig");

pub const ContextualError = error{ bbcContextualError, UnknownFunction };

pub const Context = struct {
    parent: ?*Context,
    codeContext: *analyser.Context,
    GPAlloc: std.mem.Allocator,
    heap: std.mem.Allocator,
    variables: std.hash_map.StringHashMap(Values.Value),

    pub fn init(codeContext: *analyser.Context, alloc: std.mem.Allocator, heap: std.mem.Allocator) Context {
        return Context{
            .parent = null,
            .codeContext = codeContext,
            .GPAlloc = alloc,
            .heap = heap,
            .variables = .init(alloc),
        };
    }

    pub fn createChild(self: *Context) Context {
        return Context{
            .parent = self,
            .codeContext = self.codeContext,
            .GPAlloc = self.GPAlloc,
            .heap = self.heap,
            .variables = .init(self.GPAlloc),
        };
    }

    pub fn setVariable(self: *Context, name: []const u8, value: Values.Value) !void {
        if (self.parent) |parent| {
            if (parent.variableExist(name)) {
                try parent.setVariable(name, value);
                return;
            }
        }
        try self.variables.put(name, value);
    }

    pub fn variableExist(self: *Context, name: []const u8) bool {
        if (self.variables.contains(name))
            return true;
        if (self.parent) |parent| {
            return parent.variableExist(name);
        }
        return false;
    }

    pub fn getVariable(self: *Context, name: []const u8) Values.Value {
        if (self.variables.contains(name))
            return self.variables.get(name).?;
        return self.parent.?.getVariable(name);
    }
};

pub fn interpreteScope(scope: *Ast.Scope, ctx: *Context) !Values.Value {
    var var_it = scope.ctx.variables.iterator();
    while (var_it.next()) |variable| {
        const name = variable.key_ptr.*;
        try ctx.setVariable(name, .{ .Null = {} });
    }

    for (scope.code.items) |inst| {
        const ret = try ValueInterpretor.interpreteValue(inst, ctx);
        if (ret != .Null)
            return ret;
    }
    return .Null;
}

pub fn interpreteFunction(func: *Ast.funcDef, args: std.ArrayList(Values.Value), ctx: *Context) !Values.Value {
    for (args.items, func.arguments.items) |arg, arg_def| {
        try ctx.setVariable(arg_def.name, arg);
    }
    var child_ctx = ctx.createChild();
    return try interpreteScope(func.code, &child_ctx);
}

pub fn interpreteProgram(ast: *Ast.Program, cctx: *analyser.Context, alloc: std.mem.Allocator) !void {
    var heap = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer heap.deinit();
    const heapAllocator = heap.allocator();
    var ctx = Context.init(cctx, alloc, heapAllocator);

    _ = ast;

    var func_it = cctx.functions.iterator();
    while (func_it.next()) |function| {
        try ctx.setVariable(function.key_ptr.*, .{ .Function = function.value_ptr.* });
    }

    const main_func = cctx.getFunction("main");
    var child_ctx = ctx.createChild();
    const ret = interpreteFunction(
        main_func,
        std.ArrayList(Values.Value).init(alloc),
        &child_ctx,
    ) catch |err| blk: {
        if (err == ContextualError.bbcContextualError) {
            errors.bbcRuntimeError("Uncaught error", .{}, main_func.reference);
        } else {
            std.debug.print("Internal error: {}", .{err});
        }
        break :blk .Null;
    };

    std.debug.print("---- Return value of programme ----\n", .{});
    Print.println(ret);
}
