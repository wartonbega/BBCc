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
    heap: std.mem.Allocator,
    variables: std.hash_map.StringHashMap(Values.Value),

    pub fn init(codeContext: *analyser.Context, heap: std.mem.Allocator) Context {
        return Context{
            .parent = null,
            .codeContext = codeContext,
            .heap = heap,
            .variables = .init(heap),
        };
    }

    pub fn deinit(self: *Context) void {
        var it = self.variables.iterator();
        while (it.next()) |v| {
            v.value_ptr.decrementReference(self.heap);
        }
        self.variables.deinit();
    }

    pub fn createChild(self: *Context) Context {
        return Context{
            .parent = self,
            .codeContext = self.codeContext,
            .heap = self.heap,
            .variables = .init(self.heap),
        };
    }

    pub fn createLocalChild(self: *Context) Context {
        var max_parent = self;
        while (max_parent.parent) |p| {
            max_parent = p;
        }
        return Context{
            .parent = max_parent,
            .codeContext = self.codeContext,
            .heap = self.heap,
            .variables = .init(self.heap),
        };
    }

    pub fn setVariable(self: *Context, name: []const u8, value: Values.Value) !void {
        if (self.parent) |parent| {
            if (parent.variableExist(name)) {
                try parent.setVariable(name, value);
                return;
            }
        }
        value.incrementReference();
        if (self.variables.get(name)) |old| {
            old.decrementReference(self.heap);
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

    pub fn createStringLit(self: *Context, value: []const u8) !Values.Value {
        var s_list = std.ArrayList(u8).init(self.heap);
        for (value) |c| {
            s_list.append(c) catch {};
        }
        const s_obj = try self.heap.create(Values.StringObj);
        s_obj.* = .{ .content = s_list, .references = 0 };
        return Values.Value{ .String = s_obj };
    }
};

pub fn interpreteScope(scope: *Ast.Scope, ctx: *Context) !Values.Value {
    for (scope.code.items) |inst| {
        const ret = try ValueInterpretor.interpreteValue(inst, ctx);
        if (ret != .Null)
            return ret;
    }

    return .Null;
}

pub fn interpreteFunction(func: *Ast.funcDef, args: std.ArrayList(Values.Value), parent: ?*Values.Object, ctx: *Context) !Values.Value {
    var child_ctx = ctx.createLocalChild();

    defer child_ctx.deinit();

    const func_name_str = try child_ctx.createStringLit(func.name);
    try child_ctx.setVariable("__name", func_name_str);

    if (parent) |p| {
        var hab_it = p.habitants.iterator();
        while (hab_it.next()) |hab| {
            try child_ctx.setVariable(hab.key_ptr.*, hab.value_ptr.*);
        }
        try child_ctx.setVariable("self", Values.Value{ .Object = p });
    }

    for (args.items, func.arguments.items) |arg, arg_def| {
        try child_ctx.setVariable(arg_def.name, arg);
    }
    const ret = try interpreteScope(func.code, &child_ctx);

    return ret;
}

pub fn interpreteProgram(ast: *Ast.Program, cctx: *analyser.Context, alloc: std.mem.Allocator) !void {
    var heap = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = heap.deinit();
    const heapAllocator = heap.allocator();
    var ctx = Context.init(cctx, heapAllocator);

    _ = ast;

    var func_it = cctx.functions.iterator();
    while (func_it.next()) |function| {
        try ctx.setVariable(try function.value_ptr.*.getName(alloc), .{ .Function = .{
            .func = function.value_ptr.*,
            .parentObj = null,
        } });
    }

    const main_func = cctx.getFunction("main");
    var child_ctx = ctx.createChild();
    defer child_ctx.deinit();
    const ret = interpreteFunction(
        main_func,
        std.ArrayList(Values.Value).init(heapAllocator),
        null,
        &child_ctx,
    ) catch |err| blk: {
        if (err == ContextualError.bbcContextualError) {
            errors.bbcRuntimeError("Uncaught error", .{}, main_func.reference);
        } else {
            std.debug.print("Internal error: {}", .{err});
        }
        break :blk .Null;
    };
    ctx.deinit();

    std.debug.print("---- Return value of programme ----\n", .{});
    Print.println(ret);
}
