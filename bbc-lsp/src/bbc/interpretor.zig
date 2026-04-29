const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("../interpretor/print.zig");
const Values = @import("../interpretor/values.zig");
const ValueInterpretor = @import("valueInterpretor.zig");
const InbuiltFuncs = @import("../inbuilt_funcs.zig");

const debug_gc = @import("build_options").debug_gc;

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
            if (try parent.setVariableOpt(name, value))
                return;
        }
        value.incrementReference();
        if (try self.variables.fetchPut(name, value)) |old_entry| {
            old_entry.value.decrementReference(self.heap);
        }
    }

    pub fn setVariableOpt(self: *Context, name: []const u8, value: Values.Value) !bool {
        if (self.variables.contains(name)) {
            value.incrementReference();
            if (try self.variables.fetchPut(name, value)) |old_entry| {
                old_entry.value.decrementReference(self.heap);
            }
            return true;
        }
        if (self.parent) |parent| {
            return parent.setVariableOpt(name, value);
        }
        return false;
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
        if (self.variables.get(name)) |v| return v;
        return self.parent.?.getVariable(name);
    }

    pub fn getVariableOpt(self: *Context, name: []const u8) ?Values.Value {
        if (self.variables.get(name)) |v| return v;
        if (self.parent) |p| return p.getVariableOpt(name);
        return null;
    }

    pub fn createStringLit(self: *Context, value: []const u8) !Values.Value {
        var s_list = std.ArrayList(u8).init(self.heap);
        try s_list.appendSlice(value);
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

    // Pre-size: __name + args + (habitants + self) if method call
    const var_count = 1 + func.arguments.items.len +
        if (parent) |p| p.habitants.count() + 1 else 0;
    try child_ctx.variables.ensureTotalCapacity(@intCast(var_count));

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
    // Protect the return value from being freed during deinit.
    // The local variable holding it would drop ref to 0 otherwise.
    ret.incrementReference();
    child_ctx.deinit();
    ret.decrementReferenceNoCheck();
    return ret;
}

pub fn interpreteProgram(ast: *Ast.Program, cctx: *analyser.Context, alloc: std.mem.Allocator) !void {
    if (comptime debug_gc) {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        return interpreteProgramImpl(ast, cctx, alloc, gpa.allocator());
    } else {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        return interpreteProgramImpl(ast, cctx, alloc, arena.allocator());
    }
}

fn interpreteProgramImpl(ast: *Ast.Program, cctx: *analyser.Context, alloc: std.mem.Allocator, heapAllocator: std.mem.Allocator) !void {
    var ctx = Context.init(cctx, heapAllocator);

    if (ast.instructions.items.len == 0)
        return;

    var func_it = cctx.functions.iterator();
    while (func_it.next()) |function| {
        try ctx.setVariable(try function.value_ptr.*.getName(alloc), .{ .Function = .{
            .func = function.value_ptr.*,
            .parentObj = null,
        } });
    }

    // Build NamespaceObj tree from dotted function names so that e.g. `math` evaluates
    // to a Namespace value enabling `(math).square` and `mathw.math.square`.
    {
        // Pass 1: create a NamespaceObj for every unique namespace prefix.
        var ns_objs = std.StringHashMap(*Values.NamespaceObj).init(heapAllocator);
        defer ns_objs.deinit();

        var it1 = cctx.functions.iterator();
        while (it1.next()) |entry| {
            const flat_name = try entry.value_ptr.*.getName(alloc);
            var remaining = flat_name;
            var prefix = std.ArrayList(u8).init(heapAllocator);
            defer prefix.deinit();
            while (std.mem.indexOf(u8, remaining, ".")) |dot| {
                const seg = remaining[0..dot];
                if (prefix.items.len > 0) try prefix.append('.');
                try prefix.appendSlice(seg);
                const key = try heapAllocator.dupe(u8, prefix.items);
                if (!ns_objs.contains(key)) {
                    const ns_obj = try heapAllocator.create(Values.NamespaceObj);
                    ns_obj.* = .{
                        .name = key,
                        .members = std.StringHashMap(Values.Value).init(heapAllocator),
                        .references = 0,
                    };
                    try ns_objs.put(key, ns_obj);
                } else heapAllocator.free(key);
                remaining = remaining[dot + 1 ..];
            }
        }

        // Pass 2: add leaf function members (e.g. "square" -> Function into namespace "math").
        var it2 = cctx.functions.iterator();
        while (it2.next()) |entry| {
            const flat_name = try entry.value_ptr.*.getName(alloc);
            if (std.mem.lastIndexOf(u8, flat_name, ".")) |last_dot| {
                const ns_key = flat_name[0..last_dot];
                const member_key = flat_name[last_dot + 1 ..];
                if (ns_objs.get(ns_key)) |ns_obj| {
                    if (ctx.getVariableOpt(flat_name)) |fn_val| {
                        try ns_obj.members.put(member_key, fn_val);
                    }
                }
            }
        }

        // Pass 3: add sub-namespace members (e.g. "math" -> Namespace into "mathw").
        var it3 = ns_objs.iterator();
        while (it3.next()) |ns_entry| {
            const ns_key = ns_entry.key_ptr.*;
            if (std.mem.lastIndexOf(u8, ns_key, ".")) |last_dot| {
                const parent_key = ns_key[0..last_dot];
                const member_key = ns_key[last_dot + 1 ..];
                if (ns_objs.get(parent_key)) |parent_ns| {
                    const sub_val = Values.Value{ .Namespace = ns_entry.value_ptr.* };
                    sub_val.incrementReference();
                    try parent_ns.members.put(member_key, sub_val);
                }
            }
        }

        // Pass 4: register top-level namespaces (no dot in key) as variables.
        var it4 = ns_objs.iterator();
        while (it4.next()) |ns_entry| {
            const ns_key = ns_entry.key_ptr.*;
            if (std.mem.indexOf(u8, ns_key, ".") == null) {
                try ctx.setVariable(ns_key, Values.Value{ .Namespace = ns_entry.value_ptr.* });
            }
        }
    }

    const inbuilt_list = try InbuiltFuncs.load(alloc);
    for (inbuilt_list) |f|
        try ctx.setVariable(f.name, Values.Value{ .BuiltinFunction = f.name });

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
    ret.checkReference(heapAllocator);
}
