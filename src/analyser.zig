const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");

const exit = std.process.exit;
const Allocator = std.mem.Allocator;
const VarHashmap = std.hash_map.StringHashMap(Types.Type);

const ArrayList = std.ArrayList;

pub const Context = struct {
    variables: VarHashmap,
    parent: ?*Context,

    pub fn init(allocator: Allocator) !*Context {
        const self = try allocator.create(Context);
        const vars = VarHashmap.init(allocator);
        //const vars_llvm = std.hash_map.StringHashMap(types.LLVMValueRef).init(allocator);
        self.* = .{ .variables = vars, .parent = null }; // .variable_ptr_llvm = vars_llvm,
        return self;
    }

    pub fn deinit(self: *Context) void {
        self.variables.deinit();
    }

    pub fn createChild(self: *Context, allocator: Allocator) !*Context {
        const new = try allocator.create(Context);
        const vars = VarHashmap.init(allocator);
        //const vars_llvm = std.hash_map.StringHashMap(types.LLVMValueRef).init(allocator);
        new.* = .{ .variables = vars, .parent = self }; // , .variable_ptr_llvm = vars_llvm
        return new;
    }

    pub fn createVariable(self: *Context, name: []const u8, t: Types.Type) !void {
        try self.variables.put(name, t);
    }

    pub fn setVariable(self: *Context, name: []const u8, t: Types.Type) !void {
        if (self.parent) |parent| {
            if (parent.variableExist(name)) {
                try parent.setVariable(name, t);
            } else try self.variables.put(name, t);
        } else {
            try self.variables.put(name, t);
        }
    }

    pub fn getVariable(self: *Context, name: []const u8) Types.Type {
        if (self.variables.contains(name)) {
            return self.variables.get(name).?;
        } else if (self.parent) |parent| {
            return parent.getVariable(name);
        }
        std.debug.print("{s}\n", .{name});
        return self.variables.get(name).?;
    }

    pub fn variableExist(self: *Context, name: []const u8) bool {
        if (self.parent) |parent|
            return self.variables.contains(name) or parent.variableExist(name);
        return self.variables.contains(name);
    }
};

pub fn analyseAssignationLhs(value: *ast.Value, ctx: *Context, allocator: Allocator, rightType: Types.Type) !void {
    switch (value.*) {
        .varDec => |vardec| {
            if (ctx.variableExist(vardec.name))
                errors.bbcErrorExit("The variable {s} has already been declared before", .{vardec.name}, "");
            try ctx.createVariable(vardec.name, rightType);
        },
        .identifier => |ident| {
            if (!ctx.variableExist(ident))
                errors.bbcErrorExit("The variable {s} has not been declared", .{ident}, "");
            switch (rightType) {
                .decided => {
                    const t = ctx.getVariable(ident);
                    switch (t) {
                        .decided => if (!rightType.matchType(t)) errors.bbcErrorExit("The right side's type ({s}) of the assignation does not match the left side's ({s})", .{ rightType.toString(allocator), t.toString(allocator) }, ""),
                        .undecided => try ctx.setVariable(ident, rightType),
                    }
                },
                .undecided => {},
            }
        },
        else => errors.bbcErrorExit("Cannot assign to {}", .{value.*}, ""),
    }
}

pub fn analyseFuncall(func: *ast.Funcall, ctx: *Context, allocator: Allocator) !Types.Type {
    const funcsign = try analyseValue(func.func, ctx, allocator);

    switch (funcsign) {
        .undecided => return funcsign,
        .decided => |t| {
            switch (t.base.*) {
                .name => |name| errors.bbcErrorExit("Can't call a non-function value of type {s}", .{name}, ""),
                .function => |functype| {
                    if (func.args.items.len != functype.argtypes.items.len)
                        errors.bbcErrorExit("The function expects {d} arguments, but got {d}", .{ func.args.items.len, functype.argtypes.items.len }, "");
                    for (func.args.items, functype.argtypes.items) |a, b| {
                        const argtype = try analyseValue(a, ctx, allocator);
                        switch (argtype) {
                            .undecided => return argtype,
                            .decided => if (!argtype.decided.match(b)) errors.bbcErrorExit("Arguments type {s} doesn't match the function's ({s})", .{ argtype.toString(allocator), b.toString(allocator) }, ""),
                        }
                    }
                    return Types.Type{ .decided = t.base.function.retype };
                },
            }
        },
    }
    return Types.Type{ .undecided = {} };
}

pub fn analyseValue(value: *ast.Value, ctx: *Context, allocator: Allocator) std.mem.Allocator.Error!Types.Type {
    switch (value.*) {
        .varDec => |vardec| {
            if (ctx.variableExist(vardec.name))
                errors.bbcErrorExit("The variable {s} has already been declared before", .{vardec.name}, "");
            try ctx.createVariable(vardec.name, Types.Type{ .undecided = {} });
            return Types.Type{ .undecided = {} };
        },
        .binaryOperator => |binop| {
            _ = try analyseValue(binop.rhs, ctx, allocator);
            _ = try analyseValue(binop.lhs, ctx, allocator);
            return Types.Type{ .undecided = {} };
        },
        .assignement => |assing| {
            const rhsType = try analyseValue(assing.rhs, ctx, allocator);
            try analyseAssignationLhs(assing.lhs, ctx, allocator, rhsType);

            return switch (rhsType) {
                .undecided => Types.Type{ .undecided = {} },
                .decided => Types.CreateTypeVoid(allocator, rhsType.decided.err),
            };
        },
        .If => |ifstmt| {
            _ = try analyseScope(ifstmt.scope, ctx, allocator);
            _ = try analyseValue(ifstmt.condition, ifstmt.scope.ctx, allocator);
            return Types.Type{ .undecided = {} };
        },
        .parenthesis => |val| return try analyseValue(val, ctx, allocator),
        .scope => |scope| return try analyseScope(scope, ctx, allocator),
        .identifier => |ident| {
            if (!ctx.variableExist(ident))
                errors.bbcErrorExit("Variable {s} is not declared", .{ident}, "");

            return ctx.getVariable(ident);
        },
        .intLit => return try Types.CreateTypeInt(allocator, false),
        .charLit => return try Types.CreateTypeChar(allocator, false),
        .stringLit => return try Types.CreateTypeString(allocator, false),
        .funcall => |func| {
            return try analyseFuncall(func, ctx, allocator);
        },
        else => unreachable,
    }
}

pub fn analyseScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator) std.mem.Allocator.Error!Types.Type {
    scope.ctx.deinit();
    allocator.destroy(scope.ctx); // Only works if the allocator used here is the same than in the lexer

    scope.ctx = try ctx.createChild(allocator);
    for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
        _ = try analyseValue(value, scope.ctx, allocator);
    }
    return try analyseValue(scope.code.items[scope.code.items.len - 1], scope.ctx, allocator);
}

pub fn analyseFunction(func: *ast.funcDef, ctx: *Context, allocator: Allocator) !void {
    // Defining the function's signature type
    const functype = try allocator.create(ast.Type);
    const functype_base = try allocator.create(ast.TypeBase);
    var argtypes = ArrayList(*ast.Type).init(allocator);
    for (func.arguments.items) |arg|
        try argtypes.append(arg._type);
    functype_base.* = .{ .function = ast.TypeFunc{ .argtypes = argtypes, .retype = func.return_type } };
    functype.* = .{ .base = functype_base, .err = false, .references = @intCast(0) };

    // Associating the name with the type
    try ctx.setVariable(func.name, Types.Type{ .decided = functype });

    // associating variables
    for (func.arguments.items) |arg| {
        try ctx.setVariable(arg.name, Types.Type{ .decided = arg._type });
    }

    _ = try analyseScope(func.code, ctx, allocator);
    try Types.inferTypeScope(func.code, ctx, allocator, Types.Type.init(func.return_type));
    var it = func.code.ctx.variables.iterator();
    while (it.next()) |kv| {
        std.debug.print("{s} is {s}\n", .{ kv.key_ptr.*, kv.value_ptr.toString(allocator) });
    }
}

pub fn analyse(prog: *ast.Program, ctx: *Context, allocator: Allocator) !void {
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => try analyseFunction(inst.FuncDef, ctx, allocator),
        }
    }
    if (!ctx.variableExist("main"))
        errors.bbcErrorExit("Missing function 'main'", .{}, "");

    switch (ctx.getVariable("main").decided.base.*) {
        .function => |f| {
            if (!f.retype.match((try Types.CreateTypeInt(allocator, true)).decided))
                errors.bbcErrorExit("The function 'main' should return !Int (or matching)", .{}, "");
            if (f.argtypes.items.len != 0)
                errors.bbcErrorExit("'main' function takes no arguements", .{}, "");
        },
        .name => errors.bbcErrorExit("'main' should be a function returning !Int", .{}, ""),
    }
}
