const std = @import("std");
const ast = @import("ast.zig");
const analyser = @import("analyser.zig");
const errors = @import("errors.zig");

const Context = analyser.Context;

const Allocator = std.mem.Allocator;

pub const Type = union(enum) {
    decided: *ast.Type,
    undecided: void,

    pub fn init(t: *ast.Type) Type {
        return Type{ .decided = t };
    }

    pub fn deinit(self: *const Type, allocator: Allocator) void {
        switch (self.*) {
            .decided => {
                allocator.destroy(self.decided.base);
                allocator.destroy(self.decided);
            },
            .undecided => {},
        }
    }

    pub fn match(self: *Type, t: *ast.Type) bool {
        return switch (self.*) {
            .decided => self.decided.match(t),
            .undecided => true,
        };
    }

    pub fn matchType(self: *const Type, t: Type) bool {
        return switch (self.*) {
            .decided => self.decided.match(t.decided),
            .undecided => switch (t) {
                .decided => true,
                .undecided => false,
            },
        };
    }

    pub fn toString(self: *const Type, allocator: Allocator) []const u8 {
        return switch (self.*) {
            .decided => self.decided.toString(allocator),
            .undecided => "Undecided",
        };
    }
};

pub fn CreateTypeInt(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    const basetype = try allocator.create(ast.TypeBase);
    basetype.* = ast.TypeBase{ .name = "Int" };
    _type.* = ast.Type{ .base = basetype, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeString(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    const basetype = try allocator.create(ast.TypeBase);
    basetype.* = ast.TypeBase{ .name = "String" };
    _type.* = ast.Type{ .base = basetype, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeChar(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    const basetype = try allocator.create(ast.TypeBase);
    basetype.* = ast.TypeBase{ .name = "Char" };
    _type.* = ast.Type{ .base = basetype, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeBool(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    const basetype = try allocator.create(ast.TypeBase);
    basetype.* = ast.TypeBase{ .name = "Bool" };
    _type.* = ast.Type{ .base = basetype, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeVoid(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    const basetype = try allocator.create(ast.TypeBase);
    basetype.* = ast.TypeBase{ .name = "Void" };
    _type.* = ast.Type{ .base = basetype, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

///////////////////////////////////////////////////
///                                             ///
///////////////////////////////////////////////////

pub fn getTypeOfValue(value: *ast.Value, ctx: *Context, allocator: Allocator) !Type {
    return switch (value.*) {
        .intLit => try CreateTypeInt(allocator, false),
        .stringLit => try CreateTypeString(allocator, false),
        .charLit => try CreateTypeChar(allocator, false),
        .varDec => Type{ .undecided = {} },
        .assignement => try CreateTypeVoid(allocator, false), // [TODO]: change wether it is !Void, if the right hand side can return an error
        .parenthesis => try getTypeOfValue(value.parenthesis, ctx, allocator),
        .identifier => |ident| blk: {
            if (ctx.variableExist(ident))
                break :blk ctx.getVariable(ident);
            break :blk Type{ .undecided = {} };
        },
        .scope => try analyser.analyseScope(value.scope, ctx, allocator),
        .funcall => |func| blk: {
            const funcsign = try analyser.analyseValue(func.func, ctx, allocator);
            switch (funcsign) {
                .undecided => break :blk funcsign,
                .decided => |t| {
                    switch (t.base.*) {
                        .name => |name| errors.bbcErrorExit("Can't call a non-function value of type {s}", .{name}, ""),
                        .function => |functype| {
                            break :blk Type{ .decided = functype.retype };
                        },
                    }
                },
            }
            break :blk Type{ .undecided = {} };
        },
        else => unreachable,
        //        .scope => getTypeOfScope(),
    };
}

pub fn inferTypeFuncall(value: *ast.Funcall, ctx: *Context, allocator: Allocator, expType: Type) std.mem.Allocator.Error!void {
    const function_type = try getTypeOfValue(value.func, ctx, allocator);
    switch (function_type) {
        .undecided => errors.bbcErrorExit("Not able to get the type of the function", .{}, ""),
        .decided => {},
    }
    switch (function_type.decided.base.*) {
        .name => |name| errors.bbcErrorExit("The type {s} is not callable", .{name}, ""),
        .function => {},
    }
    const signature = function_type.decided.base.function;
    if (!signature.retype.match(expType.decided))
        errors.bbcErrorExit("Expected type {s}, but the function returns type {s}", .{ signature.retype.toString(allocator), expType.toString(allocator) }, "");
    if (value.args.items.len != signature.argtypes.items.len)
        errors.bbcErrorExit("The number of arguments ({d}) does not match the function's ({d})", .{ value.args.items.len, signature.argtypes.items.len }, "");
    for (value.args.items, signature.argtypes.items) |arg, t| {
        try inferTypeValue(arg, ctx, allocator, Type{ .decided = t });
    }
}

pub fn inferTypeValue(value: *ast.Value, ctx: *Context, allocator: Allocator, expType: Type) std.mem.Allocator.Error!void {
    // expType is for expected type (what type the value should be)
    const voidType = try CreateTypeVoid(allocator, expType.decided.err);
    defer voidType.deinit(allocator);
    const intType = try CreateTypeInt(allocator, false);
    defer intType.deinit(allocator);
    const charType = try CreateTypeChar(allocator, false);
    defer charType.deinit(allocator);
    const stringType = try CreateTypeString(allocator, false);
    defer stringType.deinit(allocator);
    switch (value.*) {
        .varDec => |vardec| {
            const vartype = ctx.getVariable(vardec.name);
            switch (vartype) {
                .undecided => errors.bbcErrorExit("Unable to decide the type of '{s}'", .{vardec.name}, ""),
                .decided => {},
            }
        },
        .identifier => |ident| {
            if (!ctx.variableExist(ident))
                errors.bbcErrorExit("The variable {s} is not declared", .{ident}, "");
            if (!ctx.getVariable(ident).matchType(expType))
                errors.bbcErrorExit("The expected type {s}  does not match the type of '{s}': {s}", .{ expType.toString(allocator), ident, ctx.getVariable(ident).toString(allocator) }, "");
            try ctx.setVariable(ident, expType);
        },
        .assignement => |assign| {
            const rhsType = try getTypeOfValue(assign.rhs, ctx, allocator);
            const lhsType =
                switch (assign.lhs.*) {
                    .identifier => |ident| ctx.getVariable(ident),
                    .varDec => |vardec| ctx.getVariable(vardec.name),
                    else => unreachable,
                };
            switch (rhsType) {
                .decided => {
                    switch (lhsType) {
                        .decided => {
                            if (!rhsType.matchType(lhsType))
                                errors.bbcErrorExit("The right side's type ({s}) of the assignation does not match the left side's ({s})", .{ rhsType.toString(allocator), lhsType.toString(allocator) }, "");
                        },
                        .undecided => {
                            try inferTypeValue(assign.lhs, ctx, allocator, rhsType);
                            try inferTypeValue(assign.rhs, ctx, allocator, rhsType);
                        },
                    }
                    try inferTypeValue(assign.rhs, ctx, allocator, rhsType);
                },
                .undecided => switch (lhsType) {
                    .decided => {
                        try inferTypeValue(assign.lhs, ctx, allocator, lhsType);
                        try inferTypeValue(assign.rhs, ctx, allocator, lhsType);
                    },
                    .undecided => errors.bbcErrorExit("Cannot evaluate the type of both side of the assignation", .{}, ""),
                },
            }
        },
        .intLit => {
            if (!intType.matchType(expType))
                errors.bbcErrorExit("Expected type {s} but it evaluates to Int", .{expType.toString(allocator)}, "");
        },
        .charLit => {
            if (!charType.matchType(expType))
                errors.bbcErrorExit("Expected type {s} but it evaluates to Char", .{expType.toString(allocator)}, "");
        },
        .stringLit => {
            if (!stringType.matchType(expType))
                errors.bbcErrorExit("Expected type {s} but it evaluates to String", .{expType.toString(allocator)}, "");
        },
        .scope => |scope| {
            try inferTypeScope(scope, ctx, allocator, expType);
        },
        .funcall => |funcall| {
            try inferTypeFuncall(funcall, ctx, allocator, expType);
        },
        else => {
            unreachable;
        },
    }
}

pub fn inferTypeScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator, rettype: Type) !void {
    const voidType = try CreateTypeVoid(allocator, rettype.decided.err);
    const items = scope.code.items;
    _ = ctx;
    try inferTypeValue(items[items.len - 1], scope.ctx, allocator, rettype);
    var i: usize = items.len - 1;
    while (i > 0) {
        i -= 1;
        const value = items[i];
        try inferTypeValue(value, scope.ctx, allocator, voidType);
    }

    // Normally, at the end of both evaluation (this is the second)
    // the type of every variable should be known
    var it = scope.ctx.variables.iterator();
    while (it.next()) |kv| {
        switch (kv.value_ptr.*) {
            .undecided => errors.bbcErrorExit("Unable to decide the implicit type of '{s}'' ", .{kv.key_ptr.*}, ""),
            .decided => {},
        }
    }
}
