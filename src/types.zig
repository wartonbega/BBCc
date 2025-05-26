const std = @import("std");
const ast = @import("ast.zig");
const analyser = @import("analyser.zig");
const errors = @import("errors.zig");
const traits = @import("traits.zig");

const Context = analyser.Context;

const ArrayList = std.ArrayList;

const Allocator = std.mem.Allocator;

pub const Type = union(enum) {
    decided: *ast.Type,
    undecided: ArrayList(traits.Trait),

    pub fn init(t: *ast.Type) Type {
        return Type{ .decided = t };
    }

    pub fn deinit(self: *const Type, allocator: Allocator) void {
        switch (self.*) {
            .decided => {
                allocator.destroy(self.decided);
            },
            .undecided => {},
        }
    }

    pub fn match(self: *const Type, t: *const ast.Type) bool {
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
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "Int" }, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeString(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "String" }, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeChar(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "Char" }, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeBool(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "Bool" }, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeVoid(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "Void" }, .err = err, .references = @intCast(0) };
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
        .varDec => Type{ .undecided = ArrayList(traits.Trait).init(allocator) },
        .assignement => try CreateTypeVoid(allocator, false), // [TODO]: change wether it is !Void, if the right hand side can return an error
        .parenthesis => try getTypeOfValue(value.parenthesis, ctx, allocator),
        .identifier => |ident| blk: {
            if (ctx.variableExist(ident))
                break :blk ctx.getVariable(ident);
            break :blk Type{ .undecided = ArrayList(traits.Trait).init(allocator) };
        },
        .scope => try analyser.analyseScope(value.scope, ctx, allocator),
        .funcall => |func| blk: {
            const funcsign = try analyser.analyseValue(func.func, ctx, allocator);
            switch (funcsign) {
                .undecided => break :blk funcsign,
                .decided => |t| {
                    switch (t.base) {
                        .name => |name| errors.bbcErrorExit("Can't call a non-function value of type {s}", .{name}, ""),
                        .function => |functype| {
                            break :blk Type{ .decided = functype.retype };
                        },
                    }
                },
            }
            break :blk Type{ .undecided = ArrayList(traits.Trait).init(allocator) };
        },
        .binaryOperator => |binop| analyser.analyseBinOp(
            binop.operator,
            ctx,
            try getTypeOfValue(binop.rhs, ctx, allocator),
            try getTypeOfValue(binop.lhs, ctx, allocator),
            allocator,
        ),
        else => {
            std.debug.print("Unimplemented {}", .{value.*});
            unreachable;
        },
        //        .scope => getTypeOfScope(),
    };
}

pub fn inferTypeFuncall(value: *ast.Funcall, ctx: *Context, allocator: Allocator, expType: Type) std.mem.Allocator.Error!void {
    const function_type = try getTypeOfValue(value.func, ctx, allocator);
    switch (function_type) {
        .undecided => errors.bbcErrorExit("Not able to get the type of the function", .{}, ""),
        .decided => {},
    }
    switch (function_type.decided.base) {
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

pub fn inferTypeBinOperation(lhsValue: *ast.Value, rhsValue: *ast.Value, op: ast.binOperator, ctx: *Context, allocator: Allocator, expType: Type) std.mem.Allocator.Error!void {
    // This function infers the types for a binary operator/function call, given the expected type.
    // It is inspired by analyseBinOp, but adapted for inferring types for lhsValue and rhsValue.
    const op_trait = traits.traitFromOperator(op);
    const lhsType = try getTypeOfValue(lhsValue, ctx, allocator);
    const rhsType = try getTypeOfValue(rhsValue, ctx, allocator);
    _ = expType;
    // Check if lhsType implements the required trait for the operator.
    if (!traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, lhsType, op_trait))
        errors.bbcErrorExit("Can't use operator '{s}' on type '{s}', because it does not implement the right trait", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, "");

    switch (lhsType) {
        .decided => |_| {
            // Find the function implementation for the operator on lhsType.
            const funcs = ctx.type_implem.get(lhsType.decided.base.name).?;
            var found = false;
            switch (rhsType) {
                .decided => |_| for (funcs.items) |func| {
                    if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                        continue;
                    if (func.signature.argtypes.items.len != 1)
                        errors.bbcErrorExit("The function {s} does not have enough arguments", .{func.name}, "");
                    const arg_type = func.signature.argtypes.items[0];
                    if (!rhsType.match(arg_type))
                        errors.bbcErrorExit("The types of the argument and the value does not match", .{}, "");
                    found = true;
                },
                .undecided => |rhs_traits| {
                    for (funcs.items) |func| {
                        if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                            continue;
                        if (func.signature.argtypes.items.len != 1)
                            errors.bbcErrorExit("The function {s} does not have enough arguments", .{func.name}, "");
                        const arg_type = func.signature.argtypes.items[0];
                        var all_traits_match = true;
                        for (rhs_traits.items) |required_trait| {
                            if (!traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, Type{ .decided = arg_type }, required_trait)) {
                                all_traits_match = false;
                                break;
                            }
                        }
                        if (all_traits_match) {
                            found = true;
                            try inferTypeValue(rhsValue, ctx, allocator, Type{ .decided = arg_type });
                            break;
                        }
                    }
                },
            }
            if (!found)
                errors.bbcErrorExit("No suitable operator implementation found for '{s}' on type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, "");
        },
        .undecided => |traits_list| {
            // Find all types that implement all traits in traits_list
            var matching_types = ArrayList(*ast.Type).init(allocator);
            defer matching_types.deinit();

            var it = ctx.type_implem.iterator();
            while (it.next()) |kv| {
                const candidate_type = kv.key_ptr.*;
                const tttype = try allocator.create(ast.Type);
                tttype.* = ast.Type{ .base = .{ .name = candidate_type }, .err = false, .references = @intCast(0) };

                var implements_all = true;
                for (traits_list.items) |required_trait| {
                    if (!traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, Type{ .decided = tttype }, required_trait)) {
                        implements_all = false;
                        break;
                    }
                }
                if (implements_all) {
                    try matching_types.append(tttype);
                }
            }
            if (matching_types.items.len == 0) {
                errors.bbcErrorExit("No type implements all required traits for operator '{s}'", .{ast.reprBinOp(op)}, "");
            } else if (matching_types.items.len > 1) {
                errors.bbcErrorExit("Ambiguous operator '{s}': multiple types implement all required traits", .{ast.reprBinOp(op)}, "");
            } else {
                const funcs = ctx.type_implem.get(matching_types.items[0].base.name).?;
                var selected_func: ?@TypeOf(funcs.items[0]) = null;
                switch (rhsType) {
                    .decided => |_| {
                        for (funcs.items) |func| {
                            if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                                continue;
                            if (func.signature.argtypes.items.len != 1)
                                continue;
                            const arg_type = func.signature.argtypes.items[0];
                            if (rhsType.match(arg_type)) {
                                selected_func = func;
                                break;
                            }
                        }
                    },
                    .undecided => |rhs_traits| {
                        for (funcs.items) |func| {
                            if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                                continue;
                            if (func.signature.argtypes.items.len != 1)
                                continue;
                            const arg_type = func.signature.argtypes.items[0];
                            var all_traits_match = true;
                            for (rhs_traits.items) |required_trait| {
                                if (!traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, Type{ .decided = arg_type }, required_trait)) {
                                    all_traits_match = false;
                                    break;
                                }
                            }
                            if (all_traits_match) {
                                selected_func = func;
                                break;
                            }
                        }
                    },
                }
                if (selected_func == null) {
                    errors.bbcErrorExit("No suitable operator implementation found for '{s}' on type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, "");
                }
                switch (rhsType) {
                    .decided => {},
                    .undecided => try inferTypeValue(rhsValue, ctx, allocator, Type{ .decided = selected_func.?.signature.argtypes.items[0] }),
                }
                try inferTypeValue(lhsValue, ctx, allocator, Type{ .decided = matching_types.items[0] });
            }
        },
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
        .binaryOperator => |binop| {
            try inferTypeBinOperation(binop.lhs, binop.rhs, binop.operator, ctx, allocator, expType);
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
