const std = @import("std");
const ast = @import("ast.zig");
const analyser = @import("analyser.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");

const Context = analyser.Context;

const ArrayList = std.ArrayList;

const Allocator = std.mem.Allocator;

pub const Type = union(enum) {
    decided: *ast.Type,
    undecided: ArrayList(Traits.Trait),

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

pub fn duplicateWithErrorUnion(allocator: Allocator, base: *ast.Type, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = base.*;
    _type.err = err;
    return Type{ .decided = _type };
}

///////////////////////////////////////////////////
///                                             ///
///////////////////////////////////////////////////

pub fn getFuncallVersion(func: *ast.Funcall, functype: ast.TypeFunc, ctx: *Context, allocator: Allocator) !std.hash_map.StringHashMap(Type) {
    var func_version = std.hash_map.StringHashMap(Type).init(allocator);
    if (functype.typeparam.items.len == 0) // No need to try to analyse everything
        return func_version;
    for (func.args.items, functype.argtypes.items) |a, b| {
        const argtype = try getTypeOfValue(a, ctx, allocator);
        if (b.*.base == .name and analyser.typeparamContains(functype.typeparam, b.base.name)) {
            switch (argtype) {
                .undecided => errors.bbcErrorExit("Unable to determine the type of the argument", .{}, a.getReference()),
                .decided => {
                    // If there's a type alias, then we get to assign it
                    if (func_version.contains(b.*.base.name) and func_version.get(b.*.base.name).? == .decided) {
                        errors.bbcErrorExit("'{s}' is already set to be type '{s}'", .{
                            b.*.base.name,
                            func_version.get(b.*.base.name).?.toString(allocator),
                        }, a.getReference());
                    }
                    try func_version.put(b.*.base.name, argtype);
                },
            }
        } else if (!argtype.decided.match(b)) {
            errors.bbcErrorExit("The argument of type '{s}' don't match the expected type '{s}'", .{ argtype.toString(allocator), b.toString(allocator) }, a.getReference());
        }
    }
    return func_version;
}

pub fn getTypeOfScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator) std.mem.Allocator.Error!Type {
    _ = ctx;
    if (scope.code.items.len > 0) {
        for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
            _ = try getTypeOfValue(value, scope.ctx, allocator);
        }
        return try getTypeOfValue(scope.code.getLast(), scope.ctx, allocator);
    }
    return CreateTypeVoid(allocator, false);
}

pub fn getTypeOfValue(value: *ast.Value, ctx: *Context, allocator: Allocator) std.mem.Allocator.Error!Type {
    return switch (value.*) {
        .intLit => try CreateTypeInt(allocator, false),
        .stringLit => try CreateTypeString(allocator, false),
        .charLit => try CreateTypeChar(allocator, false),
        .varDec => try CreateTypeVoid(allocator, false),
        .boolLit => try CreateTypeBool(allocator, false),
        .assignement => |assign| blk: {
            const rhs_type = try getTypeOfValue(assign.rhs, ctx, allocator);
            if (rhs_type == .undecided)
                errors.bbcErrorExit("Can't decide the type of the right hand side of the assignation", .{}, value.getReference());
            break :blk try CreateTypeVoid(allocator, rhs_type.decided.err);
        },
        .parenthesis => try getTypeOfValue(value.parenthesis, ctx, allocator),
        .identifier => |ident| blk: {
            if (ctx.functionExist(ident.name))
                return Type{ .decided = try analyser.createFunctionSignature(ctx.getFunction(ident.name), allocator) };
            if (ctx.variableExist(ident.name))
                break :blk ctx.getVariable(ident.name);
            break :blk Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
        },
        .scope => try getTypeOfScope(value.scope, ctx, allocator),
        .funcall => |func| blk: {
            // We have to borrow a the majority of the code from analyse.analysefuncall,
            // because it's slightly different here
            const f_signature = try getTypeOfValue(func.func, ctx, allocator);
            // returns the return type of the function
            switch (f_signature) {
                .undecided => return f_signature,
                .decided => |t| {
                    switch (t.base) {
                        .name => |name| errors.bbcErrorExit("Can't call a non-function value of type {s}", .{name}, func.func.getReference()),
                        .function => |functype| {
                            if (func.args.items.len != functype.argtypes.items.len)
                                errors.bbcErrorExit("The function expects {d} arguments, but got {d}", .{ func.args.items.len, functype.argtypes.items.len }, func.func.getReference());
                            // We can build the current function version, which shall have to be compiled later
                            var func_version = try getFuncallVersion(func, functype, ctx, allocator);
                            const ret_type = t.base.function.retype.base;
                            if (ret_type == .name and analyser.typeparamContains(t.base.function.typeparam, ret_type.name)) {
                                if (!func_version.contains(ret_type.name))
                                    errors.bbcErrorExit("Unable to infer the type to type parameter '{s}'", .{ret_type.name}, func.func.getReference());
                                break :blk func_version.get(ret_type.name).?;
                            }
                            break :blk Type{ .decided = t.base.function.retype };
                        },
                    }
                },
            }
            break :blk Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
        },
        .binaryOperator => |binop| analyser.analyseBinOp( // it returns only the return type given the two operands types
            binop.operator,
            ctx,
            try getTypeOfValue(binop.rhs, ctx, allocator),
            try getTypeOfValue(binop.lhs, ctx, allocator),
            binop.reference,
            allocator,
        ),
        // For an if statement, the type is
        .If => |ifstmt| blk: {
            var has_error = false;
            for (ifstmt.scopes.items) |scope| {
                if ((try getTypeOfValue(scope, ctx, allocator)).decided.err) {
                    has_error = true;
                    break; // no need to go further
                }
            }
            if (ifstmt.elsescope) |else_scope| {
                if ((try getTypeOfValue(else_scope, ctx, allocator)).decided.err)
                    has_error = true;
            }
            break :blk duplicateWithErrorUnion(allocator, (try getTypeOfValue(ifstmt.scopes.items[0], ctx, allocator)).decided, has_error);
        },
        .errorCheck => |errcheck| {
            const val_type = try getTypeOfValue(errcheck.value, ctx, allocator);
            const scope_type = try getTypeOfValue(errcheck.scope, ctx, allocator);
            if (val_type == .decided and !val_type.decided.err)
                errors.bbcErrorExit("The value don't have errors to check", .{}, errcheck.value.getReference());
            if (scope_type == .decided and scope_type.decided.err)
                errors.bbcErrorExit("The default scope can't have errors", .{}, errcheck.scope.getReference());
            if (!scope_type.matchType(val_type))
                errors.bbcErrorExit("The scope's type don't match the value's type", .{}, errcheck.reference);
            // We can remove the error
            if (val_type == .decided)
                return duplicateWithErrorUnion(allocator, val_type.decided, false);
            return val_type;
        },

        .While => |whileloop| {
            const cond_type = try getTypeOfValue(whileloop.condition, ctx, allocator);
            const exec_type = try getTypeOfValue(whileloop.exec, ctx, allocator);
            // Can it evaluate to an error union ?
            const has_error = cond_type == .decided and cond_type.decided.err or exec_type == .decided and exec_type.decided.err;
            return CreateTypeVoid(allocator, has_error);
        },
        .structInit => |stc_init| {
            const name = stc_init.name;
            const ret_ast_type = try allocator.create(ast.Type);
            ret_ast_type.base = .{ .name = name };
            return Type{ .decided = ret_ast_type };
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const left_value = try getTypeOfValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    return Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
                if (!ctx.typeDefExist(left_value.decided.base.name)) {
                    errors.bbcErrorExit("Unknown struct type name '{s}'", .{left_value.toString(allocator)}, uop_right.reference);
                }
                return Type{ .decided = ctx.getTypeDef(left_value.decided.base.name).getHabitant(uop_right.operator.pointAttr) };
            } else {
                unreachable;
            }
        },
        .freeKeyword => |_| {
            return CreateTypeVoid(allocator, false);
        },
        else => {
            std.debug.print("Unimplemented {}", .{value.*});
            unreachable;
        },
    };
}

pub fn inferTypeFuncall(value: *ast.Funcall, ctx: *Context, allocator: Allocator, expType: Type) std.mem.Allocator.Error!void {
    const function_type = try getTypeOfValue(value.func, ctx, allocator);
    switch (function_type) {
        .undecided => errors.bbcErrorExit("Not able to get the type of the function", .{}, value.func.getReference()),
        .decided => {},
    }
    switch (function_type.decided.base) {
        .name => |name| errors.bbcErrorExit("The type {s} is not callable", .{name}, value.func.getReference()),
        .function => {},
    }
    const signature = function_type.decided.base.function;
    var func_version = std.hash_map.StringHashMap(Type).init(allocator);
    if (value.args.items.len != signature.argtypes.items.len)
        errors.bbcErrorExit("The number of arguments ({d}) does not match the function's ({d})", .{ value.args.items.len, signature.argtypes.items.len }, value.func.getReference());
    for (value.args.items, signature.argtypes.items) |arg, t| {
        const argtype = try getTypeOfValue(arg, ctx, allocator);
        // Adding a type to the version
        if (t.base == .name and analyser.typeparamContains(signature.typeparam, t.base.name)) {
            // If there's a type alias, then we get to assign it
            if (func_version.contains(t.base.name) and func_version.get(t.base.name).? == .decided) {
                errors.bbcErrorExit("'{s}'' is already set to be type '{s}'", .{
                    t.base.name,
                    func_version.get(t.base.name).?.toString(allocator),
                }, arg.getReference());
            }
            try func_version.put(t.base.name, argtype);
        }
        // Verifying that the type we're trying to infer is (or not) in the function's type aliases
        if (t.base == .name and func_version.contains(t.base.name)) {
            try inferTypeValue(arg, ctx, allocator, func_version.get(t.base.name).?);
        } else {
            try inferTypeValue(arg, ctx, allocator, Type{ .decided = t });
        }
    }

    // now checking if the return type has an alias in the function's version
    const ret_type = signature.retype.base;
    if (ret_type == .name and analyser.typeparamContains(signature.typeparam, ret_type.name)) {
        if (!func_version.contains(ret_type.name))
            errors.bbcErrorExit("Unable to infer the type to type parameter '{s}'", .{ret_type.name}, value.func.getReference());

        if (!func_version.get(ret_type.name).?.match(expType.decided))
            errors.bbcErrorExit("Expected type {s}, but the function returns type {s}", .{ expType.toString(allocator), signature.retype.toString(allocator) }, value.func.getReference());
    } else if (!signature.retype.match(expType.decided))
        errors.bbcErrorExit("Expected type {s}, but the function returns type {s}", .{ expType.toString(allocator), signature.retype.toString(allocator) }, value.func.getReference());

    try ctx.addFunctionToCompile(analyser.functionVersion{
        .name = signature.fname,
        .signature = signature,
        .version = func_version,
    });
}

pub fn inferTypeBinOperation(lhsValue: *ast.Value, rhsValue: *ast.Value, op: ast.binOperator, ctx: *Context, allocator: Allocator, expType: Type) std.mem.Allocator.Error!void {
    // This function infers the types for a binary operator/function call, given the expected type.
    // It is inspired by analyseBinOp, but adapted for inferring types for lhsValue and rhsValue.
    const op_trait = Traits.traitFromOperator(op);
    const lhsType = try getTypeOfValue(lhsValue, ctx, allocator);
    const rhsType = try getTypeOfValue(rhsValue, ctx, allocator);
    _ = expType;
    // Check if lhsType implements the required trait for the operator.
    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, lhsType, op_trait))
        errors.bbcErrorExit("Can't use operator '{s}' on type '{s}', because it does not implement the right trait", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());

    switch (lhsType) {
        .decided => |_| {
            // Find the function implementation for the operator on lhsType.
            if (!ctx.type_implem.contains(lhsType.decided.base.name)) {
                // If it is unknown, then the right type should be the same
                // as the left
                try inferTypeValue(rhsValue, ctx, allocator, lhsType);
                return;
            }
            const funcs = ctx.type_implem.get(lhsType.decided.base.name).?;
            var found = false;
            switch (rhsType) {
                .decided => |_| for (funcs.items) |func| {
                    if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                        continue;
                    if (func.signature.argtypes.items.len != 1)
                        errors.bbcErrorExit("The function {s} does not have enough arguments", .{func.name}, "");
                    const arg_type = func.signature.argtypes.items[0];
                    if (rhsType.match(arg_type))
                        //errors.bbcErrorExit("The types of the argument and the value does not match, expected '{s}' got '{s}'", .{ arg_type.toString(allocator), rhsType.toString(allocator) }, "");
                        found = true;
                },
                .undecided => |rhs_traits| {
                    var found_matching_funcs = ArrayList(analyser.funcPair).init(allocator);
                    defer found_matching_funcs.deinit();
                    for (funcs.items) |func| {
                        if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                            continue;
                        if (func.signature.argtypes.items.len != 1)
                            errors.bbcErrorExit("The function {s} does not have enough arguments", .{func.name}, "");
                        const arg_type = func.signature.argtypes.items[0];
                        if (Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, Type{ .decided = arg_type }, rhs_traits))
                            try found_matching_funcs.append(func);
                    }
                    if (found_matching_funcs.items.len == 0)
                        errors.bbcErrorExit("Found no suitable implementation of '{s}' for type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
                    if (found_matching_funcs.items.len > 1)
                        errors.bbcErrorExit("Found more than 1 suitable implementation of '{s}' for type '{s}'. Can't decide...", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
                    const matched_func = found_matching_funcs.items[0];
                    if (matched_func.signature.argtypes.items.len != 1)
                        errors.bbcErrorExit("The function {s} does not have enough arguments", .{matched_func.name}, lhsValue.getReference());
                    const arg_type = matched_func.signature.argtypes.items[0];
                    try inferTypeValue(rhsValue, ctx, allocator, Type{ .decided = arg_type });
                },
            }
            if (!found)
                errors.bbcErrorExit("No suitable operator implementation found for '{s}' on type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
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
                defer if (!implements_all) allocator.destroy(tttype);

                for (traits_list.items) |required_trait| {
                    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, Type{ .decided = tttype }, required_trait)) {
                        implements_all = false;
                        break;
                    }
                }
                if (implements_all) {
                    try matching_types.append(tttype);
                }
            }
            if (matching_types.items.len == 0) {
                errors.bbcErrorExit("No type implements all required traits for operator '{s}'", .{ast.reprBinOp(op)}, lhsValue.getReference());
            } else if (matching_types.items.len > 1) {
                errors.bbcErrorExit("Ambiguous operator '{s}': multiple types implement all required traits", .{ast.reprBinOp(op)}, lhsValue.getReference());
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
                                if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, Type{ .decided = arg_type }, required_trait)) {
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
                    errors.bbcErrorExit("No suitable operator implementation found for '{s}' on type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
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
                .undecided => errors.bbcErrorExit("Unable to decide the type of '{s}'", .{vardec.name}, vardec.reference),
                .decided => {},
            }
        },
        .identifier => |ident| {
            // identifiers and variables don't support error union
            const real_exp_type = if (expType.decided.err) blk: {
                const rhs_type = try allocator.create(ast.Type);
                rhs_type.* = expType.decided.*;
                rhs_type.err = false;
                break :blk Type{ .decided = rhs_type };
            } else expType; // We can just set it without copying, it's alright
            if (ctx.functionExist(ident.name)) {
                const f_type = Type{ .decided = try analyser.createFunctionSignature(ctx.getFunction(ident.name), allocator) };
                if (!f_type.matchType(real_exp_type))
                    errors.bbcErrorExit("The expected type {s} does not match the type of '{s}': {s}", .{ real_exp_type.toString(allocator), ident.name, ctx.getVariable(ident.name).toString(allocator) }, ident.reference);
                return;
            }
            if (!ctx.variableExist(ident.name))
                errors.bbcErrorExit("The variable {s} is not declared (infer)", .{ident.name}, "");
            if (!ctx.getVariable(ident.name).matchType(real_exp_type))
                errors.bbcErrorExit("The expected type {s}  does not match the type of '{s}': {s}", .{ real_exp_type.toString(allocator), ident.name, ctx.getVariable(ident.name).toString(allocator) }, ident.reference);
            try ctx.setVariable(ident.name, real_exp_type);
        },
        .assignement => |assign| {
            const rhsType = try getTypeOfValue(assign.rhs, ctx, allocator);
            const ret_type = try CreateTypeVoid(allocator, switch (rhsType) {
                .decided => |dec| dec.err,
                .undecided => false,
            });
            if (!ret_type.matchType(expType))
                errors.bbcErrorExit("The expected type '{s}' does not match the type got for an assignation: !Void", .{expType.toString(allocator)}, assign.reference);
            const lhsType =
                switch (assign.lhs.*) {
                    .identifier => |ident| ctx.getVariable(ident.name),
                    .varDec => |vardec| ctx.getVariable(vardec.name),
                    else => unreachable,
                };
            switch (rhsType) {
                .decided => {
                    const real_right_type = if (rhsType.decided.err) blk: {
                        const rhs_type = try allocator.create(ast.Type);
                        rhs_type.* = rhsType.decided.*;
                        rhs_type.err = false;
                        break :blk Type{ .decided = rhs_type };
                    } else rhsType; // We can just set it without copying, it's alright
                    switch (lhsType) {
                        .decided => {
                            if (!real_right_type.matchType(lhsType))
                                errors.bbcErrorExit("The right side's type ({s}) of the assignation does not match the left side's {s}", .{ real_right_type.toString(allocator), lhsType.toString(allocator) }, assign.reference);
                        },
                        .undecided => {
                            try inferTypeValue(assign.lhs, ctx, allocator, real_right_type);
                        },
                    }
                    try inferTypeValue(assign.rhs, ctx, allocator, rhsType);
                },
                .undecided => switch (lhsType) {
                    .decided => {
                        try inferTypeValue(assign.lhs, ctx, allocator, lhsType);
                        try inferTypeValue(assign.rhs, ctx, allocator, lhsType);
                    },
                    .undecided => errors.bbcErrorExit("Cannot evaluate the type of both side of the assignation", .{}, assign.reference),
                },
            }
        },
        .intLit => {
            if (!intType.matchType(expType))
                errors.bbcErrorExit("Expected type {s} but it evaluates to Int", .{expType.toString(allocator)}, value.getReference());
        },
        .charLit => {
            if (!charType.matchType(expType))
                errors.bbcErrorExit("Expected type {s} but it evaluates to Char", .{expType.toString(allocator)}, value.getReference());
        },
        .stringLit => {
            if (!stringType.matchType(expType))
                errors.bbcErrorExit("Expected type {s} but it evaluates to String", .{expType.toString(allocator)}, value.getReference());
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
        .If => |ifstmt| {
            const bool_type = try CreateTypeBool(allocator, false);
            for (ifstmt.conditions.items, ifstmt.scopes.items) |cond, scope| {
                try inferTypeValue(scope, ctx, allocator, expType);
                try inferTypeValue(cond, ctx, allocator, bool_type);
            }
            const void_type = try CreateTypeVoid(allocator, false);
            defer void_type.deinit(allocator);
            if (ifstmt.elsescope) |else_scope| {
                try inferTypeValue(else_scope, ctx, allocator, expType);
            } else if (!void_type.matchType(expType)) { // Else statements are required if the expected type isn't !Void
                errors.bbcErrorExit("The if statements is expected to be of type '{s}' and not '!Void', so it requires an else clause", .{expType.toString(allocator)}, ifstmt.reference);
            }
        },
        .parenthesis => |val| try inferTypeValue(val, ctx, allocator, expType),
        .errorCheck => |errcheck| {
            try inferTypeValue(errcheck.value, ctx, allocator, try duplicateWithErrorUnion(allocator, expType.decided, true));
            try inferTypeValue(errcheck.scope, ctx, allocator, expType);
        },
        .While => |whileloop| {
            const err_allowed = expType == .decided and expType.decided.err;
            const bool_type = try CreateTypeBool(allocator, err_allowed);
            const void_type = try CreateTypeVoid(allocator, err_allowed);
            try inferTypeValue(whileloop.condition, ctx, allocator, bool_type);
            try inferTypeValue(whileloop.exec, ctx, allocator, void_type);
        },
        .boolLit => {
            const bool_type = try CreateTypeBool(allocator, false);
            if (!expType.matchType(bool_type))
                errors.bbcErrorExit("Expected type '{s}' but the bool literal evaluates to 'Bool'", .{expType.toString(allocator)}, value.getReference());
        },

        .structInit => |stc_init| {
            const name = stc_init.name;
            if (!ctx.typeDefExist(name))
                errors.bbcErrorExit("Type name '{s}' don't exist", .{name}, stc_init.reference);
            const orgn = ctx.getTypeDef(name);
            if (stc_init.habitants.count() != orgn.habitants.count() - 2)
                errors.bbcErrorExit(
                    "Not the right number of habitants in the struct initialisation, expected {d} got {d}",
                    .{ stc_init.habitants.count(), orgn.habitants.count() },
                    stc_init.reference,
                );

            var stc_hab_it = stc_init.habitants.iterator();
            while (stc_hab_it.next()) |hab| {
                const hab_name = hab.key_ptr.*;
                if (!orgn.habitantExist(hab_name))
                    errors.bbcErrorExit("Habitant '{s}' is undefined", .{hab_name}, stc_init.reference);
                try inferTypeValue(hab.value_ptr.*, ctx, allocator, Type{ .decided = orgn.getHabitant(hab_name) });
            }
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const left_value = try analyser.analyseValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    errors.bbcErrorExit("Can't decide the left part of this unary operator, and can't infer a type", .{}, uop_right.reference);
                if (!ctx.typeDefExist(left_value.decided.base.name))
                    errors.bbcErrorExit("Can't decide the left part of this unary operator, and can't infer a type", .{}, uop_right.reference);
            } else {
                unreachable;
            }
        },
        .freeKeyword => |_| {},
        else => {
            std.debug.print("Unimplemented {}\n", .{value});
            unreachable;
        },
    }
}

pub fn inferTypeScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator, rettype: Type) !void {
    const voidType = try CreateTypeVoid(allocator, rettype.decided.err);
    const items = scope.code.items;
    if (items.len == 0)
        return;
    _ = ctx;
    try inferTypeValue(items[items.len - 1], scope.ctx, allocator, rettype);
    var i: usize = items.len - 1;
    while (i > 1) {
        i -= 1;
        const value = items[i];
        try inferTypeValue(value, scope.ctx, allocator, voidType);
    }

    // Normally, at the end of both evaluation (this is the second)
    // the type of every variable should be known
    var it = scope.ctx.variables.iterator();
    while (it.next()) |kv| {
        switch (kv.value_ptr.*) {
            .undecided => errors.bbcErrorExit("Unable to decide the implicit type of '{s}'' ", .{kv.key_ptr.*}, scope.reference),
            .decided => {},
        }
    }
}
