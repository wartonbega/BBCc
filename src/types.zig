const std = @import("std");
const ast = @import("ast.zig");
const analyser = @import("analyser.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");
const InbuiltFuncs = @import("inbuilt_funcs.zig");

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

pub fn CreateTypeFloat(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "Float" }, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeVoid(allocator: Allocator, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .name = "Void" }, .err = err, .references = @intCast(0) };
    return Type{ .decided = _type };
}

pub fn CreateTypeBuffer(allocator: Allocator, elem_type: *ast.Type, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = ast.Type{ .base = ast.TypeBase{ .buffer = elem_type }, .err = err, .references = 0 };
    return Type{ .decided = _type };
}

pub fn duplicateWithErrorUnion(allocator: Allocator, base: *ast.Type, err: bool) !Type {
    const _type = try allocator.create(ast.Type);
    _type.* = base.*;
    _type.err = err;
    return Type{ .decided = _type };
}

pub fn isVoid(t: *ast.Type) bool {
    if (t.base == .name and std.mem.eql(u8, t.base.name, "Void"))
        return true;
    return false;
}

pub fn wrapWithErr(t: *ast.Type, allocator: Allocator) !*ast.Type {
    if (t.err) return t;
    const et = try allocator.create(ast.Type);
    et.* = t.*;
    et.err = true;
    return et;
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
        const is_type_param = b.*.base == .name and analyser.typeparamContains(functype.typeparam, b.base.name);
        const is_buffer_of_type_param = b.*.base == .buffer and
            b.base.buffer.base == .name and
            analyser.typeparamContains(functype.typeparam, b.base.buffer.base.name);
        if (is_type_param) {
            switch (argtype) {
                .undecided => try ctx.Error("Unable to determine the type of the argument", .{}, a.getReference()),
                .decided => {
                    if (func_version.contains(b.*.base.name) and func_version.get(b.*.base.name).? == .decided) {
                        if (!argtype.matchType(func_version.get(b.*.base.name).?))
                            try ctx.Error("Type parameter '{s}' was already bound to '{s}' but got '{s}'", .{
                                b.*.base.name,
                                func_version.get(b.*.base.name).?.toString(allocator),
                                argtype.toString(allocator),
                            }, a.getReference());
                    } else {
                        try func_version.put(b.*.base.name, argtype);
                    }
                },
            }
        } else if (is_buffer_of_type_param) {
            if (argtype == .decided and argtype.decided.base == .buffer) {
                const tp_name = b.base.buffer.base.name;
                const elem_type = Type{ .decided = argtype.decided.base.buffer };
                if (!func_version.contains(tp_name))
                    try func_version.put(tp_name, elem_type);
            }
        } else if (b.*.base == .generic) {
            const generic = b.base.generic;
            var any_tp = false;
            for (generic.params.items) |param| {
                if (param.base == .name and analyser.typeparamContains(functype.typeparam, param.base.name)) {
                    any_tp = true;
                    break;
                }
            }
            if (any_tp and argtype == .decided and argtype.decided.base == .name) {
                const arg_spec_name = argtype.decided.base.name;
                if (ctx.typeDefExist(arg_spec_name) and ctx.typeDefExist(generic.name)) {
                    const arg_spec = ctx.getTypeDef(arg_spec_name);
                    const base_orgn = ctx.getTypeDef(generic.name);
                    if (arg_spec.type_bindings) |bindings| {
                        for (generic.params.items, 0..) |param, i| {
                            if (param.base != .name) continue;
                            const func_tp_name = param.base.name;
                            if (!analyser.typeparamContains(functype.typeparam, func_tp_name)) continue;
                            if (i >= base_orgn.typeparam.items.len) continue;
                            const struct_tp_name = base_orgn.typeparam.items[i].name;
                            if (bindings.get(struct_tp_name)) |concrete| {
                                if (!func_version.contains(func_tp_name))
                                    try func_version.put(func_tp_name, Type{ .decided = concrete });
                            }
                        }
                    }
                }
            }
        } else if (!argtype.decided.match(b)) {
            try ctx.Error("The argument of type '{s}' don't match the expected type '{s}'", .{ argtype.toString(allocator), b.toString(allocator) }, a.getReference());
        }
    }
    return func_version;
}

pub fn getTypeOfScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator) (std.mem.Allocator.Error || errors.bbcErrors)!Type {
    _ = ctx;
    var has_error = false;
    if (scope.code.items.len > 0) {
        for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
            const sec_ret = try getTypeOfValue(value, scope.ctx, allocator);
            if (sec_ret == .decided and sec_ret.decided.err)
                has_error = true;
        }
        const ret = try getTypeOfValue(scope.code.items[scope.code.items.len - 1], scope.ctx, allocator);
        if (ret == .decided)
            return try duplicateWithErrorUnion(allocator, ret.decided, ret.decided.err or has_error);
        return ret;
    }
    return CreateTypeVoid(allocator, false);
}

pub fn getTypeOfValue(value: *ast.Value, ctx: *Context, allocator: Allocator) (std.mem.Allocator.Error || errors.bbcErrors)!Type {
    return switch (value.*) {
        .intLit => try CreateTypeInt(allocator, false),
        .floatLit => try CreateTypeFloat(allocator, false),
        .stringLit => try CreateTypeString(allocator, false),
        .charLit => try CreateTypeChar(allocator, false),
        .varDec => try CreateTypeVoid(allocator, false),
        .boolLit => try CreateTypeBool(allocator, false),
        .nullLit => return CreateTypeVoid(allocator, false),
        .assignement => |assign| blk: {
            const rhs_type = try getTypeOfValue(assign.rhs, ctx, allocator);
            if (rhs_type == .undecided)
                try ctx.Error("Can't decide the type of the right hand side of the assignation", .{}, value.getReference());
            break :blk try CreateTypeVoid(allocator, rhs_type.decided.err);
        },
        .parenthesis => try getTypeOfValue(value.parenthesis, ctx, allocator),
        .identifier => |ident| blk: {
            if (ctx.functionExist(ident.name))
                return Type{ .decided = try analyser.createFunctionSignature(ctx.getFunction(ident.name), allocator) };
            if (ctx.inbuilt_funcs.contains(ident.name))
                break :blk try CreateTypeVoid(allocator, false);
            if (ctx.variableExist(ident.name))
                break :blk ctx.getVariable(ident.name);
            if (ctx.isNamespace(ident.name)) {
                const ns_t = try allocator.create(ast.Type);
                ns_t.* = .{ .base = .{ .import_ns = ident.name }, .err = false, .references = 0 };
                break :blk Type{ .decided = ns_t };
            }
            break :blk Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
        },
        .scope => try getTypeOfScope(value.scope, ctx, allocator),
        .funcall => |func| blk: {
            // Intercept inbuilt function calls
            if (func.func.* == .identifier and ctx.inbuilt_funcs.contains(func.func.identifier.name)) {
                const indef = ctx.inbuilt_funcs.get(func.func.identifier.name).?;
                var has_error = false;
                for (func.args.items) |arg| {
                    const at = try getTypeOfValue(arg, ctx, allocator);
                    if (at == .decided and at.decided.err) has_error = true;
                }
                const ret = try allocator.create(ast.Type);
                ret.* = .{ .base = .{ .name = indef.return_type }, .err = if (indef.propagate_errors) has_error else false, .references = 0 };
                break :blk Type{ .decided = ret };
            }
            // We have to borrow a the majority of the code from analyse.analysefuncall,
            // because it's slightly different here
            const f_signature = try getTypeOfValue(func.func, ctx, allocator);
            // returns the return type of the function
            switch (f_signature) {
                .undecided => return f_signature,
                .decided => |t| {
                    switch (t.base) {
                        .name => |name| try ctx.Error("Can't call a non-function value of type {s}", .{name}, func.func.getReference()),
                        .buffer => try ctx.Error("Can't call a buffer value", .{}, func.func.getReference()),
                        .generic => |g| try ctx.Error("Can't call a generic type '{s}'", .{g.name}, func.func.getReference()),
                        .import_ns => |ns| try ctx.Error("Namespace '{s}' is not callable", .{ns}, func.func.getReference()),
                        .function => |functype| {
                            if (func.args.items.len != functype.argtypes.items.len)
                                try ctx.Error("The function expects {d} arguments, but got {d}", .{ functype.argtypes.items.len, func.args.items.len }, func.func.getReference());
                            // We can build the current function version, which shall have to be compiled later
                            var func_version = try getFuncallVersion(func, functype, ctx, allocator);
                            const ret_type = t.base.function.retype.base;
                            if (ret_type == .name and analyser.typeparamContains(t.base.function.typeparam, ret_type.name)) {
                                if (!func_version.contains(ret_type.name))
                                    try ctx.Error("Unable to infer the type to type parameter '{s}'", .{ret_type.name}, func.func.getReference());
                                break :blk func_version.get(ret_type.name).?;
                            } else if (ret_type == .generic) {
                                var resolved_params = ArrayList(*ast.Type).init(allocator);
                                for (ret_type.generic.params.items) |param| {
                                    const rp: *ast.Type = if (param.base == .name and func_version.contains(param.base.name))
                                        func_version.get(param.base.name).?.decided
                                    else
                                        param;
                                    try resolved_params.append(rp);
                                }
                                const resolved_generic = ast.TypeGeneric{ .name = ret_type.generic.name, .params = resolved_params };
                                const spec_name = try analyser.ensureGenericSpecialization(resolved_generic, ctx, allocator);
                                const resolved = try allocator.create(ast.Type);
                                resolved.* = ast.Type{ .base = .{ .name = spec_name }, .err = t.base.function.retype.err, .references = 0 };
                                break :blk Type{ .decided = resolved };
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
                try ctx.Error("The value don't have errors to check", .{}, errcheck.value.getReference());
            if (scope_type == .decided and scope_type.decided.err)
                try ctx.Error("The default scope can't have errors", .{}, errcheck.scope.getReference());
            if (!scope_type.matchType(val_type))
                try ctx.Error("The scope's type don't match the value's type", .{}, errcheck.reference);
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
            ret_ast_type.err = false;
            ret_ast_type.references = 0;
            return Type{ .decided = ret_ast_type };
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const attr = uop_right.operator.pointAttr;
                // Flat qualified name resolution: handles math.func, (math).func, mathw.math.func
                if (analyser.tryExtractQualifiedIdent(uop_right.expr, allocator)) |base_name| {
                    const qualified = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ base_name, attr });
                    if (ctx.functionExist(qualified))
                        return Type{ .decided = try analyser.createFunctionSignature(ctx.getFunction(qualified), allocator) };
                    if (ctx.isNamespace(qualified)) {
                        const ns_t = try allocator.create(ast.Type);
                        ns_t.* = .{ .base = .{ .import_ns = qualified }, .err = false, .references = 0 };
                        return Type{ .decided = ns_t };
                    }
                }
                const left_value = try getTypeOfValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    return Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
                // Namespace value held in a variable: `let ns = math; ns.func`
                if (left_value.decided.base == .import_ns) {
                    const ns_name = left_value.decided.base.import_ns;
                    const qualified = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ ns_name, attr });
                    if (ctx.functionExist(qualified))
                        return Type{ .decided = try analyser.createFunctionSignature(ctx.getFunction(qualified), allocator) };
                    if (ctx.isNamespace(qualified)) {
                        const ns_t = try allocator.create(ast.Type);
                        ns_t.* = .{ .base = .{ .import_ns = qualified }, .err = false, .references = 0 };
                        return Type{ .decided = ns_t };
                    }
                    return Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
                }
                if (left_value.decided.base == .buffer) {
                    if (!std.mem.eql(u8, attr, "_size") and !std.mem.eql(u8, attr, "_count"))
                        try ctx.Error("Buffer only has '_size' and '_count' attributes, not '{s}'", .{attr}, uop_right.reference);
                    return CreateTypeInt(allocator, false);
                }
                const struct_name: []const u8 = switch (left_value.decided.base) {
                    .name => |n| n,
                    .generic => |g| try analyser.ensureGenericSpecialization(g, ctx, allocator),
                    .buffer => unreachable,
                    .import_ns => unreachable, // handled above
                    .function => {
                        try ctx.Error("Cannot access attribute of a function type", .{}, uop_right.reference);
                        unreachable;
                    },
                };
                if (!ctx.typeDefExist(struct_name)) {
                    try ctx.Error("Unknown struct type name '{s}'", .{struct_name}, uop_right.reference);
                }
                return Type{ .decided = ctx.getTypeDef(struct_name).getHabitant(attr) };
            } else {
                unreachable;
            }
        },
        .freeKeyword => |_| {
            return CreateTypeVoid(allocator, false);
        },
        .Print => |print| {
            var has_error = false;
            for (print.args.items) |arg| {
                const arg_t = try getTypeOfValue(arg, ctx, allocator);
                if (arg_t == .decided and arg_t.decided.err)
                    has_error = true;
            }
            return CreateTypeVoid(allocator, has_error);
        },
        .function => |func| {
            const ret_type = try allocator.create(ast.Type);
            var functype = ast.TypeFunc{
                .argtypes = .init(allocator),
                .retype = func.return_type,
                .typeparam = .init(allocator),
                .fname = func.name,
            };
            for (func.arguments.items) |arg| {
                try functype.argtypes.append(arg._type);
            }
            ret_type.* = ast.Type{
                .base = .{ .function = functype },
                .err = false,
                .references = 0,
            };
            return Type{ .decided = ret_type };
        },
        .bufferAlloc => |ba| {
            const ret = try allocator.create(ast.Type);
            ret.* = ast.Type{ .base = .{ .buffer = ba.elem_type }, .err = false, .references = 0 };
            return Type{ .decided = ret };
        },
        .bufferLit => |bl| {
            for (bl.elements.items) |elem| {
                const t = try getTypeOfValue(elem, ctx, allocator);
                if (t == .decided) {
                    const ret = try allocator.create(ast.Type);
                    ret.* = ast.Type{ .base = .{ .buffer = t.decided }, .err = false, .references = 0 };
                    return Type{ .decided = ret };
                }
            }
            return Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
        },
        .bufferIndex => |bi| {
            const buf_type = try getTypeOfValue(bi.buffer, ctx, allocator);
            if (buf_type == .undecided) return buf_type;
            switch (buf_type.decided.base) {
                .buffer => return Type{ .decided = try wrapWithErr(buf_type.decided.base.buffer, allocator) },
                .name => |name| {
                    if (ctx.type_implem.get(name)) |impls| {
                        for (impls.items) |func| {
                            if (std.mem.eql(u8, func.name, "Subscription"))
                                return Type{ .decided = try wrapWithErr(func.signature.retype, allocator) };
                        }
                    }
                    const has_user_sub = if (ctx.user_trait_impl.get(name)) |impls| blk: {
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, "Subscription")) break :blk true;
                        break :blk false;
                    } else false;
                    if (has_user_sub and ctx.typeDefExist(name)) {
                        const struct_def = ctx.getTypeDef(name);
                        if (struct_def.habitants.get("index")) |index_sig| {
                            if (index_sig.base == .function)
                                return Type{ .decided = try wrapWithErr(index_sig.base.function.retype, allocator) };
                        }
                    }
                    return buf_type;
                },
                .generic => |gen| {
                    const base_name: []const u8 = gen.name;
                    const has_subs = if (ctx.user_trait_impl.get(base_name)) |impls| blk: {
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, "Subscription")) break :blk true;
                        break :blk false;
                    } else false;
                    if (!has_subs)
                        try ctx.Error(
                            "Type '{s}' does not implement the 'Subscription' trait",
                            .{base_name},
                            bi.buffer.getReference(),
                        );
                    const spec_name = try analyser.ensureGenericSpecialization(gen, ctx, allocator);
                    if (ctx.typeDefExist(spec_name)) {
                        const struct_def = ctx.getTypeDef(spec_name);
                        if (struct_def.habitants.get("index")) |index_sig| {
                            if (index_sig.base == .function)
                                return Type{ .decided = try wrapWithErr(index_sig.base.function.retype, allocator) };
                        }
                    }
                    try ctx.Error("The struct type '{s}' doesn't allow for indexation", .{buf_type.toString(allocator)}, bi.reference);
                    return Type{ .undecided = .init(allocator) };
                },
                else => return buf_type,
            }
        },
        .notOp => |not| {
            const expr_type = try getTypeOfValue(not.expr, ctx, allocator);
            return try analyser.analyseNotOp(ctx, expr_type, not.reference, allocator);
        },

        else => {
            std.debug.print("Unimplemented {}", .{value.*});
            unreachable;
        },
    };
}

pub fn inferTypeFuncall(value: *ast.Funcall, ctx: *Context, allocator: Allocator, expType: Type) (std.mem.Allocator.Error || errors.bbcErrors)!void {
    // Intercept inbuilt function calls — args with typed params get hints, Any params are unconstrained
    if (value.func.* == .identifier and ctx.inbuilt_funcs.contains(value.func.identifier.name)) {
        const indef = ctx.inbuilt_funcs.get(value.func.identifier.name).?;
        for (value.args.items, 0..) |arg, i| {
            if (indef.params.len == 0) break;
            const param_idx = if (i < indef.params.len) i else indef.params.len - 1;
            const param = indef.params[param_idx];
            if (!param.any) {
                const expected_t = try allocator.create(ast.Type);
                expected_t.* = .{ .base = .{ .name = param.type_name }, .err = false, .references = 0 };
                try inferTypeValue(arg, ctx, allocator, Type{ .decided = expected_t });
            }
        }
        return;
    }
    const function_type = try getTypeOfValue(value.func, ctx, allocator);
    switch (function_type) {
        .undecided => try ctx.Error("Not able to get the type of the function", .{}, value.func.getReference()),
        .decided => {},
    }
    switch (function_type.decided.base) {
        .name => |name| try ctx.Error("The type {s} is not callable", .{name}, value.func.getReference()),
        .buffer => try ctx.Error("A buffer is not callable", .{}, value.func.getReference()),
        .generic => |g| try ctx.Error("The generic type '{s}' is not callable", .{g.name}, value.func.getReference()),
        .import_ns => |ns| try ctx.Error("The namespace '{s}' is not callable (did you mean to access a member?)", .{ns}, value.func.getReference()),
        .function => {},
    }
    const signature = function_type.decided.base.function;
    var func_version = std.hash_map.StringHashMap(Type).init(allocator);
    if (value.args.items.len != signature.argtypes.items.len)
        try ctx.Error("The number of arguments ({d}) does not match the function's ({d})", .{ value.args.items.len, signature.argtypes.items.len }, value.func.getReference());
    for (value.args.items, signature.argtypes.items) |arg, t| {
        const argtype = try getTypeOfValue(arg, ctx, allocator);
        const is_type_param = t.base == .name and analyser.typeparamContains(signature.typeparam, t.base.name);
        const is_buffer_of_type_param = t.base == .buffer and
            t.base.buffer.base == .name and
            analyser.typeparamContains(signature.typeparam, t.base.buffer.base.name);
        if (is_type_param) {
            if (func_version.contains(t.base.name) and func_version.get(t.base.name).? == .decided) {
                if (!argtype.matchType(func_version.get(t.base.name).?))
                    try ctx.Error("Type parameter '{s}' was already bound to '{s}' but got '{s}'", .{
                        t.base.name,
                        func_version.get(t.base.name).?.toString(allocator),
                        argtype.toString(allocator),
                    }, arg.getReference());
            } else {
                try func_version.put(t.base.name, argtype);
            }
            try inferTypeValue(arg, ctx, allocator, func_version.get(t.base.name).?);
        } else if (is_buffer_of_type_param) {
            const tp_name = t.base.buffer.base.name;
            if (argtype == .decided and argtype.decided.base == .buffer) {
                const elem_type = Type{ .decided = argtype.decided.base.buffer };
                if (!func_version.contains(tp_name))
                    try func_version.put(tp_name, elem_type);
            }
            if (func_version.contains(tp_name)) {
                const resolved_elem = func_version.get(tp_name).?.decided;
                const resolved_buf = try allocator.create(ast.Type);
                resolved_buf.* = ast.Type{ .base = .{ .buffer = resolved_elem }, .err = false, .references = 0 };
                try inferTypeValue(arg, ctx, allocator, Type{ .decided = resolved_buf });
            } else {
                try inferTypeValue(arg, ctx, allocator, Type{ .decided = t });
            }
        } else if (t.base == .generic) {
            const generic = t.base.generic;
            var any_tp = false;
            for (generic.params.items) |param| {
                if (param.base == .name and analyser.typeparamContains(signature.typeparam, param.base.name)) {
                    any_tp = true;
                    break;
                }
            }
            if (any_tp and argtype == .decided and argtype.decided.base == .name) {
                const arg_spec_name = argtype.decided.base.name;
                if (ctx.typeDefExist(arg_spec_name) and ctx.typeDefExist(generic.name)) {
                    const arg_spec = ctx.getTypeDef(arg_spec_name);
                    const base_orgn = ctx.getTypeDef(generic.name);
                    if (arg_spec.type_bindings) |bindings| {
                        for (generic.params.items, 0..) |param, i| {
                            if (param.base != .name) continue;
                            const func_tp_name = param.base.name;
                            if (!analyser.typeparamContains(signature.typeparam, func_tp_name)) continue;
                            if (i >= base_orgn.typeparam.items.len) continue;
                            const struct_tp_name = base_orgn.typeparam.items[i].name;
                            if (bindings.get(struct_tp_name)) |concrete| {
                                if (!func_version.contains(func_tp_name))
                                    try func_version.put(func_tp_name, Type{ .decided = concrete });
                            }
                        }
                    }
                }
            }
            // Infer with the resolved type
            const resolved = try analyser.resolveType(t, &func_version, ctx, allocator);
            try inferTypeValue(arg, ctx, allocator, Type{ .decided = resolved });
        } else {
            // Resolve already-bound type params in the expected type
            const resolved: *ast.Type = if (t.base == .name and func_version.contains(t.base.name))
                func_version.get(t.base.name).?.decided
            else
                t;
            try inferTypeValue(arg, ctx, allocator, Type{ .decided = resolved });
        }
    }

    // now checking if the return type has an alias in the function's version
    const ret_type = signature.retype.base;
    if (ret_type == .name and analyser.typeparamContains(signature.typeparam, ret_type.name)) {
        if (!func_version.contains(ret_type.name))
            try ctx.Error("Unable to infer the type to type parameter '{s}'", .{ret_type.name}, value.func.getReference());

        if (!func_version.get(ret_type.name).?.match(expType.decided))
            try ctx.Error("Expected type {s}, but the function returns type {s}", .{ expType.toString(allocator), signature.retype.toString(allocator) }, value.func.getReference());
    } else if (ret_type == .generic) {
        var resolved_params = ArrayList(*ast.Type).init(allocator);
        for (ret_type.generic.params.items) |param| {
            const rp: *ast.Type = if (param.base == .name and func_version.contains(param.base.name))
                func_version.get(param.base.name).?.decided
            else
                param;
            try resolved_params.append(rp);
        }
        const resolved_generic = ast.TypeGeneric{ .name = ret_type.generic.name, .params = resolved_params };
        const spec_name = try analyser.ensureGenericSpecialization(resolved_generic, ctx, allocator);
        const resolved_type = try allocator.create(ast.Type);
        resolved_type.* = ast.Type{ .base = .{ .name = spec_name }, .err = signature.retype.err, .references = 0 };
        if (!expType.match(resolved_type))
            try ctx.Error("Expected type {s}, but the function returns type {s}", .{ expType.toString(allocator), spec_name }, value.func.getReference());
    } else if (!signature.retype.match(expType.decided))
        try ctx.Error("Expected type {s}, but the function returns type {s}", .{ expType.toString(allocator), signature.retype.toString(allocator) }, value.func.getReference());

    try ctx.addFunctionToCompile(analyser.functionVersion{
        .name = signature.fname,
        .signature = signature,
        .version = func_version,
    });
}

pub fn inferTypeBinOperation(lhsValue: *ast.Value, rhsValue: *ast.Value, op: ast.binOperator, ctx: *Context, allocator: Allocator, expType: Type) (std.mem.Allocator.Error || errors.bbcErrors)!void {
    // This function infers the types for a binary operator/function call, given the expected type.
    // It is inspired by analyseBinOp, but adapted for inferring types for lhsValue and rhsValue.
    const op_trait = Traits.traitFromOperator(op);
    const lhsType = try getTypeOfValue(lhsValue, ctx, allocator);
    const rhsType = try getTypeOfValue(rhsValue, ctx, allocator);
    _ = expType;
    // Check if lhsType implements the required trait for the operator.
    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, lhsType, op_trait))
        try ctx.Error("Can't use operator '{s}' on type '{s}', because it does not implement the right trait", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());

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
                        try ctx.Error("The function {s} does not have enough arguments", .{func.name}, rhsValue.getReference());
                    const arg_type = func.signature.argtypes.items[0];
                    if (rhsType.match(arg_type))
                        //try ctx.Error("The types of the argument and the value does not match, expected '{s}' got '{s}'", .{ arg_type.toString(allocator), rhsType.toString(allocator) }, "");
                        found = true;
                },
                .undecided => |rhs_traits| {
                    var found_matching_funcs = ArrayList(analyser.funcPair).init(allocator);
                    defer found_matching_funcs.deinit();
                    for (funcs.items) |func| {
                        if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                            continue;
                        if (func.signature.argtypes.items.len != 1)
                            try ctx.Error("The function {s} does not have enough arguments", .{func.name}, rhsValue.getReference());
                        const arg_type = func.signature.argtypes.items[0];
                        if (Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, Type{ .decided = arg_type }, rhs_traits))
                            try found_matching_funcs.append(func);
                    }
                    if (found_matching_funcs.items.len == 0)
                        try ctx.Error("Found no suitable implementation of '{s}' for type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
                    if (found_matching_funcs.items.len > 1)
                        try ctx.Error("Found more than 1 suitable implementation of '{s}' for type '{s}'. Can't decide...", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
                    const matched_func = found_matching_funcs.items[0];
                    if (matched_func.signature.argtypes.items.len != 1)
                        try ctx.Error("The function {s} does not have enough arguments", .{matched_func.name}, lhsValue.getReference());
                    const arg_type = matched_func.signature.argtypes.items[0];
                    try inferTypeValue(rhsValue, ctx, allocator, Type{ .decided = arg_type });
                },
            }
            if (!found)
                try ctx.Error("No suitable operator implementation found for '{s}' on type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
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
                try ctx.Error("No type implements all required traits for operator '{s}'", .{ast.reprBinOp(op)}, lhsValue.getReference());
            } else if (matching_types.items.len > 1) {
                try ctx.Error("Ambiguous operator '{s}': multiple types implement all required traits", .{ast.reprBinOp(op)}, lhsValue.getReference());
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
                    try ctx.Error("No suitable operator implementation found for '{s}' on type '{s}'", .{ ast.reprBinOp(op), lhsType.toString(allocator) }, lhsValue.getReference());
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

pub fn inferTypeValue(value: *ast.Value, ctx: *Context, allocator: Allocator, expType: Type) (std.mem.Allocator.Error || errors.bbcErrors)!void {
    // expType is for expected type (what type the value should be)
    const voidType = try CreateTypeVoid(allocator, expType.decided.err);
    defer voidType.deinit(allocator);
    const intType = try CreateTypeInt(allocator, false);
    defer intType.deinit(allocator);
    const floatType = try CreateTypeFloat(allocator, false);
    defer floatType.deinit(allocator);
    const charType = try CreateTypeChar(allocator, false);
    defer charType.deinit(allocator);
    const stringType = try CreateTypeString(allocator, false);
    defer stringType.deinit(allocator);
    switch (value.*) {
        .varDec => |vardec| {
            const vartype = ctx.getVariable(vardec.name);
            switch (vartype) {
                .undecided => try ctx.Error("Unable to decide the type of '{s}'", .{vardec.name}, vardec.reference),
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
                    try ctx.Error("The expected type {s} does not match the type of '{s}': {s}", .{ real_exp_type.toString(allocator), ident.name, ctx.getVariable(ident.name).toString(allocator) }, ident.reference);
                return;
            }
            if (ctx.isNamespace(ident.name)) return; // namespace identifier: valid without variable slot
            if (!ctx.variableExist(ident.name))
                try ctx.Error("The variable {s} is not declared (infer)", .{ident.name}, ident.reference);
            if (!ctx.getVariable(ident.name).matchType(real_exp_type))
                try ctx.Error("The expected type {s} does not match the type of '{s}': {s}", .{ real_exp_type.toString(allocator), ident.name, ctx.getVariable(ident.name).toString(allocator) }, ident.reference);
            if (!ctx.variableExist(ident.name) or ctx.getVariable(ident.name) != .decided)
                try ctx.setVariable(ident.name, real_exp_type);
        },
        .assignement => |assign| {
            const rhsType = try getTypeOfValue(assign.rhs, ctx, allocator);
            const ret_type = try CreateTypeVoid(allocator, switch (rhsType) {
                .decided => |dec| dec.err,
                .undecided => false,
            });
            if (!ret_type.matchType(expType))
                try ctx.Error("The expected type '{s}' does not match the type got for an assignation: !Void", .{expType.toString(allocator)}, assign.reference);
            const lhsType =
                switch (assign.lhs.*) {
                    .identifier => |ident| ctx.getVariable(ident.name),
                    .varDec => |vardec| ctx.getVariable(vardec.name),
                    .unaryOperatorRight => |uop_right| blk: {
                        if (uop_right.operator == .pointAttr) {
                            const left_value = try getTypeOfValue(uop_right.expr, ctx, allocator);
                            if (left_value == .undecided)
                                return;
                            if (!ctx.typeDefExist(left_value.decided.base.name)) {
                                try ctx.Error("Unknown struct type name '{s}'", .{left_value.toString(allocator)}, uop_right.reference);
                            }
                            const hab_name = uop_right.operator.pointAttr;
                            const typedef = ctx.getTypeDef(left_value.decided.base.name);
                            if (!typedef.habitantExist(uop_right.operator.pointAttr))
                                try ctx.Error("No habitant with name '{s}' in the structure '{s}'", .{ hab_name, typedef.name }, uop_right.reference);
                            break :blk Type{ .decided = typedef.getHabitant(uop_right.operator.pointAttr) };
                        } else {
                            unreachable;
                        }
                    },
                    .bufferIndex => |bi| blk: {
                        const buf_type = try getTypeOfValue(bi.buffer, ctx, allocator);
                        if (buf_type == .undecided) return;
                        switch (buf_type.decided.base) {
                            .buffer => {
                                const raw_elem = buf_type.decided.base.buffer;
                                const resolved_elem: *ast.Type = if (raw_elem.base == .name and ctx.type_resolved.contains(raw_elem.base.name))
                                    ctx.type_resolved.get(raw_elem.base.name).?.decided
                                else
                                    raw_elem;
                                break :blk Type{ .decided = resolved_elem };
                            },
                            .name => |full_name| {
                                if (ctx.type_implem.get(full_name)) |impls| {
                                    for (impls.items) |func| {
                                        if (std.mem.eql(u8, func.name, "Subscription")) {
                                            const retype = func.signature.retype;
                                            if (retype.err) {
                                                const stripped = try allocator.create(ast.Type);
                                                stripped.* = retype.*;
                                                stripped.err = false;
                                                break :blk Type{ .decided = stripped };
                                            }
                                            break :blk Type{ .decided = retype };
                                        }
                                    }
                                }
                                const has_user_sub = if (ctx.user_trait_impl.get(full_name)) |impls| blk2: {
                                    for (impls.items) |impl_name|
                                        if (std.mem.eql(u8, impl_name, "Subscription")) break :blk2 true;
                                    break :blk2 false;
                                } else false;
                                if (has_user_sub and ctx.typeDefExist(full_name)) {
                                    const struct_def = ctx.getTypeDef(full_name);
                                    if (struct_def.habitants.get("index")) |index_sig| {
                                        if (index_sig.base == .function)
                                            break :blk Type{ .decided = index_sig.base.function.retype };
                                    }
                                }
                                return;
                            },
                            .generic => |g| {
                                // Check Subscription using the base name (always in clone)
                                const has_user_sub = if (ctx.user_trait_impl.get(g.name)) |impls| blk2: {
                                    for (impls.items) |impl_name|
                                        if (std.mem.eql(u8, impl_name, "Subscription")) break :blk2 true;
                                    break :blk2 false;
                                } else false;
                                if (has_user_sub) {
                                    // Use the specialization to get the resolved return type
                                    const spec_name = try analyser.ensureGenericSpecialization(g, ctx, allocator);
                                    if (ctx.typeDefExist(spec_name)) {
                                        const struct_def = ctx.getTypeDef(spec_name);
                                        if (struct_def.habitants.get("index")) |index_sig| {
                                            if (index_sig.base == .function)
                                                break :blk Type{ .decided = index_sig.base.function.retype };
                                        }
                                    }
                                }
                                return;
                            },
                            else => return,
                        }
                    },
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
                            if (!real_right_type.matchType(lhsType)) {
                                try ctx.Error("The right side's type {s} of the assignation does not match the left side's {s}", .{ real_right_type.toString(allocator), lhsType.toString(allocator) }, assign.reference);
                            }
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
                    .undecided => try ctx.Error("Cannot evaluate the type of both side of the assignation", .{}, assign.reference),
                },
            }
        },
        .intLit => {
            if (!intType.matchType(expType))
                try ctx.Error("Expected type {s} but it evaluates to Int", .{expType.toString(allocator)}, value.getReference());
        },
        .floatLit => {
            if (!floatType.matchType(expType))
                try ctx.Error("Expected type {s} but it evaluates to Float", .{expType.toString(allocator)}, value.getReference());
        },
        .charLit => {
            if (!charType.matchType(expType))
                try ctx.Error("Expected type {s} but it evaluates to Char", .{expType.toString(allocator)}, value.getReference());
        },
        .stringLit => {
            if (!stringType.matchType(expType))
                try ctx.Error("Expected type {s} but it evaluates to String", .{expType.toString(allocator)}, value.getReference());
        },
        .nullLit => {
            if (!voidType.matchType(expType))
                try ctx.Error("Expected type {s} but it evaluates to Void", .{expType.toString(allocator)}, value.getReference());
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
                try ctx.Error("The if statements is expected to be of type '{s}' and not '!Void', so it requires an else clause", .{expType.toString(allocator)}, ifstmt.reference);
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
            if (!bool_type.matchType(expType))
                try ctx.Error("Expected type '{s}' but the bool literal evaluates to 'Bool'", .{expType.toString(allocator)}, value.getReference());
        },
        .structInit => |stc_init| {
            const name = stc_init.name;
            if (!ctx.typeDefExist(name))
                try ctx.Error("Type name '{s}' don't exist", .{name}, stc_init.reference);
            const orgn = ctx.getTypeDef(name);
            if (stc_init.habitants.count() != orgn.habitants.count() - 2 - orgn.methods.count())
                try ctx.Error(
                    "Not the right number of habitants in the struct initialisation, expected {d} got {d}",
                    .{ stc_init.habitants.count(), orgn.habitants.count() },
                    stc_init.reference,
                );

            var stc_hab_it = stc_init.habitants.iterator();
            while (stc_hab_it.next()) |hab| {
                const hab_name = hab.key_ptr.*;
                if (!orgn.habitantExist(hab_name))
                    try ctx.Error("Habitant '{s}' is undefined", .{hab_name}, stc_init.reference);
                try inferTypeValue(hab.value_ptr.*, ctx, allocator, Type{ .decided = orgn.getHabitant(hab_name) });
            }
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const left_value = try analyser.analyseValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    try ctx.Error("Can't decide the left part of this unary operator, and can't infer a type", .{}, uop_right.reference);
                if (!ctx.typeDefExist(left_value.decided.base.name))
                    try ctx.Error("Can't decide the left part of this unary operator, and can't infer a type", .{}, uop_right.reference);
            } else {
                unreachable;
            }
        },
        .freeKeyword => |_| {},
        .Print => {},
        .function => {}, // do nothing because everything is done in the first pass of the anlysis
        .bufferAlloc => |ba| {
            const int_type = try CreateTypeInt(allocator, false);
            try inferTypeValue(ba.size, ctx, allocator, int_type);
        },
        .bufferLit => |bl| {
            if (expType.decided.base != .buffer)
                try ctx.Error("Expected buffer type, got '{s}'", .{expType.toString(allocator)}, value.getReference());
            const elem_type = Type{ .decided = expType.decided.base.buffer };
            for (bl.elements.items) |elem| {
                try inferTypeValue(elem, ctx, allocator, elem_type);
            }
        },
        .bufferIndex => |bi| {
            const int_type = try CreateTypeInt(allocator, false);
            try inferTypeValue(bi.index, ctx, allocator, int_type);
            const buf_val_type = try getTypeOfValue(bi.buffer, ctx, allocator);
            if (buf_val_type == .decided) {
                const base_name: ?[]const u8 = switch (buf_val_type.decided.base) {
                    .name => |n| n,
                    .generic => |g| g.name,
                    .buffer => return, // raw buffer — infer as [expType]
                    else => null,
                };
                if (base_name) |name| {
                    if (ctx.type_implem.get(name)) |impls| {
                        for (impls.items) |func| {
                            if (std.mem.eql(u8, func.name, "Subscription")) return;
                        }
                    }
                    const has_user_sub = if (ctx.user_trait_impl.get(name)) |impls| blk: {
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, "Subscription")) break :blk true;
                        break :blk false;
                    } else false;
                    if (has_user_sub) return;
                }
            }
            const buf_elem_type = try allocator.create(ast.Type);
            buf_elem_type.* = expType.decided.*;
            buf_elem_type.err = false;
            const buf_type = try CreateTypeBuffer(allocator, buf_elem_type, false);
            try inferTypeValue(bi.buffer, ctx, allocator, buf_type);
        },
        .notOp => |notop| {
            const err_allowed = expType == .decided and expType.decided.err;
            const bool_type = try CreateTypeBool(allocator, err_allowed);
            try inferTypeValue(notop.expr, ctx, allocator, bool_type);
        },
        .For => |for_loop| {
            const err_allowed = expType == .decided and expType.decided.err;
            const void_type = try CreateTypeVoid(allocator, err_allowed);
            try inferTypeValue(for_loop.exec, ctx, allocator, void_type);
        },
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

    try inferTypeValue(items[items.len - 1], scope.ctx, allocator, rettype);

    var i: usize = items.len - 1;
    while (i > 0) {
        i -= 1;
        try inferTypeValue(items[i], scope.ctx, allocator, voidType);
    }

    // Normally, at the end of both evaluation (this is the second)
    // the type of every variable should be known
    var it = scope.ctx.variables.iterator();
    while (it.next()) |kv| {
        switch (kv.value_ptr.*) {
            .undecided => try ctx.Error("Unable to decide the implicit type of '{s}'' ", .{kv.key_ptr.*}, scope.reference),
            .decided => {},
        }
    }
}
