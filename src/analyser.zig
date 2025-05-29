const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");

const exit = std.process.exit;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const VarHashmap = std.hash_map.StringHashMap(Types.Type);

const TraitHashmap = std.hash_map.StringHashMap(ArrayList(Traits.Trait));
const TypeTraitHashmap = std.hash_map.StringHashMap(ArrayList(Traits.Trait));

pub const funcPair = struct {
    name: []const u8,
    signature: ast.TypeFunc,
};

const ImplemHashmap = std.hash_map.StringHashMap(ArrayList(funcPair));

pub const Context = struct {
    variables: VarHashmap,
    trait_map: TraitHashmap,
    typealiases: TypeTraitHashmap, // For declaring <T: Add, Eq, ...> => (a: T, b: Int)
    type_implem: ImplemHashmap,
    parent: ?*Context,

    pub fn init(allocator: Allocator) !*Context {
        const self = try allocator.create(Context);
        const vars = VarHashmap.init(allocator);
        const types = TraitHashmap.init(allocator);
        const typealiases = TypeTraitHashmap.init(allocator);
        const type_implem = ImplemHashmap.init(allocator);
        self.* = .{ .variables = vars, .parent = null, .trait_map = types, .typealiases = typealiases, .type_implem = type_implem };
        return self;
    }

    pub fn deinit(self: *Context) void {
        self.variables.deinit();
    }

    pub fn createChild(self: *Context, allocator: Allocator) !*Context {
        const new = try allocator.create(Context);
        const vars = VarHashmap.init(allocator);
        const types = try self.trait_map.clone();
        const type_implem = try self.type_implem.clone();
        const typealiases = try self.typealiases.clone();
        new.* = .{ .variables = vars, .parent = self, .trait_map = types, .typealiases = typealiases, .type_implem = type_implem };
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

    pub fn extendTraits(self: *Context, name: []const u8, traits: ArrayList(Traits.Trait)) !void {
        try @constCast(&self.getVariable(name).undecided).appendSlice(traits.items);
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
                        .undecided => |traits| {
                            // If the types matches all the traits inherited in the undecided type, then we can assign
                            // otherwise, error... =)
                            if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, rightType, traits))
                                errors.bbcErrorExit("Can't infer type '{s}' to '{s}', because it needs to have the following traits:\n {}", .{ rightType.toString(allocator), ident, traits }, "");
                            try ctx.setVariable(ident, rightType);
                        },
                    }
                },
                .undecided => |traits| {
                    // We can extand the list of traits for this type
                    const var_type = ctx.getVariable(ident);
                    switch (var_type) {
                        .decided => |t| {
                            if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, var_type, traits))
                                errors.bbcErrorExit("The type '{s}' does not match all the traits required to be assigned to", .{t.toString(allocator)}, "");
                        },
                        .undecided => |new_traits| {
                            try ctx.extendTraits(ident, new_traits);
                        },
                    }
                },
            }
        },
        else => errors.bbcErrorExit("Cannot assign to {}", .{value.*}, ""),
    }
}

pub fn analyseFuncall(func: *ast.Funcall, ctx: *Context, allocator: Allocator) !Types.Type {
    const f_signature = try analyseValue(func.func, ctx, allocator);
    // returns the return type of the function
    switch (f_signature) {
        .undecided => return f_signature,
        .decided => |t| {
            switch (t.base) {
                .name => |name| errors.bbcErrorExit("Can't call a non-function value of type {s}", .{name}, ""),
                .function => |functype| {
                    if (func.args.items.len != functype.argtypes.items.len)
                        errors.bbcErrorExit("The function expects {d} arguments, but got {d}", .{ func.args.items.len, functype.argtypes.items.len }, "");
                    for (func.args.items, functype.argtypes.items) |a, b| {
                        const argtype = try analyseValue(a, ctx, allocator);
                        switch (argtype) {
                            .undecided => return argtype,
                            .decided => if (!argtype.decided.match(b))
                                errors.bbcErrorExit("Arguments type {s} doesn't match the function's ({s})", .{ argtype.toString(allocator), b.toString(allocator) }, ""),
                        }
                    }
                    return Types.Type{ .decided = t.base.function.retype };
                },
            }
        },
    }
    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
}

pub fn analyseBinOp(op: ast.binOperator, ctx: *Context, rhsType: Types.Type, lhsType: Types.Type, allocator: Allocator) !Types.Type {
    // It's a bit like calling a the function "lhsName.opName(rhsType) => ?"
    const op_trait = Traits.traitFromOperator(op);
    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, lhsType, op_trait)) {
        for (ctx.trait_map.get("Int").?.items) |trait| {
            std.debug.print("{s}", .{@tagName(trait)});
        }
        errors.bbcErrorExit("Can't use operator '{s}' on type '{s}', because it does implements the right trait", .{ ast.reprBinOp(op), rhsType.toString(allocator) }, "");
    }
    switch (lhsType) {
        .decided => |t| {
            for (ctx.type_implem.get(t.base.name).?.items) |func| {
                if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                    continue;
                // Normaly, there should be the right amount of argument (because of the way functions are registered)
                if (func.signature.argtypes.items.len != 1) errors.bbcErrorExit("The function {s} does not have enough arguments", .{func.name}, "");
                const arg_type = func.signature.argtypes.items[0];
                switch (rhsType) {
                    .decided => {
                        if (rhsType.match(arg_type))
                            //errors.bbcErrorExit("The argument of type {s} does not match the value of type {s}", .{ arg_type.toString(allocator), lhsType.toString(allocator) }, "");
                            return Types.Type{ .decided = func.signature.retype };
                    },
                    .undecided => |traits| {
                        if (Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, Types.Type{ .decided = arg_type }, traits))
                            //errors.bbcErrorExit("The type {s} does not match all the following traits:\n{}", .{ arg_type.toString(allocator), traits }, "");
                            return Types.Type{ .decided = func.signature.retype };
                    },
                }
            }
            errors.bbcErrorExit("No matching operator implementation found for the given types {s} and {s}", .{ t.toString(allocator), rhsType.toString(allocator) }, "");
        },
        .undecided => |traits| {
            // We can add the trait to the growing list of traits
            try (@constCast(&traits)).append(op_trait);
        },
    }
    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
}

pub fn analyseValue(value: *ast.Value, ctx: *Context, allocator: Allocator) std.mem.Allocator.Error!Types.Type {
    switch (value.*) {
        .varDec => |vardec| {
            if (ctx.variableExist(vardec.name))
                errors.bbcErrorExit("The variable {s} has already been declared before", .{vardec.name}, "");
            const ret_type = Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
            try ctx.createVariable(vardec.name, ret_type);
            return ret_type;
        },
        .binaryOperator => |binop| {
            const rhsType = try analyseValue(binop.rhs, ctx, allocator);
            const lhsType = try analyseValue(binop.lhs, ctx, allocator);

            //const ret_type = Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
            const ret_type = try analyseBinOp(binop.operator, ctx, rhsType, lhsType, allocator);
            return ret_type;
        },
        .assignement => |assing| {
            const rhsType = try analyseValue(assing.rhs, ctx, allocator);
            // TODO: rhsType should be without error, because the error is diverted to the assignement operator
            try analyseAssignationLhs(assing.lhs, ctx, allocator, rhsType);

            return switch (rhsType) {
                .undecided => Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) },
                .decided => Types.CreateTypeVoid(allocator, rhsType.decided.err),
            };
        },
        .If => |ifstmt| {
            _ = try analyseScope(ifstmt.scope, ctx, allocator);
            _ = try analyseValue(ifstmt.condition, ifstmt.scope.ctx, allocator);
            return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
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
    var argtypes = ArrayList(*ast.Type).init(allocator);
    for (func.arguments.items) |arg|
        try argtypes.append(arg._type);
    functype.* = .{ .base = .{ .function = ast.TypeFunc{ .argtypes = argtypes, .retype = func.return_type } }, .err = false, .references = @intCast(0) };

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
    try Traits.initBasicTraits(ctx, allocator);
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => try analyseFunction(inst.FuncDef, ctx, allocator),
        }
    }
    if (!ctx.variableExist("main"))
        errors.bbcErrorExit("Missing function 'main'", .{}, "");
    switch (ctx.getVariable("main").decided.base) {
        .function => |f| {
            if (!f.retype.match((try Types.CreateTypeInt(allocator, true)).decided))
                errors.bbcErrorExit("The function 'main' should return !Int (or matching)", .{}, "");
            if (f.argtypes.items.len != 0)
                errors.bbcErrorExit("'main' function takes no arguements", .{}, "");
        },
        .name => errors.bbcErrorExit("'main' should be a function returning !Int", .{}, ""),
    }
}
