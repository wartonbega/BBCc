const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");
const Parser = @import("parser.zig");

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

pub const functionVersion = struct {
    name: []const u8,
    signature: ast.TypeFunc,
    version: std.hash_map.StringHashMap(Types.Type),
};

pub const function = struct {
    name: []const u8,
    def: ast.funcDef,
};

pub const Context = struct {
    variables: VarHashmap,
    trait_map: TraitHashmap,
    typealiases: TypeTraitHashmap, // For declaring <T: Add, Eq, ...> => (a: T, b: Int)
    type_implem: ImplemHashmap, // All the implementations for the types: eg Int => Add(Int), Sub(Int), etc...
    typedef: std.StringHashMap(*ast.structDef),
    parent: ?*Context,
    functions_to_compile: ArrayList(functionVersion),
    functions: std.hash_map.StringHashMap(*ast.funcDef),

    pub fn init(allocator: Allocator) !*Context {
        const self = try allocator.create(Context);
        const vars = VarHashmap.init(allocator);
        const types = TraitHashmap.init(allocator);
        const typealiases = TypeTraitHashmap.init(allocator);
        const type_implem = ImplemHashmap.init(allocator);
        const function_to_compile = ArrayList(functionVersion).init(allocator);
        const functions = std.hash_map.StringHashMap(*ast.funcDef).init(allocator);
        const typedef = std.StringHashMap(*ast.structDef).init(allocator);
        self.* = .{
            .variables = vars,
            .parent = null,
            .trait_map = types,
            .typealiases = typealiases,
            .type_implem = type_implem,
            .functions_to_compile = function_to_compile,
            .functions = functions,
            .typedef = typedef,
        };
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
        const typedef = try self.typedef.clone();
        new.* = .{
            .variables = vars,
            .parent = self,
            .trait_map = types,
            .typealiases = typealiases,
            .type_implem = type_implem,
            .functions_to_compile = ArrayList(functionVersion).init(allocator),
            .functions = std.hash_map.StringHashMap(*ast.funcDef).init(allocator),
            .typedef = typedef,
        };
        return new;
    }

    pub fn typeExist(self: *Context, _type: *const ast.Type) bool {
        // returns wether a type exists
        if (_type.base == .name) {
            const name = _type.base.name;
            // Layed out like this for readability purpose
            if (std.mem.eql(u8, name, "Int"))
                return true;
            if (std.mem.eql(u8, name, "Bool"))
                return true;
            if (std.mem.eql(u8, name, "String"))
                return true;
            if (std.mem.eql(u8, name, "Char"))
                return true;
            if (std.mem.eql(u8, name, "Void"))
                return true;
            // It can be both defined as a struct or countained in the type aliases section
            return self.typedef.contains(name) or self.typealiases.contains(name);
        } else {
            // TODO: implement recursive checking for function types
            return true; // it is a function type, so it exists for now
        }
    }

    pub fn typeAliasExists(self: *Context, _type: Types.Type) bool {
        // returns wether an alias is defined for _type
        if (_type == .undecided)
            return false;
        if (_type.decided.base != .name)
            return false;
        return self.typealiases.contains(_type.decided.base.name);
    }

    pub fn typeContainHabitants(self: *Context, name: []const u8) bool {
        // returns wether a type is a struct and therefore contains habitants
        // or maybe it is unknown therefore it can't
        // TODO: implement typealiases with
        // habitant accession
        return self.typedef.contains(name);
    }

    pub fn typeDefExist(self: *Context, name: []const u8) bool {
        // Same as typeContainsHabitants, but for another usage
        return self.typedef.contains(name) or std.mem.eql(u8, "String", name);
    }

    pub fn addTypeDef(self: *Context, name: []const u8, stct: *ast.structDef) !void {
        try self.typedef.put(name, stct);
    }

    pub fn getTypeDef(self: *Context, name: []const u8) *ast.structDef {
        return self.typedef.get(name).?;
    }

    pub fn getStructHabitantIndex(self: *Context, name: []const u8, habitant: []const u8) i64 {
        const fields = self.typedef.get(name).?.fields;
        for (fields.items, 0..) |hab, i| {
            if (std.mem.eql(u8, hab, habitant))
                return @intCast(i);
        }
        return @intCast(0);
    }

    pub fn functionExist(self: *Context, name: []const u8) bool {
        if (self.parent) |parent| // The root of the context tree
            return parent.functionExist(name);
        return self.functions.contains(name);
    }

    pub fn getFunction(self: *Context, name: []const u8) *ast.funcDef {
        if (self.parent) |parent| // The root of the context tree
            return parent.getFunction(name);
        if (!self.functions.contains(name))
            std.debug.print("## /!\\ {s}\n", .{name});

        return self.functions.get(name).?;
    }

    pub fn setFunction(self: *Context, name: []const u8, func: *ast.funcDef) !void {
        if (self.parent) |parent| { // The root of the context tree
            try parent.setFunction(name, func);
        } else try self.functions.put(name, func);
    }

    pub fn addFunctionToCompile(self: *Context, func: functionVersion) !void {
        if (self.parent) |parent| { // The root of the context tree
            try parent.addFunctionToCompile(func);
        } else try self.functions_to_compile.append(func);
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
            if (rightType == .undecided) {
                try ctx.createVariable(vardec.name, rightType);
                return;
            }
            if (ctx.variableExist(vardec.name))
                errors.bbcErrorExit("The variable {s} has already been declared before", .{vardec.name}, vardec.reference);
            const lhs_type = try Types.duplicateWithErrorUnion(allocator, rightType.decided, false);
            try ctx.createVariable(vardec.name, lhs_type);
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const left_value = try analyseValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    return;
                if (!ctx.typeDefExist(left_value.decided.base.name)) {
                    errors.bbcErrorExit("Unknown struct type name '{s}'", .{left_value.toString(allocator)}, uop_right.reference);
                }
                const hab_name = uop_right.operator.pointAttr;
                const typedef = ctx.getTypeDef(left_value.decided.base.name);
                if (!typedef.habitantExist(uop_right.operator.pointAttr))
                    errors.bbcErrorExit("No habitant with name '{s}' in the structure '{s}'", .{ hab_name, typedef.name }, uop_right.reference);
            } else {
                unreachable;
            }
        },
        .identifier => |ident| {
            if (!ctx.variableExist(ident.name))
                errors.bbcErrorExit("The variable {s} has not been declared", .{ident.name}, ident.reference);
            switch (rightType) {
                .decided => {
                    const t = ctx.getVariable(ident.name);
                    const real_right_type = try Types.duplicateWithErrorUnion(allocator, rightType.decided, false);
                    switch (t) {
                        .decided => if (!real_right_type.matchType(t)) errors.bbcErrorExit(
                            "The right side's type ({s}) of the assignation does not match the left side's ({s})",
                            .{ rightType.toString(allocator), t.toString(allocator) },
                            ident.reference,
                        ),
                        .undecided => |traits| {
                            // If the types matches all the traits inherited in the undecided type, then we can assign
                            // otherwise, error... =)
                            if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, real_right_type, traits))
                                errors.bbcErrorExit(
                                    "Can't infer type '{s}' to '{s}', because it needs to have the following traits:\n {}",
                                    .{ real_right_type.toString(allocator), ident.name, traits },
                                    value.getReference(),
                                );

                            // The lhs type is the same as the rhs', but without the error union
                            try ctx.setVariable(ident.name, real_right_type); // We can just set it without copying, it's alright
                        },
                    }
                },
                .undecided => |traits| {
                    // We can extand the list of traits for this type
                    const var_type = ctx.getVariable(ident.name);
                    switch (var_type) {
                        .decided => |t| {
                            if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, var_type, traits))
                                errors.bbcErrorExit(
                                    "The type '{s}' does not match all the traits required to be assigned to",
                                    .{t.toString(allocator)},
                                    value.getReference(),
                                );
                        },
                        .undecided => |new_traits| {
                            try ctx.extendTraits(ident.name, new_traits);
                        },
                    }
                },
            }
        },
        else => errors.bbcErrorExit("Cannot assign to {}", .{value.*}, value.getReference()),
    }
}

pub fn typeparamContains(typeparam: ArrayList(ast.TypeParam), name: []const u8) bool {
    for (typeparam.items) |tp|
        if (std.mem.eql(u8, name, tp.name))
            return true;
    return false;
}

pub fn analyseFuncall(func: *ast.Funcall, ctx: *Context, allocator: Allocator) !Types.Type {
    const f_signature = try analyseValue(func.func, ctx, allocator);
    // returns the return type of the function
    switch (f_signature) {
        .undecided => return f_signature,
        .decided => |t| {
            switch (t.base) {
                .name => |name| errors.bbcErrorExit("Can't call a non-function value of type {s}", .{name}, func.func.getReference()),
                .function => |functype| {
                    if (func.args.items.len != functype.argtypes.items.len)
                        errors.bbcErrorExit("The function expects {d} arguments, but got {d}", .{ functype.argtypes.items.len, func.args.items.len }, func.func.getReference());
                    // We can build the current function version, which shall have to be compiled later
                    var func_version = std.hash_map.StringHashMap(Types.Type).init(allocator);
                    for (func.args.items, functype.argtypes.items) |a, b| {
                        const argtype = try analyseValue(a, ctx, allocator);
                        switch (argtype) {
                            .undecided => return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) },
                            .decided => {
                                // If there's a type alias, then we get to assign it
                                if (b.*.base == .name and typeparamContains(functype.typeparam, b.base.name)) {
                                    if (func_version.contains(b.*.base.name) and func_version.get(b.*.base.name).? == .decided) {
                                        errors.bbcErrorExit("'{s}' is already set to be type '{s}'", .{
                                            b.*.base.name,
                                            func_version.get(b.*.base.name).?.toString(allocator),
                                        }, a.getReference());
                                    }
                                    try func_version.put(b.*.base.name, argtype);
                                } else if (!argtype.decided.match(b)) {
                                    errors.bbcErrorExit("The argument of type '{s}' don't match the expected type '{s}'", .{ argtype.toString(allocator), b.toString(allocator) }, a.getReference());
                                }
                            },
                        }
                    }
                    try ctx.addFunctionToCompile(functionVersion{
                        .name = f_signature.decided.base.function.fname,
                        .signature = f_signature.decided.base.function,
                        .version = func_version,
                    });
                    const ret_type = t.base.function.retype.base;
                    if (ret_type == .name and typeparamContains(t.base.function.typeparam, ret_type.name)) {
                        if (!func_version.contains(ret_type.name))
                            errors.bbcErrorExit("Unable to infer the type to type parameter '{s}'", .{ret_type.name}, func.func.getReference());
                        return func_version.get(ret_type.name).?;
                    }
                    return Types.Type{ .decided = t.base.function.retype };
                },
            }
        },
    }
    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
}

pub fn analyseBinOp(op: ast.binOperator, ctx: *Context, rhsType: Types.Type, lhsType: Types.Type, reference: Parser.Location, allocator: Allocator) !Types.Type {
    // It's a bit like calling a the function "lhsName.opName(rhsType) => ?"
    const op_trait = Traits.traitFromOperator(op);
    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, lhsType, op_trait)) {
        errors.bbcErrorExit(
            "Can't use operator '{s}' on type '{s}', because it does implements the right trait",
            .{ ast.reprBinOp(op), lhsType.toString(allocator) },
            reference,
        );
    }
    switch (lhsType) {
        .decided => |t| {
            if (!ctx.type_implem.contains(t.base.name)) {
                // If there's a type as a parameter, it shall be the return type (otherwise, error :) )
                return try Traits.defaultReturnType(Traits.traitFromOperator(op), lhsType, allocator);
            }
            for (ctx.type_implem.get(t.base.name).?.items) |func| {
                if (!std.mem.eql(u8, func.name, ast.binOpFuncName(op)))
                    continue;
                // Normaly, there should be the right amount of argument (because of the way functions are registered)
                if (func.signature.argtypes.items.len != 1) errors.bbcErrorExit(
                    "The function {s} does not have enough arguments",
                    .{func.name},
                    reference,
                );
                const arg_type = func.signature.argtypes.items[0];
                switch (rhsType) {
                    .decided => {
                        if (rhsType.match(arg_type)) {
                            return Types.Type{ .decided = func.signature.retype };
                        }
                    },
                    .undecided => |traits| {
                        if (Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, Types.Type{ .decided = arg_type }, traits))
                            return Types.Type{ .decided = func.signature.retype };
                    },
                }
            }
            errors.bbcErrorExit(
                "No matching operator implementation found for the given types {s} and {s}",
                .{ t.toString(allocator), rhsType.toString(allocator) },
                reference,
            );
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
                errors.bbcErrorExit("The variable {s} has already been declared before", .{vardec.name}, vardec.reference);
            const ret_type = try Types.CreateTypeVoid(allocator, false);
            try ctx.createVariable(vardec.name, Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) });
            return ret_type;
        },
        .binaryOperator => |binop| {
            const rhsType = try analyseValue(binop.rhs, ctx, allocator);
            const lhsType = try analyseValue(binop.lhs, ctx, allocator);

            //const ret_type = Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
            const ret_type = try analyseBinOp(binop.operator, ctx, rhsType, lhsType, binop.reference, allocator);
            return ret_type;
        },
        .assignement => |assing| {
            const rhsType = try analyseValue(assing.rhs, ctx, allocator);
            if (rhsType == .decided) {
                try analyseAssignationLhs(
                    assing.lhs,
                    ctx,
                    allocator,
                    try Types.duplicateWithErrorUnion(allocator, rhsType.decided, false),
                );
            } else {
                try analyseAssignationLhs(assing.lhs, ctx, allocator, rhsType);
            }

            return switch (rhsType) {
                .undecided => Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) },
                .decided => Types.CreateTypeVoid(allocator, rhsType.decided.err),
            };
        },
        .If => |ifstmt| {
            // the number of scopes and conditions is the same (trust me hehe) and greater than 0
            // All scopes should evaluate to the same type: the type of the first scope
            const base_scope_type = try analyseValue(ifstmt.scopes.getLast(), ctx, allocator);
            if (base_scope_type == .undecided)
                return base_scope_type;
            var has_error = base_scope_type.decided.err;

            const bool_type = try Types.CreateTypeBool(allocator, false);
            defer bool_type.deinit(allocator);
            for (ifstmt.conditions.items, ifstmt.scopes.items) |cond, scope| {
                // First analysing the scope
                const scope_type = try analyseValue(scope, ctx, allocator);
                if (scope_type == .undecided)
                    return base_scope_type;
                if (scope_type.decided.err) has_error = true;
                const real_scope_type = try Types.duplicateWithErrorUnion(allocator, scope_type.decided, base_scope_type.decided.err);
                if (!real_scope_type.matchType(base_scope_type))
                    errors.bbcErrorExit("The type of this scope don't match the return type of the if statement", .{}, scope.getReference());
                // Then the condition
                const cond_type = try analyseValue(cond, ctx, allocator);
                if (!cond_type.matchType(bool_type))
                    errors.bbcErrorExit("The type of this condition does not match 'Bool'", .{}, cond.getReference());
            }

            const void_type = try Types.CreateTypeVoid(allocator, false);
            defer void_type.deinit(allocator);

            if (ifstmt.elsescope) |else_scope| {
                const else_type = try analyseValue(else_scope, ctx, allocator);
                if (else_type == .undecided)
                    return base_scope_type;
                if (else_type.decided.err) has_error = true;
                const real_else_type = try Types.duplicateWithErrorUnion(allocator, else_type.decided, base_scope_type.decided.err);
                if (!real_else_type.matchType(base_scope_type))
                    errors.bbcErrorExit("The type of the else '{s}' scope don't match the type of this if statement", .{else_type.toString(allocator)}, else_scope.getReference());
            } else if (!void_type.matchType(base_scope_type)) { // Else statements are required if the evaluation type insn't !Void
                errors.bbcErrorExit("The if statements evalutates to type '{s}' and not '!Void', so it requires an else clause", .{base_scope_type.toString(allocator)}, ifstmt.reference);
            }
            // base_scope_type.decided.err = has_error;
            return try Types.duplicateWithErrorUnion(allocator, base_scope_type.decided, has_error);
        },
        .parenthesis => |val| return try analyseValue(val, ctx, allocator),
        .scope => |scope| return try analyseScope(scope, ctx, allocator),
        .identifier => |ident| {
            if (ctx.functionExist(ident.name))
                return Types.Type{ .decided = try createFunctionSignature(ctx.getFunction(ident.name), allocator) };
            if (!ctx.variableExist(ident.name))
                errors.bbcErrorExit("Variable {s} is not declared (analysis)", .{ident.name}, ident.reference);

            return ctx.getVariable(ident.name);
        },
        .intLit => return try Types.CreateTypeInt(allocator, false),
        .charLit => return try Types.CreateTypeChar(allocator, false),
        .stringLit => return try Types.CreateTypeString(allocator, false),
        .boolLit => return Types.CreateTypeBool(allocator, false),
        .nullLit => return Types.CreateTypeVoid(allocator, false),
        .funcall => |func| {
            return try analyseFuncall(func, ctx, allocator);
        },
        .errorCheck => |errcheck| {
            const val_type = try analyseValue(errcheck.value, ctx, allocator);
            const scope_type = try analyseValue(errcheck.scope, ctx, allocator);
            if (val_type == .decided and !val_type.decided.err)
                errors.bbcErrorExit("The value don't have errors to check", .{}, errcheck.value.getReference());
            if (scope_type == .decided and scope_type.decided.err)
                errors.bbcErrorExit("The default scope can't have errors", .{}, errcheck.scope.getReference());
            if (!scope_type.matchType(val_type))
                errors.bbcErrorExit("The scope's type don't match the value's type", .{}, errcheck.reference);
            // We can remove the error
            if (val_type == .decided)
                return Types.duplicateWithErrorUnion(allocator, val_type.decided, false);
            return val_type;
        },
        .While => |whileloop| {
            const cond_type = try analyseValue(whileloop.condition, ctx, allocator);

            const bool_type = try Types.CreateTypeBool(allocator, true);
            defer bool_type.deinit(allocator);
            const void_type = try Types.CreateTypeVoid(allocator, true);
            defer void_type.deinit(allocator);

            if (!cond_type.matchType(bool_type))
                errors.bbcErrorExit("The condition of the while loop should have type 'Bool'", .{}, whileloop.condition.getReference());
            const exec_type = try analyseValue(whileloop.exec, ctx, allocator);
            if (!exec_type.matchType(void_type))
                errors.bbcErrorExit("The exec'ed code should evaluate to type 'Void'", .{}, whileloop.exec.getReference());

            // Can it evaluate to an error union ?
            const has_error = cond_type == .decided and cond_type.decided.err or exec_type == .decided and exec_type.decided.err;

            return Types.CreateTypeVoid(allocator, has_error);
        },
        .structInit => |stc_init| {
            const name = stc_init.name;
            if (!ctx.typeDefExist(name))
                errors.bbcErrorExit("Type name '{s}' don't exist", .{name}, stc_init.reference);
            const orgn = ctx.getTypeDef(name);
            if (stc_init.habitants.count() != orgn.habitants.count() - 2 - orgn.methods.count())
                errors.bbcErrorExit(
                    "Not the right number of habitants in the struct initialisation, expected {d} got {d}",
                    .{ stc_init.habitants.count(), orgn.habitants.count() },
                    stc_init.reference,
                );

            var stc_hab_it = stc_init.habitants.iterator();
            var has_error = false;
            while (stc_hab_it.next()) |hab| {
                const hab_name = hab.key_ptr.*;
                if (!orgn.habitantExist(hab_name))
                    errors.bbcErrorExit("Habitant '{s}' is undefined", .{hab_name}, stc_init.reference);
                const hab_type = try analyseValue(hab.value_ptr.*, ctx, allocator);
                // The type got does not match the expected one
                if (!hab_type.match(orgn.getHabitant(hab_name)))
                    errors.bbcErrorExit("Habitant '{s}' is expected to be of type '{s}' but is actually of type '{s}'", .{
                        hab_name,
                        hab_type.toString(allocator),
                        orgn.getHabitant(hab_name).toString(allocator),
                    }, stc_init.reference);
                if (hab_type == .decided)
                    has_error = has_error or hab_type.decided.err;
            }
            const ret_ast_type = try allocator.create(ast.Type);
            ret_ast_type.base = .{ .name = name };
            ret_ast_type.references = 0;
            ret_ast_type.err = has_error;
            return Types.Type{ .decided = ret_ast_type };
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const left_value = try analyseValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
                if (!ctx.typeDefExist(left_value.decided.base.name)) {
                    errors.bbcErrorExit("Unknown struct type name '{s}'", .{left_value.toString(allocator)}, uop_right.reference);
                }
                const hab_name = uop_right.operator.pointAttr;
                const typedef = ctx.getTypeDef(left_value.decided.base.name);
                if (!typedef.habitantExist(uop_right.operator.pointAttr))
                    errors.bbcErrorExit("No habitant with name '{s}' in the structure '{s}'", .{ hab_name, typedef.name }, uop_right.reference);
                return Types.Type{ .decided = typedef.getHabitant(hab_name) };
            } else {
                unreachable;
            }
        },
        .freeKeyword => |_| {
            return Types.CreateTypeVoid(allocator, false);
        },
        .Print => |print| {
            var has_error = false;
            for (print.args.items) |arg| {
                const arg_t = try analyseValue(arg, ctx, allocator);
                if (arg_t == .decided and arg_t.decided.err)
                    has_error = true;
            }
            return Types.CreateTypeVoid(allocator, has_error);
        },
        .function => |func| {
            //try analyseFunction(func, ctx, allocator);
            try ctx.setFunction(try func.getName(allocator), func);
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
            return Types.Type{ .decided = ret_type };
        },
        else => {
            std.debug.print("{?}", .{value.*});
            unreachable;
        },
    }
}

pub fn analyseScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator) std.mem.Allocator.Error!Types.Type {
    scope.ctx.deinit();
    allocator.destroy(scope.ctx); // Only works if the allocator used here is the same than in the lexer

    scope.ctx = try ctx.createChild(allocator);
    var has_error = false;
    if (scope.code.items.len > 0) {
        for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
            const sec_ret = try analyseValue(value, scope.ctx, allocator);
            if (sec_ret == .decided and sec_ret.decided.err)
                has_error = true;
        }
        const ret = try analyseValue(scope.code.items[scope.code.items.len - 1], scope.ctx, allocator);
        if (ret == .decided)
            return try Types.duplicateWithErrorUnion(allocator, ret.decided, ret.decided.err or has_error);
        return ret;
    }
    return Types.CreateTypeVoid(allocator, false);
}

pub fn createFunctionSignature(func: *ast.funcDef, allocator: Allocator) !*ast.Type {
    const functype = try allocator.create(ast.Type);
    var argtypes = ArrayList(*ast.Type).init(allocator);
    for (func.arguments.items) |arg|
        try argtypes.append(arg._type);
    functype.* = .{
        .base = .{ .function = ast.TypeFunc{
            .argtypes = argtypes,
            .retype = func.return_type,
            .typeparam = func.typeparam,
            .fname = try func.getName(allocator),
        } },
        .err = false,
        .references = @intCast(0),
    };
    return functype;
}

pub fn analyseFunction(func: *ast.funcDef, par_ctx: *Context, allocator: Allocator) !void {
    // Defining the function's signature type
    const functype = try createFunctionSignature(func, allocator);

    // Associating the name with the type
    try par_ctx.setVariable(func.name, Types.Type{ .decided = functype });

    // We can create a child context for the function
    var ctx = try par_ctx.createChild(allocator);

    // We can set the argument type
    for (func.typeparam.items) |type_param| {
        try ctx.typealiases.put(type_param.name, Traits.traitsFromStrings(type_param.traits, allocator));
    }

    // and add the inbuild info-functions
    try ctx.setVariable("__name", try Types.CreateTypeString(allocator, false));

    if (func.parent) |parent| {
        const typedef = ctx.getTypeDef(parent.base.name);
        for (typedef.fields.items) |field_name| {
            try ctx.setVariable(field_name, .{ .decided = typedef.habitants.get(field_name).? });
        }
        try ctx.setVariable("self", Types.Type{ .decided = parent });
    }

    if (!ctx.typeExist(func.return_type))
        errors.bbcErrorExit("The type '{s}' doesn't exist", .{func.return_type.toString(allocator)}, func.reference);

    // associating variables
    for (func.arguments.items) |arg| {
        if (!ctx.typeExist(arg._type))
            errors.bbcErrorExit("The type '{s}' doesn't exist", .{arg._type.toString(allocator)}, arg.reference);
        try ctx.setVariable(arg.name, Types.Type{ .decided = arg._type });
    }

    //    for (func.typeparam.items) |type_param| {
    //        try ctx.typealiases.put(type_param.name, Traits.traitsFromStrings(type_param.traits, allocator));
    //    }

    std.debug.print("\n#[Analysing {s}]\n", .{try func.getName(allocator)});
    _ = try analyseScope(func.code, ctx, allocator);
    try Types.inferTypeScope(func.code, ctx, allocator, Types.Type.init(func.return_type));
    var it = func.code.ctx.variables.iterator();
    while (it.next()) |kv| {
        std.debug.print("{s} is {s}\n", .{ kv.key_ptr.*, kv.value_ptr.toString(allocator) });
    }
}

pub fn verifyFunctionTypeTraits(func: *ast.funcDef, ctx: *Context, allocator: Allocator, f_version: functionVersion) !void {
    // Verifys that all the types in the current function's version are implementing the right traits
    // There's an error if one of the types don't respect the traits in play
    var val = ast.Value{ .scope = func.code };
    const t = try Traits.getTypeTraits(&val, ctx, allocator);
    var it_k = t.iterator();
    while (it_k.next()) |e| {
        const arg_type_name = e.key_ptr.*;
        const arg_type_traits = e.value_ptr.*; // ArrayList(Trait)

        if (!f_version.version.contains(arg_type_name))
            continue;

        const aliased = f_version.version.get(arg_type_name).?;
        if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, aliased, arg_type_traits))
            errors.bbcErrorExit("The type '{s}' should implement [{s}], but doesn't", .{
                aliased.toString(allocator),
                try Traits.traitListToString(arg_type_traits, allocator),
            }, func.reference);
    }
}

fn containFunction(func: functionVersion, list: ArrayList(functionVersion)) bool {
    // Returns if list contains func, by checking the names and the version (considering two
    // different functions won't have the same name)
    if (list.items.len > 0) {
        for (list.items) |compiled_func| {
            if (std.mem.eql(u8, compiled_func.name, func.name)) {
                if (compiled_func.version.count() != func.version.count())
                    return false;
                var it = compiled_func.version.iterator();
                while (it.next()) |vers| {
                    if (!func.version.contains(vers.key_ptr.*))
                        return false;
                    if (!func.version.get(vers.key_ptr.*).?.matchType(compiled_func.version.get(vers.key_ptr.*).?))
                        return false;
                }
                return true;
            }
        }
    }
    return false;
}

pub fn analyseStructDef(stct: *ast.structDef, ctx: *Context, allocator: Allocator) !void {
    var hab_it = stct.habitants.iterator();
    while (hab_it.next()) |hab| {
        if (!ctx.typeExist(hab.value_ptr.*))
            errors.bbcErrorExit("The type '{s}' doesn't exist", .{hab.value_ptr.*.toString(allocator)}, stct.reference);
    }
    try ctx.addTypeDef(stct.name, stct);

    var strct_traits = ArrayList(Traits.Trait).init(allocator);
    try strct_traits.append(Traits.Trait.Eq);
    try ctx.trait_map.put(stct.name, strct_traits);

    // Implementing the trait equals (and notEqual) for the struct and void
    // stct == void -> bool
    var stct_implems = ArrayList(funcPair).init(allocator);
    try Traits.createTraitWithBinOperators(
        &stct_implems,
        (try Types.CreateTypeVoid(allocator, false)).decided,
        (try Types.CreateTypeBool(allocator, false)).decided,
        &[_]ast.binOperator{ .Equal, .NotEqual },
        allocator,
    );

    try ctx.type_implem.put(
        stct.name,
        stct_implems,
    );

    var meth_it = stct.methods.iterator();

    while (meth_it.next()) |meth| {
        const f_signature = stct.habitants.get(meth.key_ptr.*).?;
        const meth_name = try std.mem.concat(allocator, u8, &[_][]const u8{
            stct.name,
            ".",
            meth.key_ptr.*,
        });
        try ctx.addFunctionToCompile(functionVersion{
            .name = meth_name,
            .signature = f_signature.base.function,
            .version = .init(allocator),
        });
        try ctx.setFunction(meth_name, meth.value_ptr.*);
    }
}

pub fn analyse(prog: *ast.Program, ctx: *Context, allocator: Allocator) !void {
    var funcs_to_compile = ArrayList(functionVersion).init(allocator);

    const string_type_def = try allocator.create(ast.structDef);
    string_type_def.* = .{ .habitants = .init(allocator), .name = "String", .fields = .init(allocator), .reference = Parser.getInbuiltLocation(), .methods = .init(allocator) };
    try string_type_def.habitants.put("_count", (try Types.CreateTypeInt(allocator, false)).decided);
    try string_type_def.habitants.put("_size", (try Types.CreateTypeInt(allocator, false)).decided);
    try string_type_def.fields.append("_count");
    try string_type_def.fields.append("_size");
    try ctx.addTypeDef("String", string_type_def);

    try Traits.initBasicTraits(ctx, allocator);
    // First pass : search all the function definition
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |func| {
                if (ctx.functionExist(func.name))
                    errors.bbcErrorExit("Can't shadow the definition of function '{s}' previously defined here: {s}", .{ inst.FuncDef.name, ctx.getFunction(inst.FuncDef.name).reference.toString() }, inst.FuncDef.reference);
                try ctx.setFunction(func.name, func);
            },
            else => {},
        }
    }

    const main_func = ctx.getFunction("main");
    try ctx.functions_to_compile.append(functionVersion{
        .signature = (try createFunctionSignature(main_func, allocator)).base.function,
        .name = "main",
        .version = std.hash_map.StringHashMap(Types.Type).init(allocator),
    });

    // Second pass, find all the struct definitions
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .StructDef => |stct| {
                try analyseStructDef(stct, ctx, allocator);
            },
            else => {},
        }
    }

    // we can analyse the main function
    //try analyseFunction(ctx.getFunction("main"), ctx, allocator);
    while (ctx.functions_to_compile.items.len != 0) {
        const func = ctx.functions_to_compile.pop().?;

        // fetching the function definition
        const func_def = ctx.getFunction(func.name);

        if (!containFunction(func, funcs_to_compile)) {
            try analyseFunction(func_def, ctx, allocator);
            try funcs_to_compile.append(func);
        }

        // We can verify that the current version of the function's types matches all the required traits
        try verifyFunctionTypeTraits(func_def, func_def.code.ctx, allocator, func);
    }
    // Transpilation
    ctx.functions_to_compile = funcs_to_compile;

    if (!ctx.variableExist("main"))
        errors.bbcErrorExit("Missing function 'main'", .{}, Parser.getInbuiltLocation());

    switch (ctx.getVariable("main").decided.base) {
        .function => |f| {
            if (!f.retype.match((try Types.CreateTypeInt(allocator, true)).decided))
                errors.bbcErrorExit("The function 'main' should return !Int (or matching)", .{}, ctx.getFunction("main").reference);
            if (f.argtypes.items.len != 0)
                errors.bbcErrorExit("'main' function takes no arguements", .{}, ctx.getFunction("main").reference);
        },
        .name => errors.bbcErrorExit("'main' should be a function returning !Int", .{}, Parser.getInbuiltLocation()),
    }
}
