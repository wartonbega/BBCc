const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");
const Parser = @import("parser.zig");
const position = @import("position.zig");
const InbuiltFuncs = @import("inbuilt_funcs.zig");

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

// Resolve a bare trait name (e.g. "Iter") to its qualified name as stored in trait_defs
// (e.g. "Iterators.Iter"). Falls back to the bare name if no namespaced form exists.
fn resolveTraitName(trait_defs: *const std.StringHashMap(*ast.traitDef), bare: []const u8) []const u8 {
    if (trait_defs.contains(bare)) return bare;
    var it = trait_defs.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        if (key.len > bare.len + 1 and
            key[key.len - bare.len - 1] == '.' and
            std.mem.eql(u8, key[key.len - bare.len ..], bare))
            return key;
    }
    return bare;
}

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
    variables_references: std.StringHashMap(position.Location),
    variable_stack_loc: std.StringHashMap(i64),

    trait_map: TraitHashmap,
    typealiases: TypeTraitHashmap, // For declaring <T: Add, Eq, ...> => (a: T, b: Int)
    type_implem: ImplemHashmap, // All the implementations for the types: eg Int => Add(Int), Sub(Int), etc...
    type_resolved: std.StringHashMap(Types.Type), // Concrete type bindings for current function version (e.g. T -> Int)
    typedef: std.StringHashMap(*ast.structDef),
    parent: ?*Context,
    functions_to_compile: ArrayList(functionVersion),
    functions: std.hash_map.StringHashMap(*ast.funcDef),

    // User-defined trait system
    trait_defs: std.StringHashMap(*ast.traitDef),
    user_trait_impl: std.StringHashMap(ArrayList([]const u8)), // type_name -> [trait_name, ...]
    user_typealiases: std.StringHashMap(ArrayList([]const u8)), // type_param_name -> [user_trait_name, ...]

    // Inbuilt functions loaded from inbuilt_funcs.config
    inbuilt_funcs: std.StringHashMap(InbuiltFuncs.Func),

    // Registered namespace names (e.g. "math", "mathw.math") derived from prefixed function names.
    namespace_identifiers: std.StringHashMap(void),

    // Error handling
    error_locations: std.ArrayList(position.Location),
    error_messages: std.ArrayList([]u8),

    pub fn init(allocator: Allocator) !*Context {
        const self = try allocator.create(Context);
        const vars = VarHashmap.init(allocator);
        const types = TraitHashmap.init(allocator);
        const typealiases = TypeTraitHashmap.init(allocator);
        const type_implem = ImplemHashmap.init(allocator);
        const function_to_compile = ArrayList(functionVersion).init(allocator);
        const functions = std.hash_map.StringHashMap(*ast.funcDef).init(allocator);
        const typedef = std.StringHashMap(*ast.structDef).init(allocator);
        const inbuilt_list = try InbuiltFuncs.load(allocator);
        var inbuilt_funcs = std.StringHashMap(InbuiltFuncs.Func).init(allocator);
        for (inbuilt_list) |f| try inbuilt_funcs.put(f.name, f);
        self.* = .{
            .variables = vars,
            .variables_references = .init(allocator),
            .variable_stack_loc = .init(allocator),
            .parent = null,
            .trait_map = types,
            .typealiases = typealiases,
            .type_implem = type_implem,
            .type_resolved = .init(allocator),
            .functions_to_compile = function_to_compile,
            .functions = functions,
            .typedef = typedef,
            .trait_defs = std.StringHashMap(*ast.traitDef).init(allocator),
            .user_trait_impl = std.StringHashMap(ArrayList([]const u8)).init(allocator),
            .user_typealiases = std.StringHashMap(ArrayList([]const u8)).init(allocator),
            .inbuilt_funcs = inbuilt_funcs,
            .error_locations = .init(allocator),
            .error_messages = .init(allocator),
            .namespace_identifiers = .init(allocator),
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
            .variables_references = .init(allocator),
            .variable_stack_loc = .init(allocator),
            .parent = self,
            .trait_map = types,
            .typealiases = typealiases,
            .type_implem = type_implem,
            .type_resolved = try self.type_resolved.clone(),
            .functions_to_compile = ArrayList(functionVersion).init(allocator),
            .functions = std.hash_map.StringHashMap(*ast.funcDef).init(allocator),
            .typedef = typedef,
            .trait_defs = try self.trait_defs.clone(),
            .user_trait_impl = try self.user_trait_impl.clone(),
            .user_typealiases = try self.user_typealiases.clone(),
            .inbuilt_funcs = try self.inbuilt_funcs.clone(),
            .error_locations = .init(allocator),
            .error_messages = .init(allocator),
            .namespace_identifiers = .init(allocator),
        };
        return new;
    }

    pub fn typeExist(self: *Context, _type: *const ast.Type) bool {
        switch (_type.base) {
            .buffer => |elem| return self.typeExist(elem),
            .function => return true,
            .import_ns => return true, // namespace values always exist
            .generic => |g| {
                if (!self.typeDefExist(g.name)) return false;
                for (g.params.items) |p| if (!self.typeExist(p)) return false;
                return true;
            },
            .name => |name| {
                if (std.mem.eql(u8, name, "Int")) return true;
                if (std.mem.eql(u8, name, "Bool")) return true;
                if (std.mem.eql(u8, name, "String")) return true;
                if (std.mem.eql(u8, name, "Char")) return true;
                if (std.mem.eql(u8, name, "Void")) return true;
                if (self.trait_map.contains(name)) return true;
                if (self.type_resolved.contains(name)) return true;
                return self.typeDefExist(name) or self.typealiases.contains(name) or self.user_typealiases.contains(name);
            },
        }
    }

    pub fn addNamespace(self: *Context, name: []const u8) !void {
        // Always store at root so all child contexts can see it
        if (self.parent) |p| return p.addNamespace(name);
        try self.namespace_identifiers.put(name, {});
    }

    pub fn isNamespace(self: *const Context, name: []const u8) bool {
        if (self.namespace_identifiers.contains(name)) return true;
        if (self.parent) |p| return p.isNamespace(name);
        return false;
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
        if (self.typedef.contains(name)) return true;
        if (self.parent) |p| return p.typeContainHabitants(name);
        return false;
    }

    pub fn typeDefExist(self: *Context, name: []const u8) bool {
        if (self.typedef.contains(name) or std.mem.eql(u8, "String", name)) return true;
        if (self.parent) |p| return p.typeDefExist(name);
        return false;
    }

    pub fn addTypeDef(self: *Context, name: []const u8, stct: *ast.structDef) !void {
        if (self.parent) |p| {
            try p.addTypeDef(name, stct);
        } else {
            try self.typedef.put(name, stct);
        }
    }

    pub fn getTypeDef(self: *Context, name: []const u8) *ast.structDef {
        if (self.typedef.contains(name)) return self.typedef.get(name).?;
        return self.parent.?.getTypeDef(name);
    }

    pub fn getStructHabitantIndex(self: *Context, name: []const u8, habitant: []const u8) i64 {
        const fields = self.getTypeDef(name).fields;
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
        if (std.mem.eql(u8, func.name, "unknown") or std.mem.eql(u8, func.signature.fname, "unknown"))
            return;
        if (self.parent) |parent| { // The root of the context tree
            try parent.addFunctionToCompile(func);
        } else try self.functions_to_compile.append(func);
    }

    pub fn createVariable(self: *Context, name: []const u8, t: Types.Type, reference: position.Location) !void {
        try self.variables.put(name, t);
        try self.variables_references.put(name, reference);
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

    pub fn putVariableStackIndex(self: *Context, name: []const u8, idx: i64) !void {
        try self.variable_stack_loc.put(name, idx);
    }

    pub fn getVariableStackIndex(self: *Context, name: []const u8) i64 {
        if (self.variables.contains(name))
            return self.variable_stack_loc.get(name).?;
        return self.parent.?.getVariableStackIndex(name);
    }

    pub fn extendTraits(self: *Context, name: []const u8, traits: ArrayList(Traits.Trait)) !void {
        try @constCast(&self.getVariable(name).undecided).appendSlice(traits.items);
    }

    pub fn variableExist(self: *Context, name: []const u8) bool {
        if (self.parent) |parent|
            return self.variables.contains(name) or parent.variableExist(name);
        return self.variables.contains(name);
    }

    pub fn Error(self: *Context, comptime val: []const u8, args: anytype, pos: Parser.Location) !void {
        if (self.parent) |parent|
            try parent.Error(val, args, pos);
        try self.error_locations.append(pos);
        try self.error_messages.append(try std.fmt.allocPrint(std.heap.page_allocator, val, args));
        try errors.bbcErrorExit(val, args, pos);
    }

    pub fn getTypeDefinition(self: *Context, t: *ast.Type) ?Parser.Location {
        if (t.base == .name and self.typeDefExist(t.base.name)) {
            return self.getTypeDef(t.base.name).reference;
        }
        return null;
    }

    pub fn getVariableDefinition(self: *Context, name: []const u8) ?Parser.Location {
        if (self.variables_references.contains(name)) {
            return self.variables_references.get(name).?;
        } else if (self.parent) |parent| {
            return parent.getVariableDefinition(name);
        }

        return null;
    }
};

fn isBufferTypeName(name: []const u8) bool {
    return name.len > 2 and name[0] == '[' and name[name.len - 1] == ']';
}

pub fn analyseAssignationLhs(value: *ast.Value, ctx: *Context, allocator: Allocator, rightType: Types.Type) !void {
    switch (value.*) {
        .varDec => |vardec| {
            if (rightType == .undecided) {
                try ctx.createVariable(vardec.name, rightType, vardec.reference);
                return;
            }
            if (ctx.variableExist(vardec.name))
                try ctx.Error("The variable {s} has already been declared before", .{vardec.name}, vardec.reference);
            const lhs_type = try Types.duplicateWithErrorUnion(allocator, rightType.decided, false);
            try ctx.createVariable(vardec.name, lhs_type, vardec.reference);
        },
        .unaryOperatorRight => |uop_right| {
            if (uop_right.operator == .pointAttr) {
                const left_value = try analyseValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    return;
                if (left_value.decided.base == .buffer)
                    try ctx.Error("Cannot assign to buffer attribute '{s}'", .{uop_right.operator.pointAttr}, uop_right.reference);
                const struct_name: []const u8 = switch (left_value.decided.base) {
                    .name => |n| n,
                    .generic => |g| try ensureGenericSpecialization(g, ctx, allocator),
                    .buffer => unreachable,
                    .function => {
                        try ctx.Error("Cannot assign attribute of a function type", .{}, uop_right.reference);
                        unreachable;
                    },
                    .import_ns => {
                        try ctx.Error("Cannot assign attribute of a namespace", .{}, uop_right.reference);
                        unreachable;
                    },
                };
                if (!ctx.typeDefExist(struct_name)) {
                    try ctx.Error("Unknown struct type name '{s}'", .{struct_name}, uop_right.reference);
                }
                const hab_name = uop_right.operator.pointAttr;
                const typedef = ctx.getTypeDef(struct_name);
                if (!typedef.habitantExist(uop_right.operator.pointAttr))
                    try ctx.Error("No habitant with name '{s}' in the structure '{s}'", .{ hab_name, typedef.name }, uop_right.reference);
            } else {
                unreachable;
            }
        },
        .identifier => |ident| {
            if (!ctx.variableExist(ident.name))
                try ctx.Error("The variable {s} has not been declared", .{ident.name}, ident.reference);
            switch (rightType) {
                .decided => {
                    const t = ctx.getVariable(ident.name);
                    const real_right_type = try Types.duplicateWithErrorUnion(allocator, rightType.decided, false);
                    switch (t) {
                        .decided => if (!real_right_type.matchType(t)) try ctx.Error(
                            "The right side's type ({s}) of the assignation does not match the left side's ({s})",
                            .{ rightType.toString(allocator), t.toString(allocator) },
                            ident.reference,
                        ),
                        .undecided => |traits| {
                            // If the types matches all the traits inherited in the undecided type, then we can assign
                            // otherwise, error... =)
                            if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, real_right_type, traits))
                                try ctx.Error(
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
                                try ctx.Error(
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
        .bufferIndex => |bi| {
            const buf_type = try analyseValue(bi.buffer, ctx, allocator);
            if (buf_type == .undecided) return;
            const idx_type = try analyseValue(bi.index, ctx, allocator);
            if (!idx_type.matchType(try Types.CreateTypeInt(allocator, false)))
                try ctx.Error("Buffer index must be of type Int", .{}, bi.index.getReference());
            switch (buf_type.decided.base) {
                .buffer => {
                    const raw_elem = buf_type.decided.base.buffer;
                    const resolved_elem: *ast.Type = if (raw_elem.base == .name and ctx.type_resolved.contains(raw_elem.base.name))
                        ctx.type_resolved.get(raw_elem.base.name).?.decided
                    else
                        raw_elem;
                    const elem_type = Types.Type{ .decided = resolved_elem };
                    if (rightType == .decided and !rightType.matchType(elem_type))
                        try ctx.Error("Cannot assign '{s}' to buffer element of type '{s}'", .{ rightType.toString(allocator), elem_type.toString(allocator) }, bi.reference);
                },
                .name, .generic => {
                    const base_name: []const u8 = switch (buf_type.decided.base) {
                        .name => |n| n,
                        .generic => |g| g.name,
                        else => unreachable,
                    };
                    // Built-in IndexSet (type_implem)
                    if (ctx.type_implem.get(base_name)) |impls| {
                        for (impls.items) |func| {
                            if (std.mem.eql(u8, func.name, "IndexSet")) return;
                        }
                    }
                    // User-defined IndexSet (user_trait_impl)
                    const has_index_set = if (ctx.user_trait_impl.get(base_name)) |impls| blk: {
                        const index_set_fqn = resolveTraitName(&ctx.trait_defs, "IndexSet");
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, index_set_fqn)) break :blk true;
                        break :blk false;
                    } else false;
                    if (has_index_set) return;
                    try ctx.Error("Cannot assign via index into type '{s}': it does not implement IndexSet", .{base_name}, bi.reference);
                },
                else => try ctx.Error("Cannot index-assign into type '{s}'", .{buf_type.toString(allocator)}, bi.reference),
            }
        },
        else => try ctx.Error("Cannot assign to {}", .{value.*}, value.getReference()),
    }
}

pub fn typeparamContains(typeparam: ArrayList(ast.TypeParam), name: []const u8) bool {
    for (typeparam.items) |tp|
        if (std.mem.eql(u8, name, tp.name))
            return true;
    return false;
}

fn analyseInbuiltFuncall(func_name: []const u8, func: *ast.Funcall, ctx: *Context, allocator: Allocator) !Types.Type {
    const indef = ctx.inbuilt_funcs.get(func_name).?;
    const is_variadic = indef.params.len > 0 and indef.params[indef.params.len - 1].variadic;
    if (!is_variadic and func.args.items.len != indef.params.len)
        try ctx.Error("Function '{s}' expects {d} arguments, got {d}", .{ func_name, indef.params.len, func.args.items.len }, func.func.getReference());

    var concrete_type = std.StringHashMap(*ast.Type).init(allocator);
    defer concrete_type.deinit();

    var has_error = false;
    for (func.args.items, 0..) |arg, i| {
        const arg_type = try analyseValue(arg, ctx, allocator);
        if (arg_type == .decided and arg_type.decided.err) has_error = true;
        if (indef.params.len == 0) continue;
        const param_idx = if (i < indef.params.len) i else indef.params.len - 1;
        const param = indef.params[param_idx];
        if (param.is_type_param and arg_type == .decided) {
            if (concrete_type.get(param.type_name)) |already_bound| {
                const bound = Types.Type{ .decided = already_bound };
                if (!arg_type.matchType(bound))
                    try ctx.Error("Argument {d} of '{s}': type parameter '{s}' already bound to '{s}', got '{s}'", .{ i + 1, func_name, param.type_name, bound.toString(allocator), arg_type.toString(allocator) }, arg.getReference());
            } else {
                try concrete_type.put(param.type_name, arg_type.decided);
            }
        } else if (!param.any and arg_type == .decided) {
            const expected_t = try allocator.create(ast.Type);
            expected_t.* = .{ .base = .{ .name = param.type_name }, .err = false, .references = 0 };
            if (!arg_type.matchType(Types.Type{ .decided = expected_t }))
                try ctx.Error("Argument {d} of '{s}': expected '{s}', got '{s}'", .{ i + 1, func_name, param.type_name, arg_type.toString(allocator) }, arg.getReference());
        }
    }

    const err_flag = indef.return_type_has_error or if (indef.propagate_errors) has_error else false;
    if (indef.return_is_type_param) {
        if (concrete_type.get(indef.return_type)) |bound| {
            const ret = try allocator.create(ast.Type);
            ret.* = .{ .base = bound.base, .err = err_flag, .references = 0 };
            return Types.Type{ .decided = ret };
        }
        return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
    }

    const ret = try allocator.create(ast.Type);
    ret.* = .{ .base = .{ .name = indef.return_type }, .err = err_flag, .references = 0 };
    return Types.Type{ .decided = ret };
}

pub fn analyseFuncall(func: *ast.Funcall, ctx: *Context, allocator: Allocator) !Types.Type {
    if (func.func.* == .identifier and ctx.inbuilt_funcs.contains(func.func.identifier.name))
        return try analyseInbuiltFuncall(func.func.identifier.name, func, ctx, allocator);

    const f_signature = try analyseValue(func.func, ctx, allocator);
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

                    var func_version = std.hash_map.StringHashMap(Types.Type).init(allocator);

                    // Phase 1: bind type params from arguments
                    for (func.args.items, functype.argtypes.items) |a, b| {
                        const argtype = try analyseValue(a, ctx, allocator);
                        const is_type_param = b.*.base == .name and typeparamContains(functype.typeparam, b.base.name);
                        const is_buffer_of_type_param = b.*.base == .buffer and
                            b.base.buffer.base == .name and
                            typeparamContains(functype.typeparam, b.base.buffer.base.name);
                        switch (argtype) {
                            .undecided => return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) },
                            .decided => {
                                if (is_type_param) {
                                    if (func_version.contains(b.base.name)) {
                                        // Already bound: check consistency
                                        if (!argtype.matchType(func_version.get(b.base.name).?))
                                            try ctx.Error("Type parameter '{s}' bound to '{s}' but argument has type '{s}'", .{
                                                b.base.name,
                                                func_version.get(b.base.name).?.toString(allocator),
                                                argtype.toString(allocator),
                                            }, a.getReference());
                                    } else {
                                        try func_version.put(b.base.name, argtype);
                                    }
                                } else if (is_buffer_of_type_param) {
                                    const tp_name = b.base.buffer.base.name;
                                    if (argtype.decided.base != .buffer)
                                        try ctx.Error("Expected a buffer '[{s}]' but got '{s}'", .{ tp_name, argtype.toString(allocator) }, a.getReference());
                                    const elem_type = Types.Type{ .decided = argtype.decided.base.buffer };
                                    if (func_version.contains(tp_name)) {
                                        if (!elem_type.matchType(func_version.get(tp_name).?))
                                            try ctx.Error("Type parameter '{s}' bound to '{s}' but buffer has element type '{s}'", .{
                                                tp_name,
                                                func_version.get(tp_name).?.toString(allocator),
                                                elem_type.toString(allocator),
                                            }, a.getReference());
                                    } else {
                                        try func_version.put(tp_name, elem_type);
                                    }
                                } else if (b.*.base == .generic) {
                                    // E.g. param is List<Type> where Type is a type param.
                                    // Bind type params from the concrete arg's specialization type_bindings.
                                    const generic = b.base.generic;
                                    var any_tp = false;
                                    for (generic.params.items) |param| {
                                        if (param.base == .name and typeparamContains(functype.typeparam, param.base.name)) {
                                            any_tp = true;
                                            break;
                                        }
                                    }
                                    if (any_tp) {
                                        if (argtype.decided.base == .generic) {
                                            // arg is Bag<Int> stored as .generic — bind params directly
                                            const arg_gen = argtype.decided.base.generic;
                                            if (!std.mem.eql(u8, arg_gen.name, generic.name))
                                                try ctx.Error("Expected a '{s}' struct but got '{s}'", .{ generic.name, arg_gen.name }, a.getReference());
                                            for (generic.params.items, 0..) |param, i| {
                                                if (param.base != .name) continue;
                                                const func_tp_name = param.base.name;
                                                if (!typeparamContains(functype.typeparam, func_tp_name)) continue;
                                                if (i >= arg_gen.params.items.len) continue;
                                                const concrete_type = Types.Type{ .decided = arg_gen.params.items[i] };
                                                if (func_version.contains(func_tp_name)) {
                                                    if (!concrete_type.matchType(func_version.get(func_tp_name).?))
                                                        try ctx.Error("Type parameter '{s}' bound to '{s}' but argument implies '{s}'", .{
                                                            func_tp_name, func_version.get(func_tp_name).?.toString(allocator), arg_gen.params.items[i].toString(allocator),
                                                        }, a.getReference());
                                                } else {
                                                    try func_version.put(func_tp_name, concrete_type);
                                                }
                                            }
                                        } else if (argtype.decided.base == .name) {
                                            const arg_spec_name = argtype.decided.base.name;
                                            const is_specialization = std.mem.startsWith(u8, arg_spec_name, generic.name) and
                                                (arg_spec_name.len == generic.name.len or arg_spec_name[generic.name.len] == '<');
                                            if (!is_specialization)
                                                try ctx.Error("Expected a '{s}' struct but got '{s}'", .{ generic.name, arg_spec_name }, a.getReference());
                                            if (ctx.typeDefExist(arg_spec_name) and ctx.typeDefExist(generic.name)) {
                                                const arg_spec = ctx.getTypeDef(arg_spec_name);
                                                const base_orgn = ctx.getTypeDef(generic.name);
                                                if (arg_spec.type_bindings) |bindings| {
                                                    for (generic.params.items, 0..) |param, i| {
                                                        if (param.base != .name) continue;
                                                        const func_tp_name = param.base.name;
                                                        if (!typeparamContains(functype.typeparam, func_tp_name)) continue;
                                                        if (i >= base_orgn.typeparam.items.len) continue;
                                                        const struct_tp_name = base_orgn.typeparam.items[i].name;
                                                        if (bindings.get(struct_tp_name)) |concrete| {
                                                            const concrete_type = Types.Type{ .decided = concrete };
                                                            if (func_version.contains(func_tp_name)) {
                                                                if (!concrete_type.matchType(func_version.get(func_tp_name).?))
                                                                    try ctx.Error("Type parameter '{s}' bound to '{s}' but argument implies '{s}'", .{
                                                                        func_tp_name, func_version.get(func_tp_name).?.toString(allocator), concrete.toString(allocator),
                                                                    }, a.getReference());
                                                            } else {
                                                                try func_version.put(func_tp_name, concrete_type);
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        } else {
                                            try ctx.Error("Expected a '{s}' struct but got '{s}'", .{ generic.name, argtype.toString(allocator) }, a.getReference());
                                        }
                                    } else {
                                        // No type params — resolve already-bound params then check match
                                        const resolved = try resolveType(b, &func_version, ctx, allocator);
                                        if (!argtype.decided.match(resolved))
                                            try ctx.Error("The argument of type '{s}' doesn't match the expected type '{s}'", .{ argtype.toString(allocator), resolved.toString(allocator) }, a.getReference());
                                    }
                                } else {
                                    // Resolve b through func_version in case it's already-bound type param
                                    const expected: *ast.Type = if (b.*.base == .name and func_version.contains(b.base.name))
                                        func_version.get(b.base.name).?.decided
                                    else if (b.*.base == .buffer and b.base.buffer.base == .name and func_version.contains(b.base.buffer.base.name)) blk: {
                                        const resolved_elem = func_version.get(b.base.buffer.base.name).?.decided;
                                        const resolved_buf = try allocator.create(ast.Type);
                                        resolved_buf.* = ast.Type{ .base = .{ .buffer = resolved_elem }, .err = false, .references = 0 };
                                        break :blk resolved_buf;
                                    } else b;
                                    if (!argtype.decided.match(expected))
                                        try ctx.Error("The argument of type '{s}' doesn't match the expected type '{s}'", .{ argtype.toString(allocator), expected.toString(allocator) }, a.getReference());
                                }
                            },
                        }
                    }

                    // Phase 2: propagate unresolved type params from the caller's resolved context
                    for (functype.typeparam.items) |tp| {
                        if (!func_version.contains(tp.name)) {
                            if (ctx.type_resolved.contains(tp.name))
                                try func_version.put(tp.name, ctx.type_resolved.get(tp.name).?);
                        }
                    }

                    try ctx.addFunctionToCompile(functionVersion{
                        .name = f_signature.decided.base.function.fname,
                        .signature = f_signature.decided.base.function,
                        .version = func_version,
                    });

                    // Resolve return type through func_version
                    const ret_type = t.base.function.retype.base;
                    if (ret_type == .name and typeparamContains(t.base.function.typeparam, ret_type.name)) {
                        if (!func_version.contains(ret_type.name))
                            try ctx.Error("Unable to infer type for type parameter '{s}'", .{ret_type.name}, func.func.getReference());
                        return func_version.get(ret_type.name).?;
                    } else if (ret_type == .buffer and ret_type.buffer.base == .name and typeparamContains(t.base.function.typeparam, ret_type.buffer.base.name)) {
                        const tp_name = ret_type.buffer.base.name;
                        if (!func_version.contains(tp_name))
                            try ctx.Error("Unable to infer type for type parameter '{s}'", .{tp_name}, func.func.getReference());
                        const resolved_elem = func_version.get(tp_name).?.decided;
                        const resolved_buf = try allocator.create(ast.Type);
                        resolved_buf.* = ast.Type{ .base = .{ .buffer = resolved_elem }, .err = false, .references = 0 };
                        return Types.Type{ .decided = resolved_buf };
                    } else if (ret_type == .generic) {
                        // Substitute type params in generic return type through func_version
                        var resolved_params = ArrayList(*ast.Type).init(allocator);
                        for (ret_type.generic.params.items) |param| {
                            const rp: *ast.Type = if (param.base == .name and func_version.contains(param.base.name))
                                func_version.get(param.base.name).?.decided
                            else
                                param;
                            try resolved_params.append(rp);
                        }
                        const resolved_generic = ast.TypeGeneric{ .name = ret_type.generic.name, .params = resolved_params };
                        _ = try ensureGenericSpecialization(resolved_generic, ctx, allocator);
                        const resolved = try allocator.create(ast.Type);
                        resolved.* = ast.Type{ .base = .{ .generic = resolved_generic }, .err = t.base.function.retype.err, .references = 0 };
                        return Types.Type{ .decided = resolved };
                    }
                    return Types.Type{ .decided = t.base.function.retype };
                },
            }
        },
    }
    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
}

pub fn analyseNotOp(ctx: *Context, operandType: Types.Type, reference: Parser.Location, allocator: Allocator) !Types.Type {
    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, operandType, .Not)) {
        try ctx.Error(
            "Type '{s}' doesn't implement the 'Not' trait",
            .{operandType.toString(allocator)},
            reference,
        );
    }
    switch (operandType) {
        .decided => |t| {
            if (ctx.type_implem.get(t.base.name)) |impls| {
                for (impls.items) |func| {
                    if (std.mem.eql(u8, func.name, "Not"))
                        return Types.Type{ .decided = func.signature.retype };
                }
            }
            return try Traits.defaultReturnType(.Not, operandType, allocator);
        },
        .undecided => |traits| {
            try (@constCast(&traits)).append(.Not);
        },
    }
    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
}

pub fn analyseBinOp(op: ast.binOperator, ctx: *Context, rhsType: Types.Type, lhsType: Types.Type, reference: Parser.Location, allocator: Allocator) !Types.Type {
    // It's a bit like calling a the function "lhsName.opName(rhsType) => ?"
    const op_trait = Traits.traitFromOperator(op);
    if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, lhsType, op_trait)) {
        try ctx.Error(
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
                if (func.signature.argtypes.items.len != 1) try ctx.Error(
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
            try ctx.Error(
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

/// Recursively extract a flat dotted identifier from a value expression.
/// Returns "math" for `identifier("math")`, "math" for `(math)`,
/// "mathw.math" for `mathw.math` (unaryOperatorRight chain), etc.
/// Returns null when the expression is not a pure identifier/namespace chain.
pub fn tryExtractQualifiedIdent(val: *const ast.Value, allocator: Allocator) ?[]const u8 {
    return switch (val.*) {
        .identifier => |ident| ident.name,
        .parenthesis => |inner| tryExtractQualifiedIdent(inner, allocator),
        .unaryOperatorRight => |uop| blk: {
            if (uop.operator != .pointAttr) break :blk null;
            const left = tryExtractQualifiedIdent(uop.expr, allocator) orelse break :blk null;
            break :blk std.fmt.allocPrint(allocator, "{s}.{s}", .{ left, uop.operator.pointAttr }) catch null;
        },
        else => null,
    };
}

pub fn analyseValue(value: *ast.Value, ctx: *Context, allocator: Allocator) (std.mem.Allocator.Error || errors.bbcErrors)!Types.Type {
    switch (value.*) {
        .varDec => |vardec| {
            if (ctx.variableExist(vardec.name))
                try ctx.Error("The variable {s} has already been declared before", .{vardec.name}, vardec.reference);
            const ret_type = try Types.CreateTypeVoid(allocator, false);
            try ctx.createVariable(vardec.name, Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) }, vardec.reference);
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
                    try ctx.Error("The type of this scope don't match the return type of the if statement", .{}, scope.getReference());
                // Then the condition
                const cond_type = try analyseValue(cond, ctx, allocator);
                if (!cond_type.matchType(bool_type))
                    try ctx.Error("The type of this condition does not match 'Bool'", .{}, cond.getReference());
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
                    try ctx.Error("The type of the else '{s}' scope don't match the type of this if statement", .{else_type.toString(allocator)}, else_scope.getReference());
            } else if (!void_type.matchType(base_scope_type)) { // Else statements are required if the evaluation type insn't !Void
                try ctx.Error("The if statements evalutates to type '{s}' and not '!Void', so it requires an else clause", .{base_scope_type.toString(allocator)}, ifstmt.reference);
            }
            // base_scope_type.decided.err = has_error;
            return try Types.duplicateWithErrorUnion(allocator, base_scope_type.decided, has_error);
        },
        .parenthesis => |val| return try analyseValue(val, ctx, allocator),
        .scope => |scope| return try analyseScope(scope, ctx, allocator),
        .identifier => |ident| {
            if (ctx.functionExist(ident.name))
                return Types.Type{ .decided = try createFunctionSignature(ctx.getFunction(ident.name), allocator) };
            if (ctx.inbuilt_funcs.contains(ident.name))
                return try Types.CreateTypeVoid(allocator, false);
            // Namespace identifier: `math` where math is an import namespace
            if (ctx.isNamespace(ident.name)) {
                const ns_t = try allocator.create(ast.Type);
                ns_t.* = .{ .base = .{ .import_ns = ident.name }, .err = false, .references = 0 };
                return Types.Type{ .decided = ns_t };
            }
            if (!ctx.variableExist(ident.name))
                try ctx.Error("Variable {s} is not declared (analysis)", .{ident.name}, ident.reference);

            return ctx.getVariable(ident.name);
        },
        .intLit => return try Types.CreateTypeInt(allocator, false),
        .floatLit => return try Types.CreateTypeFloat(allocator, false),
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
                try ctx.Error("The value don't have errors to check", .{}, errcheck.value.getReference());
            if (scope_type == .decided and scope_type.decided.err)
                try ctx.Error("The default scope can't have errors", .{}, errcheck.scope.getReference());
            if (!scope_type.matchType(val_type))
                try ctx.Error("The scope's type don't match the value's type", .{}, errcheck.reference);
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
                try ctx.Error("The condition of the while loop should have type 'Bool'", .{}, whileloop.condition.getReference());
            const exec_type = try analyseValue(whileloop.exec, ctx, allocator);
            if (!exec_type.matchType(void_type))
                try ctx.Error("The exec'ed code should evaluate to type 'Void'", .{}, whileloop.exec.getReference());

            // Can it evaluate to an error union ?
            const has_error = cond_type == .decided and cond_type.decided.err or exec_type == .decided and exec_type.decided.err;

            return Types.CreateTypeVoid(allocator, has_error);
        },
        .structInit => |stc_init| {
            // Strip any prior specialization suffix so re-analysis of a generic function
            // with different type params always starts from the base template (e.g. "List<Char>" → "List").
            const name: []const u8 = if (std.mem.indexOf(u8, stc_init.name, "<")) |idx|
                stc_init.name[0..idx]
            else
                stc_init.name;
            if (!ctx.typeDefExist(name))
                try ctx.Error("Type name '{s}' don't exist", .{name}, stc_init.reference);
            const orgn = ctx.getTypeDef(name);

            if (orgn.typeparam.items.len > 0) {
                // Generic struct: analyse all field values first
                var hab_types = std.StringHashMap(Types.Type).init(allocator);
                var hab_it_pre = stc_init.habitants.iterator();
                while (hab_it_pre.next()) |hab|
                    try hab_types.put(hab.key_ptr.*, try analyseValue(hab.value_ptr.*, ctx, allocator));

                // Infer one concrete type per type parameter from the field values
                var version = std.StringHashMap(Types.Type).init(allocator);
                for (orgn.typeparam.items) |tp| {
                    var found = false;
                    var fld_it = orgn.habitants.iterator();
                    while (fld_it.next()) |fld| {
                        const fld_type = fld.value_ptr.*;
                        const is_bare_param = fld_type.base == .name and
                            std.mem.eql(u8, fld_type.base.name, tp.name);
                        const is_buf_param = fld_type.base == .buffer and
                            fld_type.base.buffer.base == .name and
                            std.mem.eql(u8, fld_type.base.buffer.base.name, tp.name);
                        if (!is_bare_param and !is_buf_param) continue;
                        if (hab_types.get(fld.key_ptr.*)) |val_type| {
                            if (val_type != .decided) continue;
                            // For [Type] fields extract the element type from the buffer value
                            const resolved_type: Types.Type = if (is_buf_param) blk: {
                                if (val_type.decided.base != .buffer) continue;
                                break :blk Types.Type{ .decided = val_type.decided.base.buffer };
                            } else val_type;
                            if (version.contains(tp.name)) {
                                if (!resolved_type.matchType(version.get(tp.name).?))
                                    try ctx.Error("Conflicting types for type parameter '{s}'", .{tp.name}, stc_init.reference);
                            } else {
                                try version.put(tp.name, resolved_type);
                            }
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                        try ctx.Error("Cannot infer type parameter '{s}' from struct literal", .{tp.name}, stc_init.reference);
                }

                // Validate trait constraints on the inferred types
                for (orgn.typeparam.items) |tp| {
                    if (!version.contains(tp.name)) continue;
                    const concrete = version.get(tp.name).?;
                    if (concrete != .decided) continue;
                    for (tp.traits.items) |trait_name| {
                        if (Traits.isBuiltinTraitName(trait_name)) {
                            const trait = Traits.traitFromString(trait_name);
                            if (!Traits.typeMatchTrait(&ctx.trait_map, &ctx.typealiases, concrete, trait))
                                try ctx.Error("Type '{s}' doesn't implement trait '{s}' required by '{s}'", .{
                                    concrete.toString(allocator), trait_name, tp.name,
                                }, stc_init.reference);
                        } else {
                            const concrete_name = concrete.decided.base.name;
                            const has_impl = if (ctx.user_trait_impl.get(concrete_name)) |impls| blk: {
                                for (impls.items) |impl_name|
                                    if (std.mem.eql(u8, impl_name, trait_name)) break :blk true;
                                break :blk false;
                            } else false;
                            if (!has_impl)
                                try ctx.Error("Type '{s}' doesn't implement trait '{s}' required by '{s}'", .{
                                    concrete_name, trait_name, tp.name,
                                }, stc_init.reference);
                        }
                    }
                }

                const spec_name = try createSpecializationFromVersion(orgn, &version, ctx, allocator);

                // Validate fields against specialised struct
                const spec_stct = ctx.getTypeDef(spec_name);
                if (stc_init.habitants.count() != spec_stct.habitants.count() - 2 - spec_stct.methods.count())
                    try ctx.Error(
                        "Not the right number of habitants in the struct initialisation, expected {d} got {d}",
                        .{ stc_init.habitants.count(), spec_stct.habitants.count() - 2 - spec_stct.methods.count() },
                        stc_init.reference,
                    );
                var has_error_spec = false;
                var hab_valid_it = stc_init.habitants.iterator();
                while (hab_valid_it.next()) |hab| {
                    const hab_name = hab.key_ptr.*;
                    if (!spec_stct.habitantExist(hab_name))
                        try ctx.Error("Habitant '{s}' is undefined in '{s}'", .{ hab_name, spec_name }, stc_init.reference);
                    const hab_type = hab_types.get(hab_name).?;
                    if (!hab_type.match(spec_stct.getHabitant(hab_name)))
                        try ctx.Error("Habitant '{s}' type mismatch in '{s}'", .{ hab_name, spec_name }, stc_init.reference);
                    if (hab_type == .decided) has_error_spec = has_error_spec or hab_type.decided.err;
                }

                // Mutate the AST node name so the interpreter and infer pass see the concrete type
                stc_init.name = spec_name;

                var resolved_params_spec = std.ArrayList(*ast.Type).init(allocator);
                for (orgn.typeparam.items) |tp| {
                    if (version.get(tp.name)) |concrete|
                        try resolved_params_spec.append(concrete.decided);
                }
                const ret_spec = try allocator.create(ast.Type);
                ret_spec.base = .{ .generic = .{ .name = orgn.name, .params = resolved_params_spec } };
                ret_spec.references = 0;
                ret_spec.err = has_error_spec;
                return Types.Type{ .decided = ret_spec };
            }

            if (stc_init.habitants.count() != orgn.habitants.count() - 2 - orgn.methods.count())
                try ctx.Error(
                    "Not the right number of habitants in the struct initialisation, expected {d} got {d}",
                    .{ stc_init.habitants.count(), orgn.habitants.count() },
                    stc_init.reference,
                );

            var stc_hab_it = stc_init.habitants.iterator();
            var has_error = false;
            while (stc_hab_it.next()) |hab| {
                const hab_name = hab.key_ptr.*;
                if (!orgn.habitantExist(hab_name))
                    try ctx.Error("Habitant '{s}' is undefined", .{hab_name}, stc_init.reference);
                const hab_type = try analyseValue(hab.value_ptr.*, ctx, allocator);
                // The type got does not match the expected one
                if (!hab_type.match(orgn.getHabitant(hab_name)))
                    try ctx.Error("Habitant '{s}' is expected to be of type '{s}' but is actually of type '{s}'", .{
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
                const attr = uop_right.operator.pointAttr;
                // Resolve the base of the chain to a flat qualified name (handles
                // `math.func`, `(math).func`, `mathw.math.func`, and any depth).
                if (tryExtractQualifiedIdent(uop_right.expr, allocator)) |base_name| {
                    const qualified = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ base_name, attr });
                    if (ctx.functionExist(qualified)) {
                        return Types.Type{ .decided = try createFunctionSignature(ctx.getFunction(qualified), allocator) };
                    }
                    // Qualified name is itself a namespace (e.g. `mathw.math`)
                    if (ctx.isNamespace(qualified)) {
                        const ns_t = try allocator.create(ast.Type);
                        ns_t.* = .{ .base = .{ .import_ns = qualified }, .err = false, .references = 0 };
                        return Types.Type{ .decided = ns_t };
                    }
                }
                // Left side might be a variable holding a namespace (e.g. `let ns = math; ns.func`)
                const left_value = try analyseValue(uop_right.expr, ctx, allocator);
                if (left_value == .undecided)
                    return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
                // Namespace value: look up `attr` within it
                if (left_value.decided.base == .import_ns) {
                    const ns_name = left_value.decided.base.import_ns;
                    const qualified = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ ns_name, attr });
                    if (ctx.functionExist(qualified)) {
                        return Types.Type{ .decided = try createFunctionSignature(ctx.getFunction(qualified), allocator) };
                    }
                    if (ctx.isNamespace(qualified)) {
                        const ns_t = try allocator.create(ast.Type);
                        ns_t.* = .{ .base = .{ .import_ns = qualified }, .err = false, .references = 0 };
                        return Types.Type{ .decided = ns_t };
                    }
                    try ctx.Error("Namespace '{s}' has no member '{s}'", .{ ns_name, attr }, uop_right.reference);
                }
                // Buffer point access: only _size and _count are allowed
                if (left_value.decided.base == .buffer) {
                    if (!std.mem.eql(u8, attr, "_size") and !std.mem.eql(u8, attr, "_count"))
                        try ctx.Error("Buffer only has '_size' and '_count' attributes, not '{s}'", .{attr}, uop_right.reference);
                    return Types.CreateTypeInt(allocator, false);
                }
                // Resolve .generic (e.g. List<Int>) to its specialization name
                const struct_name: []const u8 = switch (left_value.decided.base) {
                    .name => |n| n,
                    .generic => |g| try ensureGenericSpecialization(g, ctx, allocator),
                    .buffer => unreachable, // handled above
                    .import_ns => unreachable, // handled above
                    .function => {
                        try ctx.Error("Cannot access attribute of a function type", .{}, uop_right.reference);
                        unreachable;
                    },
                };
                if (!ctx.typeDefExist(struct_name)) {
                    try ctx.Error("Unknown struct type name '{s}'", .{struct_name}, uop_right.reference);
                }
                const hab_name = attr;
                const typedef = ctx.getTypeDef(struct_name);
                if (!typedef.habitantExist(attr))
                    try ctx.Error("No habitant with name '{s}' in the structure '{s}'", .{ hab_name, typedef.name }, uop_right.reference);
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
        .bufferAlloc => |ba| {
            const size_type = try analyseValue(ba.size, ctx, allocator);
            if (!size_type.matchType(try Types.CreateTypeInt(allocator, false)))
                try ctx.Error("Buffer size must be of type Int", .{}, ba.size.getReference());
            if (!ctx.typeExist(ba.elem_type))
                try ctx.Error("Unknown element type '{s}'", .{ba.elem_type.toString(allocator)}, ba.reference);
            const resolved_elem: *ast.Type = if (ba.elem_type.base == .name and ctx.type_resolved.contains(ba.elem_type.base.name))
                ctx.type_resolved.get(ba.elem_type.base.name).?.decided
            else
                ba.elem_type;
            const ret = try allocator.create(ast.Type);
            ret.* = ast.Type{ .base = .{ .buffer = resolved_elem }, .err = false, .references = 0 };
            return Types.Type{ .decided = ret };
        },
        .bufferLit => |bl| {
            var elem_type: ?*ast.Type = null;
            for (bl.elements.items) |elem| {
                const t = try analyseValue(elem, ctx, allocator);
                if (t == .decided) {
                    if (elem_type == null) {
                        elem_type = t.decided;
                    } else if (!t.decided.match(elem_type.?)) {
                        try ctx.Error("Buffer literal has inconsistent element types", .{}, bl.reference);
                    }
                }
            }
            if (elem_type == null)
                return Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
            const ret = try allocator.create(ast.Type);
            ret.* = ast.Type{ .base = .{ .buffer = elem_type.? }, .err = false, .references = 0 };
            return Types.Type{ .decided = ret };
        },
        .bufferIndex => |bi| {
            const buf_type = try analyseValue(bi.buffer, ctx, allocator);
            if (buf_type == .undecided) return buf_type;
            const idx_type = try analyseValue(bi.index, ctx, allocator);
            if (!idx_type.matchType(try Types.CreateTypeInt(allocator, false)))
                try ctx.Error("Buffer index must be of type Int", .{}, bi.index.getReference());
            switch (buf_type.decided.base) {
                .buffer => return Types.Type{ .decided = try Types.wrapWithErr(buf_type.decided.base.buffer, allocator) },
                .name => |name| {
                    if (ctx.type_implem.get(name)) |impls| {
                        for (impls.items) |func| {
                            if (std.mem.eql(u8, func.name, "Subscription"))
                                return Types.Type{ .decided = try Types.wrapWithErr(func.signature.retype, allocator) };
                        }
                    }
                    // User-defined Subscription trait (e.g. implement Subscription: List<Type>)
                    const has_user_sub = if (ctx.user_trait_impl.get(name)) |impls| blk: {
                        const sub_fqn = resolveTraitName(&ctx.trait_defs, "Subscription");
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, sub_fqn)) break :blk true;
                        break :blk false;
                    } else false;
                    if (has_user_sub and ctx.typeDefExist(name)) {
                        const struct_def = ctx.getTypeDef(name);
                        if (struct_def.habitants.get("index")) |index_sig| {
                            if (index_sig.base == .function)
                                return Types.Type{ .decided = try Types.wrapWithErr(index_sig.base.function.retype, allocator) };
                        }
                    }
                    try ctx.Error("Cannot index into type '{s}': it does not implement Subscription", .{name}, bi.reference);
                    return Types.CreateTypeVoid(allocator, false);
                },
                .generic => |gen| {
                    const base_name: []const u8 = gen.name;
                    const has_subs = if (ctx.user_trait_impl.get(base_name)) |impls| blk: {
                        const sub_fqn = resolveTraitName(&ctx.trait_defs, "Subscription");
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, sub_fqn)) break :blk true;
                        break :blk false;
                    } else false;
                    if (!has_subs)
                        try ctx.Error(
                            "Type '{s}' does not implement the 'Subscription' trait",
                            .{base_name},
                            bi.buffer.getReference(),
                        );
                    const spec_name = try ensureGenericSpecialization(gen, ctx, allocator);
                    if (ctx.typeDefExist(spec_name)) {
                        const struct_def = ctx.getTypeDef(spec_name);
                        if (struct_def.habitants.get("index")) |index_sig| {
                            if (index_sig.base == .function)
                                return Types.Type{ .decided = try Types.wrapWithErr(index_sig.base.function.retype, allocator) };
                        }
                    }
                    try ctx.Error("The struct type '{s}' doesn't allow for indexation", .{buf_type.toString(allocator)}, bi.reference);
                    return Types.Type{ .undecided = .init(allocator) };
                },
                else => {
                    try ctx.Error("Cannot index into non-indexable type '{s}'", .{buf_type.toString(allocator)}, bi.reference);
                    return Types.CreateTypeVoid(allocator, false);
                },
            }
        },
        .notOp => |notop| {
            const expr_type = try analyseValue(notop.expr, ctx, allocator);
            return try analyseNotOp(ctx, expr_type, notop.reference, allocator);
        },
        .For => |for_loop| {
            const iter_type = try analyseValue(for_loop.iterable, ctx, allocator);

            const void_type = try Types.CreateTypeVoid(allocator, true);
            defer void_type.deinit(allocator);

            // Check Iter trait and resolve element type from next() return type.
            // Generic types stay as .generic so g.name is the clean base name.
            var elem_type: Types.Type = Types.Type{ .undecided = ArrayList(Traits.Trait).init(allocator) };
            switch (iter_type) {
                .decided => |t| {
                    const base_name: []const u8 = switch (t.base) {
                        .generic => |g| g.name,
                        .name => |n| n,
                        else => {
                            try ctx.Error("For loop iterable must be a named or generic type", .{}, for_loop.iterable.getReference());
                            return Types.CreateTypeVoid(allocator, false);
                        },
                    };
                    const has_iter = if (ctx.user_trait_impl.get(base_name)) |impls| blk: {
                        const iter_fqn = resolveTraitName(&ctx.trait_defs, "Iter");
                        for (impls.items) |impl_name|
                            if (std.mem.eql(u8, impl_name, iter_fqn)) break :blk true;
                        break :blk false;
                    } else false;
                    if (!has_iter)
                        try ctx.Error(
                            "Type '{s}' does not implement the 'Iter' trait",
                            .{base_name},
                            for_loop.iterable.getReference(),
                        );
                    // Resolve element type: look up next() return type on the specialisation
                    const spec_name: []const u8 = switch (t.base) {
                        .generic => |g| try ensureGenericSpecialization(g, ctx, allocator),
                        .name => |n| n,
                        else => base_name,
                    };
                    if (ctx.typeDefExist(spec_name)) {
                        const struct_def = ctx.getTypeDef(spec_name);
                        if (struct_def.habitants.get("next")) |next_sig| {
                            if (next_sig.base == .function)
                                elem_type = Types.Type{ .decided = next_sig.base.function.retype };
                        }
                    }
                },
                .undecided => {},
            }

            if (!ctx.variableExist(for_loop.var_name))
                try ctx.createVariable(for_loop.var_name, elem_type, for_loop.iterable.getReference());

            const exec_type = try analyseValue(for_loop.exec, ctx, allocator);
            if (!exec_type.matchType(void_type))
                try ctx.Error("The body of a for loop should evaluate to 'Void'", .{}, for_loop.exec.getReference());

            const has_error = iter_type == .decided and iter_type.decided.err or
                exec_type == .decided and exec_type.decided.err;
            return Types.CreateTypeVoid(allocator, has_error);
        },
        else => {
            std.debug.print("{?}", .{value.*});
            unreachable;
        },
    }
}

pub fn analyseScope(scope: *ast.Scope, ctx: *Context, allocator: Allocator) (std.mem.Allocator.Error || errors.bbcErrors)!Types.Type {
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

pub fn analyseFunction(func: *ast.funcDef, par_ctx: *Context, version: std.StringHashMap(Types.Type), allocator: Allocator) !void {
    // Defining the function's signature type
    const functype = try createFunctionSignature(func, allocator);

    // Associating the name with the type
    try par_ctx.createVariable(func.name, Types.Type{ .decided = functype }, func.reference);

    // We can create a child context for the function
    var ctx = try par_ctx.createChild(allocator);
    func.ctx = ctx;

    // Register trait constraints for type params, separating built-in from user-defined
    for (func.typeparam.items) |type_param| {
        var builtin_traits = ArrayList(Traits.Trait).init(allocator);
        var user_trait_names = ArrayList([]const u8).init(allocator);
        for (type_param.traits.items) |trait_name| {
            if (Traits.isBuiltinTraitName(trait_name)) {
                try builtin_traits.append(Traits.traitFromString(trait_name));
            } else {
                try user_trait_names.append(trait_name);
            }
        }
        try ctx.typealiases.put(type_param.name, builtin_traits);
        if (user_trait_names.items.len > 0) {
            try ctx.user_typealiases.put(type_param.name, user_trait_names);
        }
    }

    // Populate concrete type bindings from the resolved version (e.g. T -> Int)
    var version_it = version.iterator();
    while (version_it.next()) |kv| {
        try ctx.type_resolved.put(kv.key_ptr.*, kv.value_ptr.*);
    }

    // and add the inbuild info-functions
    try ctx.setVariable("__name", try Types.CreateTypeString(allocator, false));

    if (func.parent) |parent| {
        const typedef = ctx.getTypeDef(parent.base.name);
        for (typedef.fields.items) |field_name| {
            try ctx.createVariable(field_name, .{ .decided = typedef.habitants.get(field_name).? }, typedef.reference);
        }
        try ctx.setVariable("self", Types.Type{ .decided = parent });
    }

    if (!ctx.typeExist(func.return_type))
        try ctx.Error("The type '{s}' doesn't exist", .{func.return_type.toString(allocator)}, func.reference);

    // associating variables — substitute concrete types for type params
    for (func.arguments.items) |arg| {
        if (!ctx.typeExist(arg._type))
            try ctx.Error("The type '{s}' doesn't exist", .{arg._type.toString(allocator)}, arg.reference);
        const resolved_ast = try resolveType(arg._type, &ctx.type_resolved, ctx, allocator);
        try ctx.createVariable(arg.name, Types.Type{ .decided = resolved_ast }, arg.reference);
    }

    // Resolve declared return type through concrete type bindings
    const resolved_ret = Types.Type{ .decided = try resolveType(func.return_type, &ctx.type_resolved, ctx, allocator) };

    std.debug.print("\n#[Analysing {s}]\n", .{try func.getName(allocator)});
    const actual_ret = try analyseScope(func.code, ctx, allocator);

    // Validate body return type against resolved declared return type
    if (actual_ret == .decided and resolved_ret == .decided) {
        const actual_bare = try Types.duplicateWithErrorUnion(allocator, actual_ret.decided, false);
        const declared_bare = try Types.duplicateWithErrorUnion(allocator, resolved_ret.decided, false);
        if (!actual_bare.matchType(declared_bare))
            try ctx.Error("Function '{s}' body evaluates to '{s}' but declared return type is '{s}'", .{
                func.name,
                actual_ret.toString(allocator),
                resolved_ret.toString(allocator),
            }, func.return_type_ref);
    }

    try Types.inferTypeScope(func.code, ctx, allocator, resolved_ret);
    var it = func.code.ctx.variables.iterator();
    while (it.next()) |kv| {
        std.debug.print("{s} is {s}\n", .{ kv.key_ptr.*, kv.value_ptr.toString(allocator) });
    }
}

pub fn verifyFunctionTypeTraits(func: *ast.funcDef, ctx: *Context, allocator: Allocator, f_version: functionVersion) !void {
    // Check traits inferred from body usage
    var val = ast.Value{ .scope = func.code };
    const t = try Traits.getTypeTraits(&val, ctx, allocator);
    var it_k = t.iterator();
    while (it_k.next()) |e| {
        const arg_type_name = e.key_ptr.*;
        const arg_type_traits = e.value_ptr.*;

        if (!f_version.version.contains(arg_type_name))
            continue;

        const aliased = f_version.version.get(arg_type_name).?;
        if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, aliased, arg_type_traits))
            try ctx.Error("The type '{s}' should implement [{s}], but doesn't", .{
                aliased.toString(allocator),
                try Traits.traitListToString(arg_type_traits, allocator),
            }, func.reference);
    }

    // Check declared type parameter constraints (e.g. <T: Add, Mul> — verify Mul even if unused in body)
    for (func.typeparam.items) |tp| {
        if (!f_version.version.contains(tp.name))
            continue;
        const concrete = f_version.version.get(tp.name).?;

        // Check built-in trait constraints
        var builtin_traits = ArrayList(Traits.Trait).init(allocator);
        for (tp.traits.items) |trait_name| {
            if (Traits.isBuiltinTraitName(trait_name))
                try builtin_traits.append(Traits.traitFromString(trait_name));
        }
        if (!Traits.typeMatchTraits(&ctx.trait_map, &ctx.typealiases, concrete, builtin_traits))
            try ctx.Error("Type '{s}' is bound to '{s}' which doesn't implement the required built-in traits [{s}]", .{
                tp.name,
                concrete.toString(allocator),
                try Traits.traitListToString(builtin_traits, allocator),
            }, func.reference);

        // Check user-defined trait constraints
        if (concrete != .decided) continue;
        const concrete_name = concrete.decided.base.name;
        for (tp.traits.items) |trait_name| {
            if (Traits.isBuiltinTraitName(trait_name)) continue;
            const has_impl = if (ctx.user_trait_impl.get(concrete_name)) |impls| blk: {
                for (impls.items) |impl_name| {
                    if (std.mem.eql(u8, impl_name, trait_name)) break :blk true;
                }
                break :blk false;
            } else false;
            if (!has_impl)
                try ctx.Error("Type '{s}' doesn't implement trait '{s}'", .{ concrete_name, trait_name }, func.reference);
        }
    }
}

pub fn analyseTraitDef(trait: *ast.traitDef, ctx: *Context) !void {
    try ctx.trait_defs.put(trait.name, trait);
}

pub fn analyseTraitImpl(impl: *ast.traitImpl, ctx: *Context, allocator: Allocator) !void {
    if (!ctx.trait_defs.contains(impl.trait_name))
        try ctx.Error("Unknown trait '{s}'", .{impl.trait_name}, impl.reference);

    const trait_def = ctx.trait_defs.get(impl.trait_name).?;

    if (!ctx.typeDefExist(impl.type_name))
        try ctx.Error("Unknown type '{s}' in implement", .{impl.type_name}, impl.reference);

    const struct_def = ctx.getTypeDef(impl.type_name);

    // Verify the implementing type has enough type params for the trait's inner params
    // if (trait_def.inner_params.items.len > 0 and
    //     struct_def.typeparam.items.len < trait_def.inner_params.items.len)
    //     try ctx.Error(
    //         "Trait '{s}' requires {d} type parameter(s), but '{s}' only has {d}",
    //         .{ impl.trait_name, trait_def.inner_params.items.len, impl.type_name, struct_def.typeparam.items.len },
    //         impl.reference,
    //     );
    // EDIT : don't really need this

    // Verify all required trait methods are provided
    var method_it = trait_def.methods.iterator();
    while (method_it.next()) |entry| {
        const required_name = entry.key_ptr.*;
        var found = false;
        for (impl.methods.items) |method| {
            if (std.mem.eql(u8, method.name, required_name)) {
                found = true;
                break;
            }
        }
        if (!found)
            try ctx.Error("Missing implementation of method '{s}' required by trait '{s}'", .{ required_name, impl.trait_name }, impl.reference);
    }

    // For generic implementations (Toto<Type>), methods are registered on the generic
    // struct without immediate compilation — specialization handles them later.
    const is_generic_impl = impl.type_params.items.len > 0 and struct_def.typeparam.items.len > 0;

    // Register each method on the struct
    for (impl.methods.items) |method| {
        const method_sig = try createFunctionSignature(method, allocator);
        if (!struct_def.habitants.contains(method.name)) {
            try struct_def.habitants.put(method.name, method_sig);
            try struct_def.fields.append(method.name);
        }
        if (!struct_def.methods.contains(method.name)) {
            try struct_def.methods.put(method.name, method);
        }
        if (!is_generic_impl) {
            const full_name = try method.getName(allocator);
            try ctx.setFunction(full_name, method);
            try ctx.addFunctionToCompile(functionVersion{
                .name = full_name,
                .signature = method_sig.base.function,
                .version = std.hash_map.StringHashMap(Types.Type).init(allocator),
            });
        }
    }

    // Record that type_name implements trait_name
    if (!ctx.user_trait_impl.contains(impl.type_name)) {
        try ctx.user_trait_impl.put(impl.type_name, ArrayList([]const u8).init(allocator));
    }
    try ctx.user_trait_impl.getPtr(impl.type_name).?.append(impl.trait_name);
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

// Resolves all type parameters in `t` using `version`, recursively.
// Handles .name (scalar param), .buffer (element param), .generic (nested specialization).
// Returns `t` unchanged if nothing needed substituting.
pub fn resolveType(t: *ast.Type, version: *const std.StringHashMap(Types.Type), ctx: *Context, allocator: Allocator) (std.mem.Allocator.Error || errors.bbcErrors)!*ast.Type {
    switch (t.base) {
        .name => |name| {
            if (version.contains(name)) return version.get(name).?.decided;
            return t;
        },
        .buffer => |elem| {
            const resolved_elem = try resolveType(elem, version, ctx, allocator);
            if (resolved_elem == elem) return t;
            const buf = try allocator.create(ast.Type);
            buf.* = ast.Type{ .base = .{ .buffer = resolved_elem }, .err = t.err, .references = 0 };
            return buf;
        },
        .generic => |g| {
            var resolved_params = ArrayList(*ast.Type).init(allocator);
            for (g.params.items) |param|
                try resolved_params.append(try resolveType(param, version, ctx, allocator));
            const resolved_g = ast.TypeGeneric{ .name = g.name, .params = resolved_params };
            _ = try ensureGenericSpecialization(resolved_g, ctx, allocator);
            const result = try allocator.create(ast.Type);
            result.* = ast.Type{ .base = .{ .generic = resolved_g }, .err = t.err, .references = 0 };
            return result;
        },
        .function => return t,
        .import_ns => return t,
    }
}

// Creates a specialization of generic struct `orgn` using `version` (TypeParam.name -> concrete Type).
// Returns the spec name (e.g. "List<Int>"). Idempotent — safe to call multiple times.
pub fn createSpecializationFromVersion(
    orgn: *ast.structDef,
    version: *const std.StringHashMap(Types.Type),
    ctx: *Context,
    allocator: Allocator,
) ![]const u8 {
    var spec_buf = std.ArrayList(u8).init(allocator);
    try spec_buf.appendSlice(orgn.name);
    try spec_buf.append('<');
    for (orgn.typeparam.items, 0..) |tp, i| {
        if (i > 0) try spec_buf.appendSlice(", ");
        if (version.get(tp.name)) |tv| try spec_buf.appendSlice(tv.toString(allocator));
    }
    try spec_buf.append('>');
    const spec_name = try spec_buf.toOwnedSlice();

    if (ctx.typeDefExist(spec_name)) return spec_name;

    // Build type_bindings: original param name -> concrete *ast.Type (for reverse-lookup)
    var type_bindings = std.StringHashMap(*ast.Type).init(allocator);
    var ver_it2 = version.iterator();
    while (ver_it2.next()) |kv| try type_bindings.put(kv.key_ptr.*, kv.value_ptr.*.decided);

    const spec_stct = try allocator.create(ast.structDef);
    spec_stct.* = ast.structDef{
        .habitants = .init(allocator),
        .fields = .init(allocator),
        .name = spec_name,
        .reference = orgn.reference,
        .methods = .init(allocator),
        .typeparam = ArrayList(ast.TypeParam).init(allocator),
        .type_bindings = type_bindings,
    };

    for (orgn.fields.items) |field_name| {
        const orig_type = orgn.habitants.get(field_name).?;
        const concrete_type: *ast.Type = try resolveType(orig_type, version, ctx, allocator);
        try spec_stct.habitants.put(field_name, concrete_type);
        try spec_stct.fields.append(field_name);
    }

    const spec_parent = try allocator.create(ast.Type);
    spec_parent.* = ast.Type{ .base = .{ .name = spec_name }, .err = false, .references = 0 };
    var meth_it = orgn.methods.iterator();
    while (meth_it.next()) |meth| {
        const spec_method = try allocator.create(ast.funcDef);
        spec_method.* = meth.value_ptr.*.*;
        spec_method.parent = spec_parent;
        spec_method.return_type = try resolveType(meth.value_ptr.*.return_type, version, ctx, allocator);
        var spec_args = ArrayList(*ast.Arguments).init(allocator);
        for (meth.value_ptr.*.arguments.items) |arg| {
            const spec_arg = try allocator.create(ast.Arguments);
            spec_arg.* = arg.*;
            spec_arg._type = try resolveType(arg._type, version, ctx, allocator);
            try spec_args.append(spec_arg);
        }
        spec_method.arguments = spec_args;
        try spec_stct.methods.put(meth.key_ptr.*, spec_method);

        const spec_sig = try createSpecializedFunctionType(spec_method, spec_name, version, ctx, allocator);
        try spec_stct.habitants.put(meth.key_ptr.*, spec_sig);

        const meth_full = try std.mem.concat(allocator, u8, &[_][]const u8{ spec_name, ".", meth.key_ptr.* });
        try ctx.setFunction(meth_full, spec_method);
        try ctx.addFunctionToCompile(functionVersion{
            .name = meth_full,
            .signature = spec_sig.base.function,
            .version = std.hash_map.StringHashMap(Types.Type).init(allocator),
        });
    }

    try ctx.addTypeDef(spec_name, spec_stct);
    var spec_traits = ArrayList(Traits.Trait).init(allocator);
    try spec_traits.append(Traits.Trait.Eq);
    var spec_implems = ArrayList(funcPair).init(allocator);
    try Traits.createTraitWithBinOperators(
        &spec_implems,
        (try Types.CreateTypeVoid(allocator, false)).decided,
        (try Types.CreateTypeBool(allocator, false)).decided,
        &[_]ast.binOperator{ .Equal, .NotEqual },
        allocator,
    );
    var root_ctx = ctx;
    while (root_ctx.parent) |p| root_ctx = p;

    // Inherit traits from the base generic type
    if (root_ctx.trait_map.get(orgn.name)) |base_traits| {
        for (base_traits.items) |t| {
            var already = false;
            for (spec_traits.items) |st| {
                if (st == t) {
                    already = true;
                    break;
                }
            }
            if (!already) try spec_traits.append(t);
        }
    }
    // Inherit non-binary-op implementations from base (e.g. Subscription, IndexSet), resolving type params
    if (root_ctx.type_implem.get(orgn.name)) |base_implems| {
        for (base_implems.items) |impl| {
            if (std.mem.eql(u8, impl.name, "Subscription") or std.mem.eql(u8, impl.name, "IndexSet")) {
                var resolved_impl = impl;
                resolved_impl.signature.retype = try resolveType(impl.signature.retype, version, ctx, allocator);
                try spec_implems.append(resolved_impl);
            }
        }
    }
    // Propagate user trait implementations so specializations inherit user-defined traits
    if (root_ctx.user_trait_impl.get(orgn.name)) |base_user_impls| {
        if (!root_ctx.user_trait_impl.contains(spec_name)) {
            var spec_user_impls = ArrayList([]const u8).init(allocator);
            try spec_user_impls.appendSlice(base_user_impls.items);
            try root_ctx.user_trait_impl.put(spec_name, spec_user_impls);
        }
    }

    try root_ctx.trait_map.put(spec_name, spec_traits);
    try root_ctx.type_implem.put(spec_name, spec_implems);

    return spec_name;
}

// Ensures a specialization exists for a .generic type annotation (e.g. List<Int>).
// Returns the spec name string.
pub fn ensureGenericSpecialization(g: ast.TypeGeneric, ctx: *Context, allocator: Allocator) ![]const u8 {
    if (!ctx.typeDefExist(g.name)) return g.name;
    const orgn = ctx.getTypeDef(g.name);
    var version = std.StringHashMap(Types.Type).init(allocator);
    for (orgn.typeparam.items, 0..) |tp, i| {
        if (i >= g.params.items.len) break;
        try version.put(tp.name, Types.Type{ .decided = g.params.items[i] });
    }
    return createSpecializationFromVersion(orgn, &version, ctx, allocator);
}

pub fn createSpecializedFunctionType(func: *ast.funcDef, spec_parent_name: []const u8, version: *const std.StringHashMap(Types.Type), ctx: *Context, allocator: Allocator) !*ast.Type {
    const functype_val = try allocator.create(ast.Type);
    var argtypes = ArrayList(*ast.Type).init(allocator);
    for (func.arguments.items) |arg|
        try argtypes.append(try resolveType(arg._type, version, ctx, allocator));
    const resolved_ret = try resolveType(func.return_type, version, ctx, allocator);
    const fname = try std.mem.concat(allocator, u8, &[_][]const u8{ spec_parent_name, ".", func.name });
    functype_val.* = ast.Type{
        .base = .{ .function = ast.TypeFunc{
            .argtypes = argtypes,
            .retype = resolved_ret,
            .typeparam = ArrayList(ast.TypeParam).init(allocator),
            .fname = fname,
        } },
        .err = false,
        .references = 0,
    };
    return functype_val;
}

pub fn analyseStructDef(stct: *ast.structDef, ctx: *Context, allocator: Allocator) !void {
    var hab_it = stct.habitants.iterator();
    while (hab_it.next()) |hab| {
        // Skip field type validation for names that are struct type parameters
        const is_type_param = (hab.value_ptr.*.base == .name and
            typeparamContains(stct.typeparam, hab.value_ptr.*.base.name)) or
            (hab.value_ptr.*.base == .buffer and
                hab.value_ptr.*.base.buffer.base == .name and
                typeparamContains(stct.typeparam, hab.value_ptr.*.base.buffer.base.name));
        if (!is_type_param and !ctx.typeExist(hab.value_ptr.*))
            try ctx.Error("The type '{s}' doesn't exist", .{hab.value_ptr.*.toString(allocator)}, stct.reference);
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

    // For generic structs, methods are compiled per specialization, not here
    if (stct.typeparam.items.len > 0) return;

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
    if (prog.instructions.items.len == 0)
        return;

    var funcs_to_compile = ArrayList(functionVersion).init(allocator);

    const string_type_def = try allocator.create(ast.structDef);
    string_type_def.* = .{ .habitants = .init(allocator), .name = "String", .fields = .init(allocator), .reference = Parser.getInbuiltLocation(), .methods = .init(allocator), .typeparam = .init(allocator) };
    try string_type_def.habitants.put("_count", (try Types.CreateTypeInt(allocator, false)).decided);
    try string_type_def.habitants.put("_size", (try Types.CreateTypeInt(allocator, false)).decided);
    try string_type_def.fields.append("_count");
    try string_type_def.fields.append("_size");
    try ctx.addTypeDef("String", string_type_def);

    try Traits.initBasicTraits(ctx, allocator);
    // First pass: register all top-level function definitions
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |func| {
                if (ctx.functionExist(func.name))
                    try ctx.Error("Can't shadow the definition of function '{s}' previously defined here: {s}", .{ inst.FuncDef.name, ctx.getFunction(inst.FuncDef.name).reference.toString() }, inst.FuncDef.reference);
                try ctx.setFunction(func.name, func);
            },
            .TraitDef, .TraitImpl, .StructDef, .ImportDef => {},
        }
    }

    // Derive namespace names from dotted function names (e.g. "math.square" → register "math";
    // "mathw.math.square" → register "mathw" and "mathw.math").
    {
        var fn_it = ctx.functions.iterator();
        while (fn_it.next()) |entry| {
            const name = entry.key_ptr.*;
            var remaining = name;
            var prefix_buf = std.ArrayList(u8).init(allocator);
            defer prefix_buf.deinit();
            while (std.mem.indexOf(u8, remaining, ".")) |dot| {
                const segment = remaining[0..dot];
                if (prefix_buf.items.len > 0) try prefix_buf.append('.');
                try prefix_buf.appendSlice(segment);
                try ctx.addNamespace(try allocator.dupe(u8, prefix_buf.items));
                remaining = remaining[dot + 1 ..];
            }
        }
    }

    const main_func = ctx.getFunction("main");
    try ctx.functions_to_compile.append(functionVersion{
        .signature = (try createFunctionSignature(main_func, allocator)).base.function,
        .name = "main",
        .version = std.hash_map.StringHashMap(Types.Type).init(allocator),
    });

    // Second pass: register struct definitions
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .StructDef => |stct| {
                try analyseStructDef(stct, ctx, allocator);
            },
            .FuncDef, .TraitDef, .TraitImpl, .ImportDef => {},
        }
    }

    // Third pass: register trait definitions
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .TraitDef => |trait| {
                try analyseTraitDef(trait, ctx);
            },
            else => {},
        }
    }

    var code_has_errors = false;

    // Fourth pass: register trait implementations
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .TraitImpl => |impl| {
                analyseTraitImpl(impl, ctx, allocator) catch |err| {
                    if (err == errors.bbcErrors.bbcContextualError) {
                        code_has_errors = true;
                    } else return err;
                };
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
            analyseFunction(func_def, ctx, func.version, allocator) catch |err| {
                if (err == errors.bbcErrors.bbcContextualError) {
                    code_has_errors = true;
                } else {
                    return err;
                }
            };
            try funcs_to_compile.append(func);
        }

        // We can verify that the current version of the function's types matches all the required traits
        verifyFunctionTypeTraits(func_def, func_def.code.ctx, allocator, func) catch |err| {
            if (err == errors.bbcErrors.bbcContextualError) {
                code_has_errors = true;
            } else {
                return err;
            }
        };
    }

    if (code_has_errors) return errors.bbcErrors.bbcContextualError;

    // Transpilation
    ctx.functions_to_compile = funcs_to_compile;

    if (!ctx.variableExist("main"))
        try ctx.Error("Missing function 'main'", .{}, Parser.getInbuiltLocation());

    switch (ctx.getVariable("main").decided.base) {
        .function => |f| {
            if (!f.retype.match((try Types.CreateTypeInt(allocator, true)).decided))
                try ctx.Error("The function 'main' should return !Int (or matching)", .{}, ctx.getFunction("main").reference);
            if (f.argtypes.items.len != 0)
                try ctx.Error("'main' function takes no arguements", .{}, ctx.getFunction("main").reference);
        },
        .name => try ctx.Error("'main' should be a function returning !Int", .{}, Parser.getInbuiltLocation()),
        .buffer => try ctx.Error("'main' should be a function returning !Int", .{}, Parser.getInbuiltLocation()),
        .generic => try ctx.Error("'main' should be a function returning !Int", .{}, Parser.getInbuiltLocation()),
        .import_ns => try ctx.Error("'main' should be a function returning !Int", .{}, Parser.getInbuiltLocation()),
    }
}
