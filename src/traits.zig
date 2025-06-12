const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const analyser = @import("analyser.zig");

const ArrayList = std.ArrayList;

// All the traits
pub const Trait = enum {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Eq,
    GrEq,
    LeEq,
    Le,
    Gr,

    Display,
};

const TraitHashmap = std.hash_map.StringHashMap(ArrayList(Trait));
const TypeTraitHashmap = std.hash_map.StringHashMap(ArrayList(Trait));

pub fn initBasicTraits(ctx: *analyser.Context, allocator: std.mem.Allocator) !void {
    // Create the Int type
    var int_traits = ArrayList(Trait).init(allocator);
    const int_type = try Types.CreateTypeInt(allocator, false);
    const bool_type = try Types.CreateTypeBool(allocator, false);
    try int_traits.append(Trait.Add);
    try int_traits.append(Trait.Sub);
    try int_traits.append(Trait.Mult);
    try int_traits.append(Trait.Div);
    try int_traits.append(Trait.Mod);
    try int_traits.append(Trait.Eq);
    try int_traits.append(Trait.GrEq);
    try int_traits.append(Trait.LeEq);
    try int_traits.append(Trait.Le);
    try int_traits.append(Trait.Gr);

    try ctx.trait_map.put("Int", int_traits);

    // Implementing int + int -> int
    var int_implems = ArrayList(analyser.funcPair).init(allocator);
    for ([_]ast.binOperator{ .Plus, .Minus, .Times, .Modulus }) |op| {
        try int_implems.append(.{
            .name = ast.binOpFuncName(op),
            .signature = ast.TypeFunc{
                .argtypes = blk: {
                    var args = ArrayList(*ast.Type).init(allocator);
                    try args.append(int_type.decided);
                    break :blk args;
                },
                .retype = int_type.decided,
                .typeparam = ArrayList(ast.TypeParam).init(allocator),
                .fname = stringFromTrait(traitFromOperator(op)),
            },
        });
    }

    // Implementing int / int -> !int
    try int_implems.append(.{
        .name = ast.binOpFuncName(.Div),
        .signature = ast.TypeFunc{
            .argtypes = blk: {
                var args = ArrayList(*ast.Type).init(allocator);
                try args.append(int_type.decided);
                break :blk args;
            },
            .retype = (try Types.CreateTypeInt(allocator, true)).decided,
            .typeparam = ArrayList(ast.TypeParam).init(allocator),
            .fname = stringFromTrait(traitFromOperator(.Div)),
        },
    });

    // int + bool -> int
    for ([_]ast.binOperator{ .Plus, .Minus, .Times }) |op| {
        try int_implems.append(.{
            .name = ast.binOpFuncName(op),
            .signature = ast.TypeFunc{
                .argtypes = blk: {
                    var args = ArrayList(*ast.Type).init(allocator);
                    try args.append(bool_type.decided);
                    break :blk args;
                },
                .retype = int_type.decided,
                .typeparam = ArrayList(ast.TypeParam).init(allocator),
                .fname = stringFromTrait(traitFromOperator(op)),
            },
        });
    }

    // int == int -> bool
    for ([_]ast.binOperator{ .Equal, .NotEqual, .Ge, .Le, .Gt, .Lt }) |op| {
        try int_implems.append(.{
            .name = ast.binOpFuncName(op),
            .signature = ast.TypeFunc{
                .argtypes = blk: {
                    var args = ArrayList(*ast.Type).init(allocator);
                    try args.append(int_type.decided);
                    break :blk args;
                },
                .retype = bool_type.decided,
                .typeparam = ArrayList(ast.TypeParam).init(allocator),
                .fname = stringFromTrait(traitFromOperator(op)),
            },
        });
    }

    try ctx.type_implem.put(
        "Int",
        int_implems,
    );
}

pub fn typeMatchTrait(trait_map: *TraitHashmap, typealiases: *TypeTraitHashmap, typ: Types.Type, trait: Trait) bool {
    // returns wether a type matches a trait
    switch (typ) {
        .decided => |t| {
            switch (t.base) { // if there is a base that is aliased and the alias matches the requiered traits
                .name => |name| if (typealiases.contains(name)) {
                    for (typealiases.get(name).?.items) |aliased_trait| {
                        if (trait == aliased_trait)
                            return true;
                    }
                } else {
                    if (trait_map.get(name)) |trait_list| {
                        for (trait_list.items) |non_aliased_trait| {
                            if (non_aliased_trait == trait) return true;
                        }
                    }
                },
                // function has no traits
                else => return false,
            }
        },
        .undecided => |traits| {
            // If undecided but one of the traits matches
            for (traits.items) |t| {
                if (trait == t)
                    return true;
            }
            // if there's no match, then we can add it to the type's traits list
            (@constCast(&traits)).append(trait) catch {};
            return true;
        },
    }

    return false;
}

pub fn typeMatchTraits(trait_map: *TraitHashmap, typealiases: *TypeTraitHashmap, typ: Types.Type, traits: ArrayList(Trait)) bool {
    // returns wether a type matches some traits,
    var counter: i32 = @intCast(0);
    if (traits.items.len == 0)
        return true;
    for (traits.items) |trait| {
        if (typeMatchTrait(trait_map, typealiases, typ, trait))
            counter += 1;
        if (counter == traits.items.len)
            return true;
    }
    return false;
}

pub fn traitFromOperator(op: ast.binOperator) Trait {
    switch (op) {
        .Plus => return Trait.Add,
        .Equal => return Trait.Eq,
        .Minus => return Trait.Sub,
        .Times => return Trait.Mult,
        .Div => return Trait.Div,
        .Modulus => return Trait.Mod,
        .NotEqual => return Trait.Eq,
        .Ge => return Trait.GrEq,
        .Le => return Trait.LeEq,
        .Lt => return Trait.Le,
        .Gt => return Trait.Gr,
        else => unreachable,
    }
}

pub fn traitFromString(t: []const u8) Trait {
    if (std.mem.eql(u8, t, "Add")) return Trait.Add;
    if (std.mem.eql(u8, t, "Sub")) return Trait.Sub;
    if (std.mem.eql(u8, t, "Mult")) return Trait.Mult;
    if (std.mem.eql(u8, t, "Div")) return Trait.Div;
    if (std.mem.eql(u8, t, "Mod")) return Trait.Mod;
    if (std.mem.eql(u8, t, "Eq")) return Trait.Eq;
    if (std.mem.eql(u8, t, "GrEq")) return Trait.GrEq;
    if (std.mem.eql(u8, t, "LeEq")) return Trait.LeEq;
    if (std.mem.eql(u8, t, "Le")) return Trait.Le;
    if (std.mem.eql(u8, t, "Gr")) return Trait.Gr;
    if (std.mem.eql(u8, t, "Display")) return Trait.Display;
    std.debug.panic("Unknown trait string: {s}", .{t});
}

pub fn stringFromTrait(t: Trait) []const u8 {
    switch (t) {
        .Add => return "Add",
        .Sub => return "Sub",
        .Mult => return "Mult",
        .Div => return "Div",
        .Mod => return "Mod",
        .Eq => return "Eq",
        .GrEq => return "GrEq",
        .LeEq => return "LeEq",
        .Le => return "Le",
        .Gr => return "Gr",
        .Display => return "Display",
    }
}

pub fn traitsFromStrings(t: ArrayList([]const u8), allocator: std.mem.Allocator) ArrayList(Trait) {
    var traits = ArrayList(Trait).init(allocator);
    for (t.items) |trait_str| {
        traits.append(traitFromString(trait_str)) catch {};
    }
    return traits;
}

pub fn defaultReturnType(t: Trait, param: Types.Type, alloc: std.mem.Allocator) !Types.Type {
    // Return the default return type of a trait, used on type 'param'
    // for example (here, '.' representes any type), with type T, (T + .) should return T,
    // but (T == .) should return Bool
    switch (t) {
        .Add, .Sub, .Mult, .Div, .Mod => return param,
        .Eq, .Gr, .GrEq, .Le, .LeEq => return try Types.CreateTypeBool(alloc, false),
        .Display => return try Types.CreateTypeString(alloc, false),
    }
}

pub fn traitListToString(traits: ArrayList(Trait), allocator: std.mem.Allocator) ![]const u8 {
    // Considering traits won't have a name greater than 10
    var buffer = try allocator.alloc(u8, traits.items.len * 10);
    var i: usize = @intCast(0);
    for (traits.items) |t| {
        for (stringFromTrait(t)) |c| {
            buffer[i] = c;
            i += 1;
        }
        buffer[i] = ',';
        buffer[i + 1] = ' ';
        i += 2;
    }
    return buffer[0..i];
}

pub fn traitUnion(map1: *std.hash_map.StringHashMap(ArrayList(Trait)), map2: *const std.hash_map.StringHashMap(ArrayList(Trait))) !void {
    const allocator = map1.allocator;
    var it = map2.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        const traits = entry.value_ptr.*;

        if (map1.getPtr(key)) |list| {
            try list.appendSlice(traits.items);
        } else {
            var new_list = ArrayList(Trait).init(allocator);
            try new_list.appendSlice(traits.items);
            try map1.put(key, new_list);
        }
    }
}

pub fn getTypeTraits(instruction: *const ast.Value, ctx: *analyser.Context, allocator: std.mem.Allocator) !std.hash_map.StringHashMap(ArrayList(Trait)) {
    // Return all the traits associated with each types in the scope
    var ret = std.hash_map.StringHashMap(ArrayList(Trait)).init(allocator);
    switch (instruction.*) {
        .charLit => {},
        .intLit => {},
        .stringLit => {},
        .identifier => {},
        .varDec => {},
        .boolLit => {},
        .assignement => |inst| {
            try traitUnion(&ret, &try getTypeTraits(inst.lhs, ctx, allocator));
            try traitUnion(&ret, &try getTypeTraits(inst.rhs, ctx, allocator));
        },
        .funcall => |inst| {
            try traitUnion(&ret, &try getTypeTraits(inst.func, ctx, allocator));
            for (inst.args.items) |arg| {
                try traitUnion(&ret, &try getTypeTraits(arg, ctx, allocator));
            }
        },
        .parenthesis => |inst| {
            try traitUnion(&ret, &try getTypeTraits(inst, ctx, allocator));
        },
        .scope => |inst| {
            for (inst.code.items) |val| {
                try traitUnion(&ret, &try getTypeTraits(val, ctx, allocator));
            }
        },
        .binaryOperator => |binop| {
            try traitUnion(&ret, &try getTypeTraits(binop.lhs, ctx, allocator));
            try traitUnion(&ret, &try getTypeTraits(binop.rhs, ctx, allocator));
            const lhs_type = try Types.getTypeOfValue(binop.lhs, ctx, allocator);
            switch (lhs_type) {
                .decided => |_type| {
                    switch (_type.base) {
                        .function => errors.bbcErrorExit("'function' like type (here {s}) don't support operations", .{_type.toString(allocator)}, ""),
                        .name => |n| {
                            if (!ret.contains(n))
                                try ret.put(n, ArrayList(Trait).init(allocator));
                            try ret.getPtr(n).?.append(traitFromOperator(binop.operator));
                        },
                    }
                },
                .undecided => {},
            }
        },
        .If => |ifstmt| {
            for (ifstmt.conditions.items, ifstmt.scopes.items) |cond, scope| {
                try traitUnion(&ret, &try getTypeTraits(cond, ctx, allocator));
                try traitUnion(&ret, &try getTypeTraits(scope, ctx, allocator));
            }
            if (ifstmt.elsescope) |else_scope| {
                try traitUnion(&ret, &try getTypeTraits(else_scope, ctx, allocator));
            }
        },
        .errorCheck => |errcheck| {
            try traitUnion(&ret, &try getTypeTraits(errcheck.value, ctx, allocator));
            for (errcheck.scope.code.items) |val| {
                try traitUnion(&ret, &try getTypeTraits(val, ctx, allocator));
            }
        },
        .While => |whileloop| {
            try traitUnion(&ret, &try getTypeTraits(whileloop.condition, ctx, allocator));
            try traitUnion(&ret, &try getTypeTraits(whileloop.exec, ctx, allocator));
        },
        else => {
            std.debug.print("Unimplemented {}", .{instruction});
            unreachable;
        },
    }
    return ret;
}
