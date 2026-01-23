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

    PointAccession,
};

const TraitHashmap = std.hash_map.StringHashMap(ArrayList(Trait));
const TypeTraitHashmap = std.hash_map.StringHashMap(ArrayList(Trait));

pub fn createTraitWithBinOperator(type_implems: *ArrayList(analyser.funcPair), other: *ast.Type, ret: *ast.Type, binop: ast.binOperator, allocator: std.mem.Allocator) !void {
    try type_implems.append(.{
        .name = ast.binOpFuncName(binop),
        .signature = ast.TypeFunc{
            .argtypes = blk: {
                var args = ArrayList(*ast.Type).init(allocator);
                try args.append(other);
                break :blk args;
            },
            .retype = ret,
            .typeparam = ArrayList(ast.TypeParam).init(allocator),
            .fname = stringFromTrait(traitFromOperator(binop)),
        },
    });
}

pub fn createTraitWithBinOperators(type_implems: *ArrayList(analyser.funcPair), other: *ast.Type, ret: *ast.Type, binops: []const ast.binOperator, allocator: std.mem.Allocator) !void {
    for (binops) |op| {
        try createTraitWithBinOperator(
            type_implems,
            other,
            ret,
            op,
            allocator,
        );
    }
}

pub fn initBasicTraits(ctx: *analyser.Context, allocator: std.mem.Allocator) !void {
    // Create the Int type
    const int_type = try Types.CreateTypeInt(allocator, false);
    const int_type_err = try Types.CreateTypeInt(allocator, true);
    const bool_type = try Types.CreateTypeBool(allocator, false);
    const char_type = try Types.CreateTypeChar(allocator, false);
    const string_type = try Types.CreateTypeString(allocator, false);
    const void_type = try Types.CreateTypeVoid(allocator, false);

    // +————————————————————————————————+
    // |   INT TRAITS AND OPERATION     |
    // +————————————————————————————————+

    var int_traits = ArrayList(Trait).init(allocator);
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

    var int_implems = ArrayList(analyser.funcPair).init(allocator);

    // Implementing int + int -> int
    try createTraitWithBinOperators(
        &int_implems,
        int_type.decided,
        int_type.decided,
        &[_]ast.binOperator{ .Plus, .Minus, .Times, .Modulus },
        allocator,
    );

    // Implementing int / int -> !int
    try createTraitWithBinOperator(
        &int_implems,
        int_type.decided,
        int_type_err.decided,
        .Div,
        allocator,
    );

    // int + bool -> int
    try createTraitWithBinOperators(
        &int_implems,
        bool_type.decided,
        int_type.decided,
        &[_]ast.binOperator{ .Plus, .Minus },
        allocator,
    );

    // int + char -> int
    try createTraitWithBinOperators(
        &int_implems,
        char_type.decided,
        int_type.decided,
        &[_]ast.binOperator{ .Plus, .Minus },
        allocator,
    );

    // int == int -> bool
    try createTraitWithBinOperators(
        &int_implems,
        int_type.decided,
        bool_type.decided,
        &[_]ast.binOperator{ .Equal, .NotEqual, .Ge, .Le, .Gt, .Lt },
        allocator,
    );

    // int == char -> bool
    try createTraitWithBinOperators(
        &int_implems,
        char_type.decided,
        bool_type.decided,
        &[_]ast.binOperator{ .Equal, .NotEqual, .Ge, .Le, .Gt, .Lt },
        allocator,
    );

    // int == void -> bool
    try createTraitWithBinOperators(
        &int_implems,
        void_type.decided,
        bool_type.decided,
        &[_]ast.binOperator{ .Equal, .NotEqual },
        allocator,
    );

    try ctx.type_implem.put(
        "Int",
        int_implems,
    );

    // +—————————————————————————————————+
    // |   CHAR TRAITS AND OPERATION     |
    // +—————————————————————————————————+
    var char_traits = ArrayList(Trait).init(allocator);
    try char_traits.append(Trait.Add);
    try char_traits.append(Trait.Sub);
    try char_traits.append(Trait.Eq);
    try char_traits.append(Trait.GrEq);
    try char_traits.append(Trait.LeEq);
    try char_traits.append(Trait.Le);
    try char_traits.append(Trait.Gr);

    try ctx.trait_map.put("Char", char_traits);

    var char_implems = ArrayList(analyser.funcPair).init(allocator);

    // Char + char -> char
    try createTraitWithBinOperators(
        &char_implems,
        char_type.decided,
        char_type.decided,
        &[_]ast.binOperator{ .Plus, .Minus },
        allocator,
    );

    // Char + int -> char
    try createTraitWithBinOperators(
        &char_implems,
        int_type.decided,
        char_type.decided,
        &[_]ast.binOperator{ .Plus, .Minus },
        allocator,
    );

    // char == char -> bool
    try createTraitWithBinOperators(
        &char_implems,
        char_type.decided,
        bool_type.decided,
        &[_]ast.binOperator{ .Equal, .NotEqual, .Ge, .Le, .Gt, .Lt },
        allocator,
    );

    // char == int -> bool
    try createTraitWithBinOperators(
        &char_implems,
        int_type.decided,
        bool_type.decided,
        &[_]ast.binOperator{ .Equal, .NotEqual, .Ge, .Le, .Gt, .Lt },
        allocator,
    );

    try ctx.type_implem.put(
        "Char",
        char_implems,
    );

    // +———————————————————————————————————+
    // |   STRING TRAITS AND OPERATION     |
    // +———————————————————————————————————+
    var string_traits = ArrayList(Trait).init(allocator);
    try string_traits.append(Trait.Add);
    try string_traits.append(Trait.Eq);

    try ctx.trait_map.put("String", string_traits);

    var string_implems = ArrayList(analyser.funcPair).init(allocator);

    // String + char -> String
    try createTraitWithBinOperators(
        &string_implems,
        char_type.decided,
        string_type.decided,
        &[_]ast.binOperator{.Plus},
        allocator,
    );

    // string == string -> bool
    try createTraitWithBinOperators(
        &string_implems,
        string_type.decided,
        bool_type.decided,
        &[_]ast.binOperator{ .Equal, .NotEqual },
        allocator,
    );

    try ctx.type_implem.put(
        "String",
        string_implems,
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
        .PointAccession => return "PointAccession",
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
        .PointAccession => return try Types.CreateTypeInt(alloc, false),
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
        .nullLit => {},
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
                        .function => errors.bbcErrorExit(
                            "'function' like type (here {s}) don't support operations",
                            .{_type.toString(allocator)},
                            binop.reference,
                        ),
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
            try traitUnion(&ret, &try getTypeTraits(errcheck.scope, ctx, allocator));
        },
        .While => |whileloop| {
            try traitUnion(&ret, &try getTypeTraits(whileloop.condition, ctx, allocator));
            try traitUnion(&ret, &try getTypeTraits(whileloop.exec, ctx, allocator));
        },

        .structInit => |stc_init| {
            var stc_hab_it = stc_init.habitants.iterator();
            while (stc_hab_it.next()) |hab| {
                try traitUnion(&ret, &try getTypeTraits(hab.value_ptr.*, ctx, allocator));
            }
        },
        .unaryOperatorRight => |uop_right| {
            try traitUnion(&ret, &try getTypeTraits(uop_right.expr, ctx, allocator));
            const expr_type = try Types.getTypeOfValue(uop_right.expr, ctx, allocator);
            switch (expr_type) {
                .decided => |_type| {
                    switch (_type.base) {
                        .function => errors.bbcErrorExit(
                            "'function' like type (here {s}) don't support point attribute",
                            .{_type.toString(allocator)},
                            uop_right.reference,
                        ),
                        .name => |n| {
                            if (!ret.contains(n))
                                try ret.put(n, ArrayList(Trait).init(allocator));
                            try ret.getPtr(n).?.append(.PointAccession);
                        },
                    }
                },
                .undecided => {},
            }
        },
        .freeKeyword => |freekwd| {
            try traitUnion(&ret, &try getTypeTraits(freekwd.val, ctx, allocator));
        },
        .Print => |print| {
            for (print.args.items) |arg| {
                try traitUnion(&ret, &try getTypeTraits(arg, ctx, allocator));
            }
        },
        .function => |func| {
            try traitUnion(&ret, &try getTypeTraits(&ast.Value{ .scope = func.code }, ctx, allocator));
        },
        else => {
            std.debug.print("Unimplemented {}", .{instruction});
            unreachable;
        },
    }
    return ret;
}
