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
    And,
    Or,

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

const inbuilt_config = @embedFile("inbuilt_types.config");

pub fn initBasicTraits(ctx: *analyser.Context, allocator: std.mem.Allocator) !void {
    var type_cache = std.StringHashMap(*ast.Type).init(allocator);

    var current_type: ?[]const u8 = null;

    var lines = std.mem.splitScalar(u8, inbuilt_config, '\n');
    while (lines.next()) |raw_line| {
        const line = std.mem.trim(u8, raw_line, " \t\r");
        if (line.len == 0 or line[0] == '#') continue;

        // Type header: "TypeName:" (no spaces)
        if (std.mem.endsWith(u8, line, ":") and std.mem.indexOf(u8, line, " ") == null) {
            current_type = line[0 .. line.len - 1];
            if (!ctx.trait_map.contains(current_type.?)) {
                try ctx.trait_map.put(current_type.?, ArrayList(Trait).init(allocator));
                try ctx.type_implem.put(current_type.?, ArrayList(analyser.funcPair).init(allocator));
            }
            continue;
        }

        if (current_type == null) continue;

        // "op1 op2 ... : RhsType1 RhsType2 ... -> ReturnType"
        const colon_pos = std.mem.indexOf(u8, line, " : ") orelse continue;
        const ops_str = std.mem.trim(u8, line[0..colon_pos], " \t");
        const after_colon = std.mem.trim(u8, line[colon_pos + 3 ..], " \t");

        const arrow_pos = std.mem.indexOf(u8, after_colon, "->") orelse continue;
        const rhs_str = std.mem.trim(u8, after_colon[0..arrow_pos], " \t");
        const ret_str = std.mem.trim(u8, after_colon[arrow_pos + 2 ..], " \t");

        const is_err = ret_str.len > 0 and ret_str[0] == '!';
        const ret_name = if (is_err) ret_str[1..] else ret_str;
        const ret_type = try configGetOrCreateType(&type_cache, ret_name, is_err, allocator);

        var ops = ArrayList(ast.binOperator).init(allocator);
        defer ops.deinit();
        var ops_iter = std.mem.splitScalar(u8, ops_str, ' ');
        while (ops_iter.next()) |tok| {
            const trimmed = std.mem.trim(u8, tok, " \t");
            if (trimmed.len == 0) continue;
            const op = configStringToOperator(trimmed) orelse continue;
            try ops.append(op);
            const trait = traitFromOperator(op);
            const t_traits = ctx.trait_map.getPtr(current_type.?).?;
            var already = false;
            for (t_traits.items) |tr| {
                if (tr == trait) { already = true; break; }
            }
            if (!already) try t_traits.append(trait);
        }

        var rhs_iter = std.mem.splitScalar(u8, rhs_str, ' ');
        while (rhs_iter.next()) |tok| {
            const trimmed = std.mem.trim(u8, tok, " \t");
            if (trimmed.len == 0) continue;
            const rhs_type = try configGetOrCreateType(&type_cache, trimmed, false, allocator);
            const t_implems = ctx.type_implem.getPtr(current_type.?).?;
            for (ops.items) |op| {
                try createTraitWithBinOperator(t_implems, rhs_type, ret_type, op, allocator);
            }
        }
    }
}

fn configGetOrCreateType(cache: *std.StringHashMap(*ast.Type), name: []const u8, err: bool, allocator: std.mem.Allocator) !*ast.Type {
    var key_buf: [128]u8 = undefined;
    const key: []const u8 = if (err) try std.fmt.bufPrint(&key_buf, "!{s}", .{name}) else name;
    if (cache.get(key)) |existing| return existing;
    const t = try allocator.create(ast.Type);
    t.* = .{ .base = .{ .name = name }, .err = err, .references = 0 };
    try cache.put(try allocator.dupe(u8, key), t);
    return t;
}

fn configStringToOperator(s: []const u8) ?ast.binOperator {
    if (std.mem.eql(u8, s, "+")) return .Plus;
    if (std.mem.eql(u8, s, "-")) return .Minus;
    if (std.mem.eql(u8, s, "*")) return .Times;
    if (std.mem.eql(u8, s, "/")) return .Div;
    if (std.mem.eql(u8, s, "%")) return .Modulus;
    if (std.mem.eql(u8, s, "==")) return .Equal;
    if (std.mem.eql(u8, s, "!=")) return .NotEqual;
    if (std.mem.eql(u8, s, ">")) return .Gt;
    if (std.mem.eql(u8, s, "<")) return .Lt;
    if (std.mem.eql(u8, s, ">=")) return .Ge;
    if (std.mem.eql(u8, s, "<=")) return .Le;
    if (std.mem.eql(u8, s, "and") or std.mem.eql(u8, s, "&&")) return .And;
    if (std.mem.eql(u8, s, "or") or std.mem.eql(u8, s, "||")) return .Or;
    return null;
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
    return switch (op) {
        .Plus => .Add,
        .Minus => .Sub,
        .Times => .Mult,
        .Div => .Div,
        .Modulus => .Mod,
        .Equal, .NotEqual => .Eq,
        .Ge => .GrEq,
        .Le => .LeEq,
        .Lt => .Le,
        .Gt => .Gr,
        .And => .And,
        .Or => .Or,
    };
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
    if (std.mem.eql(u8, t, "And")) return Trait.And;
    if (std.mem.eql(u8, t, "Or")) return Trait.Or;
    if (std.mem.eql(u8, t, "Display")) return Trait.Display;
    std.debug.panic("Unknown trait string: {s}", .{t});
}

pub fn stringFromTrait(t: Trait) []const u8 {
    return switch (t) {
        .Add => "Add",
        .Sub => "Sub",
        .Mult => "Mult",
        .Div => "Div",
        .Mod => "Mod",
        .Eq => "Eq",
        .GrEq => "GrEq",
        .LeEq => "LeEq",
        .Le => "Le",
        .Gr => "Gr",
        .And => "And",
        .Or => "Or",
        .Display => "Display",
        .PointAccession => "PointAccession",
    };
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
    return switch (t) {
        .Add, .Sub, .Mult, .Div, .Mod, .And, .Or => param,
        .Eq, .Gr, .GrEq, .Le, .LeEq => try Types.CreateTypeBool(alloc, false),
        .Display => try Types.CreateTypeString(alloc, false),
        .PointAccession => try Types.CreateTypeInt(alloc, false),
    };
}

pub fn isBuiltinTraitName(name: []const u8) bool {
    const builtin = [_][]const u8{ "Add", "Sub", "Mult", "Div", "Mod", "Eq", "GrEq", "LeEq", "Le", "Gr", "And", "Or", "Display" };
    for (builtin) |n| {
        if (std.mem.eql(u8, n, name)) return true;
    }
    return false;
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
        .floatLit => {},
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
                        .function => try ctx.Error(
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
                        .function => try ctx.Error(
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
