const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const analyser = @import("analyser.zig");

const ArrayList = std.ArrayList;

// All the traits
pub const Trait = enum {
    Eq,
    Add,
    Display,
};

const TraitHashmap = std.hash_map.StringHashMap(ArrayList(Trait));
const TypeTraitHashmap = std.hash_map.StringHashMap(ArrayList(Trait));

pub fn initBasicTraits(ctx: *analyser.Context, allocator: std.mem.Allocator) !void {
    // Create the Int type
    var int_traits = ArrayList(Trait).init(allocator);
    try int_traits.append(Trait.Add);
    try int_traits.append(Trait.Eq);

    try ctx.trait_map.put("Int", int_traits);

    var int_implems = ArrayList(analyser.funcPair).init(allocator);
    try int_implems.append(.{
        .name = ast.binOpFuncName(.Plus),
        .signature = ast.TypeFunc{
            .argtypes = blk: {
                var args = ArrayList(*ast.Type).init(allocator);
                try args.append((try Types.CreateTypeInt(allocator, false)).decided);
                break :blk args;
            },
            .retype = (try Types.CreateTypeInt(allocator, false)).decided,
        },
    });
    try int_implems.append(.{
        .name = ast.binOpFuncName(.Equal),
        .signature = ast.TypeFunc{
            .argtypes = blk: {
                var args = ArrayList(*ast.Type).init(allocator);
                try args.append((try Types.CreateTypeInt(allocator, false)).decided);
                break :blk args;
            },
            .retype = (try Types.CreateTypeInt(allocator, false)).decided,
        },
    });
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
        else => unreachable,
    }
}
