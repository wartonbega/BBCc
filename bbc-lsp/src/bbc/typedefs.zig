const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");
const Analyser = @import("analyser.zig");

const exit = std.process.exit;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const VarHashmap = std.hash_map.StringHashMap(Types.Type);
const TraitHashmap = std.hash_map.StringHashMap(ArrayList(Traits.Trait));
const TypeTraitHashmap = std.hash_map.StringHashMap(ArrayList(Traits.Trait));
const ImplemHashmap = std.hash_map.StringHashMap(ArrayList(funcPair));

// Imported from analyser
const funcPair = Analyser.funcPair;
const Context = Analyser.Context;

const Struct = struct {
    // The order of the member
    // Ex :
    // struct toto {
    //    a: Int,
    //    b: Int,
    // }
    // a will have order 0
    // b --------------- 1
    member_order: std.hash_map.StringHashMap(usize),
    members: ArrayList(*ast.Type),
};

const Enum = struct {
    // enum toto {
    //    a(Int),
    //    b(Bool),
    // }
    options: std.hash_map.StringHashMap(*ast.Type),
    max_size: usize,
    max_size_type: *ast.Type,
};

const typeDef = union(enum) {
    name: []const u8,
    array: *ast.Type,
    _struct: Struct,
    _enum: Enum,
};
