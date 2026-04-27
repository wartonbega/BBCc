const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("print.zig");
const Values = @import("values.zig");
const Itpr = @import("interpretor.zig");
const Parser = @import("../parser.zig");

const Context = Itpr.Context;
const Value = Values.Value;

pub const Object = struct {
    name: []const u8,
    habitants: std.hash_map.StringHashMap(Value),
    references: usize,

    pub fn deleteIfNoRef(self: *Object, allocator: std.mem.Allocator) void {
        if (self.references == 0) {
            var it = self.habitants.iterator();
            while (it.next()) |hab| {
                hab.value_ptr.decrementReference(allocator);
            }
            self.habitants.deinit();
            allocator.destroy(self);
        }
    }

    pub fn setHabitant(self: *Object, name: []const u8, value: Value, allocator: std.mem.Allocator) void {
        value.incrementReference();
        if (self.habitants.fetchPut(name, value) catch null) |old_entry| {
            old_entry.value.decrementReference(allocator);
        }
    }

    pub fn getHabitant(self: *Object, name: []const u8) Value {
        if (std.mem.eql(u8, "_count", name))
            return Value{ .Int = @intCast(self.references) };
        if (std.mem.eql(u8, "_size", name))
            return Value{ .Int = @intCast(self.habitants.count()) };

        if (self.habitants.get(name)) |val| {
            // Si c'est une fonction et qu'elle n'a pas de parent,
            // c'est une méthode de cet objet : on fait le lien (binding) maintenant.
            if (val == .Function and val.Function.parentObj == null) {
                const ret = Value{
                    .Function = .{
                        .func = val.Function.func,
                        .parentObj = self, // On attache l'objet "self" dynamiquement
                    },
                };
                ret.incrementReference();
                return ret;
            }
            return val;
        }
        return Value{ .Null = {} };
    }
};
