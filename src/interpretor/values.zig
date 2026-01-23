const std = @import("std");
const ast = @import("../ast.zig");
const interpretor = @import("interpretor.zig");

const Context = interpretor.Context;

pub const StringObj = struct {
    content: std.ArrayList(u8),
    references: usize,

    pub fn deleteIfNoRef(self: *StringObj, allocator: std.mem.Allocator) void {
        if (self.references == 0) {
            self.content.deinit();
            allocator.destroy(self);
        }
    }
};

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
        if (self.habitants.get(name) != null) {
            self.habitants.get(name).?.decrementReference(allocator);
        }
        self.habitants.put(name, value) catch {};
    }
};

pub const Value = union(enum) {
    Int: i32,
    Bool: bool,
    Null: void,
    Char: u8,
    String: *StringObj,
    Function: struct {
        func: *ast.funcDef,
        parentObj: ?*Object,
    },
    Object: *Object,
    Error: struct {
        reference: []const u8,
        message: []const u8,
    },

    pub fn getHabitant(self: *const Value, name: []const u8) Value {
        switch (self.*) {
            .Object => |o| {
                if (std.mem.eql(u8, "_count", name))
                    return Value{ .Int = @intCast(o.references) };
                if (std.mem.eql(u8, "_size", name))
                    return Value{ .Int = @intCast(o.habitants.count()) };

                if (o.habitants.get(name)) |val| {
                    // Si c'est une fonction et qu'elle n'a pas de parent,
                    // c'est une méthode de cet objet : on fait le lien (binding) maintenant.
                    if (val == .Function and val.Function.parentObj == null) {
                        const ret = Value{
                            .Function = .{
                                .func = val.Function.func,
                                .parentObj = o, // On attache l'objet "self" dynamiquement
                            },
                        };
                        ret.incrementReference();
                        return ret;
                    }
                    return val;
                }
                return o.habitants.get(name).?;
            },
            .String => |s| {
                if (std.mem.eql(u8, "_count", name))
                    return Value{ .Int = @intCast(s.references) };
                if (std.mem.eql(u8, "_size", name))
                    return Value{ .Int = @intCast(s.content.items.len) };
            },
            else => unreachable,
        }
        return .Null;
    }

    pub fn decrementReference(self: *const Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Object => |o| {
                @constCast(o).references -= 1;
                o.deleteIfNoRef(allocator);
            },
            .String => |s| {
                @constCast(s).references -= 1;
                s.deleteIfNoRef(allocator);
            },
            .Function => |f| {
                if (f.parentObj) |p| {
                    var s = Value{ .Object = p };
                    s.decrementReference(allocator);
                }
            },
            else => {},
        }
    }

    pub fn incrementReference(self: *const Value) void {
        switch (self.*) {
            .Object => |o| {
                @constCast(o).references += 1;
            },
            .String => |s| {
                @constCast(s).references += 1;
            },
            .Function => |f| {
                if (f.parentObj) |p| {
                    var s = Value{ .Object = p };
                    s.incrementReference();
                }
            },
            else => {},
        }
    }

    pub fn getReference(self: *const Value) usize {
        switch (self.*) {
            .Object => |o| {
                return o.references;
            },
            .String => |s| {
                return s.references;
            },
            else => {
                return 69;
            },
        }
    }

    pub fn checkReference(self: *const Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Object => |o| {
                o.deleteIfNoRef(allocator);
            },
            .String => |s| {
                s.deleteIfNoRef(allocator);
            },
            .Function => |f| {
                if (f.parentObj) |p| {
                    var s = Value{ .Object = p };
                    s.checkReference(allocator);
                }
            },
            else => {},
        }
    }
};

pub fn Plus(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Int = self.Int + other.Int },
            .Char => Value{ .Int = self.Int + other.Char },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add non-integer values" } },
        },
        .Char => switch (other) {
            .Int => Value{ .Char = @intCast(self.Char + other.Int) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot add non-integer values" } },
    };
}

pub fn Minus(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Int = self.Int - other.Int },
            .Char => Value{ .Int = self.Int - other.Char },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add non-integer values" } },
        },
        .Char => switch (other) {
            .Int => Value{ .Char = @intCast(self.Char - other.Int) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot add non-integer values" } },
    };
}

pub fn Times(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Int = self.Int * other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot multiply non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot multiply non-integer values" } },
    };
}

pub fn Div(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => if (other.Int != 0) Value{ .Int = @divFloor(self.Int, other.Int) } else Value{ .Error = .{ .reference = reference, .message = "Cannot divide by zero" } },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot divide non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot divide non-integer values" } },
    };
}

pub fn Modulus(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => if (other.Int != 0) Value{ .Int = @mod(self.Int, other.Int) } else Value{ .Error = .{ .reference = reference, .message = "Cannot modulo by zero" } },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot modulo non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot modulo non-integer values" } },
    };
}

pub fn Lt(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Bool = self.Int < other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
    };
}

pub fn Gt(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Bool = self.Int > other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
    };
}

pub fn Le(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Bool = self.Int <= other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
    };
}

pub fn Ge(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Bool = self.Int >= other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
    };
}

pub fn Equal(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Bool = self.Int == other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
        },
        .Bool => switch (other) {
            .Bool => Value{ .Bool = self.Bool == other.Bool },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-boolean values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Unsupported comparison" } },
    };
}

pub fn NotEqual(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Bool = self.Int != other.Int },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-integer values" } },
        },
        .Bool => switch (other) {
            .Bool => Value{ .Bool = self.Bool != other.Bool },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare non-boolean values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Unsupported comparison" } },
    };
}

pub fn Or(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Bool => switch (other) {
            .Bool => Value{ .Bool = self.Bool or other.Bool },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot perform logical OR on non-boolean values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot perform logical OR on non-boolean values" } },
    };
}

pub fn And(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Bool => switch (other) {
            .Bool => Value{ .Bool = self.Bool and other.Bool },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot perform logical AND on non-boolean values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot perform logical AND on non-boolean values" } },
    };
}
