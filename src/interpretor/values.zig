const std = @import("std");
const ast = @import("../ast.zig");
const interpretor = @import("interpretor.zig");
const Parser = @import("../parser.zig");

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

pub const BufferObj = struct {
    content: []?Value,
    buffAllocator: std.mem.Allocator, // The allocator for the buffer
    size: usize,
    references: usize,

    pub fn deleteIfNoRef(self: *BufferObj, allocator: std.mem.Allocator) void {
        if (self.references == 0) {
            for (self.content) |v| {
                if (v) |value_sure|
                    value_sure.decrementReference(allocator);
            }
            self.buffAllocator.free(self.content);
            allocator.destroy(self);
        }
    }

    pub fn setElement(self: *BufferObj, idx: usize, value: Value, allocator: std.mem.Allocator) void {
        value.incrementReference();
        if (self.content[idx]) |v| { // On created with alloc, the memory is set to undefined
            v.decrementReference(allocator);
        }
        self.content[idx] = value;
    }
};

/// A first-class namespace value produced by `import("file") as ns`.
/// Members map short names -> functions or sub-NamespaceObj values.
pub const NamespaceObj = struct {
    name: []const u8,
    members: std.StringHashMap(Value),
    references: usize,

    pub fn deleteIfNoRef(self: *NamespaceObj, allocator: std.mem.Allocator) void {
        if (self.references == 0) {
            var it = self.members.iterator();
            while (it.next()) |m| m.value_ptr.decrementReference(allocator);
            self.members.deinit();
            allocator.free(self.name);
            allocator.destroy(self);
        }
    }

    pub fn getMember(self: *const NamespaceObj, name: []const u8) ?Value {
        return self.members.get(name);
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
        if (self.habitants.fetchPut(name, value) catch null) |old_entry| {
            old_entry.value.decrementReference(allocator);
        }
    }
};

pub const Value = union(enum) {
    Int: i32,
    Float: f64,
    Bool: bool,
    Null: void,
    Char: u8,
    String: *StringObj,
    Buffer: *BufferObj,
    Function: struct {
        func: *ast.funcDef,
        parentObj: ?*Object,
    },
    BuiltinFunction: []const u8,
    Object: *Object,
    Namespace: *NamespaceObj,
    Error: struct {
        reference: Parser.Location,
        message: []const u8,
    },

    pub fn getHabitant(self: *const Value, name: []const u8) Value {
        switch (self.*) {
            .Namespace => |ns| {
                if (ns.getMember(name)) |v| return v;
                return .Null;
            },
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
            .Buffer => |b| {
                if (std.mem.eql(u8, "_count", name))
                    return Value{ .Int = @intCast(b.references) };
                if (std.mem.eql(u8, "_size", name))
                    return Value{ .Int = @intCast(b.content.len) };
            },
            else => unreachable,
        }
        return .Null;
    }

    pub fn decrementReferenceNoCheck(self: *const Value) void {
        switch (self.*) {
            .Object => |o| {
                @constCast(o).references -= 1;
            },
            .Namespace => |ns| {
                @constCast(ns).references -= 1;
            },
            .String => |s| {
                @constCast(s).references -= 1;
            },
            .Buffer => |b| {
                @constCast(b).references -= 1;
            },
            .Function => |f| {
                if (f.parentObj) |p| {
                    var s = Value{ .Object = p };
                    s.decrementReferenceNoCheck();
                }
            },
            .BuiltinFunction => {},
            else => {},
        }
    }

    pub fn decrementReference(self: *const Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Object => |o| {
                @constCast(o).references -= 1;
                o.deleteIfNoRef(allocator);
            },
            .Namespace => |ns| {
                @constCast(ns).references -= 1;
                ns.deleteIfNoRef(allocator);
            },
            .String => |s| {
                @constCast(s).references -= 1;
                s.deleteIfNoRef(allocator);
            },
            .Buffer => |b| {
                @constCast(b).references -= 1;
                b.deleteIfNoRef(allocator);
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
            .Namespace => |ns| {
                @constCast(ns).references += 1;
            },
            .String => |s| {
                @constCast(s).references += 1;
            },
            .Buffer => |b| {
                @constCast(b).references += 1;
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
            .Object => |o| return o.references,
            .Namespace => |ns| return ns.references,
            .String => |s| return s.references,
            .Buffer => |b| return b.references,
            else => return 69,
        }
    }

    pub fn checkReference(self: *const Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Object => |o| {
                o.deleteIfNoRef(allocator);
            },
            .Namespace => |ns| {
                ns.deleteIfNoRef(allocator);
            },
            .String => |s| {
                s.deleteIfNoRef(allocator);
            },
            .Buffer => |b| {
                b.deleteIfNoRef(allocator);
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

    pub fn getType(self: *const Value) []const u8 {
        return switch (self.*) {
            .Int => "Int",
            .Float => "Float",
            .Bool => "Bool",
            .Null => "Null",
            .Char => "Char",
            .String => "String",
            .Function => "Function",
            .BuiltinFunction => "BuiltinFunction",
            .Object => "Object",
            .Buffer => "Buffer",
            .Error => "Error",
            .Namespace => "Namespace",
        };
    }
};

fn floatFromInt(i: i32) f64 {
    return @as(f64, @floatFromInt(i));
}

pub fn Plus(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Int = lhs + rhs },
            .Char => |rhs| Value{ .Int = lhs + @as(i32, rhs) },
            .Bool => |rhs| Value{ .Int = lhs + if (rhs) @as(i32, 1) else @as(i32, 0) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add this type to Int" } },
        },
        .Char => |lhs| switch (other) {
            .Int => |rhs| Value{ .Char = @intCast(@as(i32, lhs) + rhs) },
            .Char => |rhs| Value{ .Char = lhs +% rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add this type to Char" } },
        },
        .String => |lhs| switch (other) {
            .Char => |rhs| blk: {
                var new_content = std.ArrayList(u8).init(ctx.heap);
                new_content.appendSlice(lhs.content.items) catch break :blk Value{ .Error = .{ .reference = reference, .message = "Out of memory" } };
                new_content.append(rhs) catch break :blk Value{ .Error = .{ .reference = reference, .message = "Out of memory" } };
                const new_str = ctx.heap.create(StringObj) catch break :blk Value{ .Error = .{ .reference = reference, .message = "Out of memory" } };
                new_str.* = .{ .content = new_content, .references = 0 };
                break :blk Value{ .String = new_str };
            },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add this type to String" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs + rhs },
            .Int => |rhs| Value{ .Float = lhs + floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot add this type to Float" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use + on this type" } },
    };
}

pub fn Minus(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Int = lhs - rhs },
            .Char => |rhs| Value{ .Int = lhs - @as(i32, rhs) },
            .Bool => |rhs| Value{ .Int = lhs - if (rhs) @as(i32, 1) else @as(i32, 0) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot subtract this type from Int" } },
        },
        .Char => |lhs| switch (other) {
            .Int => |rhs| Value{ .Char = @intCast(@as(i32, lhs) - rhs) },
            .Char => |rhs| Value{ .Char = lhs -% rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot subtract this type from Char" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs - rhs },
            .Int => |rhs| Value{ .Float = lhs - floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot subtract this type from Float" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use - on this type" } },
    };
}

pub fn Times(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Int = lhs * rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot multiply this type with Int" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs * rhs },
            .Int => |rhs| Value{ .Float = lhs * floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot multiply this type with Float" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use * on this type" } },
    };
}

pub fn Div(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| if (rhs != 0) Value{ .Int = @divFloor(lhs, rhs) } else Value{ .Error = .{ .reference = reference, .message = "Cannot divide by zero" } },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot divide Int by this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs / rhs },
            .Int => |rhs| Value{ .Float = lhs / floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot divide Float by this type" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use / on this type" } },
    };
}

pub fn Modulus(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| if (rhs != 0) Value{ .Int = @mod(lhs, rhs) } else Value{ .Error = .{ .reference = reference, .message = "Cannot modulo by zero" } },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot modulo Int by this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = @mod(lhs, rhs) },
            .Int => |rhs| Value{ .Float = @mod(lhs, floatFromInt(rhs)) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot modulo Float by this type" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use % on this type" } },
    };
}

pub fn Lt(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs < rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Int with this type" } },
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs < rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) < rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Char with this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs < rhs },
            .Int => |rhs| Value{ .Bool = lhs < floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Float with this type" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use < on this type" } },
    };
}

pub fn Gt(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs > rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Int with this type" } },
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs > rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) > rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Char with this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs > rhs },
            .Int => |rhs| Value{ .Bool = lhs > floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Float with this type" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use > on this type" } },
    };
}

pub fn Le(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs <= rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Int with this type" } },
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs <= rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) <= rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Char with this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs <= rhs },
            .Int => |rhs| Value{ .Bool = lhs <= floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Float with this type" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use <= on this type" } },
    };
}

pub fn Ge(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs >= rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Int with this type" } },
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs >= rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) >= rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Char with this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs >= rhs },
            .Int => |rhs| Value{ .Bool = lhs >= floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Float with this type" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use >= on this type" } },
    };
}

pub fn Equal(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs == rhs },
            .Null => Value{ .Bool = false },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Int with this type" } },
        },
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs == rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Bool with this type" } },
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs == rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) == rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Char with this type" } },
        },
        .String => |lhs| switch (other) {
            .String => |rhs| Value{ .Bool = std.mem.eql(u8, lhs.content.items, rhs.content.items) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare String with this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs == rhs },
            .Int => |rhs| Value{ .Bool = lhs == floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Float with this type" } },
        },
        .Null => switch (other) {
            .Null => Value{ .Bool = true },
            else => Value{ .Bool = false },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Unsupported == comparison" } },
    };
}

pub fn NotEqual(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs != rhs },
            .Null => Value{ .Bool = true },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Int with this type" } },
        },
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs != rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Bool with this type" } },
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs != rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) != rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Char with this type" } },
        },
        .String => |lhs| switch (other) {
            .String => |rhs| Value{ .Bool = !std.mem.eql(u8, lhs.content.items, rhs.content.items) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare String with this type" } },
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs != rhs },
            .Int => |rhs| Value{ .Bool = lhs != floatFromInt(rhs) },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot compare Float with this type" } },
        },
        .Null => switch (other) {
            .Null => Value{ .Bool = false },
            else => Value{ .Bool = true },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Unsupported != comparison" } },
    };
}

pub fn Or(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs or rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot use 'or' on non-boolean values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use 'or' on non-boolean values" } },
    };
}

pub fn And(self: Value, other: Value, ctx: *Context, reference: Parser.Location) Value {
    _ = ctx;
    return switch (self) {
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs and rhs },
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot use 'and' on non-boolean values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot use 'and' on non-boolean values" } },
    };
}
