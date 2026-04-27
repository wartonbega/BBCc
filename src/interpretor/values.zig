const std = @import("std");
const ast = @import("../ast.zig");
const interpretor = @import("interpretor.zig");
const Parser = @import("../parser.zig");

const Objects = @import("objects.zig");

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

pub const ErrorObj = struct {
    message: []u8,
    reference: Parser.Location,
    references: usize,

    pub fn deleteIfNoRef(self: *ErrorObj, allocator: std.mem.Allocator) void {
        if (self.references == 0) {
            allocator.free(self.message);
            allocator.destroy(self);
        }
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

pub const Object = Objects.Object;

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
    Error: *ErrorObj,

    pub fn getHabitant(self: *const Value, name: []const u8) Value {
        switch (self.*) {
            .Namespace => |ns| {
                if (ns.getMember(name)) |v| return v;
                return .Null;
            },
            .Object => |o| {
                return o.getHabitant(name);
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
            .Error => |e| {
                @constCast(e).references -= 1;
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
            .Error => |e| {
                @constCast(e).references -= 1;
                e.deleteIfNoRef(allocator);
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
            .Error => |e| {
                @constCast(e).references += 1;
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
            .Error => |e| {
                e.deleteIfNoRef(allocator);
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

pub fn makeError(alloc: std.mem.Allocator, reference: Parser.Location, comptime fmt: []const u8, args: anytype) !Value {
    const message = try std.fmt.allocPrint(alloc, fmt, args);
    const obj = try alloc.create(ErrorObj);
    obj.* = .{ .message = message, .reference = reference, .references = 0 };
    return Value{ .Error = obj };
}

pub fn Plus(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Int = lhs + rhs },
            .Char => |rhs| Value{ .Int = lhs + @as(i32, rhs) },
            .Bool => |rhs| Value{ .Int = lhs + if (rhs) @as(i32, 1) else @as(i32, 0) },
            else => try makeError(ctx.heap, reference, "Cannot add {s} to Int", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Int => |rhs| Value{ .Char = @intCast(@as(i32, lhs) + rhs) },
            .Char => |rhs| Value{ .Char = lhs +% rhs },
            else => try makeError(ctx.heap, reference, "Cannot add {s} to Char", .{other.getType()}),
        },
        .String => |lhs| switch (other) {
            .Char => |rhs| blk: {
                if (rhs == 0) {
                    return Value{ .String = lhs };
                }
                if (lhs.references == 1) { // "I'm the single owner"
                    try lhs.content.append(rhs);
                    break :blk Value{ .String = lhs };
                }
                var new_content = std.ArrayList(u8).init(ctx.heap);
                try new_content.appendSlice(lhs.content.items);
                try new_content.append(rhs);
                const new_str = try ctx.heap.create(StringObj);
                new_str.* = .{ .content = new_content, .references = 0 };
                break :blk Value{ .String = lhs };
            },
            else => try makeError(ctx.heap, reference, "Cannot add {s} to String", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs + rhs },
            .Int => |rhs| Value{ .Float = lhs + floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot add {s} to Float", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use + on {s}", .{self.getType()}),
    };
}

pub fn Minus(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Int = lhs - rhs },
            .Char => |rhs| Value{ .Int = lhs - @as(i32, rhs) },
            .Bool => |rhs| Value{ .Int = lhs - if (rhs) @as(i32, 1) else @as(i32, 0) },
            else => try makeError(ctx.heap, reference, "Cannot subtract {s} from Int", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Int => |rhs| Value{ .Char = @intCast(@as(i32, lhs) - rhs) },
            .Char => |rhs| Value{ .Char = lhs -% rhs },
            else => try makeError(ctx.heap, reference, "Cannot subtract {s} from Char", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs - rhs },
            .Int => |rhs| Value{ .Float = lhs - floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot subtract {s} from Float", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use - on {s}", .{self.getType()}),
    };
}

pub fn Times(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Int = lhs * rhs },
            else => try makeError(ctx.heap, reference, "Cannot multiply Int by {s}", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs * rhs },
            .Int => |rhs| Value{ .Float = lhs * floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot multiply Float by {s}", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use * on {s}", .{self.getType()}),
    };
}

pub fn Div(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| if (rhs != 0) Value{ .Int = @divFloor(lhs, rhs) } else try makeError(ctx.heap, reference, "Cannot divide {d} by zero", .{lhs}),
            else => try makeError(ctx.heap, reference, "Cannot divide Int by {s}", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = lhs / rhs },
            .Int => |rhs| Value{ .Float = lhs / floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot divide Float by {s}", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use / on {s}", .{self.getType()}),
    };
}

pub fn Modulus(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| if (rhs != 0) Value{ .Int = @mod(lhs, rhs) } else try makeError(ctx.heap, reference, "Cannot modulo {d} by zero", .{lhs}),
            else => try makeError(ctx.heap, reference, "Cannot modulo Int by {s}", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Float = @mod(lhs, rhs) },
            .Int => |rhs| Value{ .Float = @mod(lhs, floatFromInt(rhs)) },
            else => try makeError(ctx.heap, reference, "Cannot modulo Float by {s}", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use %% on {s}", .{self.getType()}),
    };
}

pub fn Lt(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs < rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Int with {s} using <", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs < rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) < rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Char with {s} using <", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs < rhs },
            .Int => |rhs| Value{ .Bool = lhs < floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot compare Float with {s} using <", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use < on {s}", .{self.getType()}),
    };
}

pub fn Gt(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs > rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Int with {s} using >", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs > rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) > rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Char with {s} using >", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs > rhs },
            .Int => |rhs| Value{ .Bool = lhs > floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot compare Float with {s} using >", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use > on {s}", .{self.getType()}),
    };
}

pub fn Le(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs <= rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Int with {s} using <=", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs <= rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) <= rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Char with {s} using <=", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs <= rhs },
            .Int => |rhs| Value{ .Bool = lhs <= floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot compare Float with {s} using <=", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use <= on {s}", .{self.getType()}),
    };
}

pub fn Ge(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs >= rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Int with {s} using >=", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs >= rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) >= rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Char with {s} using >=", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs >= rhs },
            .Int => |rhs| Value{ .Bool = lhs >= floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot compare Float with {s} using >=", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use >= on {s}", .{self.getType()}),
    };
}

pub fn Equal(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs == rhs },
            .Null => Value{ .Bool = false },
            else => try makeError(ctx.heap, reference, "Cannot compare Int with {s} using ==", .{other.getType()}),
        },
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs == rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Bool with {s} using ==", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs == rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) == rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Char with {s} using ==", .{other.getType()}),
        },
        .String => |lhs| switch (other) {
            .String => |rhs| Value{ .Bool = std.mem.eql(u8, lhs.content.items, rhs.content.items) },
            else => try makeError(ctx.heap, reference, "Cannot compare String with {s} using ==", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs == rhs },
            .Int => |rhs| Value{ .Bool = lhs == floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot compare Float with {s} using ==", .{other.getType()}),
        },
        .Null => switch (other) {
            .Null => Value{ .Bool = true },
            else => Value{ .Bool = false },
        },
        else => try makeError(ctx.heap, reference, "Cannot use == on {s}", .{self.getType()}),
    };
}

pub fn NotEqual(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Int => |lhs| switch (other) {
            .Int => |rhs| Value{ .Bool = lhs != rhs },
            .Null => Value{ .Bool = true },
            else => try makeError(ctx.heap, reference, "Cannot compare Int with {s} using !=", .{other.getType()}),
        },
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs != rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Bool with {s} using !=", .{other.getType()}),
        },
        .Char => |lhs| switch (other) {
            .Char => |rhs| Value{ .Bool = lhs != rhs },
            .Int => |rhs| Value{ .Bool = @as(i32, lhs) != rhs },
            else => try makeError(ctx.heap, reference, "Cannot compare Char with {s} using !=", .{other.getType()}),
        },
        .String => |lhs| switch (other) {
            .String => |rhs| Value{ .Bool = !std.mem.eql(u8, lhs.content.items, rhs.content.items) },
            else => try makeError(ctx.heap, reference, "Cannot compare String with {s} using !=", .{other.getType()}),
        },
        .Float => |lhs| switch (other) {
            .Float => |rhs| Value{ .Bool = lhs != rhs },
            .Int => |rhs| Value{ .Bool = lhs != floatFromInt(rhs) },
            else => try makeError(ctx.heap, reference, "Cannot compare Float with {s} using !=", .{other.getType()}),
        },
        .Null => switch (other) {
            .Null => Value{ .Bool = false },
            else => Value{ .Bool = true },
        },
        else => try makeError(ctx.heap, reference, "Cannot use != on {s}", .{self.getType()}),
    };
}

pub fn Or(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs or rhs },
            else => try makeError(ctx.heap, reference, "Cannot use 'or' between Bool and {s}", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use 'or' on {s}", .{self.getType()}),
    };
}

pub fn And(self: Value, other: Value, ctx: *Context, reference: Parser.Location) !Value {
    return switch (self) {
        .Bool => |lhs| switch (other) {
            .Bool => |rhs| Value{ .Bool = lhs and rhs },
            else => try makeError(ctx.heap, reference, "Cannot use 'and' between Bool and {s}", .{other.getType()}),
        },
        else => try makeError(ctx.heap, reference, "Cannot use 'and' on {s}", .{self.getType()}),
    };
}
