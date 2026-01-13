const std = @import("std");
const ast = @import("../ast.zig");
const interpretor = @import("interpretor.zig");

const Context = interpretor.Context;

// Bin operation
// .Plus => {},
// .Minus => {},
// .Times => {},
// .Div => {},
// .Modulus => {},
// .Lt => {},
// .Gt => {},
// .Le => {},
// .Ge => {},
// .Equal => {},
// .NotEqual => {},
// .Or => {},
// .And => {},

pub const Value = union(enum) {
    Int: i32,
    Bool: bool,
    Null: void,
    Function: *ast.funcDef,
    Object: struct {
        name: []const u8,
        habitants: std.hash_map.StringHashMap(Value),
    },
    Error: struct {
        reference: []const u8,
        message: []const u8,
    },
};

pub fn Plus(self: Value, other: Value, ctx: *Context, reference: []const u8) Value {
    _ = ctx;
    return switch (self) {
        .Int => switch (other) {
            .Int => Value{ .Int = self.Int + other.Int },
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
            else => Value{ .Error = .{ .reference = reference, .message = "Cannot subtract non-integer values" } },
        },
        else => Value{ .Error = .{ .reference = reference, .message = "Cannot subtract non-integer values" } },
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
