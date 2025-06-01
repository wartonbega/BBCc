const std = @import("std");
const parser = @import("parser.zig");
const analyser = @import("analyser.zig");

const ArrayList = std.ArrayList;

//const ProgInstructions = union(enum) {
//    Number: i32,
//    Add: struct {
//        left: *Expr,
//        right: *Expr,
//    },
//    Var: []const u8,
//};

pub const Errors = enum {
    DivByZeroErr,
    ValueErr,
};

pub const TypeFunc = struct {
    argtypes: ArrayList(*Type),
    retype: *Type,
    typeparam: ArrayList(TypeParam),
    fname: []const u8,
    //uid: []const u8, // A unique Id/name for compilation purpose
};

pub const TypeBase = union(enum) {
    name: []const u8,
    function: TypeFunc,
};

// The number of references (base = 0), is the number of time
// there is a '*' before the type
pub const Type = struct {
    base: TypeBase,
    references: i32,
    err: bool,

    pub fn print(self: *Type) void {
        if (self.err) {
            std.debug.print("!", .{});
        }
        for (0..@intCast(self.references)) |_| {
            std.debug.print("*", .{});
        }
        _ = switch (self.base) {
            .name => std.debug.print("{s}", .{self.base.name}),
            else => void,
        };
    }

    pub fn toString(self: *Type, allocator: std.mem.Allocator) []const u8 {
        var fallback_buf: [100]u8 = [_]u8{0} ** 100;
        var buf = allocator.alloc(u8, 100) catch fallback_buf[0..];
        var idx: usize = @intCast(0);
        if (self.err) {
            buf[idx] = '!';
            idx += 1;
        }
        for (0..@intCast(self.references)) |_| {
            buf[idx] = '*';
            idx += 1;
        }
        _ = switch (self.base) {
            .name => |name| for (name) |c| {
                buf[idx] = c;
                idx += 1;
            },
            .function => |tfunc| {
                for (tfunc.retype.toString(allocator)) |c| {
                    buf[idx] = c;
                    idx += 1;
                }
                buf[idx] = '(';
                idx += 1;
                for (tfunc.argtypes.items) |at| {
                    for (at.toString(allocator)) |c| {
                        buf[idx] = c;
                        idx += 1;
                    }
                    buf[idx] = ' ';
                    idx += 1;
                }
                buf[idx] = ')';
                idx += 1;
            },
        };
        return buf[0..idx];
    }

    pub fn match(self: *const Type, other: *const Type) bool {
        if (self.references != other.references) return false;
        if (!other.err and self.err) return false;
        return switch (self.base) {
            .name => |name| switch (other.base) {
                .name => |name2| std.mem.eql(u8, name, name2),
                .function => false,
            },
            .function => switch (other.base) {
                .name => false,
                .function => false, // [TODO]
            },
        };
    }
};

pub const binOperator = enum {
    Plus,
    Minus,
    Times,
    Div,
    Modulus,
    Lt,
    Gt,
    Le,
    Ge,
    Equal,
    NotEqual,
    Or,
    And,
};

pub fn reprBinOp(op: binOperator) []const u8 {
    return switch (op) {
        .Plus => "+",
        .Minus => "-",
        .Times => "*",
        .Div => "/",
        .Modulus => "%",
        .Lt => "<",
        .Gt => ">",
        .Le => "<=",
        .Ge => ">=",
        .Equal => "==",
        .NotEqual => "!=",
        .Or => "||",
        .And => "&&",
    };
}

pub fn binOpFuncName(op: binOperator) []const u8 {
    return switch (op) {
        .Plus => "Plus",
        .Minus => "Minus",
        .Times => "Times",
        .Div => "Div",
        .Modulus => "Mod",
        .Lt => "LessThan",
        .Gt => "GreaterThan",
        .Le => "LessEqual",
        .Ge => "GreaterEqual",
        .Equal => "Equal",
        .NotEqual => "NotEqual",
        .Or => "Or",
        .And => "And",
    };
}

pub const binaryOperation = struct {
    rhs: *Value,
    operator: binOperator,
    lhs: *Value,
    pub fn print(self: *binaryOperation, rec: i32) void {
        _ = switch (self.operator) {
            .Plus => std.debug.print("+", .{}),
            .Minus => std.debug.print("-", .{}),
            .Times => std.debug.print("*", .{}),
            .Div => std.debug.print("/", .{}),
            else => null,
        };
        std.debug.print("\n", .{});
        self.lhs.print(rec + 1);
        self.rhs.print(rec + 1);
    }
};

pub const RightUnaryOperators = union(enum) {
    pointAttr: []const u8,
};

pub const UnaryOperatorRight = struct { operator: RightUnaryOperators, expr: *Value };

pub const IfStmt = struct { condition: *Value, scope: *Scope };

pub const VarDeclaration = struct {
    mutable: bool,
    name: []const u8,
    pub fn print(self: *VarDeclaration) void {
        std.debug.print("{s} {s}\n", .{ if (self.mutable) "const" else "mut", self.name });
    }
};

pub const Assignement = struct { lhs: *Value, rhs: *Value };

pub const ErrCheck = struct { // Value ? Err (Errors,...) {scope};    Value: *Value,
    errs: ArrayList(*Errors),
    scope: *Scope,
};

pub const TypeCasting = struct { dest_type: *Type, orgn_Value: *Value };

pub const Funcall = struct { // For a function call
    func: *Value,
    args: ArrayList(*Value),
};

pub const Function = struct { arguments: ArrayList(*Arguments), return_type: *Type, code: *Scope };

pub const Value = union(enum) {
    intLit: i32,
    stringLit: []const u8, // String literal : "Hello World"
    charLit: u8,
    parenthesis: *Value,
    identifier: []const u8,
    //UnaryOperatorLeft(UnaryOperatorLeft),
    unaryOperatorRight: *UnaryOperatorRight, // Like a.b ('.b' is the operator)
    binaryOperator: *binaryOperation,
    errorCheck: *ErrCheck,
    If: *IfStmt,
    typeCasting: *TypeCasting,
    scope: *Scope,
    funcall: *Funcall,
    assignement: *Assignement,
    varDec: *VarDeclaration,
    function: *Function,
    NULL: bool,

    pub fn print(self: *Value, rec: i32) void {
        for (0..@intCast(rec)) |_| {
            std.debug.print("    ", .{});
        }
        _ = switch (self.*) {
            .intLit => std.debug.print("{d}\n", .{self.intLit}),
            .stringLit => std.debug.print("{s}\n", .{self.stringLit}),
            .charLit => std.debug.print("{c}\n", .{self.charLit}),
            .parenthesis => {
                std.debug.print("parenthesis :\n", .{});
                self.parenthesis.print(rec + 1);
            },
            .identifier => std.debug.print("identifier {s}\n", .{self.identifier}),
            .binaryOperator => self.binaryOperator.print(rec),
            .assignement => {
                std.debug.print("= (assignation)\n", .{});
                self.assignement.lhs.print(rec + 1);
                self.assignement.rhs.print(rec + 1);
            },
            .varDec => self.varDec.print(),
            else => std.debug.print("{}\n", .{self.*}),
        };
    }
};

pub const Scope = struct {
    code: ArrayList(*Value),
    ctx: *analyser.Context,

    pub fn init(code: ArrayList(*Value), allocator: std.mem.Allocator) !*Scope {
        const self = try allocator.create(Scope);
        self.* = Scope{
            .code = code,
            .ctx = try analyser.Context.init(allocator),
        };
        return self;
    }

    pub fn print(self: *Scope, rec: i32) void {
        for (self.code.items) |value| {
            value.print(rec + 1);
        }
    }
};

pub const Arguments = struct {
    _type: *Type,
    name: []const u8,
    pub fn print(self: *Arguments) void {
        self._type.print();
        std.debug.print(" {s}", .{self.name});
    }
};

pub const TypeParam = struct {
    name: []const u8,
    traits: ArrayList([]const u8),
};

pub const funcDef = struct {
    name: []const u8,
    arguments: ArrayList(*Arguments),
    return_type: *Type,
    code: *Scope,
    typeparam: ArrayList(TypeParam),
    pub fn print(self: *funcDef) void {
        std.debug.print("function '{s}' (", .{self.name});
        for (self.arguments.items) |arg| {
            arg.print();
            std.debug.print(", ", .{});
        }
        std.debug.print(")", .{});
        self.return_type.print();
        std.debug.print("\n", .{});
        self.code.print(0);
    }
};

pub const ProgInstructions = union(enum) {
    FuncDef: *funcDef,
    pub fn print(self: *ProgInstructions) void {
        switch (self.*) {
            .FuncDef => self.FuncDef.print(),
        }
    }
};

pub const Program = struct {
    instructions: ArrayList(*ProgInstructions),

    pub fn print(self: *Program) void {
        for (self.instructions.items) |inst| {
            inst.print();
        }
    }
};
