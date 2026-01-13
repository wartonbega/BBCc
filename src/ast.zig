const std = @import("std");
const parser = @import("parser.zig");
const analyser = @import("analyser.zig");

const ArrayList = std.ArrayList;

pub const Errors = enum {
    DivByZeroErr,
    ValueErr,
};

pub const TypeFunc = struct {
    argtypes: ArrayList(*Type),
    retype: *Type,
    typeparam: ArrayList(TypeParam),
    fname: []const u8,
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
    reference: []const u8,

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

pub const UnaryOperatorRight = struct {
    operator: RightUnaryOperators,
    expr: *Value,
    reference: []const u8,
};

// If statement, the multiple elif conditions are stored in conditions, and the corresponding scopes in scopes
// finally the else statement is in elsescope
pub const IfStmt = struct {
    conditions: ArrayList(*Value),
    scopes: ArrayList(*Value), // The different 'scopes'
    elsescope: ?*Value,
    reference: []const u8,
};

pub const WhileLoop = struct {
    condition: *Value,
    exec: *Value, // The code executed in loop
    reference: []const u8,
};

pub const VarDeclaration = struct {
    mutable: bool,
    name: []const u8,
    reference: []const u8,
    pub fn print(self: *VarDeclaration) void {
        std.debug.print("{s} {s}\n", .{ if (self.mutable) "const" else "mut", self.name });
    }
};

pub const Assignement = struct {
    lhs: *Value,
    rhs: *Value,
    reference: []const u8,
};

pub const ErrCheck = struct { // Value ? err_name scope;    Value: *Value, scope: *Value
    value: *Value,
    err: []const u8, // just the name
    scope: *Value, // The return value if 'value' has errors
    reference: []const u8,
};

pub const TypeCasting = struct { dest_type: *Type, orgn_Value: *Value };

pub const Funcall = struct { // For a function call
    func: *Value,
    args: ArrayList(*Value),
};

pub const Function = struct {
    arguments: ArrayList(*Arguments),
    return_type: *Type,
    code: *Scope,
    reference: []const u8,
};

pub const StructInit = struct {
    reference: []const u8,
    name: []const u8,
    habitants: std.StringHashMap(*Value),
};

pub const Value = union(enum) {
    intLit: struct {
        value: i32,
        reference: []const u8,
    },
    boolLit: struct {
        value: bool,
        reference: []const u8,
    },
    stringLit: struct {
        value: []const u8,
        reference: []const u8,
    }, // String literal : "Hello World"
    charLit: struct {
        value: u8,
        reference: []const u8,
    },
    nullLit: struct {
        reference: []const u8,
    },
    identifier: struct {
        name: []const u8,
        reference: []const u8,
    },
    parenthesis: *Value,
    //UnaryOperatorLeft(UnaryOperatorLeft),
    unaryOperatorRight: *UnaryOperatorRight, // Like a.b ('.b' is the operator)
    binaryOperator: *binaryOperation,
    errorCheck: *ErrCheck,
    If: *IfStmt,
    While: *WhileLoop,
    //typeCasting: *TypeCasting,
    scope: *Scope,
    funcall: *Funcall,
    assignement: *Assignement,
    varDec: *VarDeclaration,
    structInit: *StructInit,
    function: *Function,
    freeKeyword: struct {
        val: *Value,
        reference: []const u8,
    },
    NULL: bool,

    pub fn print(self: *Value, rec: i32) void {
        for (0..@intCast(rec)) |_| {
            std.debug.print("    ", .{});
        }
        _ = switch (self.*) {
            .intLit => std.debug.print("{d}\n", .{self.intLit.value}),
            .stringLit => std.debug.print("{s}\n", .{self.stringLit.value}),
            .charLit => std.debug.print("{c}\n", .{self.charLit.value}),
            .parenthesis => {
                std.debug.print("parenthesis :\n", .{});
                self.parenthesis.print(rec + 1);
            },
            .identifier => std.debug.print("identifier {s}\n", .{self.identifier.name}),
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

    pub fn getReference(self: *Value) []const u8 {
        return switch (self.*) {
            .intLit => |value| value.reference,
            .charLit => |value| value.reference,
            .boolLit => |value| value.reference,
            .stringLit => |value| value.reference,
            .nullLit => |value| value.reference,
            .identifier => |value| value.reference,
            .parenthesis => |value| value.getReference(),
            .unaryOperatorRight => |value| value.reference,
            .binaryOperator => |value| value.reference,
            .errorCheck => |value| value.reference,
            .If => |value| value.reference,
            .While => |value| value.reference,
            .scope => |value| value.reference,
            .funcall => |value| value.func.getReference(),
            .assignement => |value| value.reference,
            .varDec => |value| value.reference,
            .function => |value| value.reference,
            .structInit => |value| value.reference,
            .freeKeyword => |value| value.reference,

            .NULL => "",
        };
    }
};

pub const Scope = struct {
    code: ArrayList(*Value),
    ctx: *analyser.Context,
    reference: []const u8,

    pub fn init(code: ArrayList(*Value), reference: []const u8, allocator: std.mem.Allocator) !*Scope {
        const self = try allocator.create(Scope);
        self.* = Scope{
            .code = code,
            .ctx = try analyser.Context.init(allocator),
            .reference = reference,
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
    reference: []const u8,

    pub fn print(self: *Arguments) void {
        self._type.print();
        std.debug.print(" {s}", .{self.name});
    }
};

pub const TypeParam = struct {
    name: []const u8,
    traits: ArrayList([]const u8),
    reference: []const u8,
};

pub const funcDef = struct {
    name: []const u8,
    arguments: ArrayList(*Arguments),
    return_type: *Type,
    code: *Scope,
    typeparam: ArrayList(TypeParam),
    reference: []const u8,

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

pub const structDef = struct {
    habitants: std.hash_map.StringHashMap(*Type),
    order: std.ArrayList([]const u8),
    name: []const u8,
    reference: []const u8,

    pub fn habitantExist(self: *structDef, name: []const u8) bool {
        return self.habitants.contains(name);
    }

    pub fn getHabitant(self: *structDef, name: []const u8) *Type {
        return self.habitants.get(name).?;
    }

    pub fn print(self: *structDef) void {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        std.debug.print("Struct {s} : \n", .{self.name});
        var it = self.habitants.iterator();
        while (it.next()) |hab| {
            std.debug.print("\t+ {s} : {s} \n", .{ hab.key_ptr.*, hab.value_ptr.*.toString(arena.allocator()) });
        }
    }
};

pub const ProgInstructions = union(enum) {
    FuncDef: *funcDef,
    StructDef: *structDef,
    pub fn print(self: *ProgInstructions) void {
        switch (self.*) {
            .FuncDef => self.FuncDef.print(),
            .StructDef => self.StructDef.print(),
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
