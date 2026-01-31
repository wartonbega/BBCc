const std = @import("std");
const parser = @import("parser.zig");
const analyser = @import("analyser.zig");
const Parser = @import("parser.zig");

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
        _ = switch (self.base) {
            .name => |name| {
                if (self.err) {
                    return std.fmt.allocPrint(allocator, "!{s}", .{name}) catch return "";
                } else {
                    return std.fmt.allocPrint(allocator, "{s}", .{name}) catch return "";
                }
            },
            .function => |tfunc| {
                var args = std.fmt.allocPrint(allocator, "(", .{}) catch return "";
                for (tfunc.argtypes.items, 0..) |arg, i| {
                    const argStr = arg.toString(allocator);
                    args = std.fmt.allocPrint(allocator, "{s}{s}{s}", .{
                        args,
                        if (i > 0) ", " else "",
                        argStr,
                    }) catch return "";
                }
                args = std.fmt.allocPrint(allocator, "{s})", .{args}) catch return "";
                if (self.err) {
                    return std.fmt.allocPrint(allocator, "!{s} -> {s}", .{
                        args,
                        tfunc.retype.toString(allocator),
                    }) catch return "";
                } else {
                    return std.fmt.allocPrint(allocator, "{s} -> {s}", .{
                        args,
                        tfunc.retype.toString(allocator),
                    }) catch return "";
                }
            },
        };
        return "";
    }

    pub fn match(self: *const Type, other: *const Type) bool {
        if (self.references != other.references) return false;
        if (!other.err and self.err) return false;
        return switch (self.base) {
            .name => |name| switch (other.base) {
                .name => |name2| std.mem.eql(u8, name, name2),
                .function => false,
            },
            .function => |f1| switch (other.base) {
                .name => false,
                .function => |f2| {
                    if (!f1.retype.match(f2.retype))
                        return false;
                    if (f1.argtypes.items.len != f2.argtypes.items.len)
                        return false;
                    for (f1.argtypes.items, f2.argtypes.items) |a1, a2| {
                        if (!a1.match(a2))
                            return false;
                    }
                    return true;
                },
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
    reference: Parser.Location,

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
    reference: Parser.Location,
};

// If statement, the multiple elif conditions are stored in conditions, and the corresponding scopes in scopes
// finally the else statement is in elsescope
pub const IfStmt = struct {
    conditions: ArrayList(*Value),
    scopes: ArrayList(*Value), // The different 'scopes'
    elsescope: ?*Value,
    reference: Parser.Location,
};

pub const WhileLoop = struct {
    condition: *Value,
    exec: *Value, // The code executed in loop
    reference: Parser.Location,
};

pub const VarDeclaration = struct {
    mutable: bool,
    name: []const u8,
    reference: Parser.Location,
    pub fn print(self: *VarDeclaration) void {
        std.debug.print("{s} {s}\n", .{ if (self.mutable) "const" else "mut", self.name });
    }
};

pub const Assignement = struct {
    lhs: *Value,
    rhs: *Value,
    reference: Parser.Location,
};

pub const ErrCheck = struct { // Value ? err_name scope;    Value: *Value, scope: *Value
    value: *Value,
    err: []const u8, // just the name
    scope: *Value, // The return value if 'value' has errors
    reference: Parser.Location,
};

pub const TypeCasting = struct { dest_type: *Type, orgn_Value: *Value };

pub const Funcall = struct { // For a function call
    func: *Value,
    args: ArrayList(*Value),
};

pub const Function = struct {
    arguments: ArrayList(*Arguments),
    return_type: *Type,
    code: *Value,
    reference: Parser.Location,
};

pub const StructInit = struct {
    reference: Parser.Location,
    name: []const u8,
    habitants: std.StringHashMap(*Value),
};

pub const Value = union(enum) {
    intLit: struct {
        value: i32,
        reference: Parser.Location,
    },
    boolLit: struct {
        value: bool,
        reference: Parser.Location,
    },
    stringLit: struct {
        value: []const u8,
        reference: Parser.Location,
    }, // String literal : "Hello World"
    charLit: struct {
        value: u8,
        reference: Parser.Location,
    },
    nullLit: struct {
        reference: Parser.Location,
    },
    identifier: struct {
        name: []const u8,
        reference: Parser.Location,
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
    function: *funcDef,
    freeKeyword: struct {
        val: *Value,
        reference: Parser.Location,
    },
    Print: struct {
        ln: bool, // new line
        args: ArrayList(*Value),
        reference: Parser.Location,
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

    pub fn getReference(self: *Value) parser.Location {
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
            .Print => |value| value.reference,
            .NULL => parser.getInbuiltLocation(),
        };
    }
};

pub const Scope = struct {
    code: ArrayList(*Value),
    ctx: *analyser.Context,
    reference: Parser.Location,

    pub fn init(code: ArrayList(*Value), reference: Parser.Location, allocator: std.mem.Allocator) !*Scope {
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
    typeref: Parser.Location,
    name: []const u8,
    reference: Parser.Location,

    pub fn print(self: *Arguments) void {
        self._type.print();
        std.debug.print(" {s}", .{self.name});
    }
};

pub const TypeParam = struct {
    name: []const u8,
    traits: ArrayList([]const u8),
    reference: Parser.Location,
};

pub const funcDef = struct {
    name: []const u8,
    arguments: ArrayList(*Arguments),
    return_type: *Type,
    return_type_ref: Parser.Location,
    code: *Scope,
    typeparam: ArrayList(TypeParam),
    reference: Parser.Location,
    parent: ?*Type,

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

    pub fn getName(self: *funcDef, allocator: std.mem.Allocator) ![]const u8 {
        if (self.parent) |parent| {
            return try std.mem.concat(allocator, u8, &[_][]const u8{ parent.toString(allocator), ".", self.name });
        } else return self.name;
    }
};

pub const structDef = struct {
    habitants: std.hash_map.StringHashMap(*Type),
    fields: std.ArrayList([]const u8),
    name: []const u8,
    reference: Parser.Location,
    methods: std.hash_map.StringHashMap(*funcDef),

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
