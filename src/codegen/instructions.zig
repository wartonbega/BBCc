const std = @import("std");

const Prog = std.ArrayList(Instructions);
const InstArray = std.ArrayList(Instructions);
const Array = std.ArrayList;
const HashMap = std.StringArrayHashMap(Value);
const Allocator = std.mem.Allocator;

pub const Location = union(enum) {
    stack: struct {
        idx: usize,
        stack_state: Array(Type),
    }, // The index on the stack, as well as the stack's state
    register: Registers,
    label: []const u8,
    void: void,
};
// r15 is reserved for the dumpers for swapping values
pub const Registers = enum { r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14 };

pub const RegIter = [_]Registers{ .r0, .r1, .r2, .r3, .r4, .r5, .r6, .r7, .r8, .r9, .r10, .r11, .r12, .r13, .r14 };

fn registerIndex(reg: Registers) usize {
    return switch (reg) {
        .r0 => @intCast(0),
        .r1 => @intCast(1),
        .r2 => @intCast(2),
        .r3 => @intCast(3),
        .r4 => @intCast(4),
        .r5 => @intCast(5),
        .r6 => @intCast(6),
        .r7 => @intCast(7),
        .r8 => @intCast(8),
        .r9 => @intCast(9),
        .r10 => @intCast(10),
        .r11 => @intCast(11),
        .r12 => @intCast(12),
        .r13 => @intCast(13),
        .r14 => @intCast(14),
    };
}

pub const RegisterTable = struct {
    utilised: [15]bool,

    pub fn init() RegisterTable {
        return RegisterTable{ .utilised = [_]bool{false} ** 15 };
    }

    pub fn getRegister(self: *RegisterTable, reg: Registers) bool {
        return self.utilised[registerIndex(reg)];
    }

    pub fn setRegister(self: *RegisterTable, reg: Registers, val: bool) void {
        self.utilised[registerIndex(reg)] = val;
    }
};

// A value indicates the position on the stack
// and the type associated
pub const Value = struct {
    position: Location,
    type: void,
};

pub const Type = union(enum) {
    // The boolean is for error union
    voidType: bool,
    intType: bool,
    boolType: bool,
    stringType: bool,
    arrayLike: bool,
    charType: bool,
    pointer: bool,
    function: void,
    structType: struct {
        habitants: []const Type,
    },

    pub fn hasError(self: *const Type) bool {
        switch (self.*) {
            .voidType, .intType, .boolType, .stringType, .arrayLike, .charType, .pointer => |eu| return eu,
            else => return false,
        }
    }
};

pub fn getCompileSize(t: Type) i64 {
    // Returns the size in bytes of the Compile Type
    return switch (t) {
        .intType => 8,
        .pointer => 8,
        .function => 8,
        .stringType => 8,
        .arrayLike => 8,
        .charType => 1,
        else => unreachable,
    };
}

pub fn isAlloced(t: Type) bool {
    // Returns wether the type is alloced in memory, or can just be used in registers
    return switch (t) {
        .arrayLike, .structType => true,
        .charType, .intType, .boolType, .pointer, .function, .voidType, .stringType => false,
    };
}

pub const varContentType = std.ArrayList(struct {
    size: usize,
    content: []const u8,
});

pub const Instructions = union(enum) {
    Plus: struct {
        x: Location,
        y: Location,
    },
    addImmediate: struct {
        x: Location,
        y: i64,
    },
    Minus: struct {
        x: Location,
        y: Location,
    },
    Multiply: struct {
        x: Location,
        y: Location,
    },
    Divide: struct {
        x: Location,
        y: Location,
    },
    Modulo: struct {
        x: Location,
        y: Location,
    },
    Equal: struct {
        x: Location,
        y: Location,
    },
    NotEqual: struct {
        x: Location,
        y: Location,
    },
    LessThan: struct {
        x: Location,
        y: Location,
    },
    LessEqual: struct {
        x: Location,
        y: Location,
    },
    GreaterThan: struct {
        x: Location,
        y: Location,
    },
    GreaterEqual: struct {
        x: Location,
        y: Location,
    },
    Not: Location,
    writeArrayElement: struct { // To write at a specific index of an array (fat-pointed)
        arr: Location,
        idx: usize,
        value: Location,
        _type: Type,
    },
    reserveStack: i64,
    decreaseStack: Type,
    getBasePointer: Location,
    getStackPointer: Location,
    Return: Location,
    ConditionalJump: struct { value: Location, label: []const u8 },
    Jump: struct { label: []const u8 },
    Function: []const u8,
    BeginFunction: void,
    EndFunction: void,
    Move: struct {
        from: Location,
        to: Location,
        _type: Type,
    },
    Load: struct {
        from: Location,
        to: Location,
    },
    loadAddress: struct {
        from: Location,
        to: Location,
    },
    CopyOnStack: struct { // Automatically grows the stack
        from: Location,
        _type: Type,
    },
    IntLit: struct { val: i64, to: Location },
    CharLit: struct { val: u8, to: Location },
    ExitWith: Location,
    Print: Location,
    Comment: []const u8,
    Funcall: struct {
        func: Location,
        args: Array(Location),
    },
    Label: []const u8,
    beginVariableSection: void,
    declareVariable: struct { // under a label, there can be multiple segments of memory
        name: []const u8,
        content: varContentType,
    },

    pub fn print(self: *const Instructions, idx: usize) void {
        std.debug.print("#{d}", .{idx});
        switch (self.*) {
            .Plus => |inst| std.debug.print("\tPlus({}, {})\n", .{ inst.x, inst.y }),
            .Minus => |inst| std.debug.print("\tMinus({}, {})\n", .{ inst.x, inst.y }),
            .Multiply => |inst| std.debug.print("\tMultiply({}, {})\n", .{ inst.x, inst.y }),
            .Divide => |inst| std.debug.print("\tDivide({}, {})\n", .{ inst.x, inst.y }),
            .Modulo => |inst| std.debug.print("\tModulo({}, {})\n", .{ inst.x, inst.y }),
            .Equal => |inst| std.debug.print("\tEqual({}, {})\n", .{ inst.x, inst.y }),
            .NotEqual => |inst| std.debug.print("\tNotEqual({}, {})\n", .{ inst.x, inst.y }),
            .LessThan => |inst| std.debug.print("\tLessThan({}, {})\n", .{ inst.x, inst.y }),
            .LessEqual => |inst| std.debug.print("\tLessEqual({}, {})\n", .{ inst.x, inst.y }),
            .GreaterThan => |inst| std.debug.print("\tGreaterThan({}, {})\n", .{ inst.x, inst.y }),
            .GreaterEqual => |inst| std.debug.print("\tGreaterEqual({}, {})\n", .{ inst.x, inst.y }),
            .reserveStack => |inst| std.debug.print("\tReserveStack({d})\n", .{inst}),
            .Return => |inst| std.debug.print("\tReturn({})\n", .{inst}),
            .Function => |inst| std.debug.print("Function {s}\n", .{inst}),
            .BeginFunction => |_| std.debug.print("\tBegining function:...\n", .{}),
            .EndFunction => |_| std.debug.print("\tEnding function:...\n", .{}),
            .Move => |inst| std.debug.print("\tMove {} to {} (type {})\n", .{ inst.from, inst.to, inst._type }),
            .loadAddress => |inst| std.debug.print("\tLoad address {} to {}\n", .{ inst.from, inst.to }),
            .CopyOnStack => |inst| std.debug.print("\tCopy {} on stack (type {})\n", .{ inst.from, inst._type }),
            .IntLit => |inst| std.debug.print("\tIntlit {d} in {}\n", .{ inst.val, inst.to }),
            .ExitWith => |inst| std.debug.print("\tExit({})\n", .{inst}),
            .Print => |inst| std.debug.print("\tPrint({})\n", .{inst}),
            .Comment => |comment| std.debug.print("\t// {s}\n", .{comment}),
            .ConditionalJump => |cond| std.debug.print("\tConditional jump if {}, to {s}", .{ cond.value, cond.label }),
            .Jump => |cond| std.debug.print("\tJump to {s}", .{cond.label}),
            .Funcall => |func| {
                std.debug.print("\tCall {}(", .{func.func});
                for (func.args.items) |a| {
                    std.debug.print("{}, ", .{a});
                }
                std.debug.print(")\n", .{});
            },
            .addImmediate => |inst| std.debug.print("\tAddImmediate({}, {d})\n", .{ inst.x, inst.y }),
            .getBasePointer => |inst| std.debug.print("\tGetBasePointer({})\n", .{inst}),
            .getStackPointer => |inst| std.debug.print("\tGetStackPointer({})\n", .{inst}),
            .CharLit => |inst| std.debug.print("\tCharlit {d} in {}\n", .{ inst.val, inst.to }),
            .writeArrayElement => |inst| std.debug.print("\tWriteArrayElement(arr: {}, idx: {d}, value: {}, type: {})\n", .{ inst.arr, inst.idx, inst.value, inst._type }),
            .decreaseStack => |inst| std.debug.print("\tDecreaseStack({})\n", .{inst}),
            .declareVariable => |inst| std.debug.print("\tDeclaring variable {s} ...\n", .{inst.name}),
            .beginVariableSection => std.debug.print("Begining variable section:\n", .{}),
            .Label => |inst| std.debug.print("label {s}:\n", .{inst}),
            else => {},
        }
    }
};

pub const Builder = struct {
    code: Prog,
    stack_state: HashMap,
    allocator: Allocator,

    pub fn init(alloc: Allocator) Builder {
        const list = Prog.init(alloc);
        return .{
            .code = list,
            .stack_state = HashMap.init(alloc),
            .allocator = alloc,
        };
    }

    pub fn addInstruction(self: *Builder, inst: Instructions) !void {
        try self.code.append(inst);
    }

    pub fn reserveStack(self: *Builder, size: i32) !void {
        try self.code.append(Instructions{ .reserveStack = size });
    }

    pub fn decreaseStack(self: *Builder, _type: Type) !void {
        try self.code.append(Instructions{ .decreaseStack = _type });
    }

    pub fn writeArrayElement(self: *Builder, arr: Location, idx: usize, value: Location, _type: Type) !void {
        try self.code.append(Instructions{ .writeArrayElement = .{ .arr = arr, .idx = idx, .value = value, ._type = _type } });
    }

    pub fn getBasePointer(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .getBasePointer = loc });
    }

    pub fn getStackPointer(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .getStackPointer = loc });
    }

    pub fn comment(self: *Builder, com: []const u8) !void {
        try self.code.append(Instructions{ .Comment = com });
    }

    pub fn conditionalJump(self: *Builder, val: Location, label: []const u8) !void {
        try self.code.append(Instructions{ .ConditionalJump = .{ .value = val, .label = label } });
    }

    pub fn jump(self: *Builder, label: []const u8) !void {
        try self.code.append(Instructions{ .Jump = .{ .label = label } });
    }

    pub fn functionDec(self: *Builder, name: []const u8) !void {
        try self.code.append(Instructions{ .Function = name });
    }

    pub fn beginFunction(self: *Builder) !void {
        try self.code.append(Instructions{ .BeginFunction = {} });
    }

    pub fn endFunction(self: *Builder) !void {
        try self.code.append(Instructions{ .EndFunction = {} });
    }

    pub fn returnInst(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .Return = loc });
    }

    pub fn moveInst(self: *Builder, from: Location, to: Location, _type: Type) !void {
        try self.code.append(Instructions{ .Move = .{ .from = from, .to = to, ._type = _type } });
    }

    pub fn load(self: *Builder, from: Location, to: Location) !void {
        try self.code.append(Instructions{ .Load = .{ .from = from, .to = to } });
    }

    pub fn loadAddress(self: *Builder, from: Location, to: Location) !void {
        try self.code.append(Instructions{ .loadAddress = .{ .from = from, .to = to } });
    }

    pub fn intLit(self: *Builder, val: i64, to: Location) !void {
        try self.code.append(Instructions{ .IntLit = .{ .val = val, .to = to } });
    }

    pub fn charLit(self: *Builder, val: u8, to: Location) !void {
        try self.code.append(Instructions{ .CharLit = .{ .val = val, .to = to } });
    }

    pub fn exitWith(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .ExitWith = loc });
    }

    pub fn print(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .Print = loc });
    }

    pub fn plus(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Plus = .{ .x = x, .y = y } });
    }

    pub fn addImmediate(self: *Builder, x: Location, y: i64) !void {
        try self.code.append(Instructions{ .addImmediate = .{ .x = x, .y = y } });
    }

    pub fn minus(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Minus = .{ .x = x, .y = y } });
    }
    pub fn multiply(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Multiply = .{ .x = x, .y = y } });
    }

    pub fn divide(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Divide = .{ .x = x, .y = y } });
    }

    pub fn modulo(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Modulo = .{ .x = x, .y = y } });
    }

    pub fn not(self: *Builder, x: Location) !void {
        try self.code.append(Instructions{ .Not = x });
    }

    pub fn equal(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Equal = .{ .x = x, .y = y } });
    }

    pub fn notEqual(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .NotEqual = .{ .x = x, .y = y } });
    }

    pub fn lessThan(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .LessThan = .{ .x = x, .y = y } });
    }

    pub fn lessEqual(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .LessEqual = .{ .x = x, .y = y } });
    }

    pub fn greaterThan(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .GreaterThan = .{ .x = x, .y = y } });
    }

    pub fn greaterEqual(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .GreaterEqual = .{ .x = x, .y = y } });
    }

    pub fn funcall(self: *Builder, func: Location, args: Array(Location)) !void {
        try self.code.append(Instructions{ .Funcall = .{ .func = func, .args = args } });
    }

    pub fn declareVariable(self: *Builder, name: []const u8, content: *const varContentType) !void {
        try self.code.append(Instructions{ .declareVariable = .{ .name = name, .content = content.* } });
    }

    pub fn beginVariableSection(self: *Builder) !void {
        try self.code.append(Instructions{ .beginVariableSection = {} });
    }

    pub fn labelDec(self: *Builder, name: []const u8) !void {
        try self.code.append(Instructions{ .Label = name });
    }
};

pub const FuncContexte = struct {
    vars: std.StringArrayHashMap(Value),
};
