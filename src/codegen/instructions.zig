const std = @import("std");

const Prog = std.ArrayList(Instructions);
const InstArray = std.ArrayList(Instructions);
const Array = std.ArrayList;
const HashMap = std.StringArrayHashMap(Value);
const Allocator = std.mem.Allocator;

pub const Location = union(enum) {
    stack: usize, // The index on the stack, beggining at 0
    register: Registers,
    label: []const u8,
    argument: usize, // The number of the argument: 0 to \infnity
    void: void,
};

// The 'duration' of a value in the IR
// - scratch means a very short duration, e.g. placing an immediate into the stack
//   performing a small computation, etc... => scratch registers
// - mid means a value that should be stored across one or more function call
//   => callee saved
pub const DurationPriority = enum {
    scratch, // Only used
    mid,
    long,
};

pub const REGISTER_NB = 14;

pub const Registers = enum {
    r0, // rax
    r1, // rbx
    r2, // rcx
    r3, // rdx
    r4, // rsi
    r5, // rdi
    r6, // r8
    r7, // r9
    r8, // r10
    r9, // r11
    r10, // r12
    r11, // r13
    r12, // r14
    r13, // r15
};

pub const RegIter = [_]Registers{ .r0, .r1, .r2, .r3, .r4, .r5, .r6, .r7, .r8, .r9, .r10, .r11, .r12, .r13 };

// rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
pub const ScratchReg = [_]Registers{ .r0, .r7, .r8, .r9 };

// rbx, rsp, rbp, r12, r13, r14, and r15                                       // from here, technically scratch
pub const CalleeSave = [_]Registers{ .r1, .r10, .r11, .r12, .r13, .r2, .r3, .r5, .r4, .r6 };

pub fn in(reg: Registers, list: []const Registers) bool {
    for (list) |r| {
        if (reg == r) return true;
    }
    return false;
}

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
    };
}

pub const RegisterTable = struct {
    utilised: [REGISTER_NB]bool,

    pub fn init() RegisterTable {
        return RegisterTable{ .utilised = [_]bool{false} ** REGISTER_NB };
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
    structType: Array(Type),

    pub fn hasError(self: *const Type) bool {
        switch (self.*) {
            .voidType, .intType, .boolType, .stringType, .arrayLike, .charType, .pointer => |eu| return eu,
            else => return false,
        }
    }
};

pub fn getCompileSize(t: Type) i64 {
    // Returns the size in bytes of the Compile Type
    return blk: switch (t) {
        .structType => |stc| {
            var acc: i64 = @intCast(2);
            for (stc.items) |hab|
                acc += getCompileSize(hab);
            std.debug.print("{d}\n", .{acc});
            break :blk acc;
        },
        .intType => 8,
        .pointer => 8,
        .function => 8,
        .stringType => 8,
        .arrayLike => 8,
        .charType => 1,
        .voidType => 1,
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
    content: u32,
});

const binInstruction = struct { x: Location, y: Location };

pub const Instructions = union(enum) {
    addImmediate: struct {
        x: Location,
        y: i64,
    },
    Plus: binInstruction,
    Minus: binInstruction,
    Multiply: binInstruction,
    Divide: binInstruction,
    Modulo: binInstruction,
    Equal: binInstruction,
    NotEqual: binInstruction,
    LessThan: binInstruction,
    LessEqual: binInstruction,
    GreaterThan: binInstruction,
    GreaterEqual: binInstruction,
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
    heapAlloc: struct {
        size: i64,
        dest: Location,
    },
    heapFree: Location,
    writeToPointer: struct {
        dest: Location,
        decal: i64,
        content: Location,
    },
    readFromPointer: struct {
        dest: Location,
        decal: i64,
        origin: Location,
    },
    incrementeReferenceCounter: Location,
    decrementReferenceCounter: Location,
    pushValue: Location,
    popValue: Location,

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
            .heapAlloc => |inst| std.debug.print("heap alloc {d} bytes in {}", .{ inst.size, inst.dest }),
            else => {},
        }
    }
};

pub const Builder = struct {
    code: Prog,
    position: usize,
    stack_state: HashMap,
    allocator: Allocator,

    pub fn init(alloc: Allocator) Builder {
        const list = Prog.init(alloc);
        return .{
            .code = list,
            .stack_state = HashMap.init(alloc),
            .allocator = alloc,
            .position = 0,
        };
    }

    pub fn getPosition(self: *Builder) builderPos {
        return builderPos{ .index = self.position };
    }

    pub fn setPosition(self: *Builder, pos: builderPos) void {
        switch (pos) {
            .index => |idx| {
                self.position = idx;
            },
            .last => {
                self.position = self.code.items.len;
            },
        }
    }

    pub fn shiftStackBetween(self: *Builder, pos1: builderPos, pos2: builderPos, amount: i64) void {
        const p1 = if (pos1 == .index) pos1.index else self.code.items.len;
        const p2 = if (pos2 == .index) pos2.index else self.code.items.len;
        for (p1..p2) |i| {
            const inst = self.code.items[i];
            var inst_mut = inst;
            switch (inst_mut) {
                .Plus, .Minus, .Multiply, .Divide, .Modulo, .Equal, .NotEqual, .LessThan, .LessEqual, .GreaterThan, .GreaterEqual => |*bin| {
                    if (bin.x == .stack) bin.x.stack += @intCast(amount);
                    if (bin.y == .stack) bin.y.stack += @intCast(amount);
                },
                .addImmediate => |*ai| {
                    if (ai.x == .stack) ai.x.stack += @intCast(amount);
                },
                .Not, .getBasePointer, .getStackPointer, .Return, .ExitWith, .Print, .heapFree, .incrementeReferenceCounter, .decrementReferenceCounter, .pushValue, .popValue => |*loc| {
                    if (loc.* == .stack) loc.stack += @intCast(amount);
                },
                .writeArrayElement => |*wae| {
                    if (wae.arr == .stack) wae.arr.stack += @intCast(amount);
                    if (wae.value == .stack) wae.value.stack += @intCast(amount);
                },
                .Move => |*mv| {
                    if (mv.from == .stack) mv.from.stack += @intCast(amount);
                    if (mv.to == .stack) mv.to.stack += @intCast(amount);
                },
                .Load => |*ld| {
                    if (ld.from == .stack) ld.from.stack += @intCast(amount);
                    if (ld.to == .stack) ld.to.stack += @intCast(amount);
                },
                .loadAddress => |*la| {
                    if (la.from == .stack) la.from.stack += @intCast(amount);
                    if (la.to == .stack) la.to.stack += @intCast(amount);
                },
                .CopyOnStack => |*cos| {
                    if (cos.from == .stack) cos.from.stack += @intCast(amount);
                },
                .IntLit => |*il| {
                    if (il.to == .stack) il.to.stack += @intCast(amount);
                },
                .CharLit => |*cl| {
                    if (cl.to == .stack) cl.to.stack += @intCast(amount);
                },
                .Funcall => |*fc| {
                    if (fc.func == .stack) fc.func.stack += @intCast(amount);
                    for (fc.args.items) |*a| {
                        if (a.* == .stack) a.stack += @intCast(amount);
                    }
                },
                .heapAlloc => |*ha| {
                    if (ha.dest == .stack) ha.dest.stack += @intCast(amount);
                },
                .writeToPointer => |*wtp| {
                    if (wtp.dest == .stack) wtp.dest.stack += @intCast(amount);
                    if (wtp.content == .stack) wtp.content.stack += @intCast(amount);
                },
                .readFromPointer => |*rfp| {
                    if (rfp.dest == .stack) rfp.dest.stack += @intCast(amount);
                    if (rfp.origin == .stack) rfp.origin.stack += @intCast(amount);
                },
                else => {},
            }
            self.code.items[i] = inst_mut;
        }
    }

    pub fn addInstruction(self: *Builder, inst: Instructions) !void {
        try self.code.insert(self.position, inst);
        self.position += 1;
    }

    pub fn reserveStack(self: *Builder, size: i32) !void {
        try self.code.insert(self.position, Instructions{ .reserveStack = size });
        self.position += 1;
    }

    pub fn decreaseStack(self: *Builder, _type: Type) !void {
        try self.code.insert(self.position, Instructions{ .decreaseStack = _type });
        self.position += 1;
    }

    pub fn writeArrayElement(self: *Builder, arr: Location, idx: usize, value: Location, _type: Type) !void {
        try self.code.insert(self.position, Instructions{ .writeArrayElement = .{ .arr = arr, .idx = idx, .value = value, ._type = _type } });
        self.position += 1;
    }

    pub fn getBasePointer(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .getBasePointer = loc });
        self.position += 1;
    }

    pub fn getStackPointer(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .getStackPointer = loc });
        self.position += 1;
    }

    pub fn comment(self: *Builder, com: []const u8) !void {
        try self.code.insert(self.position, Instructions{ .Comment = com });
        self.position += 1;
    }

    pub fn conditionalJump(self: *Builder, val: Location, label: []const u8) !void {
        try self.code.insert(self.position, Instructions{ .ConditionalJump = .{ .value = val, .label = label } });
        self.position += 1;
    }

    pub fn jump(self: *Builder, label: []const u8) !void {
        try self.code.insert(self.position, Instructions{ .Jump = .{ .label = label } });
        self.position += 1;
    }

    pub fn functionDec(self: *Builder, name: []const u8) !void {
        try self.code.insert(self.position, Instructions{ .Function = name });
        self.position += 1;
    }

    pub fn beginFunction(self: *Builder) !void {
        try self.code.insert(self.position, Instructions{ .BeginFunction = {} });
        self.position += 1;
    }

    pub fn endFunction(self: *Builder) !void {
        try self.code.insert(self.position, Instructions{ .EndFunction = {} });
        self.position += 1;
    }

    pub fn returnInst(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .Return = loc });
        self.position += 1;
    }

    pub fn moveInst(self: *Builder, from: Location, to: Location, _type: Type) !void {
        try self.code.insert(self.position, Instructions{ .Move = .{ .from = from, .to = to, ._type = _type } });
        self.position += 1;
    }

    pub fn load(self: *Builder, from: Location, to: Location) !void {
        try self.code.insert(self.position, Instructions{ .Load = .{ .from = from, .to = to } });
        self.position += 1;
    }

    pub fn loadAddress(self: *Builder, from: Location, to: Location) !void {
        try self.code.insert(self.position, Instructions{ .loadAddress = .{ .from = from, .to = to } });
        self.position += 1;
    }

    pub fn intLit(self: *Builder, val: i64, to: Location) !void {
        try self.code.insert(self.position, Instructions{ .IntLit = .{ .val = val, .to = to } });
        self.position += 1;
    }

    pub fn charLit(self: *Builder, val: u8, to: Location) !void {
        try self.code.insert(self.position, Instructions{ .CharLit = .{ .val = val, .to = to } });
        self.position += 1;
    }

    pub fn exitWith(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .ExitWith = loc });
        self.position += 1;
    }

    pub fn print(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .Print = loc });
        self.position += 1;
    }

    pub fn plus(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .Plus = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn addImmediate(self: *Builder, x: Location, y: i64) !void {
        try self.code.insert(self.position, Instructions{ .addImmediate = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn minus(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .Minus = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn multiply(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .Multiply = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn divide(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .Divide = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn modulo(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .Modulo = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn not(self: *Builder, x: Location) !void {
        try self.code.insert(self.position, Instructions{ .Not = x });
        self.position += 1;
    }

    pub fn equal(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .Equal = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn notEqual(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .NotEqual = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn lessThan(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .LessThan = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn lessEqual(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .LessEqual = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn greaterThan(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .GreaterThan = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn greaterEqual(self: *Builder, x: Location, y: Location) !void {
        try self.code.insert(self.position, Instructions{ .GreaterEqual = .{ .x = x, .y = y } });
        self.position += 1;
    }

    pub fn funcall(self: *Builder, func: Location, args: Array(Location)) !void {
        try self.code.insert(self.position, Instructions{ .Funcall = .{ .func = func, .args = args } });
        self.position += 1;
    }

    pub fn declareVariable(self: *Builder, name: []const u8, content: *const varContentType) !void {
        try self.code.insert(self.position, Instructions{ .declareVariable = .{ .name = name, .content = content.* } });
        self.position += 1;
    }

    pub fn beginVariableSection(self: *Builder) !void {
        try self.code.insert(self.position, Instructions{ .beginVariableSection = {} });
        self.position += 1;
    }

    pub fn labelDec(self: *Builder, name: []const u8) !void {
        try self.code.insert(self.position, Instructions{ .Label = name });
        self.position += 1;
    }

    pub fn heapAlloc(self: *Builder, size: i64, dest: Location) !void {
        try self.code.insert(self.position, Instructions{ .heapAlloc = .{ .dest = dest, .size = size } });
        self.position += 1;
    }

    pub fn heapFree(self: *Builder, dest: Location) !void {
        try self.code.insert(self.position, Instructions{ .heapFree = dest });
        self.position += 1;
    }

    pub fn writeToPointer(self: *Builder, dest: Location, decal: i64, content: Location) !void {
        try self.code.insert(self.position, Instructions{ .writeToPointer = .{ .content = content, .decal = decal, .dest = dest } });
        self.position += 1;
    }

    pub fn readFromPointer(self: *Builder, dest: Location, decal: i64, origin: Location) !void {
        try self.code.insert(self.position, Instructions{ .readFromPointer = .{ .dest = dest, .decal = decal, .origin = origin } });
        self.position += 1;
    }

    pub fn incrementeReferenceCounter(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .incrementeReferenceCounter = loc });
        self.position += 1;
    }

    pub fn decrementReferenceCounter(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .decrementReferenceCounter = loc });
        self.position += 1;
    }

    pub fn pushValue(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .pushValue = loc });
        self.position += 1;
    }

    pub fn popValue(self: *Builder, loc: Location) !void {
        try self.code.insert(self.position, Instructions{ .popValue = loc });
        self.position += 1;
    }
};

pub const builderPos = union(enum) {
    index: usize,
    last: void,
};

pub const FuncContexte = struct {
    vars: std.StringArrayHashMap(Value),
};
