const std = @import("std");

const Prog = std.ArrayList(Instructions);
const InstArray = std.ArrayList(Instructions);
const Array = std.ArrayList;
const HashMap = std.StringArrayHashMap(Value);
const Allocator = std.mem.Allocator;

pub const Location = union(enum) {
    stack: i64,
    register: Registers,
    label: []const u8,
    void: void,
};

pub const Registers = enum { r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15 };

pub const RegIter = [_]Registers{ .r0, .r1, .r2, .r3, .r4, .r5, .r6, .r7, .r8, .r9, .r10, .r11, .r12, .r13, .r14, .r15 };

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
        .r15 => @intCast(15),
    };
}

pub const RegisterTable = struct {
    utilised: [16]bool,

    pub fn init() RegisterTable {
        return RegisterTable{ .utilised = [_]bool{false} ** 16 };
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
    voidType: void,
    intType: void,
    stringType: void,
    charType: void,
    pointer: *Type,
    declarefunc: void,
    structType: struct {
        habitants: []const Type,
    },
};

pub const Instructions = union(enum) {
    Plus: struct {
        x: Location,
        y: Location,
    },
    Minus: struct {
        x: Location,
        y: Location,
    },
    //Multiply: struct {
    //    x: Location,
    //    y: Location,
    //},
    //Divide: struct {
    //    x: Location,
    //    y: Location,
    //},
    //Modulo: struct {
    //    x: Location,
    //    y: Location,
    //},
    //Equal: struct {
    //    x: Location,
    //    y: Location,
    //},
    //NotEqual: struct {
    //    x: Location,
    //    y: Location,
    //},
    //LessThan: struct {
    //    x: Location,
    //    y: Location,
    //},
    //LessEqual: struct {
    //    x: Location,
    //    y: Location,
    //},
    //GreaterThan: struct {
    //    x: Location,
    //    y: Location,
    //},
    //GreaterEqual: struct {
    //    x: Location,
    //    y: Location,
    //},
    reserveStack: i64,
    Return: Location,
    Function: []const u8,
    Move: struct { from: Location, to: Location },
    IntLit: struct { val: i64, to: Location },
    ExitWith: Location,
    Comment: []const u8,
    Funcall: struct {
        func: Location,
        args: Array(Location),
    },

    pub fn print(self: *const Instructions) void {
        switch (self.*) {
            .Plus => |inst| std.debug.print("\tPlus({}, {})\n", .{ inst.x, inst.y }),
            .Minus => |inst| std.debug.print("\tMinus({}, {})\n", .{ inst.x, inst.y }),
            .reserveStack => |inst| std.debug.print("\tReserveStack({d})\n", .{inst}),
            .Return => |inst| std.debug.print("\tReturn({})\n", .{inst}),
            .Function => |inst| std.debug.print("Function {s}\n", .{inst}),
            .Move => |inst| std.debug.print("\tMove {} to {}\n", .{ inst.from, inst.to }),
            .IntLit => |inst| std.debug.print("\tIntlit {d} in {}\n", .{ inst.val, inst.to }),
            .ExitWith => |inst| std.debug.print("\tExit({})\n", .{inst}),
            .Comment => |comment| std.debug.print("\t// {s}\n", .{comment}),
            .Funcall => |func| {
                std.debug.print("\tCall {}(", .{func.func});
                for (func.args.items) |a| {
                    std.debug.print("{}, ", .{a});
                }
                std.debug.print(")\n", .{});
            },
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

    pub fn comment(self: *Builder, com: []const u8) !void {
        try self.code.append(Instructions{ .Comment = com });
    }

    pub fn functionDec(self: *Builder, name: []const u8) !void {
        try self.code.append(Instructions{ .Function = name });
    }

    pub fn returnInst(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .Return = loc });
    }

    pub fn moveInst(self: *Builder, from: Location, to: Location) !void {
        try self.code.append(Instructions{ .Move = .{ .from = from, .to = to } });
    }

    pub fn intLit(self: *Builder, val: i64, to: Location) !void {
        try self.code.append(Instructions{ .IntLit = .{ .val = val, .to = to } });
    }

    pub fn exitWith(self: *Builder, loc: Location) !void {
        try self.code.append(Instructions{ .ExitWith = loc });
    }

    pub fn plus(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Plus = .{ .x = x, .y = y } });
    }

    pub fn minus(self: *Builder, x: Location, y: Location) !void {
        try self.code.append(Instructions{ .Minus = .{ .x = x, .y = y } });
    }

    pub fn funcall(self: *Builder, func: Location, args: Array(Location)) !void {
        try self.code.append(Instructions{ .Funcall = .{ .func = func, .args = args } });
    }
};

pub const FuncContexte = struct {
    vars: std.StringArrayHashMap(Value),
};
