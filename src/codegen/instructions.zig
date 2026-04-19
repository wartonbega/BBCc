const std = @import("std");

const FileWriter = std.io.Writer(std.fs.File, std.fs.File.WriteError, std.fs.File.write);
const Allocator = std.mem.Allocator;

pub const Register = enum {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    RBP,
    RSP,
    RNULL,

    pub fn x86Reg(self: *const Register) []const u8 {
        return switch (self.*) {
            .R0 => "rax",
            .R1 => "rbx",
            .R2 => "rcx",
            .R3 => "rdx",
            .R4 => "rsi",
            .R5 => "rdi",
            .R6 => "r8",
            .R7 => "r9",
            .R8 => "r10",
            .R9 => "r11",
            .R10 => "r12",
            .R11 => "r13",
            .R12 => "r14",
            .R13 => "r15",
            .RBP => "rbp",
            .RSP => "rsp",
            .RNULL => "",
        };
    }

    pub fn from(i: u64) Register {
        return @enumFromInt(i);
    }
};

pub const SCRATCH_REGS = [_]Register{
    .R9, .R8, .R7, .R6,
    .R4, .R5, .R3, .R2,
};

pub const ARGUMENT_REGS = [_]Register{
    .R5, // rdi
    .R4, // rsi
    .R3, // rdx
    .R2, // rcx
    .R6, // r8
    .R7, // r9
};

pub const CALLER_SAVED = [_]Register{
    // R0 (rax) ignored because
    .R2, .R3, .R4, .R5, .R6, .R7, .R8, .R9,
};

const CodeCondition = enum {
    O,
    NO,

    B,
    C,
    NAE,
    AE,
    NB,
    NC,

    E,
    Z,
    NE,
    NZ,

    BE,
    NA,
    A,
    NBE,

    N,
    S,

    P,
    PE,
    NP,
    PO,

    L,
    G,

    NL,
    NG,

    GE,
    LE,

    NGE,
    NLE,
};

const StatusFlags = enum {
    CF,
    PF,
    AF,
    ZF,
    SF,
    OF,
};

const RegisterOffset = struct {
    register: Register,
    offset: i64,
};

const Dval = union(enum) {
    register: Register,
    registerOffset: RegisterOffset,
    label: []const u8,
    immediate: i64,

    pub fn toAsm(self: *const Dval, alloc: Allocator) ![]const u8 {
        return switch (self.*) {
            .register => |r| r.x86Reg(),
            .registerOffset => |ro| try std.fmt.allocPrint(alloc, "[{s} + {d}]", .{ ro.register.x86Reg(), ro.offset }),
            .label => |l| l,
            .immediate => |i| try std.fmt.allocPrint(alloc, "{d}", .{i}),
        };
    }
};

const Dadr = union(enum) {
    registerOffset: RegisterOffset,
    label: []const u8,

    pub fn toAsm(self: *const Dadr, alloc: Allocator) ![]const u8 {
        return switch (self.*) {
            .registerOffset => |ro| {
                if (ro.offset >= 0) {
                    return try std.fmt.allocPrint(alloc, "QWORD [{s} + {d}]", .{ ro.register.x86Reg(), ro.offset });
                } else {
                    return try std.fmt.allocPrint(alloc, "QWORD [{s} - {d}]", .{ ro.register.x86Reg(), -ro.offset });
                }
            },
            .label => |l| try std.fmt.allocPrint(alloc, "[{s}]", .{l}),
        };
    }
};

pub fn registerOffsetValue(reg: Register, offset: u64) Dval {
    return Dval{
        .registerOffset = .{
            .offset = offset,
            .register = reg,
        },
    };
}

pub fn registerOffsetAdress(reg: Register, offset: u64) Dadr {
    return Dadr{
        .registerOffset = .{
            .offset = offset,
            .register = reg,
        },
    };
}

pub const Instruction = union(enum) {
    inlineAsm: []const u8,
    comment: []const u8,
    load: LOAD,
    store: STORE,
    lea: LEA,
    label: LABEL,
    ret: RET,
    call: CALL,
    callwithargs: CALLWithArgs,
    generateFunctionFrame: GenerateFunctionFrame,
    exit: EXIT,
    pop: POP,
    push: PUSH,
    plus: PLUS,
    minus: MINUS,
    inc: INC,
    dec: DEC,
    cmp: CMP,
    jcond: JCond,
    jmp: JMP,
    malloc: MALLOC,
    free: FREE,

    pub fn toAsm(self: *const Instruction, writer: anytype, alloc: Allocator) !void {
        switch (self.*) {
            .inlineAsm => |i| _ = try writer.write(i),
            .comment => |c| try writer.print("\t; {s}", .{c}),
            .load => |p| try p.toAsm(writer, alloc),
            .store => |p| try p.toAsm(writer, alloc),
            .lea => |p| try p.toAsm(writer, alloc),
            .label => |p| try p.toAsm(writer, alloc),
            .ret => |p| try p.toAsm(writer, alloc),
            .call => |p| try p.toAsm(writer, alloc),
            .callwithargs => |p| try p.toAsm(writer, alloc),
            .generateFunctionFrame => |p| try p.toAsm(writer, alloc),
            .exit => |p| try p.toAsm(writer, alloc),
            .pop => |p| try p.toAsm(writer, alloc),
            .push => |p| try p.toAsm(writer, alloc),

            .jcond => |p| try p.toAsm(writer, alloc),
            .jmp => |p| try p.toAsm(writer, alloc),
            .cmp => |p| try p.toAsm(writer, alloc),

            .plus => |p| try p.toAsm(writer, alloc),
            .minus => |p| try p.toAsm(writer, alloc),
            .inc => |p| try p.toAsm(writer, alloc),
            .dec => |p| try p.toAsm(writer, alloc),

            .malloc => |p| try p.toAsm(writer, alloc),
            .free => |p| try p.toAsm(writer, alloc),
        }
        _ = try writer.write("\n");
    }
};

pub const LOAD = struct {
    from: Dval,
    to: Register,

    pub fn toAsm(self: *const LOAD, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tmov {s}, {s}", .{
            self.to.x86Reg(),
            try self.from.toAsm(alloc),
        });
    }
};

pub const STORE = struct {
    from: Dval,
    to: Dadr,

    pub fn toAsm(self: *const STORE, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tmov {s}, {s}", .{
            try self.to.toAsm(alloc),
            try self.from.toAsm(alloc),
        });
    }
};

pub const LEA = struct {
    adr: Dadr,
    to: Register,

    pub fn toAsm(self: *const LEA, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tlea {s}, {s}", .{
            self.to.x86Reg(),
            try self.adr.toAsm(alloc),
        });
    }
};

pub const LABEL = struct {
    name: []const u8,

    pub fn toAsm(self: *const LABEL, writer: anytype, alloc: Allocator) !void {
        try writer.print("{s}:", .{self.name});
        _ = alloc;
    }
};

pub const RET = struct {
    pub fn toAsm(self: *const RET, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tret", .{});
        _ = self;
        _ = alloc;
    }
};

pub const CALL = struct {
    value: Dval,

    pub fn toAsm(self: *const CALL, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tcall {s}", .{
            try self.value.toAsm(alloc),
        });
    }
};

pub const CALLWithArgs = struct {
    value: Dval,
    argnum: usize, // Total incluant self

    pub fn toAsm(self: *const CALLWithArgs, writer: anytype, alloc: Allocator) !void {
        const real_args = self.argnum;
        const args_in_regs = @min(real_args, ARGUMENT_REGS.len);

        // First the padding
        const stack_args = if (real_args > ARGUMENT_REGS.len)
            real_args - ARGUMENT_REGS.len
        else
            0;
        const total_stack = 8 + stack_args * @as(usize, 8);
        const needs_padding = (total_stack % 16) != 8;

        // Then the arguments pushed
        for (0..args_in_regs) |i| {
            const reg = ARGUMENT_REGS[i];
            const offset = (i + 1) * 8;
            try writer.print("\tmov {s}, [rsp + {d}]\n", .{ reg.x86Reg(), offset });
        }

        try writer.print("\tmov rax, [rsp]\n", .{});

        // Cleaning the arguments pushed
        try writer.print("\tadd rsp, {d}\n", .{(args_in_regs + 1) * @as(usize, 8)});

        // adding 'self' on top of the stack
        try writer.print("\tpush rax\n", .{});

        try writer.print("\tcall {s}\n", .{try self.value.toAsm(alloc)});

        // Finally cleanup
        var cleanup = @as(usize, 8);
        cleanup += stack_args * @as(usize, 8);
        cleanup += if (needs_padding) @as(usize, 8) else 0;

        if (cleanup > 0) {
            try writer.print("\tadd rsp, {d}\n", .{cleanup});
        }
    }
};

pub const GenerateFunctionFrame = struct {
    argnum: usize, // Total incluant self

    pub fn toAsm(self: *const GenerateFunctionFrame, writer: anytype, alloc: Allocator) !void {
        _ = alloc;

        try writer.print("\tpush rbp\n", .{});
        try writer.print("\tmov rbp, rsp\n", .{});

        const real_args = self.argnum;
        const args_in_regs = @min(real_args, ARGUMENT_REGS.len);
        const mult: usize = @intCast(8);
        const args_space = args_in_regs * mult;
        const total_space = args_space;

        const aligned_space = (total_space + 15) & ~@as(usize, 15);

        if (aligned_space > 0) {
            try writer.print("\tsub rsp, {d}\n", .{aligned_space});
        }

        for (0..args_in_regs) |i| {
            const reg = ARGUMENT_REGS[i];
            const offset = (i + 1) * 8;
            try writer.print("\tmov [rbp - {d}], {s}\n", .{ offset, reg.x86Reg() });
        }
    }
};
pub const EXIT = struct {
    code: Dval,

    pub fn toAsm(self: *const EXIT, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tmov rdi, {s}\n", .{
            try self.code.toAsm(alloc),
        });
        try writer.print("\tmov rax, 0x2000001\n", .{});
        try writer.print("\tsyscall\n", .{});
    }
};

pub const PUSH = struct {
    reg: Register,

    pub fn toAsm(self: *const PUSH, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tpush {s}", .{
            self.reg.x86Reg(),
        });
        _ = alloc;
    }
};

pub const POP = struct {
    reg: Register,

    pub fn toAsm(self: *const POP, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tpop {s}", .{
            self.reg.x86Reg(),
        });
        _ = alloc;
    }
};

pub const CMP = struct {
    val1: Dval,
    val2: Dval,

    pub fn toAsm(self: *const CMP, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tcmp {s}, {s}", .{
            try self.val1.toAsm(alloc),
            try self.val2.toAsm(alloc),
        });
    }
};

pub const JCond = struct {
    where: []const u8,
    cc: CodeCondition,

    pub fn toAsm(self: *const JCond, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tJ{s} {s}", .{
            @tagName(self.cc),
            self.where,
        });
        _ = alloc;
    }
};

pub const JMP = struct {
    where: []const u8,

    pub fn toAsm(self: *const JMP, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tJMP {s}", .{
            self.where,
        });
        _ = alloc;
    }
};

///////////////////////////////
//          OPERATORS        //
///////////////////////////////
pub const PLUS = struct {
    // lhs <- lhs + rhs
    lhs: Register,
    rhs: Dval,

    pub fn toAsm(self: *const PLUS, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tadd {s}, {s}", .{
            self.lhs.x86Reg(),
            try self.rhs.toAsm(alloc),
        });
    }
};

pub const MINUS = struct {
    // lhs <- lhs - rhs
    lhs: Register,
    rhs: Dval,

    pub fn toAsm(self: *const MINUS, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tsub {s}, {s}", .{
            self.lhs.x86Reg(),
            try self.rhs.toAsm(alloc),
        });
    }
};

pub const DEC = struct {
    // lhs <- lhs - 1
    lhs: Register,

    pub fn toAsm(self: *const DEC, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tdec {s}", .{
            self.lhs.x86Reg(),
        });
        _ = alloc;
    }
};

pub const INC = struct {
    // lhs <- lhs + 1
    lhs: Register,

    pub fn toAsm(self: *const INC, writer: anytype, alloc: Allocator) !void {
        try writer.print("\tinc {s}", .{
            self.lhs.x86Reg(),
        });
        _ = alloc;
    }
};

///////////////////////////////
//    STANDARD C FUNCTION    //
///////////////////////////////

pub const MALLOC = struct {
    size: usize,
    stack_size: usize,

    pub fn toAsm(self: *const MALLOC, writer: anytype, alloc: Allocator) !void {
        if (self.stack_size % 16 != 0)
            try writer.print("\tsub rsp, 8\n", .{});
        try writer.print("\tmov rsi, {d}\n", .{self.size});
        try writer.print("\tcall _malloc", .{});
        if (self.stack_size % 16 != 0)
            try writer.print("\n\tadd rsp, 8\n", .{});
        _ = alloc;
    }
};
pub const FREE = struct {
    value: Dval,
    stack_size: usize,

    pub fn toAsm(self: *const FREE, writer: anytype, alloc: Allocator) !void {
        if (self.stack_size % 16 != 0)
            try writer.print("\tsub rsp, 8\n", .{});
        try writer.print("\tmov rsi, {s}\n", .{try self.value.toAsm(alloc)});
        try writer.print("\tcall _malloc", .{});
        if (self.stack_size % 16 != 0)
            try writer.print("\n\tadd rsp, 8\n", .{});
    }
};
