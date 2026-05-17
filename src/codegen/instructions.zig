const std = @import("std");

const FileWriter = std.io.Writer(std.fs.File, std.fs.File.WriteError, std.fs.File.write);
const Allocator = std.mem.Allocator;

const ArchConfig = @import("arch.zig").ArchConfig;

/// Architecture selector used throughout the codegen pipeline.
pub const ArchTarget = enum { x86_64, arm64 };

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
            .R0  => "rax",
            .R1  => "rbx",
            .R2  => "rcx",
            .R3  => "rdx",
            .R4  => "rsi",
            .R5  => "rdi",
            .R6  => "r8",
            .R7  => "r9",
            .R8  => "r10",
            .R9  => "r11",
            .R10 => "r12",
            .R11 => "r13",
            .R12 => "r14",
            .R13 => "r15",
            .RBP => "rbp",
            .RSP => "rsp",
            .RNULL => "",
        };
    }

    pub fn arm64Reg(self: Register) []const u8 {
        return switch (self) {
            .R0  => "x9",   // BBC return accumulator (caller-saved scratch, NOT ABI x0)
            .R1  => "x19",  // callee-saved GC scratch
            .R2  => "x2",
            .R3  => "x3",
            .R4  => "x1",   // ABI arg 1
            .R5  => "x0",   // ABI arg 0 (also ABI return register)
            .R6  => "x4",
            .R7  => "x5",
            .R8  => "x6",
            .R9  => "x7",
            .R10 => "x20",  // callee-saved (not used by allocator)
            .R11 => "x21",
            .R12 => "x22",
            .R13 => "x23",
            .RBP => "x29",  // frame pointer
            .RSP => "sp",
            .RNULL => "",
        };
    }

    /// Return the physical register name for the given architecture.
    pub fn regName(self: Register, target: ArchTarget) []const u8 {
        return switch (target) {
            .x86_64 => self.x86Reg(),
            .arm64  => self.arm64Reg(),
        };
    }

    pub fn from(i: u64) Register {
        return @enumFromInt(i);
    }
};

// ── x86-64 legacy top-level constants (still used by callers not yet migrated) ──
pub const SCRATCH_REGS = [_]Register{ .R9, .R8, .R7, .R6, .R4, .R5, .R3, .R2 };
pub const ARGUMENT_REGS = [_]Register{ .R5, .R4, .R3, .R2, .R6, .R7 };
pub const CALLER_SAVED = [_]Register{ .R2, .R3, .R4, .R5, .R6, .R7, .R8, .R9 };

pub const CodeCondition = enum {
    O, NO,
    B, C, NAE,
    AE, NB, NC,
    E, Z,
    NE, NZ,
    BE, NA,
    A, NBE,
    N, S,
    P, PE,
    NP, PO,
    L, G,
    NL, NG,
    GE, LE,
    NGE, NLE,
};

const StatusFlags = enum { CF, PF, AF, ZF, SF, OF };

const RegisterOffset = struct {
    register: Register,
    offset: i64,
};

pub const SIB_Address = struct {
    base: Register,
    index: Register,
    scale: u8,
    disp: i64,
};

const Dval = union(enum) {
    register: Register,
    registerOffset: RegisterOffset,
    label: []const u8,
    immediate: i64,
    rip_memory: []const u8,
    sib: SIB_Address,

    /// x86-64 only: return a NASM operand string.
    pub fn toAsm(self: *const Dval, alloc: Allocator) ![]const u8 {
        return switch (self.*) {
            .register        => |r| r.x86Reg(),
            .registerOffset  => |ro| try std.fmt.allocPrint(alloc, "[{s} + {d}]", .{ ro.register.x86Reg(), ro.offset }),
            .label           => |l| l,
            .immediate       => |i| try std.fmt.allocPrint(alloc, "{d}", .{i}),
            .rip_memory      => |n| try std.fmt.allocPrint(alloc, "QWORD [rel {s}]", .{n}),
            .sib             => |s| try std.fmt.allocPrint(alloc, "QWORD [{s} + {s}*{d} + {d}]", .{ s.base.x86Reg(), s.index.x86Reg(), s.scale, s.disp }),
        };
    }
};

const Dadr = union(enum) {
    registerOffset: RegisterOffset,
    label: []const u8,
    rip_memory: []const u8,

    /// x86-64 only: return a NASM destination-address string.
    pub fn toAsm(self: *const Dadr, alloc: Allocator) ![]const u8 {
        return switch (self.*) {
            .registerOffset => |ro| {
                if (ro.offset >= 0) {
                    return try std.fmt.allocPrint(alloc, "QWORD [{s} + {d}]", .{ ro.register.x86Reg(), ro.offset });
                } else {
                    return try std.fmt.allocPrint(alloc, "QWORD [{s} - {d}]", .{ ro.register.x86Reg(), -ro.offset });
                }
            },
            .label      => |l| try std.fmt.allocPrint(alloc, "[{s}]", .{l}),
            .rip_memory => |n| try std.fmt.allocPrint(alloc, "QWORD [rel {s}]", .{n}),
        };
    }
};

pub fn registerOffsetValue(reg: Register, offset: u64) Dval {
    return Dval{ .registerOffset = .{ .offset = offset, .register = reg } };
}

pub fn registerOffsetAdress(reg: Register, offset: u64) Dadr {
    return Dadr{ .registerOffset = .{ .offset = offset, .register = reg } };
}

/// Map a CodeCondition to an ARM64 conditional branch mnemonic.
fn arm64BranchMnemonic(cc: CodeCondition) []const u8 {
    return switch (cc) {
        .E,   .Z   => "b.eq",
        .NE,  .NZ  => "b.ne",
        .L,   .NGE => "b.lt",
        .G,   .NLE => "b.gt",
        .GE,  .NL  => "b.ge",
        .LE,  .NG  => "b.le",
        .B,   .C,  .NAE => "b.lo",
        .AE,  .NB, .NC  => "b.hs",
        .BE,  .NA  => "b.ls",
        .A,   .NBE => "b.hi",
        .S    => "b.mi",
        .N    => "b.pl",
        .O    => "b.vs",
        .NO   => "b.vc",
        .P,   .PE  => "b.ne", // parity — approximate
        .NP,  .PO  => "b.eq",
    };
}

pub const Instruction = union(enum) {
    comment: []const u8,
    load: LOAD,
    store: STORE,
    lea: LEA,
    label: LABEL,
    ret: RET,
    call: CALL,
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
    alloc: ALLOC,
    dealloc: DEALLOC,
    xor: XOR,
    shl: SHL,
    imul: IMUL,
    cqo: CQO,
    idiv_mem: IDIV_MEM,
    idiv: IDIV,
    gc_inc: GC_INC,
    gc_dec_no_check: GC_DEC_NO_CHECK,
    gc_dec: GC_DEC,
    float_lit: FLOAT_LIT,
    float_binop: FLOAT_BINOP,
    float_cmp_flags: FLOAT_CMP_FLAGS,
    float_print: FLOAT_PRINT,
    /// Arch-specific frame prologue: x86-64 = `push rbp; mov rbp, rsp`; ARM64 = `stp x29, x30, [sp, #-16]!; mov x29, sp`
    frame_enter: FRAME_ENTER,
    /// Arch-specific frame epilogue: x86-64 = `pop rbp; ret`; ARM64 = `ldp x29, x30, [sp], #16; ret`
    frame_leave: FRAME_LEAVE,

    pub fn toAsm(self: *const Instruction, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (self.*) {
            .comment         => |c| try writer.print("\t; {s}", .{c}),
            .load            => |p| try p.toAsm(writer, alloc, arch),
            .store           => |p| try p.toAsm(writer, alloc, arch),
            .lea             => |p| try p.toAsm(writer, alloc, arch),
            .label           => |p| try p.toAsm(writer, alloc, arch),
            .ret             => |p| try p.toAsm(writer, alloc, arch),
            .call            => |p| try p.toAsm(writer, alloc, arch),
            .exit            => |p| try p.toAsm(writer, alloc, arch),
            .pop             => |p| try p.toAsm(writer, alloc, arch),
            .push            => |p| try p.toAsm(writer, alloc, arch),
            .jcond           => |p| try p.toAsm(writer, alloc, arch),
            .jmp             => |p| try p.toAsm(writer, alloc, arch),
            .cmp             => |p| try p.toAsm(writer, alloc, arch),
            .plus            => |p| try p.toAsm(writer, alloc, arch),
            .minus           => |p| try p.toAsm(writer, alloc, arch),
            .inc             => |p| try p.toAsm(writer, alloc, arch),
            .dec             => |p| try p.toAsm(writer, alloc, arch),
            .alloc           => |p| try p.toAsm(writer, alloc, arch),
            .dealloc         => |p| try p.toAsm(writer, alloc, arch),
            .xor             => |p| try p.toAsm(writer, alloc, arch),
            .shl             => |p| try p.toAsm(writer, alloc, arch),
            .imul            => |p| try p.toAsm(writer, alloc, arch),
            .cqo             => |p| try p.toAsm(writer, alloc, arch),
            .idiv_mem        => |p| try p.toAsm(writer, alloc, arch),
            .idiv            => |p| try p.toAsm(writer, alloc, arch),
            .gc_inc          => |p| try p.toAsm(writer, alloc, arch),
            .gc_dec_no_check => |p| try p.toAsm(writer, alloc, arch),
            .gc_dec          => |p| try p.toAsm(writer, alloc, arch),
            .float_lit       => |p| try p.toAsm(writer, alloc, arch),
            .float_binop     => |p| try p.toAsm(writer, alloc, arch),
            .float_cmp_flags => |p| try p.toAsm(writer, alloc, arch),
            .float_print     => |p| try p.toAsm(writer, alloc, arch),
            .frame_enter     => |p| try p.toAsm(writer, alloc, arch),
            .frame_leave     => |p| try p.toAsm(writer, alloc, arch),
        }
        _ = try writer.write("\n");
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Bitwise / shift
// ─────────────────────────────────────────────────────────────────────────────

pub const XOR = struct {
    lhs: Register,
    rhs: Dval,

    pub fn toAsm(self: *const XOR, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\txor {s}, {s}", .{ self.lhs.x86Reg(), try self.rhs.toAsm(alloc) }),
            .arm64  => {
                switch (self.rhs) {
                    .register  => |r| try writer.print("\teor {s}, {s}, {s}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), r.arm64Reg() }),
                    .immediate => |i| try writer.print("\teor {s}, {s}, #{d}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), i }),
                    else       => try writer.print("\t; TODO: XOR.rhs on ARM64\n", .{}),
                }
            },
        }
    }
};

pub const SHL = struct {
    lhs: Register,
    imm: u6,

    pub fn toAsm(self: *const SHL, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tshl {s}, {d}", .{ self.lhs.x86Reg(), self.imm }),
            .arm64  => try writer.print("\tlsl {s}, {s}, #{d}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), self.imm }),
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Memory: LOAD, STORE, LEA
// ─────────────────────────────────────────────────────────────────────────────

pub const LOAD = struct {
    from: Dval,
    to: Register,

    pub fn toAsm(self: *const LOAD, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tmov {s}, {s}", .{ self.to.x86Reg(), try self.from.toAsm(alloc) }),
            .arm64  => {
                const dst = self.to.arm64Reg();
                switch (self.from) {
                    .register => |r| {
                        if (r == self.to) return; // mov x, x is a no-op
                        try writer.print("\tmov {s}, {s}", .{ dst, r.arm64Reg() });
                    },
                    .immediate => |imm| {
                        if (imm == 0) {
                            try writer.print("\tmov {s}, xzr", .{dst});
                        } else if (imm >= 0 and imm <= 0xFFFF) {
                            try writer.print("\tmov {s}, #{d}", .{ dst, imm });
                        } else {
                            // Use movz + movk for large immediates
                            const u: u64 = @bitCast(imm);
                            try writer.print("\tmovz {s}, #{d}, lsl #0", .{ dst, u & 0xFFFF });
                            if ((u >> 16) & 0xFFFF != 0)
                                try writer.print("\n\tmovk {s}, #{d}, lsl #16", .{ dst, (u >> 16) & 0xFFFF });
                            if ((u >> 32) & 0xFFFF != 0)
                                try writer.print("\n\tmovk {s}, #{d}, lsl #32", .{ dst, (u >> 32) & 0xFFFF });
                            if ((u >> 48) & 0xFFFF != 0)
                                try writer.print("\n\tmovk {s}, #{d}, lsl #48", .{ dst, (u >> 48) & 0xFFFF });
                        }
                    },
                    .registerOffset => |ro| {
                        if (ro.offset >= 0) {
                            try writer.print("\tldr {s}, [{s}, #{d}]", .{ dst, ro.register.arm64Reg(), ro.offset });
                        } else {
                            try writer.print("\tldur {s}, [{s}, #{d}]", .{ dst, ro.register.arm64Reg(), ro.offset });
                        }
                    },
                    .rip_memory => |label| {
                        // adrp dst, label@PAGE  ;  ldr dst, [dst, label@PAGEOFF]
                        try writer.print("\tadrp {s}, {s}@PAGE\n", .{ dst, label });
                        try writer.print("\tldr {s}, [{s}, {s}@PAGEOFF]", .{ dst, dst, label });
                    },
                    .sib => |s| {
                        // add dst, base, index, lsl #log2(scale)  ;  ldr dst, [dst, #disp]
                        const shift: u6 = switch (s.scale) {
                            1 => 0, 2 => 1, 4 => 2, 8 => 3, else => 3,
                        };
                        try writer.print("\tadd {s}, {s}, {s}, lsl #{d}\n", .{ dst, s.base.arm64Reg(), s.index.arm64Reg(), shift });
                        if (s.disp != 0) {
                            try writer.print("\tldr {s}, [{s}, #{d}]", .{ dst, dst, s.disp });
                        } else {
                            try writer.print("\tldr {s}, [{s}]", .{ dst, dst });
                        }
                    },
                    .label => |l| {
                        try writer.print("\tadrp {s}, {s}@PAGE\n", .{ dst, l });
                        try writer.print("\tadd {s}, {s}, {s}@PAGEOFF", .{ dst, dst, l });
                    },
                }
            },
        }
    }
};

pub const STORE = struct {
    from: Dval,
    to: Dadr,

    pub fn toAsm(self: *const STORE, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tmov {s}, {s}", .{ try self.to.toAsm(alloc), try self.from.toAsm(alloc) }),
            .arm64  => {
                // Determine source register (or load immediate into x17 scratch).
                var src_reg: []const u8 = undefined;
                switch (self.from) {
                    .register  => |r| src_reg = r.arm64Reg(),
                    .immediate => |imm| {
                        if (imm == 0) {
                            src_reg = "xzr";
                        } else {
                            try writer.print("\tmov x17, #{d}\n", .{imm});
                            src_reg = "x17";
                        }
                    },
                    else => {
                        // Complex from — load via x17
                        try writer.print("\t; TODO: complex STORE.from on ARM64\n", .{});
                        src_reg = "x17";
                    },
                }
                switch (self.to) {
                    .registerOffset => |ro| {
                        if (ro.offset >= 0) {
                            try writer.print("\tstr {s}, [{s}, #{d}]", .{ src_reg, ro.register.arm64Reg(), ro.offset });
                        } else {
                            try writer.print("\tstur {s}, [{s}, #{d}]", .{ src_reg, ro.register.arm64Reg(), ro.offset });
                        }
                    },
                    .rip_memory => |label| {
                        try writer.print("\tadrp x16, {s}@PAGE\n", .{label});
                        try writer.print("\tstr {s}, [x16, {s}@PAGEOFF]", .{ src_reg, label });
                    },
                    .label => |label| {
                        try writer.print("\tadrp x16, {s}@PAGE\n", .{label});
                        try writer.print("\tstr {s}, [x16, {s}@PAGEOFF]", .{ src_reg, label });
                    },
                }
            },
        }
    }
};

pub const LEA = struct {
    adr: Dadr,
    to: Register,

    pub fn toAsm(self: *const LEA, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tlea {s}, {s}", .{ self.to.x86Reg(), try self.adr.toAsm(alloc) }),
            .arm64  => {
                const dst = self.to.arm64Reg();
                switch (self.adr) {
                    .registerOffset => |ro| {
                        if (ro.offset >= 0) {
                            try writer.print("\tadd {s}, {s}, #{d}", .{ dst, ro.register.arm64Reg(), ro.offset });
                        } else {
                            try writer.print("\tsub {s}, {s}, #{d}", .{ dst, ro.register.arm64Reg(), -ro.offset });
                        }
                    },
                    .label => |l| {
                        try writer.print("\tadrp {s}, {s}@PAGE\n", .{ dst, l });
                        try writer.print("\tadd {s}, {s}, {s}@PAGEOFF", .{ dst, dst, l });
                    },
                    .rip_memory => |l| {
                        try writer.print("\tadrp {s}, {s}@PAGE\n", .{ dst, l });
                        try writer.print("\tadd {s}, {s}, {s}@PAGEOFF", .{ dst, dst, l });
                    },
                }
            },
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Control flow
// ─────────────────────────────────────────────────────────────────────────────

pub const LABEL = struct {
    name: []const u8,

    pub fn toAsm(self: *const LABEL, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc; _ = arch;
        try writer.print("{s}:", .{self.name});
    }
};

pub const RET = struct {
    pub fn toAsm(self: *const RET, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = self; _ = alloc; _ = arch;
        try writer.print("\tret", .{});
    }
};

pub const CALL = struct {
    value: Dval,

    pub fn toAsm(self: *const CALL, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tcall {s}", .{ try self.value.toAsm(alloc) }),
            .arm64  => {
                // After every call, copy ABI return (x0) into BBC accumulator (x9=R0)
                // so that load-from-R0 patterns work identically to x86-64 (rax=R0).
                switch (self.value) {
                    .label         => |l| try writer.print("\tbl {s}\n\tmov x9, x0", .{l}),
                    .register      => |r| try writer.print("\tblr {s}\n\tmov x9, x0", .{r.arm64Reg()}),
                    .registerOffset => |ro| {
                        // indirect call through memory — load into x17 first
                        if (ro.offset >= 0) {
                            try writer.print("\tldr x17, [{s}, #{d}]\n", .{ ro.register.arm64Reg(), ro.offset });
                        } else {
                            try writer.print("\tldur x17, [{s}, #{d}]\n", .{ ro.register.arm64Reg(), ro.offset });
                        }
                        try writer.print("\tblr x17\n\tmov x9, x0", .{});
                    },
                    else => try writer.print("\t; TODO: complex CALL.value on ARM64\n", .{}),
                }
            },
        }
    }
};

pub const EXIT = struct {
    code: Dval,

    pub fn toAsm(self: *const EXIT, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tmov rdi, {s}\n", .{ try self.code.toAsm(alloc) });
                try writer.print("\tmov rax, 0x2000001\n", .{});
                try writer.print("\tsyscall\n", .{});
            },
            .arm64  => {
                switch (self.code) {
                    .immediate => |i| try writer.print("\tmov x0, #{d}\n", .{i}),
                    .register  => |r| {
                        if (r.arm64Reg().len > 0 and r != .R5)
                            try writer.print("\tmov x0, {s}\n", .{r.arm64Reg()});
                    },
                    else => try writer.print("\tmov x0, #0\n", .{}),
                }
                try writer.print("\tmov x16, #1\n", .{});
                try writer.print("\tsvc #0x80\n", .{});
            },
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Stack: PUSH, POP
// ─────────────────────────────────────────────────────────────────────────────

pub const PUSH = struct {
    reg: Register,

    pub fn toAsm(self: *const PUSH, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tpush {s}", .{self.reg.x86Reg()}),
            // ARM64: reserve 16 bytes, store register at [sp].
            .arm64  => try writer.print("\tstr {s}, [sp, #-16]!", .{self.reg.arm64Reg()}),
        }
    }
};

pub const POP = struct {
    reg: Register,

    pub fn toAsm(self: *const POP, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tpop {s}", .{self.reg.x86Reg()}),
            .arm64  => try writer.print("\tldr {s}, [sp], #16", .{self.reg.arm64Reg()}),
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Comparisons & branches
// ─────────────────────────────────────────────────────────────────────────────

pub const CMP = struct {
    val1: Dval,
    val2: Dval,

    pub fn toAsm(self: *const CMP, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tcmp {s}, {s}", .{ try self.val1.toAsm(alloc), try self.val2.toAsm(alloc) }),
            .arm64  => {
                // Load val1 into a register if needed (rip_memory → x17)
                const lhs: []const u8 = switch (self.val1) {
                    .register      => |r| r.arm64Reg(),
                    .rip_memory    => |label| blk: {
                        try writer.print("\tadrp x17, {s}@PAGE\n", .{label});
                        try writer.print("\tldr x17, [x17, {s}@PAGEOFF]\n", .{label});
                        break :blk "x17";
                    },
                    .immediate     => |i| blk: {
                        try writer.print("\tmov x17, #{d}\n", .{i});
                        break :blk "x17";
                    },
                    else => blk: {
                        try writer.print("\t; TODO: complex CMP val1 on ARM64\n", .{});
                        break :blk "x17";
                    },
                };
                switch (self.val2) {
                    .immediate => |i| try writer.print("\tcmp {s}, #{d}", .{ lhs, i }),
                    .register  => |r| try writer.print("\tcmp {s}, {s}", .{ lhs, r.arm64Reg() }),
                    .registerOffset => |ro| {
                        if (ro.offset >= 0) {
                            try writer.print("\tldr x16, [{s}, #{d}]\n", .{ ro.register.arm64Reg(), ro.offset });
                        } else {
                            try writer.print("\tldur x16, [{s}, #{d}]\n", .{ ro.register.arm64Reg(), ro.offset });
                        }
                        try writer.print("\tcmp {s}, x16", .{lhs});
                    },
                    .rip_memory => |label| {
                        try writer.print("\tadrp x16, {s}@PAGE\n", .{label});
                        try writer.print("\tldr x16, [x16, {s}@PAGEOFF]\n", .{label});
                        try writer.print("\tcmp {s}, x16", .{lhs});
                    },
                    else => try writer.print("\t; TODO: complex CMP val2 on ARM64\n", .{}),
                }
            },
        }
    }
};

pub const JCond = struct {
    where: []const u8,
    cc: CodeCondition,

    pub fn toAsm(self: *const JCond, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tJ{s} {s}", .{ @tagName(self.cc), self.where }),
            .arm64  => try writer.print("\t{s} {s}", .{ arm64BranchMnemonic(self.cc), self.where }),
        }
    }
};

pub const JMP = struct {
    where: []const u8,

    pub fn toAsm(self: *const JMP, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tJMP {s}", .{self.where}),
            .arm64  => try writer.print("\tb {s}", .{self.where}),
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Arithmetic: PLUS, MINUS, INC, DEC
// ─────────────────────────────────────────────────────────────────────────────

pub const PLUS = struct {
    lhs: Register,
    rhs: Dval,

    pub fn toAsm(self: *const PLUS, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tadd {s}, {s}", .{ self.lhs.x86Reg(), try self.rhs.toAsm(alloc) }),
            .arm64  => {
                switch (self.rhs) {
                    .immediate => |i| try writer.print("\tadd {s}, {s}, #{d}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), i }),
                    .register  => |r| try writer.print("\tadd {s}, {s}, {s}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), r.arm64Reg() }),
                    else       => try writer.print("\t; TODO: PLUS.rhs on ARM64\n", .{}),
                }
            },
        }
    }
};

pub const MINUS = struct {
    lhs: Register,
    rhs: Dval,

    pub fn toAsm(self: *const MINUS, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\tsub {s}, {s}", .{ self.lhs.x86Reg(), try self.rhs.toAsm(alloc) }),
            .arm64  => {
                switch (self.rhs) {
                    .immediate => |i| try writer.print("\tsub {s}, {s}, #{d}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), i }),
                    .register  => |r| try writer.print("\tsub {s}, {s}, {s}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), r.arm64Reg() }),
                    else       => try writer.print("\t; TODO: MINUS.rhs on ARM64\n", .{}),
                }
            },
        }
    }
};

pub const DEC = struct {
    lhs: Register,

    pub fn toAsm(self: *const DEC, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tdec {s}", .{self.lhs.x86Reg()}),
            .arm64  => try writer.print("\tsub {s}, {s}, #1", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg() }),
        }
    }
};

pub const INC = struct {
    lhs: Register,

    pub fn toAsm(self: *const INC, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => try writer.print("\tinc {s}", .{self.lhs.x86Reg()}),
            .arm64  => try writer.print("\tadd {s}, {s}, #1", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg() }),
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Heap: ALLOC, DEALLOC
// ─────────────────────────────────────────────────────────────────────────────

/// Allocate `size` bytes on the heap via _malloc. Result lands in R0 (rax / x9).
pub const ALLOC = struct {
    size: usize,
    stack_size: usize,

    pub fn toAsm(self: *const ALLOC, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                if (self.stack_size % 2 != 0)
                    try writer.print("\tsub rsp, 8\n", .{});
                try writer.print("\tmov rdi, {d}\n", .{self.size});
                try writer.print("\tcall _malloc", .{});
                if (self.stack_size % 2 != 0)
                    try writer.print("\n\tadd rsp, 8", .{});
            },
            .arm64  => {
                // ARM64 sp is always 16-byte aligned — no padding needed.
                // Pass size in x0 (R5), call _malloc, copy ABI return (x0) → R0 (x9).
                try writer.print("\tmov x0, #{d}\n", .{self.size});
                try writer.print("\tbl _malloc\n", .{});
                try writer.print("\tmov x9, x0", .{}); // R0 = x9 = result
            },
        }
    }
};

/// Free a heap pointer via _free.
pub const DEALLOC = struct {
    ptr: Dval,
    stack_size: usize,

    pub fn toAsm(self: *const DEALLOC, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => {
                if (self.stack_size % 2 != 0)
                    try writer.print("\tsub rsp, 8\n", .{});
                try writer.print("\tmov rdi, {s}\n", .{try self.ptr.toAsm(alloc)});
                try writer.print("\tcall _free", .{});
                if (self.stack_size % 2 != 0)
                    try writer.print("\n\tadd rsp, 8", .{});
            },
            .arm64  => {
                switch (self.ptr) {
                    .register => |r| {
                        if (r != .R5)
                            try writer.print("\tmov x0, {s}\n", .{r.arm64Reg()});
                    },
                    else => try writer.print("\t; TODO: DEALLOC ptr on ARM64\n", .{}),
                }
                try writer.print("\tbl _free", .{});
            },
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Integer arithmetic: IMUL, CQO, IDIV_MEM (x86-64), IDIV (cross-arch)
// ─────────────────────────────────────────────────────────────────────────────

pub const IMUL = struct {
    lhs: Register,
    rhs: Dval,

    pub fn toAsm(self: *const IMUL, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        switch (arch.target) {
            .x86_64 => try writer.print("\timul {s}, {s}", .{ self.lhs.x86Reg(), try self.rhs.toAsm(alloc) }),
            .arm64  => {
                switch (self.rhs) {
                    .register => |r| try writer.print("\tmul {s}, {s}, {s}", .{ self.lhs.arm64Reg(), self.lhs.arm64Reg(), r.arm64Reg() }),
                    else      => try writer.print("\t; TODO: IMUL.rhs on ARM64\n", .{}),
                }
            },
        }
    }
};

/// Sign-extend rax into rdx:rax (x86-64 only; precedes IDIV_MEM).
pub const CQO = struct {
    pub fn toAsm(self: *const CQO, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = self; _ = alloc; _ = arch;
        try writer.print("\tcqo", .{});
    }
};

/// idiv QWORD [rsp] — x86-64 only; quotient → rax, remainder → rdx.
pub const IDIV_MEM = struct {
    pub fn toAsm(self: *const IDIV_MEM, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = self; _ = alloc; _ = arch;
        try writer.print("\tidiv QWORD [rsp]", .{});
    }
};

/// Cross-arch integer division.  x86-64 path is unreachable (use cqo+idiv_mem).
/// ARM64: quotient via sdiv (in lhs); modulo via sdiv+msub (in R3).
pub const IDIV = struct {
    lhs: Register,
    rhs: Register,
    is_modulo: bool,

    pub fn toAsm(self: *const IDIV, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => unreachable, // x86-64 uses cqo + idiv_mem in binop.zig
            .arm64  => {
                if (!self.is_modulo) {
                    // sdiv dst, lhs, rhs  → quotient in lhs
                    try writer.print("\tsdiv {s}, {s}, {s}", .{
                        self.lhs.arm64Reg(), self.lhs.arm64Reg(), self.rhs.arm64Reg(),
                    });
                } else {
                    // remainder = lhs - (lhs/rhs)*rhs
                    // Use R0 (x9) as quotient scratch (not in allocator's pool).
                    try writer.print("\tsdiv {s}, {s}, {s}\n", .{
                        Register.R0.arm64Reg(), self.lhs.arm64Reg(), self.rhs.arm64Reg(),
                    });
                    // msub R3, R0, rhs, lhs  → R3 = lhs - R0*rhs
                    try writer.print("\tmsub {s}, {s}, {s}, {s}", .{
                        Register.R3.arm64Reg(),
                        Register.R0.arm64Reg(),
                        self.rhs.arm64Reg(),
                        self.lhs.arm64Reg(),
                    });
                }
            },
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  GC: GC_INC, GC_DEC_NO_CHECK, GC_DEC
// ─────────────────────────────────────────────────────────────────────────────

/// Inline refcount increment: [ptr + 0] += 1.
/// x86-64: uses rbx (R1) as scratch.  ARM64: uses x19 (R1) as scratch.
pub const GC_INC = struct {
    ptr: Register,

    pub fn toAsm(self: *const GC_INC, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tmov rbx, [{s} + 0]\n", .{self.ptr.x86Reg()});
                try writer.print("\tinc rbx\n", .{});
                try writer.print("\tmov [{s} + 0], rbx", .{self.ptr.x86Reg()});
            },
            .arm64  => {
                try writer.print("\tldr x19, [{s}]\n", .{self.ptr.arm64Reg()});
                try writer.print("\tadd x19, x19, #1\n", .{});
                try writer.print("\tstr x19, [{s}]", .{self.ptr.arm64Reg()});
            },
        }
    }
};

/// Inline refcount decrement WITHOUT calling the destructor.
pub const GC_DEC_NO_CHECK = struct {
    ptr: Register,

    pub fn toAsm(self: *const GC_DEC_NO_CHECK, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tmov rbx, [{s} + 0]\n", .{self.ptr.x86Reg()});
                try writer.print("\tdec rbx\n", .{});
                try writer.print("\tmov [{s} + 0], rbx", .{self.ptr.x86Reg()});
            },
            .arm64  => {
                try writer.print("\tldr x19, [{s}]\n", .{self.ptr.arm64Reg()});
                try writer.print("\tsub x19, x19, #1\n", .{});
                try writer.print("\tstr x19, [{s}]", .{self.ptr.arm64Reg()});
            },
        }
    }
};

/// Decrement refcount via global.object.free (calls the type destructor if count hits 0).
/// x86-64: stack-based calling convention (push free_fn, push self, call).
/// ARM64: same stack convention but with 16-byte pushes (str [sp,#-16]!).
pub const GC_DEC = struct {
    ptr: Register,
    free_fn: []const u8,
    stack_size: usize,

    pub fn toAsm(self: *const GC_DEC, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                const needs_padding = self.stack_size % 2 != 0;
                if (needs_padding)
                    try writer.print("\tsub rsp, 8\n", .{});
                try writer.print("\tlea rbx, [{s}]\n", .{self.free_fn});
                try writer.print("\tpush rbx\n", .{});
                try writer.print("\tpush {s}\n", .{self.ptr.x86Reg()});
                try writer.print("\tcall global.object.free\n", .{});
                try writer.print("\tadd rsp, 16", .{});
                if (needs_padding)
                    try writer.print("\n\tadd rsp, 8", .{});
            },
            .arm64  => {
                // ARM64 sp is always 16-byte aligned — no padding check needed.
                // Push free_fn address (via adrp/add into x16 scratch), then push self.
                try writer.print("\tadrp x16, {s}@PAGE\n", .{self.free_fn});
                try writer.print("\tadd x16, x16, {s}@PAGEOFF\n", .{self.free_fn});
                try writer.print("\tstr x16, [sp, #-16]!\n", .{});    // push free_fn
                try writer.print("\tstr {s}, [sp, #-16]!\n", .{self.ptr.arm64Reg()}); // push self
                try writer.print("\tbl global.object.free\n", .{});
                try writer.print("\tadd sp, sp, #32", .{});
            },
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Float / SSE (x86-64) / NEON (ARM64)
// ─────────────────────────────────────────────────────────────────────────────

/// Load a float constant from the .data section into a GP register.
pub const FLOAT_LIT = struct {
    dst: Register,
    label: []const u8,

    pub fn toAsm(self: *const FLOAT_LIT, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tmovsd xmm0, [rel {s}]\n", .{self.label});
                try writer.print("\tmovq {s}, xmm0", .{self.dst.x86Reg()});
            },
            .arm64  => {
                // adrp x17, label@PAGE ; ldr d0, [x17, label@PAGEOFF] ; fmov dst, d0
                try writer.print("\tadrp x17, {s}@PAGE\n", .{self.label});
                try writer.print("\tldr d0, [x17, {s}@PAGEOFF]\n", .{self.label});
                try writer.print("\tfmov {s}, d0", .{self.dst.arm64Reg()});
            },
        }
    }
};

/// Binary float arithmetic. Trampolines through xmm0/xmm1 (x86-64) or d0/d1 (ARM64).
pub const FLOAT_BINOP = struct {
    pub const Op = enum { Add, Sub, Mul, Div };
    op: Op,
    lhs: Register,
    rhs: Register,

    pub fn toAsm(self: *const FLOAT_BINOP, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                const mnemonic: []const u8 = switch (self.op) {
                    .Add => "addsd", .Sub => "subsd", .Mul => "mulsd", .Div => "divsd",
                };
                try writer.print("\tmovq xmm0, {s}\n", .{self.lhs.x86Reg()});
                try writer.print("\tmovq xmm1, {s}\n", .{self.rhs.x86Reg()});
                try writer.print("\t{s} xmm0, xmm1\n", .{mnemonic});
                try writer.print("\tmovq {s}, xmm0", .{self.lhs.x86Reg()});
            },
            .arm64  => {
                const mnemonic: []const u8 = switch (self.op) {
                    .Add => "fadd", .Sub => "fsub", .Mul => "fmul", .Div => "fdiv",
                };
                try writer.print("\tfmov d0, {s}\n", .{self.lhs.arm64Reg()});
                try writer.print("\tfmov d1, {s}\n", .{self.rhs.arm64Reg()});
                try writer.print("\t{s} d0, d0, d1\n", .{mnemonic});
                try writer.print("\tfmov {s}, d0", .{self.lhs.arm64Reg()});
            },
        }
    }
};

/// Float comparison that sets CPU flags for a subsequent jcond.
pub const FLOAT_CMP_FLAGS = struct {
    lhs: Register,
    rhs: Register,

    pub fn toAsm(self: *const FLOAT_CMP_FLAGS, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tmovq xmm0, {s}\n", .{self.lhs.x86Reg()});
                try writer.print("\tmovq xmm1, {s}\n", .{self.rhs.x86Reg()});
                try writer.print("\tucomisd xmm0, xmm1", .{});
            },
            .arm64  => {
                try writer.print("\tfmov d0, {s}\n", .{self.lhs.arm64Reg()});
                try writer.print("\tfmov d1, {s}\n", .{self.rhs.arm64Reg()});
                try writer.print("\tfcmp d0, d1", .{});
            },
        }
    }
};

/// Print a float via printf.
pub const FLOAT_PRINT = struct {
    val: Register,
    fmt_label: []const u8,
    stack_size: usize,

    pub fn toAsm(self: *const FLOAT_PRINT, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                const needs_padding = self.stack_size % 2 != 0;
                if (needs_padding)
                    try writer.print("\tsub rsp, 8\n", .{});
                try writer.print("\tmovq xmm0, {s}\n", .{self.val.x86Reg()});
                try writer.print("\tlea rdi, [rel {s}]\n", .{self.fmt_label});
                try writer.print("\tmov al, 1\n", .{});
                try writer.print("\tcall _printf", .{});
                if (needs_padding)
                    try writer.print("\n\tadd rsp, 8", .{});
            },
            .arm64  => {
                // Apple ARM64: variadic args go on the stack. The float value is
                // already in a GP register (stored as IEEE-754 bits by FLOAT_LIT).
                // Push it at [sp] as a raw 8-byte value so printf's va_list sees it.
                try writer.print("\tstr {s}, [sp, #-16]!\n", .{self.val.arm64Reg()});
                try writer.print("\tadrp x0, {s}@PAGE\n", .{self.fmt_label});
                try writer.print("\tadd x0, x0, {s}@PAGEOFF\n", .{self.fmt_label});
                try writer.print("\tbl _printf\n", .{});
                try writer.print("\tadd sp, sp, #16", .{});
            },
        }
    }
};

// ─────────────────────────────────────────────────────────────────────────────
//  Frame prologue / epilogue helpers
// ─────────────────────────────────────────────────────────────────────────────

pub const FRAME_ENTER = struct {
    pub fn toAsm(self: *const FRAME_ENTER, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = self; _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tpush rbp\n", .{});
                try writer.print("\tmov rbp, rsp", .{});
            },
            .arm64 => {
                try writer.print("\tstp x29, x30, [sp, #-16]!\n", .{});
                try writer.print("\tmov x29, sp", .{});
            },
        }
    }
};

pub const FRAME_LEAVE = struct {
    /// emit_ret: when true, also emit the `ret` mnemonic.
    emit_ret: bool = true,
    pub fn toAsm(self: *const FRAME_LEAVE, writer: anytype, alloc: Allocator, arch: ArchConfig) !void {
        _ = alloc;
        switch (arch.target) {
            .x86_64 => {
                try writer.print("\tpop rbp", .{});
                if (self.emit_ret) try writer.print("\n\tret", .{});
            },
            .arm64 => {
                try writer.print("\tldp x29, x30, [sp], #16", .{});
                if (self.emit_ret) try writer.print("\n\tret", .{});
            },
        }
    }
};
