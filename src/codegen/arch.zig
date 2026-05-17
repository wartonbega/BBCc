const inst = @import("instructions.zig");
const Register = inst.Register;
pub const ArchTarget = inst.ArchTarget;

const x86_64_argument_regs = [_]Register{ .R5, .R4, .R3, .R2, .R6, .R7 };
const x86_64_caller_saved  = [_]Register{ .R2, .R3, .R4, .R5, .R6, .R7, .R8, .R9 };
const x86_64_scratch_regs  = [_]Register{ .R9, .R8, .R7, .R6, .R4, .R5, .R3, .R2 };

// ARM64 (Apple Silicon / AArch64):
//   ARGUMENT_REGS map to x0-x7 via the register mapping in instructions.zig.
//   R5→x0 (arg0/ABI-return), R4→x1, R2→x2, R3→x3, R6→x4, R7→x5, R8→x6, R9→x7.
//   R0→x9 (BBC return accumulator, NOT the ABI return register).
//   After each `bl`, funcall.zig copies x0 (R5) into x9 (R0) so the existing
//   `lastReg / load R0` pattern works unchanged.
const arm64_argument_regs = [_]Register{ .R5, .R4, .R2, .R3, .R6, .R7, .R8, .R9 };
const arm64_caller_saved  = [_]Register{ .R0, .R2, .R3, .R4, .R5, .R6, .R7, .R8, .R9 };
const arm64_scratch_regs  = [_]Register{ .R9, .R8, .R7, .R6, .R4, .R5, .R3, .R2 };

pub const ArchConfig = struct {
    target: ArchTarget,

    /// Integer argument registers, ordered by ABI position (arg0 first).
    argument_regs: []const Register,
    /// Registers that the caller must save across a call.
    caller_saved: []const Register,
    /// Registers that the register allocator may freely assign.
    scratch_regs: []const Register,

    stack_alignment: usize,
    ptr_size: usize,

    /// Symbol name of the C-runtime entry point.
    entry_point: []const u8,

    /// Callee-saved register used as GC scratch (R1 on both arches).
    gc_scratch: Register,

    /// BBC accumulator for the return value of a call (R0 on both arches).
    return_reg: Register,

    /// Bytes consumed by one logical PUSH on this architecture.
    /// x86-64: 8 (push reg shrinks rsp by 8).
    /// ARM64: 16 (str reg,[sp,#-16]! to maintain 16-byte sp alignment).
    push_size: usize,

    pub fn forX86_64() ArchConfig {
        return .{
            .target = .x86_64,
            .argument_regs = &x86_64_argument_regs,
            .caller_saved  = &x86_64_caller_saved,
            .scratch_regs  = &x86_64_scratch_regs,
            .stack_alignment = 16,
            .ptr_size = 8,
            .entry_point = "_main",
            .gc_scratch = .R1,
            .return_reg = .R0,
            .push_size = 8,
        };
    }

    pub fn forArm64() ArchConfig {
        return .{
            .target = .arm64,
            .argument_regs = &arm64_argument_regs,
            .caller_saved  = &arm64_caller_saved,
            .scratch_regs  = &arm64_scratch_regs,
            .stack_alignment = 16,
            .ptr_size = 8,
            .entry_point = "_main",
            .gc_scratch = .R1,
            .return_reg = .R0,
            .push_size = 16,
        };
    }
};
