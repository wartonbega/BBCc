const std = @import("std");
const inst = @import("instructions.zig");
const arch_mod = @import("arch.zig");

const Instruction = inst.Instruction;
const Register = inst.Register;
const CodeCondition = inst.CodeCondition;

/// Optimization level passed in via compiler flags (-O0 / -O1 / -O2).
/// O0 = disabled
/// O1 = safe structural passes only (no liveness reasoning)
/// O2 = all passes (default)
pub const OptLevel = enum(u8) { O0 = 0, O1 = 1, O2 = 2 };

/// Run all enabled passes to a fixed point.
/// Returns the number of instructions eliminated.
pub fn optimize(
    program: *std.ArrayList(Instruction),
    arch: arch_mod.ArchConfig,
    alloc: std.mem.Allocator,
    level: OptLevel,
) !usize {
    if (level == .O0) return 0;
    const initial = program.items.len;
    var changed = true;
    while (changed) {
        const before = program.items.len;
        try passCmpBranchCollapse(program, alloc);
        if (level == .O2) {
            try passLeaStoreFold(program, alloc);
            try passImmediateArith(program, alloc);
        }
        try passPushPopElim(program, alloc);
        try passDeadLoad(program, alloc);
        if (level == .O2) {
            try passMovChainCollapse(program, alloc);
            try passImmIntoStore(program, alloc);
            try passImmIntoCmp(program, alloc);
            if (arch.target == .x86_64)
                try passSetcc(program, alloc);
        }
        try passCmpToTest(program, alloc);
        changed = program.items.len != before;
    }
    const diff: f32 = @floatFromInt(initial - program.items.len);
    std.log.info("Opitmizing the program, compressed program by {d:3.1}%", .{100.0 * diff / @as(f32, @floatFromInt(program.items.len))});
    return initial - program.items.len;
}

// ─── helpers ─────────────────────────────────────────────────────────────────

fn buildLabelRefMap(items: []const Instruction, alloc: std.mem.Allocator) !std.StringHashMap(usize) {
    var map = std.StringHashMap(usize).init(alloc);
    for (items) |insn| {
        const target: ?[]const u8 = switch (insn) {
            .jcond => |j| j.where,
            .jmp => |j| j.where,
            else => null,
        };
        if (target) |t| {
            const entry = try map.getOrPutValue(t, 0);
            entry.value_ptr.* += 1;
        }
    }
    return map;
}

/// True if instruction reads register `r` as a source operand.
fn instrReadsReg(i: Instruction, r: Register) bool {
    return switch (i) {
        .load => |p| dvalUsesReg(p.from, r),
        .store => |p| dvalUsesReg(p.from, r) or dadrUsesReg(p.to, r),
        .lea => false,
        .plus => |p| p.lhs == r or dvalUsesReg(p.rhs, r),
        .minus => |p| p.lhs == r or dvalUsesReg(p.rhs, r),
        .imul => |p| p.lhs == r or dvalUsesReg(p.rhs, r),
        .xor => |p| p.lhs == r or dvalUsesReg(p.rhs, r),
        .shl => |p| p.lhs == r,
        .inc => |p| p.lhs == r,
        .dec => |p| p.lhs == r,
        .cmp => |p| dvalUsesReg(p.val1, r) or dvalUsesReg(p.val2, r),
        .push => |p| p.reg == r,
        .gc_inc => |p| p.ptr == r,
        .gc_dec_no_check => |p| p.ptr == r,
        .gc_dec => |p| p.ptr == r,
        .call => |p| dvalUsesReg(p.value, r),
        .float_binop => |p| p.lhs == r or p.rhs == r,
        .float_cmp_flags => |p| p.lhs == r or p.rhs == r,
        .float_print => |p| p.val == r,
        else => false,
    };
}

fn dvalUsesReg(d: anytype, r: Register) bool {
    return switch (d) {
        .register => |reg| reg == r,
        .registerOffset => |ro| ro.register == r,
        .sib => |s| s.base == r or s.index == r,
        else => false,
    };
}

fn dadrUsesReg(d: anytype, r: Register) bool {
    return switch (d) {
        .registerOffset => |ro| ro.register == r,
        else => false,
    };
}

/// True if `rA` is provably dead starting at `items[start]`.
/// Scans forward: stops (dead=true) at ret/jmp/end-of-array or a write to rA;
/// stops (dead=false) at a read of rA or a label (unknown incoming edge).
fn isRegDeadFrom(items: []const Instruction, start: usize, rA: Register) bool {
    var i = start;
    while (i < items.len) {
        const insn = items[i];
        if (instrReadsReg(insn, rA)) return false;
        switch (insn) {
            .ret, .jmp => return true,
            .label => return false, // unknown incoming edge, be conservative
            .load => |p| if (p.to == rA) return true, // rA overwritten
            else => {},
        }
        i += 1;
    }
    return true;
}

/// True if the instruction is a conditional or unconditional jump to `label`.
fn instrJumpsTo(i: Instruction, label: []const u8) bool {
    return switch (i) {
        .jcond => |j| std.mem.eql(u8, j.where, label),
        .jmp => |j| std.mem.eql(u8, j.where, label),
        else => false,
    };
}

// ─── Pass 1: CMP + bool-materialise + branch-on-zero collapse ────────────────
//
// Matches this 9-instruction window emitted for every while/if condition:
//
//   [0] CMP(A, B)
//   [1] JCond(cc, true_label)
//   [2] LOAD(to=C, from=immediate(0))
//   [3] JMP(end_label)
//   [4] LABEL(true_label)
//   [5] LOAD(to=C, from=immediate(1))   dst == C
//   [6] LABEL(end_label)
//   [7] CMP(register(C), immediate(0))
//   [8] JCond(cc2=E|NE|Z|NZ, target)
//
// Replaced by:
//   [0] CMP(A, B)
//   [1] JCond(effective_cc, target)
//   [2] LABEL(end_label)               kept so other jumps to it still land
//
// effective_cc = negate(cc) when cc2 is E/Z  (branch-if-false = branch-if-NOT(cc))
//              = cc          when cc2 is NE/NZ (branch-if-true  = branch-if-cc)

fn passCmpBranchCollapse(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    const items = program.items;
    const n = items.len;
    const label_refs = try buildLabelRefMap(items, alloc);
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        // Need at least 9 instructions ahead.
        if (r + 9 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            const p2 = items[r + 2];
            const p3 = items[r + 3];
            const p4 = items[r + 4];
            const p5 = items[r + 5];
            const p6 = items[r + 6];
            const p7 = items[r + 7];
            const p8 = items[r + 8];

            if (p0 == .cmp and
                p1 == .jcond and
                p2 == .load and
                p3 == .jmp and
                p4 == .label and
                p5 == .load and
                p6 == .label and
                p7 == .cmp and
                p8 == .jcond)
            {
                const load0 = p2.load;
                const load1 = p5.load;
                const jcond1 = p1.jcond;
                const jmp3 = p3.jmp;
                const lbl4 = p4.label;
                const lbl6 = p6.label;
                const cmp7 = p7.cmp;
                const jcond8 = p8.jcond;

                // Validate shape:
                // - [2] LOAD(to=C, from=0)
                // - [5] LOAD(to=C, from=1), same dst
                // - [4] true_label matches [1].where
                // - [6] end_label  matches [3].where
                // - [7] cmp(register(C), 0)
                // - [8] jcond cc2 is E/Z/NE/NZ
                const is_false_load = load0.from == .immediate and load0.from.immediate == 0;
                const is_true_load = load1.from == .immediate and load1.from.immediate == 1;
                const same_dst = load0.to == load1.to;
                const labels_match = std.mem.eql(u8, lbl4.name, jcond1.where) and
                    std.mem.eql(u8, lbl6.name, jmp3.where);
                const c_reg = load0.to;
                const cmp7_ok = cmp7.val1 == .register and cmp7.val1.register == c_reg and
                    cmp7.val2 == .immediate and cmp7.val2.immediate == 0;
                const cc2 = jcond8.cc;
                const cc2_ok = cc2 == .E or cc2 == .Z or cc2 == .NE or cc2 == .NZ;

                // Guard: true_label must not be referenced more than once
                // (once by jcond1 inside this window). Extra refs (e.g. the
                // second JNZ emitted by the OR operator) would dangle.
                const true_label_refs = label_refs.get(lbl4.name) orelse 0;
                const true_label_external = true_label_refs > 1;

                if (is_false_load and is_true_load and same_dst and
                    labels_match and cmp7_ok and cc2_ok and !true_label_external)
                {
                    const branch_if_false = (cc2 == .E or cc2 == .Z);
                    const effective_cc = if (branch_if_false) inst.negate(jcond1.cc) else jcond1.cc;

                    items[w + 0] = p0; // CMP(A, B)
                    items[w + 1] = .{ .jcond = .{ .cc = effective_cc, .where = jcond8.where } };
                    items[w + 2] = .{ .label = lbl6 }; // keep end_label
                    w += 3;
                    r += 9;
                    continue;
                }
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 2: LEA(rbp-N) + STORE([rA+0], X)  →  STORE([rbp-N], X) ──────────

fn passLeaStoreFold(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];

            if (p0 == .lea and p1 == .store) {
                const lea = p0.lea;
                const store = p1.store;

                // LEA must be loading address of a frame slot ([rbp + offset] or [rbp - offset])
                if (lea.adr == .registerOffset and lea.adr.registerOffset.register == .RBP) {
                    const frame_offset = lea.adr.registerOffset.offset;
                    const rA = lea.to;

                    // STORE must write through rA+0
                    if (store.to == .registerOffset and
                        store.to.registerOffset.register == rA and
                        store.to.registerOffset.offset == 0 and
                        // rA must not appear in the source operand
                        !dvalUsesReg(store.from, rA))
                    {
                        items[w] = .{ .store = .{
                            .from = store.from,
                            .to = .{ .registerOffset = .{ .register = .RBP, .offset = frame_offset } },
                        } };
                        w += 1;
                        r += 2;
                        continue;
                    }
                }
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 3: LOAD(rA, imm C) + PLUS/MINUS(rB, rA)  →  INC/DEC/PLUS/MINUS(rB, imm C) ─

fn passImmediateArith(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];

            if (p0 == .load and p0.load.from == .immediate) {
                const rA = p0.load.to;
                const C = p0.load.from.immediate;

                // Check next instruction doesn't also read rA as a non-folded operand.
                // We'll fold PLUS(rB, rA) and MINUS(rB, rA) where rA != rB.
                if (p1 == .plus and p1.plus.rhs == .register and p1.plus.rhs.register == rA and p1.plus.lhs != rA) {
                    const rB = p1.plus.lhs;
                    // Verify rA not needed after (check [r+2] conservatively)
                    const next_reads_rA = if (r + 2 < n) instrReadsReg(items[r + 2], rA) else false;
                    if (!next_reads_rA) {
                        items[w] = if (C == 1)
                            .{ .inc = .{ .lhs = rB } }
                        else
                            .{ .plus = .{ .lhs = rB, .rhs = .{ .immediate = C } } };
                        w += 1;
                        r += 2;
                        continue;
                    }
                } else if (p1 == .minus and p1.minus.rhs == .register and p1.minus.rhs.register == rA and p1.minus.lhs != rA) {
                    const rB = p1.minus.lhs;
                    const next_reads_rA = if (r + 2 < n) instrReadsReg(items[r + 2], rA) else false;
                    if (!next_reads_rA) {
                        items[w] = if (C == 1)
                            .{ .dec = .{ .lhs = rB } }
                        else
                            .{ .minus = .{ .lhs = rB, .rhs = .{ .immediate = C } } };
                        w += 1;
                        r += 2;
                        continue;
                    }
                }
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 4: standalone bool materialisation → SETCC (x86-64 only) ──────────
//
// Matches the 7-instruction window not consumed by Pass 1 (result stored, not
// immediately branched on):
//
//   [0] CMP(A, B)
//   [1] JCond(cc, true_label)
//   [2] LOAD(to=C, from=0)
//   [3] JMP(end_label)
//   [4] LABEL(true_label)
//   [5] LOAD(to=C, from=1)
//   [6] LABEL(end_label)
//
// Guard: C must not appear in A or B.
//
// Replaced by:
//   [0] XOR(C, register(C))   — zero C without touching flags
//   [1] CMP(A, B)
//   [2] SETCC(C, cc)
//   [3] LABEL(end_label)

fn passSetcc(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    const items = program.items;
    const n = items.len;
    const label_refs = try buildLabelRefMap(items, alloc);
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 7 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            const p2 = items[r + 2];
            const p3 = items[r + 3];
            const p4 = items[r + 4];
            const p5 = items[r + 5];
            const p6 = items[r + 6];

            if (p0 == .cmp and
                p1 == .jcond and
                p2 == .load and
                p3 == .jmp and
                p4 == .label and
                p5 == .load and
                p6 == .label)
            {
                const load0 = p2.load;
                const load1 = p5.load;
                const jcond1 = p1.jcond;
                const jmp3 = p3.jmp;
                const lbl4 = p4.label;
                const lbl6 = p6.label;

                const is_false_load = load0.from == .immediate and load0.from.immediate == 0;
                const is_true_load = load1.from == .immediate and load1.from.immediate == 1;
                const same_dst = load0.to == load1.to;
                const labels_match = std.mem.eql(u8, lbl4.name, jcond1.where) and
                    std.mem.eql(u8, lbl6.name, jmp3.where);
                const c_reg = load0.to;
                // Guard: C must not be a CMP operand (XOR would clobber it before CMP)
                const c_not_in_cmp = !dvalUsesReg(p0.cmp.val1, c_reg) and
                    !dvalUsesReg(p0.cmp.val2, c_reg);

                const true_label_refs2 = label_refs.get(lbl4.name) orelse 0;
                const true_label_ext = true_label_refs2 > 1;

                if (is_false_load and is_true_load and same_dst and labels_match and
                    c_not_in_cmp and !true_label_ext)
                {
                    items[w + 0] = .{ .xor = .{ .lhs = c_reg, .rhs = .{ .register = c_reg } } };
                    items[w + 1] = p0; // CMP(A, B)
                    items[w + 2] = .{ .setcc = .{ .dst = c_reg, .cc = jcond1.cc } };
                    items[w + 3] = .{ .label = lbl6 };
                    w += 4;
                    r += 7;
                    continue;
                }
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 5: PUSH(rA) + POP(rA) → eliminate ─────────────────────────────────

fn passPushPopElim(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            if (p0 == .push and p1 == .pop and p0.push.reg == p1.pop.reg) {
                r += 2;
                continue;
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 6: LOAD(rA, imm C) + STORE([addr], rA) → STORE([addr], imm C) ─────

fn passImmIntoStore(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            if (p0 == .load and p0.load.from == .immediate and p1 == .store) {
                const rA = p0.load.to;
                const imm = p0.load.from.immediate;
                const st = p1.store;
                if (st.from == .register and st.from.register == rA and
                    !dadrUsesReg(st.to, rA))
                {
                    if (isRegDeadFrom(items, r + 2, rA)) {
                        items[w] = .{ .store = .{ .from = .{ .immediate = imm }, .to = st.to } };
                        w += 1;
                        r += 2;
                        continue;
                    }
                }
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 7: LOAD(rA, imm C) + CMP(X, rA) → CMP(X, imm C) ──────────────────

fn passImmIntoCmp(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            if (p0 == .load and p0.load.from == .immediate and p1 == .cmp) {
                const rA = p0.load.to;
                const imm = p0.load.from.immediate;
                const cmp = p1.cmp;
                // Only fold rA when it's in val2 (right operand).
                // Folding val1=rA → imm would produce `cmp imm, X` which is
                // invalid on x86-64 (immediate cannot be the destination).
                if (cmp.val2 == .register and cmp.val2.register == rA and
                    !dvalUsesReg(cmp.val1, rA) and cmp.val1 != .immediate and
                    isRegDeadFrom(items, r + 2, rA))
                {
                    items[w] = .{ .cmp = .{ .val1 = cmp.val1, .val2 = .{ .immediate = imm } } };
                    w += 1;
                    r += 2;
                    continue;
                }
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 8: CMP(rA, 0) → TST(rA, rA) ───────────────────────────────────────

fn passCmpToTest(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    for (program.items) |*insn| {
        if (insn.* == .cmp) {
            const cmp = insn.*.cmp;
            if (cmp.val1 == .register and cmp.val2 == .immediate and cmp.val2.immediate == 0) {
                insn.* = .{ .tst = .{ .lhs = cmp.val1.register, .rhs = cmp.val1.register } };
            }
        }
    }
}

// ─── Pass 9: LOAD(rA, X) + LOAD(rA, Y) → LOAD(rA, Y)  (dead write) ──────────

fn passDeadLoad(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            if (p0 == .load and p1 == .load and p0.load.to == p1.load.to and
                !dvalUsesReg(p1.load.from, p0.load.to))
            {
                r += 1; // drop the first (dead) write
                continue;
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Pass 10: LOAD(rX, V) + LOAD(rZ, register rX) → LOAD(rZ, V)  (copy chain) ─

fn passMovChainCollapse(program: *std.ArrayList(Instruction), alloc: std.mem.Allocator) !void {
    _ = alloc;
    const items = program.items;
    const n = items.len;
    var w: usize = 0;
    var r: usize = 0;

    while (r < n) {
        if (r + 2 <= n) {
            const p0 = items[r + 0];
            const p1 = items[r + 1];
            if (p0 == .load and p1 == .load and
                p1.load.from == .register and p1.load.from.register == p0.load.to and
                p1.load.to != p0.load.to and
                isRegDeadFrom(items, r + 2, p0.load.to))
            {
                items[w] = .{ .load = .{ .to = p1.load.to, .from = p0.load.from } };
                w += 1;
                r += 2;
                continue;
            }
        }
        items[w] = items[r];
        w += 1;
        r += 1;
    }
    program.shrinkRetainingCapacity(w);
}

// ─── Tests ───────────────────────────────────────────────────────────────────

const testing = std.testing;

fn makeProgram(alloc: std.mem.Allocator, insns: []const Instruction) !std.ArrayList(Instruction) {
    var prog = std.ArrayList(Instruction).init(alloc);
    try prog.appendSlice(insns);
    return prog;
}

// helper shorthands
const cmp_r2_r3: Instruction = .{ .cmp = .{ .val1 = .{ .register = .R2 }, .val2 = .{ .register = .R3 } } };
const jcond_L_true: Instruction = .{ .jcond = .{ .cc = .L, .where = "true_lbl" } };
const load_r0_0: Instruction = .{ .load = .{ .to = .R0, .from = .{ .immediate = 0 } } };
const load_r0_1: Instruction = .{ .load = .{ .to = .R0, .from = .{ .immediate = 1 } } };
const jmp_end: Instruction = .{ .jmp = .{ .where = "end_lbl" } };
const lbl_true: Instruction = .{ .label = .{ .name = "true_lbl" } };
const lbl_end: Instruction = .{ .label = .{ .name = "end_lbl" } };
const cmp_r0_0: Instruction = .{ .cmp = .{ .val1 = .{ .register = .R0 }, .val2 = .{ .immediate = 0 } } };

// 9-instruction window shared by several tests
const if_pattern_ne = [9]Instruction{
    cmp_r2_r3,
    jcond_L_true,
    load_r0_0,
    jmp_end,
    lbl_true,
    load_r0_1,
    lbl_end,
    cmp_r0_0,
    .{ .jcond = .{ .cc = .NE, .where = "target_lbl" } },
};

test "passCmpBranchCollapse: NE branch keeps original cc" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var prog = try makeProgram(alloc, &if_pattern_ne);
    defer prog.deinit();

    try passCmpBranchCollapse(&prog, alloc);

    try testing.expectEqual(@as(usize, 3), prog.items.len);
    try testing.expect(prog.items[0] == .cmp);
    try testing.expect(prog.items[1] == .jcond);
    try testing.expectEqual(CodeCondition.L, prog.items[1].jcond.cc);
    try testing.expectEqualStrings("target_lbl", prog.items[1].jcond.where);
    try testing.expectEqualStrings("end_lbl", prog.items[2].label.name);
}

test "passCmpBranchCollapse: E branch negates cc (L → GE)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[9]Instruction{
        cmp_r2_r3,
        jcond_L_true,
        load_r0_0,
        jmp_end,
        lbl_true,
        load_r0_1,
        lbl_end,
        cmp_r0_0,
        .{ .jcond = .{ .cc = .E, .where = "target_lbl" } },
    });
    defer prog.deinit();

    try passCmpBranchCollapse(&prog, alloc);

    try testing.expectEqual(@as(usize, 3), prog.items.len);
    try testing.expectEqual(CodeCondition.GE, prog.items[1].jcond.cc);
}

test "passCmpBranchCollapse: external ref to true_label blocks folding" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Prepend an extra JNZ to true_lbl before the window (simulates OR operator).
    var prog = try makeProgram(alloc, &[10]Instruction{
        .{ .jcond = .{ .cc = .NZ, .where = "true_lbl" } }, // extra external ref
        cmp_r2_r3,
        jcond_L_true,
        load_r0_0,
        jmp_end,
        lbl_true,
        load_r0_1,
        lbl_end,
        cmp_r0_0,
        .{ .jcond = .{ .cc = .NE, .where = "target_lbl" } },
    });
    defer prog.deinit();

    try passCmpBranchCollapse(&prog, alloc);

    // Guard fired: nothing collapsed — 10 instructions remain.
    try testing.expectEqual(@as(usize, 10), prog.items.len);
}

test "passCmpBranchCollapse: non-matching window left intact" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[3]Instruction{
        cmp_r2_r3,
        jcond_L_true,
        lbl_end,
    });
    defer prog.deinit();

    try passCmpBranchCollapse(&prog, alloc);
    try testing.expectEqual(@as(usize, 3), prog.items.len);
}

// ─── passLeaStoreFold ─────────────────────────────────────────────────────────

test "passLeaStoreFold: LEA(rbp-8) + STORE([rA+0], rB) → STORE([rbp-8], rB)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .lea = .{ .adr = .{ .registerOffset = .{ .register = .RBP, .offset = @bitCast(@as(u64, @bitCast(@as(i64, -8)))) } }, .to = .R2 } },
        .{ .store = .{ .from = .{ .register = .R3 }, .to = .{ .registerOffset = .{ .register = .R2, .offset = 0 } } } },
    });
    defer prog.deinit();

    try passLeaStoreFold(&prog, alloc);

    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expect(prog.items[0] == .store);
    try testing.expectEqual(Register.RBP, prog.items[0].store.to.registerOffset.register);
}

test "passLeaStoreFold: guard — rA in store source prevents fold" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .lea = .{ .adr = .{ .registerOffset = .{ .register = .RBP, .offset = @bitCast(@as(u64, @bitCast(@as(i64, -8)))) } }, .to = .R2 } },
        // from = R2 (same as LEA dest) — fold is unsafe
        .{ .store = .{ .from = .{ .register = .R2 }, .to = .{ .registerOffset = .{ .register = .R2, .offset = 0 } } } },
    });
    defer prog.deinit();

    try passLeaStoreFold(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
}

// ─── passImmediateArith ───────────────────────────────────────────────────────

test "passImmediateArith: LOAD(rA,1) + PLUS(rB,rA) → INC(rB)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 1 } } },
        .{ .plus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } },
    });
    defer prog.deinit();

    try passImmediateArith(&prog, alloc);
    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expect(prog.items[0] == .inc);
    try testing.expectEqual(Register.R2, prog.items[0].inc.lhs);
}

test "passImmediateArith: LOAD(rA,5) + PLUS(rB,rA) → PLUS(rB, imm 5)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 5 } } },
        .{ .plus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } },
    });
    defer prog.deinit();

    try passImmediateArith(&prog, alloc);
    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expect(prog.items[0] == .plus);
    try testing.expectEqual(@as(i64, 5), prog.items[0].plus.rhs.immediate);
}

test "passImmediateArith: LOAD(rA,1) + MINUS(rB,rA) → DEC(rB)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 1 } } },
        .{ .minus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } },
    });
    defer prog.deinit();

    try passImmediateArith(&prog, alloc);
    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expect(prog.items[0] == .dec);
}

test "passImmediateArith: guard — rA read by instruction after PLUS" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 1 } } },
        .{ .plus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } },
        .{ .plus = .{ .lhs = .R0, .rhs = .{ .register = .R3 } } }, // R3 live
    });
    defer prog.deinit();

    try passImmediateArith(&prog, alloc);
    try testing.expectEqual(@as(usize, 3), prog.items.len); // not folded
}

// ─── passPushPopElim ──────────────────────────────────────────────────────────

test "passPushPopElim: PUSH(rA) + POP(rA) → eliminated" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .push = .{ .reg = .R2 } },
        .{ .pop = .{ .reg = .R2 } },
    });
    defer prog.deinit();

    try passPushPopElim(&prog, alloc);
    try testing.expectEqual(@as(usize, 0), prog.items.len);
}

test "passPushPopElim: PUSH(rA) + POP(rB) with different regs stays" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .push = .{ .reg = .R2 } },
        .{ .pop = .{ .reg = .R3 } },
    });
    defer prog.deinit();

    try passPushPopElim(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
}

test "passPushPopElim: multiple redundant pairs all eliminated" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[4]Instruction{
        .{ .push = .{ .reg = .R2 } },
        .{ .pop = .{ .reg = .R2 } },
        .{ .push = .{ .reg = .R3 } },
        .{ .pop = .{ .reg = .R3 } },
    });
    defer prog.deinit();

    try passPushPopElim(&prog, alloc);
    try testing.expectEqual(@as(usize, 0), prog.items.len);
}

// ─── passImmIntoStore ─────────────────────────────────────────────────────────

test "passImmIntoStore: LOAD(rA,42) + STORE([rbp-8],rA) → STORE([rbp-8], imm 42)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R2, .from = .{ .immediate = 42 } } },
        .{ .store = .{ .from = .{ .register = .R2 }, .to = .{ .registerOffset = .{ .register = .RBP, .offset = @bitCast(@as(u64, @bitCast(@as(i64, -8)))) } } } },
        .{ .ret = .{} },
    });
    defer prog.deinit();

    try passImmIntoStore(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
    try testing.expect(prog.items[0] == .store);
    try testing.expectEqual(@as(i64, 42), prog.items[0].store.from.immediate);
}

test "passImmIntoStore: guard — rA live after store blocks fold" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R2, .from = .{ .immediate = 42 } } },
        .{ .store = .{ .from = .{ .register = .R2 }, .to = .{ .registerOffset = .{ .register = .RBP, .offset = @bitCast(@as(u64, @bitCast(@as(i64, -8)))) } } } },
        .{ .push = .{ .reg = .R2 } }, // R2 still live
    });
    defer prog.deinit();

    try passImmIntoStore(&prog, alloc);
    try testing.expectEqual(@as(usize, 3), prog.items.len);
}

// ─── passImmIntoCmp ───────────────────────────────────────────────────────────

test "passImmIntoCmp: LOAD(rA,5) + CMP(rB, rA) → CMP(rB, imm 5)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 5 } } },
        .{ .cmp = .{ .val1 = .{ .register = .R2 }, .val2 = .{ .register = .R3 } } },
        .{ .ret = .{} }, // R3 dead after cmp
    });
    defer prog.deinit();

    try passImmIntoCmp(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
    try testing.expect(prog.items[0] == .cmp);
    try testing.expectEqual(@as(i64, 5), prog.items[0].cmp.val2.immediate);
}

test "passImmIntoCmp: guard — label between cmp and next use blocks fold" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // R3 is used after the CMP (through a label → unknown incoming edge)
    var prog = try makeProgram(alloc, &[4]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 5 } } },
        .{ .cmp = .{ .val1 = .{ .register = .R2 }, .val2 = .{ .register = .R3 } } },
        .{ .label = .{ .name = "some_lbl" } }, // unknown edge: R3 may be live
        .{ .push = .{ .reg = .R3 } },
    });
    defer prog.deinit();

    try passImmIntoCmp(&prog, alloc);
    try testing.expectEqual(@as(usize, 4), prog.items.len);
}

test "passImmIntoCmp: guard — does not produce imm-imm compare" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // CMP(rA, 0) — val2 is already immediate, fold would yield CMP(imm, imm)
    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 5 } } },
        // val2 is rA BUT val1 is also an immediate — shouldn't fold
        .{ .cmp = .{ .val1 = .{ .immediate = 0 }, .val2 = .{ .register = .R3 } } },
        .{ .ret = .{} },
    });
    defer prog.deinit();

    try passImmIntoCmp(&prog, alloc);
    try testing.expectEqual(@as(usize, 3), prog.items.len); // val1 is immediate — guard fires
}

// ─── passCmpToTest ────────────────────────────────────────────────────────────

test "passCmpToTest: CMP(rA, 0) → TST(rA, rA)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[1]Instruction{
        .{ .cmp = .{ .val1 = .{ .register = .R2 }, .val2 = .{ .immediate = 0 } } },
    });
    defer prog.deinit();

    try passCmpToTest(&prog, alloc);
    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expect(prog.items[0] == .tst);
    try testing.expectEqual(Register.R2, prog.items[0].tst.lhs);
    try testing.expectEqual(Register.R2, prog.items[0].tst.rhs);
}

test "passCmpToTest: CMP(rA, non-zero) left unchanged" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[1]Instruction{
        .{ .cmp = .{ .val1 = .{ .register = .R2 }, .val2 = .{ .immediate = 1 } } },
    });
    defer prog.deinit();

    try passCmpToTest(&prog, alloc);
    try testing.expect(prog.items[0] == .cmp);
}

test "passCmpToTest: batch — all CMP(rX, 0) in a stream converted" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[4]Instruction{
        .{ .cmp = .{ .val1 = .{ .register = .R0 }, .val2 = .{ .immediate = 0 } } },
        .{ .cmp = .{ .val1 = .{ .register = .R1 }, .val2 = .{ .immediate = 0 } } },
        .{ .cmp = .{ .val1 = .{ .register = .R2 }, .val2 = .{ .immediate = 5 } } },
        .{ .cmp = .{ .val1 = .{ .register = .R3 }, .val2 = .{ .immediate = 0 } } },
    });
    defer prog.deinit();

    try passCmpToTest(&prog, alloc);
    try testing.expect(prog.items[0] == .tst);
    try testing.expect(prog.items[1] == .tst);
    try testing.expect(prog.items[2] == .cmp); // non-zero, unchanged
    try testing.expect(prog.items[3] == .tst);
}

// ─── passSetcc ────────────────────────────────────────────────────────────────

test "passSetcc: 7-instruction bool-materialise collapses to XOR+CMP+SETCC+LABEL" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[7]Instruction{
        cmp_r2_r3,
        jcond_L_true,
        load_r0_0,
        jmp_end,
        lbl_true,
        load_r0_1,
        lbl_end,
    });
    defer prog.deinit();

    try passSetcc(&prog, alloc);
    try testing.expectEqual(@as(usize, 4), prog.items.len);
    try testing.expect(prog.items[0] == .xor);
    try testing.expectEqual(Register.R0, prog.items[0].xor.lhs);
    try testing.expect(prog.items[1] == .cmp);
    try testing.expect(prog.items[2] == .setcc);
    try testing.expectEqual(CodeCondition.L, prog.items[2].setcc.cc);
    try testing.expect(prog.items[3] == .label);
}

test "passSetcc: guard — C in CMP operands blocks fold" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // R0 (dst) is used inside the CMP — XOR would clobber it
    var prog = try makeProgram(alloc, &[7]Instruction{
        .{ .cmp = .{ .val1 = .{ .register = .R0 }, .val2 = .{ .register = .R3 } } },
        jcond_L_true,
        load_r0_0,
        jmp_end,
        lbl_true,
        load_r0_1,
        lbl_end,
    });
    defer prog.deinit();

    try passSetcc(&prog, alloc);
    try testing.expectEqual(@as(usize, 7), prog.items.len);
}

// ─── full pipeline / instruction-count stress tests ──────────────────────────

test "optimize O0: no change" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &if_pattern_ne);
    defer prog.deinit();
    const arch = arch_mod.ArchConfig.forX86_64();

    const saved = try optimize(&prog, arch, alloc, .O0);
    try testing.expectEqual(@as(usize, 0), saved);
    try testing.expectEqual(@as(usize, 9), prog.items.len);
}

test "optimize O1 vs O2: O2 removes at least as many instructions as O1" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const arch = arch_mod.ArchConfig.forX86_64();

    // A stream with patterns for both level sets:
    // - cmp+branch collapse (both levels)
    // - LOAD(rA,1)+PLUS→INC (O2 only)
    // - PUSH+POP elim (both levels)
    const stream = [_]Instruction{
        cmp_r2_r3,                                  jcond_L_true,                                             load_r0_0,                                                 jmp_end,
        lbl_true,                                   load_r0_1,                                                lbl_end,                                                   cmp_r0_0,
        .{ .jcond = .{ .cc = .NE, .where = "x" } }, .{ .load = .{ .to = .R3, .from = .{ .immediate = 1 } } }, .{ .plus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } }, .{ .push = .{ .reg = .R1 } },
        .{ .pop = .{ .reg = .R1 } },
    };

    var prog1 = try makeProgram(alloc, &stream);
    defer prog1.deinit();
    var prog2 = try makeProgram(alloc, &stream);
    defer prog2.deinit();

    const saved1 = try optimize(&prog1, arch, alloc, .O1);
    const saved2 = try optimize(&prog2, arch, alloc, .O2);

    try testing.expect(saved2 >= saved1);
    try testing.expect(saved1 > 0);
    try testing.expect(saved2 > 0);
}

test "optimize: fixed-point — chained patterns collapse fully" {
    // LOAD(rA,1) + PLUS(rB,rA) → INC(rB), followed by cmp/branch that
    // becomes eligible only after INC replaces the PLUS.
    // The fixed-point loop ensures all passes run until stable.
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const arch = arch_mod.ArchConfig.forX86_64();

    // Two independent push/pop nop pairs and an immediate arith chain.
    const stream = [_]Instruction{
        .{ .push = .{ .reg = .R1 } },
        .{ .pop = .{ .reg = .R1 } },
        .{ .push = .{ .reg = .R2 } },
        .{ .pop = .{ .reg = .R2 } },
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 1 } } },
        .{ .plus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } },
    };

    var prog = try makeProgram(alloc, &stream);
    defer prog.deinit();

    const saved = try optimize(&prog, arch, alloc, .O2);
    try testing.expectEqual(@as(usize, 5), saved); // 4 push/pop + 1 load = 5
    try testing.expectEqual(@as(usize, 1), prog.items.len); // only INC remains
}

test "optimize: instruction count strictly decreasing for real-world snippet" {
    // Mimics the pattern emitted for:  if (a < b) { c = c + 1 }
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    const arch = arch_mod.ArchConfig.forX86_64();

    const before_count = 9 + 2; // 9-insn branch pattern + LOAD(1)+PLUS
    const stream = [_]Instruction{
        // 9-insn branch collapse pattern
        cmp_r2_r3,
        jcond_L_true,
        load_r0_0,
        jmp_end,
        lbl_true,
        load_r0_1,
        lbl_end,
        cmp_r0_0,
        .{ .jcond = .{ .cc = .NE, .where = "body" } },
        // Body: c = c + 1  (via LOAD scratch + PLUS)
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 1 } } },
        .{ .plus = .{ .lhs = .R2, .rhs = .{ .register = .R3 } } },
    };
    try testing.expectEqual(@as(usize, before_count), stream.len);

    var prog = try makeProgram(alloc, &stream);
    defer prog.deinit();

    const saved = try optimize(&prog, arch, alloc, .O2);
    try testing.expect(saved > 0);
    try testing.expect(prog.items.len < before_count);
}

// ─── passDeadLoad ─────────────────────────────────────────────────────────────

test "passDeadLoad: adjacent same-dst loads — first dropped" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .register = .R5 } } },
        .{ .load = .{ .to = .R0, .from = .{ .immediate = 1 } } },
    });
    defer prog.deinit();

    try passDeadLoad(&prog, alloc);
    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expect(prog.items[0] == .load);
    try testing.expectEqual(@as(i64, 1), prog.items[0].load.from.immediate);
}

test "passDeadLoad: different-dst loads — untouched" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .register = .R5 } } },
        .{ .load = .{ .to = .R2, .from = .{ .immediate = 1 } } },
    });
    defer prog.deinit();

    try passDeadLoad(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
}

test "passDeadLoad: guard — second load reads rA in its source address" {
    // LOAD(rA, rB) then LOAD(rA, [rA + 8]) — first is NOT dead, rA feeds the address
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .register = .R5 } } },
        .{ .load = .{ .to = .R0, .from = .{ .registerOffset = .{ .register = .R0, .offset = 8 } } } },
    });
    defer prog.deinit();

    try passDeadLoad(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
}

test "passDeadLoad: three in a row — collapses to last one" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .immediate = 1 } } },
        .{ .load = .{ .to = .R0, .from = .{ .immediate = 2 } } },
        .{ .load = .{ .to = .R0, .from = .{ .immediate = 3 } } },
    });
    defer prog.deinit();

    try passDeadLoad(&prog, alloc);
    try testing.expectEqual(@as(usize, 1), prog.items.len);
    try testing.expectEqual(@as(i64, 3), prog.items[0].load.from.immediate);
}

// ─── passMovChainCollapse ─────────────────────────────────────────────────────

test "passMovChainCollapse: two-hop register copy → single load" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // mov x9, x0  ;  mov x2, x9  →  mov x2, x0
    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .register = .R5 } } },
        .{ .load = .{ .to = .R2, .from = .{ .register = .R0 } } },
        .{ .ret  = .{} }, // R0 dead after second load
    });
    defer prog.deinit();

    try passMovChainCollapse(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
    try testing.expect(prog.items[0] == .load);
    try testing.expectEqual(Register.R2, prog.items[0].load.to);
    try testing.expectEqual(Register.R5, prog.items[0].load.from.register);
}

test "passMovChainCollapse: two-hop immediate chain → single immediate load" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // mov rX, #42  ;  mov rZ, rX  →  mov rZ, #42
    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R3, .from = .{ .immediate = 42 } } },
        .{ .load = .{ .to = .R2, .from = .{ .register = .R3 } } },
        .{ .ret  = .{} },
    });
    defer prog.deinit();

    try passMovChainCollapse(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
    try testing.expectEqual(Register.R2, prog.items[0].load.to);
    try testing.expectEqual(@as(i64, 42), prog.items[0].load.from.immediate);
}

test "passMovChainCollapse: guard — rX still live after fold blocks it" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // R0 is read by the push after the two loads — can't eliminate first load
    var prog = try makeProgram(alloc, &[3]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .register = .R5 } } },
        .{ .load = .{ .to = .R2, .from = .{ .register = .R0 } } },
        .{ .push = .{ .reg = .R0 } }, // R0 still live
    });
    defer prog.deinit();

    try passMovChainCollapse(&prog, alloc);
    try testing.expectEqual(@as(usize, 3), prog.items.len);
}

test "passMovChainCollapse: guard — same src and dst (rX == rZ) leaves untouched" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // mov R0, R5  ;  mov R0, R0  — second is a self-copy; don't fold weirdly
    var prog = try makeProgram(alloc, &[2]Instruction{
        .{ .load = .{ .to = .R0, .from = .{ .register = .R5 } } },
        .{ .load = .{ .to = .R0, .from = .{ .register = .R0 } } },
    });
    defer prog.deinit();

    // passDeadLoad handles R0→R0 (same dst); passMovChainCollapse guard: rZ==rX fires
    try passMovChainCollapse(&prog, alloc);
    try testing.expectEqual(@as(usize, 2), prog.items.len);
}
