const std = @import("std");
const inst = @import("instructions.zig");
const Compiler = @import("compiler.zig").Compiler;

pub const regtableError = error{
    InvalidIndex,
    AccessingFreedValue,
    NoLiveRegisters,
    InvalidState,
    DoubleFree,
};

pub const RegState = struct {
    reg: ?inst.Register = null,
    spilled: bool = false,
    stack_slot: ?usize = null,
    freed: bool = false,

    pub fn init(reg: inst.Register) RegState {
        return .{ .reg = reg };
    }

    pub fn spill(self: *RegState, slot: usize) void {
        self.spilled = true;
        self.reg = null;
        self.stack_slot = slot;
    }

    pub fn reload(self: *RegState, reg: inst.Register) void {
        self.spilled = false;
        self.reg = reg;
        self.stack_slot = null;
    }
};

pub const RegisterTable = struct {
    allocator: std.mem.Allocator,

    /// Available physical registers
    free_regs: std.ArrayList(inst.Register),

    /// Indices of values currently in registers
    live: std.ArrayList(usize),

    /// State for each allocated value
    states: std.ArrayList(RegState),

    /// Pool of reusable stack slots
    free_slots: std.ArrayList(usize),

    /// Next stack slot to allocate if free_slots is empty
    next_slot: usize = 0,

    /// Track the last allocated index
    last_used: ?usize = null,

    /// Number of registers we started with
    initial_reg_count: usize = 0,

    /// Base register for stack access (typically RBP)
    stack_base: inst.Register = .RBP,

    pub fn init(allocator: std.mem.Allocator) !RegisterTable {
        var self = RegisterTable{
            .allocator = allocator,
            .free_regs = std.ArrayList(inst.Register).init(allocator),
            .live = std.ArrayList(usize).init(allocator),
            .states = std.ArrayList(RegState).init(allocator),
            .free_slots = std.ArrayList(usize).init(allocator),
        };

        for (inst.SCRATCH_REGS) |reg| {
            try self.free_regs.append(reg);
        }
        self.initial_reg_count = inst.SCRATCH_REGS.len;

        return self;
    }

    pub fn deinit(self: *RegisterTable) void {
        self.free_regs.deinit();
        self.live.deinit();
        self.states.deinit();
        self.free_slots.deinit();
    }

    /// Allocate a new value, returns its index
    pub fn allocate(self: *RegisterTable, compiler: *Compiler) !usize {
        if (self.free_regs.items.len == 0) {
            try self.spillOneLive(compiler);
        }

        const reg = self.free_regs.pop().?;
        const idx = self.states.items.len;

        try self.states.append(RegState.init(reg));
        try self.live.append(idx);
        self.last_used = idx;

        return idx;
    }

    /// Get the register for a value, reloading from stack if necessary
    pub fn getValue(self: *RegisterTable, idx: usize, compiler: *Compiler) !inst.Register {
        if (idx >= self.states.items.len) {
            return regtableError.InvalidIndex;
        }

        var rs = &self.states.items[idx];

        if (rs.freed) {
            return regtableError.AccessingFreedValue;
        }

        if (rs.spilled) {
            if (self.free_regs.items.len == 0) {
                try self.spillOneLive(compiler);
            }

            const reg = self.free_regs.pop().?;
            const slot = rs.stack_slot.?;

            // Emit: mov reg, [rbp - offset]
            try compiler.addInstruction(.{
                .load = .{
                    .to = reg,
                    .from = .{
                        .registerOffset = .{
                            .register = self.stack_base,
                            .offset = -@as(i64, @intCast((slot + 1) * 8)),
                        },
                    },
                },
            });

            try self.free_slots.append(slot);

            rs.reload(reg);
            try self.live.append(idx);
        }

        return rs.reg.?;
    }

    /// Free a value, releasing its register or stack slot
    pub fn free(self: *RegisterTable, idx: usize) !void {
        if (idx >= self.states.items.len) {
            return regtableError.InvalidIndex;
        }

        var rs = &self.states.items[idx];

        if (rs.freed) {
            return regtableError.DoubleFree;
        }

        if (rs.spilled) {
            try self.free_slots.append(rs.stack_slot.?);
            rs.stack_slot = null;
            rs.spilled = false;
        } else {
            try self.free_regs.append(rs.reg.?);
            self.removeFromLive(idx);
            rs.reg = null;
        }

        rs.freed = true;
    }

    /// Spill one live value to make a register available
    fn spillOneLive(self: *RegisterTable, compiler: *Compiler) !void {
        if (self.live.items.len == 0) {
            return regtableError.NoLiveRegisters;
        }

        const victim_idx = self.live.orderedRemove(0);
        var victim = &self.states.items[victim_idx];

        const reg = victim.reg orelse return regtableError.InvalidState;
        const slot = self.allocateStackSlot();

        // Emit: mov [rbp - offset], reg
        try compiler.addInstruction(.{
            .store = .{
                .to = .{
                    .registerOffset = .{
                        .register = self.stack_base,
                        .offset = -@as(i64, @intCast((slot + 1) * 8)),
                    },
                },
                .from = .{ .register = reg },
            },
        });

        victim.spill(slot);
        try self.free_regs.append(reg);
    }

    pub fn isRegisterInUse(self: *RegisterTable, reg: inst.Register) bool {
        for (self.free_regs.items) |free_reg| {
            if (free_reg == reg) {
                return false;
            }
        }
        return true;
    }

    /// Get a stack slot (reuse freed ones or allocate new)
    fn allocateStackSlot(self: *RegisterTable) usize {
        if (self.free_slots.items.len > 0) {
            return self.free_slots.pop().?;
        }
        const slot = self.next_slot;
        self.next_slot += 1;
        return slot;
    }

    /// Remove an index from the live list
    fn removeFromLive(self: *RegisterTable, idx: usize) void {
        for (self.live.items, 0..) |live_idx, i| {
            if (live_idx == idx) {
                _ = self.live.orderedRemove(i);
                return;
            }
        }
    }

    pub fn lastReg(self: *RegisterTable) ?usize {
        const ret = self.last_used;
        self.last_used = null;
        return ret;
    }

    pub fn isCorrectlyFreed(self: *RegisterTable) bool {
        return self.free_regs.items.len == self.initial_reg_count;
    }

    /// Get the maximum stack space needed for spills (in bytes)
    pub fn maxStackUsage(self: *RegisterTable) usize {
        return self.next_slot * 8;
    }
};
