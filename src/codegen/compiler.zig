const std = @import("std");
const inst = @import("instructions.zig");
const arch_mod = @import("arch.zig");
const regTable = @import("registerTable.zig");
const analyser = @import("../analyser.zig");

pub const FloatConst = struct { label: []const u8, bits: u64 };

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    arch: arch_mod.ArchConfig,
    registerTable: regTable.RegisterTable,
    program: std.ArrayList(inst.Instruction),

    // the function table, registering all the function with the same name
    f_table: std.hash_map.StringHashMap(usize),
    func_uid_list: std.ArrayList(struct { uid: []const u8, version: analyser.functionVersion }),
    label_table: std.hash_map.StringHashMap(usize),

    // Float literal constants accumulated during codegen; emitted into .data by x86.zig.
    float_constants: std.ArrayList(FloatConst),

    stack_size: i64 = 0,
    // True when the current function's return type has `.err == true` (!T).
    // Divisions and error-producing calls should emit an early return when this is set.
    current_func_propagates_errors: bool = false,
    // True while evaluating the expression inside `value ? err fallback`.
    // Suppresses early-return-on-error inside sub-expressions so the ? err can catch it.
    in_error_check_expr: bool = false,
    // Set by codegenFuncdef to the label of the function-level GC cleanup block.
    // emitEarlyReturnOnError jumps here instead of doing a bare frame restore,
    // so heap locals and args are decremented before the function returns on error.
    error_exit_label: ?[]const u8 = null,
    // Frame pre-computation: when true, the full frame is reserved in the prologue.
    // codegenScope and forloop skip sub/add rsp and use frame_slot_cursor instead of
    // stack_size for variable slot assignment. stack_size stays fixed = total frame depth.
    frame_is_fixed: bool = false,
    // Next available slot index for local variable assignment in fixed-frame mode.
    // Starts after arg slots; incremented by codegenScope and forloop for each local.
    frame_slot_cursor: i64 = 0,

    /// Returns true if an extra 8-byte pad is needed before a C call to maintain 16-byte
    /// alignment. Always false on ARM64 (every push is 16 bytes, sp never misaligns).
    pub fn needsCallPadding(self: *const Compiler) bool {
        return self.arch.target != .arm64 and @mod(self.stack_size, 2) != 0;
    }

    pub fn incrementStackOffset(self: *Compiler) void {
        self.stack_size += 1;
    }

    pub fn decrementStackOffset(self: *Compiler) void {
        self.stack_size -= 1;
    }

    // here are defined global variables for the code generation
    pub inline fn getErrorUnionFunction() []const u8 {
        return "global.error.union_callback";
    }
    pub inline fn getErrorUnionLabel() []const u8 {
        return "global.error.union";
    }
    pub inline fn getMainWrapper(self: *const Compiler) []const u8 {
        return self.arch.entry_point;
    }
    pub inline fn getGlobalFreeObject() []const u8 {
        return "global.object.free";
    }

    pub fn init(alloc: std.mem.Allocator, arch: arch_mod.ArchConfig) !*Compiler {
        const c = try alloc.create(Compiler);
        c.* = Compiler{
            .allocator = alloc,
            .arch = arch,
            .registerTable = try regTable.RegisterTable.init(alloc, arch),
            .program = .init(alloc),
            .f_table = .init(alloc),
            .label_table = .init(alloc),
            .func_uid_list = .init(alloc),
            .float_constants = .init(alloc),
        };
        return c;
    }

    pub fn addInstruction(self: *Compiler, instruction: inst.Instruction) !void {
        try self.program.append(instruction);
    }

    // ── Platform-specific emit helpers ─────────────────────────────────────────
    // These encapsulate the differences between x86-64 and ARM64 so that callers
    // (print.zig, errorcheck.zig, …) can stay arch-agnostic.  Adding a new target
    // only requires implementing the dispatch here, not touching every call site.

    /// Emit a platform-aligned function call.
    /// x86-64: if stack_depth is odd, pad with sub/add rsp,8 around the call.
    /// ARM64:  no padding — every push is 16 bytes so sp stays 16-byte aligned.
    pub fn emitAlignedCall(self: *Compiler, label: []const u8, stack_depth: i64) !void {
        const needs_padding = self.arch.target != .arm64 and @mod(stack_depth, 2) != 0;
        if (needs_padding)
            try self.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
        try self.addInstruction(.{ .call = .{ .value = .{ .label = label } } });
        if (needs_padding)
            try self.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = 8 } } });
    }

    /// Emit printf(fmt_label, int_val) using the platform's variadic calling convention.
    /// x86-64: val → R4 (rsi), fmt → R5 (rdi), aligned call.
    /// ARM64:  val pushed to [sp] (Apple ABI: variadic args on stack), fmt → x0, bl _printf.
    pub fn emitPrintfInt(self: *Compiler, val_reg: inst.Register, fmt_label: []const u8, stack_depth: i64) !void {
        if (self.arch.target == .arm64) {
            try self.addInstruction(.{ .push  = .{ .reg = val_reg } });
            try self.addInstruction(.{ .lea   = .{ .adr = .{ .label = fmt_label }, .to = .R5 } });
            try self.addInstruction(.{ .call  = .{ .value = .{ .label = "_printf" } } });
            try self.addInstruction(.{ .pop   = .{ .reg = .R2 } }); // restore sp (dummy pop)
        } else {
            try self.addInstruction(.{ .load  = .{ .from = .{ .register = val_reg }, .to = .R4 } });
            try self.addInstruction(.{ .lea   = .{ .adr = .{ .label = fmt_label }, .to = .R5 } });
            try self.emitAlignedCall("_printf", stack_depth);
        }
    }

    /// Reserve one pointer-sized stack slot for a temporary variable and track it.
    /// x86-64: sub rsp, 8   + incrementStackOffset
    /// ARM64:  sub sp, sp, 16 (16-byte alignment required; slot is at [sp] after decrement)
    pub fn pushStackSlot(self: *Compiler) !void {
        const sz: i64 = @intCast(self.arch.push_size);
        try self.addInstruction(.{ .minus = .{ .lhs = .RSP, .rhs = .{ .immediate = sz } } });
        self.incrementStackOffset();
    }

    /// Release a stack slot previously allocated with pushStackSlot.
    /// x86-64: add rsp, 8   + decrementStackOffset
    /// ARM64:  add sp, sp, 16
    pub fn popStackSlot(self: *Compiler) !void {
        const sz: i64 = @intCast(self.arch.push_size);
        try self.addInstruction(.{ .plus = .{ .lhs = .RSP, .rhs = .{ .immediate = sz } } });
        self.decrementStackOffset();
    }

    // Emit: if bbc_error_flag != 0 then restore frame and ret.
    // rax must already hold the ErrorObj* when this fires — do NOT zero it.
    // Invariant: whenever bbc_error_flag=1, the most recent error was produced by
    // bbc_make_error_fn (rax = malloc result) or propagated from a callee (rax = callee rax).
    pub fn emitEarlyReturnOnError(self: *Compiler, ok_label: inst.LABEL) !void {
        try self.addInstruction(.{ .cmp = .{ .val1 = .{ .rip_memory = "bbc_error_flag" }, .val2 = .{ .immediate = 0 } } });
        try self.addInstruction(.{ .jcond = .{ .cc = .Z, .where = ok_label.name } });
        if (self.error_exit_label) |lbl| {
            try self.addInstruction(.{ .jmp = .{ .where = lbl } });
        } else {
            try self.addInstruction(.{ .load = .{ .from = .{ .register = .RBP }, .to = .RSP } });
            if (self.arch.target == .arm64)
                try self.addInstruction(.{ .load = .{ .from = .{ .register = .R0 }, .to = .R5 } });
            try self.addInstruction(.{ .frame_leave = .{} });
        }
        try self.addInstruction(.{ .label = ok_label });
    }

    pub fn generateLabel(self: *Compiler, name: []const u8) !inst.LABEL {
        const r = try self.label_table.getOrPutValue(name, 0);
        try self.label_table.put(name, r.value_ptr.* + 1);
        return inst.LABEL{ .name = try std.fmt.allocPrint(
            self.allocator,
            "{s}_{d}",
            .{ name, r.value_ptr.* },
        ) };
    }

    /// Strip `!` from inside `<...>` in a function name so that an error-union
    /// variant like `Foo<!Bar>` resolves to the same UID as `Foo<Bar>`.
    fn normalizeErrVariant(name: []const u8, alloc: std.mem.Allocator) ![]const u8 {
        var out = try std.ArrayList(u8).initCapacity(alloc, name.len);
        var depth: usize = 0;
        for (name) |c| {
            switch (c) {
                '<' => { depth += 1; try out.append(c); },
                '>' => { depth -|= 1; try out.append(c); },
                '!' => if (depth == 0) try out.append(c),
                else => try out.append(c),
            }
        }
        return out.toOwnedSlice();
    }

    pub fn getUid(self: *Compiler, version: analyser.functionVersion) []const u8 {
        // First pass: exact match.
        base_for: for (self.func_uid_list.items) |func| {
            if (std.mem.eql(u8, func.version.name, version.name)) {
                if (func.version.version.count() != version.version.count())
                    continue;
                var it = func.version.version.iterator();
                while (it.next()) |vers| {
                    if (!version.version.contains(vers.key_ptr.*))
                        continue :base_for;
                    const arg_type = version.version.get(vers.key_ptr.*).?;
                    if (!arg_type.matchType(vers.value_ptr.*))
                        continue :base_for;
                }
                return func.uid;
            }
        }
        // Second pass: if this is an error-union variant (name contains `!`
        // inside `<>`), fall back to the base (non-error) specialisation's UID.
        const norm = normalizeErrVariant(version.name, self.allocator) catch version.name;
        if (!std.mem.eql(u8, norm, version.name)) {
            for (self.func_uid_list.items) |func| {
                if (std.mem.eql(u8, func.version.name, norm)) {
                    return func.uid;
                }
            }
        }
        std.debug.print("[INTERN ERROR]: Couldnt find version for {s}\n", .{version.name});
        return version.name;
    }
};
