const std = @import("std");

const Inst = @import("instructions.zig");

const Allocator = std.mem.Allocator;

const FileWriter = std.io.Writer(std.fs.File, std.fs.File.WriteError, std.fs.File.write);

pub fn reg(r: Inst.Registers, size: i64) []const u8 {
    if (size == 8) {
        return switch (r) {
            .r0 => "rax",
            .r1 => "rbx",
            .r2 => "rcx",
            .r3 => "rdx",
            .r4 => "rsi",
            .r5 => "rdi",
            .r6 => "r8",
            .r7 => "r9",
            .r8 => "r10",
            .r9 => "r11",
            .r10 => "r12",
            .r11 => "r13",
            .r12 => "r14",
            else => unreachable,
        };
    }
    return switch (r) {
        .r0 => "al",
        .r1 => "bl",
        .r2 => "cl",
        .r3 => "dl",
        .r4 => "sil",
        .r5 => "dil",
        .r6 => "r8b",
        .r7 => "r9b",
        .r8 => "r10b",
        .r9 => "r11b",
        .r10 => "r12b",
        .r11 => "r13b",
        .r12 => "r14b",
        else => unreachable,
    };
}

fn getWordSize(size: i64) []const u8 {
    if (size == 8)
        return "qword";
    if (size == 1)
        return "byte";
    unreachable;
}

const isAlloced = Inst.isAlloced;

fn prepareForLoc(writer: FileWriter, loc: Inst.Location) !bool {
    switch (loc) {
        .stack => |d| {
            var size_accumulator: i64 = @intCast(0);
            try writer.print("\txor r15, r15\n", .{});
            for (d.idx..d.stack_state.items.len) |i| {
                const ttype = d.stack_state.items[d.stack_state.items.len - (i - d.idx) - 1];
                if (isAlloced(ttype)) {
                    if (size_accumulator != 0)
                        try writer.print("\tadd r15, {d}\n", .{size_accumulator});

                    try writer.print("\tadd r15, qword [rsp + r15]\n", .{});
                    size_accumulator = 8; // The size from the fat-pointer
                } else {
                    size_accumulator += Inst.getCompileSize(ttype);
                }
            }
            if (size_accumulator != 0)
                try writer.print("\tadd r15, {d}\n", .{size_accumulator});

            return true;
        },
        .register => {},
        .label => |l| {
            try writer.print("\tlea r15, {s}\n", .{l});
            return true;
        },
        .void => unreachable,
    }
    return false;
}

fn dumpLocation(loc: Inst.Location, size: i64) []const u8 {
    return switch (loc) {
        .register => |_r| reg(_r, size),
        .stack => "qword [rsp + r15]",
        .label => "qword [r15]",
        .void => unreachable,
    };
}

pub fn dumpAssemblyX86(builder: *const Inst.Builder, entry_point: []const u8) !void {
    const file = try std.fs.cwd().createFile(
        "output.asm",
        .{ .read = false, .truncate = true },
    );
    defer file.close();

    const writer = file.writer();

    //_ = try writer.write("%macro LOAD_ADDRESS 2\n\tadrp %1, %2@PAGE\n\tadd  %1, %1, %2@PAGEOFF\n%endmacro\n");
    _ = try writer.print("default rel\nsection .text align=8\n\tglobal {s}\n", .{entry_point});
    for (builder.code.items) |instruction| {
        switch (instruction) {
            //.Plus => |inst| writer.writeAll("\tPlus({}, {})\n"),
            //.Minus => |inst| writer.writeAll("\tMinus({}, {})\n"),
            .Function => |fname| {
                try writer.print("\n{s}:\n", .{fname});
            },
            .BeginFunction => {
                try writer.print("\tpush rbp\n", .{});
                try writer.print("\tmov rbp, rsp\n", .{});
            },
            .EndFunction => {
                try writer.print("\tmov rsp, rbp \n", .{});
                try writer.print("\tpop rbp\n", .{});
            },
            .reserveStack => |inst| {
                try writer.print("\tadd rsp, {d}\n", .{-inst});
            },
            .Move => |inst| {
                if (inst.to == .register and inst.from == .label) {
                    try writer.print("\tlea {s}, {s}\n", .{ reg(inst.to.register, 8), inst.from.label });
                } else if (inst.to == .stack and inst.from == .label) {
                    try writer.print("\tpush rdx\n", .{});
                    try writer.print("\tlea rdx, {s}\n", .{inst.from.label});
                    _ = try prepareForLoc(writer, inst.to); // So the value sould be in r15
                    try writer.print("\tmov [rsp + r15 + 8], rdx\n", .{}); // We need to offset by 8 because of rdx being on the stack
                    try writer.print("\tpop rdx\n", .{});
                } else {
                    _ = try prepareForLoc(writer, inst.to);
                    _ = try prepareForLoc(writer, inst.from);
                    // Assuming inst.to and inst.from are not both on the stack
                    if (inst.to == .stack and inst.from == .stack) {
                        return error.UnsupportedBothStackValues;
                    }
                    try writer.print("\tmov {s}, {s}\n", .{
                        dumpLocation(inst.to, Inst.getCompileSize(inst._type)),
                        dumpLocation(inst.from, Inst.getCompileSize(inst._type)),
                    });
                }
            },
            .Load => |inst| {
                _ = try prepareForLoc(writer, inst.to);
                _ = try prepareForLoc(writer, inst.from);
                try writer.print("\tmov {s}, {s}\n", .{
                    dumpLocation(inst.to, 8),
                    dumpLocation(inst.from, 8),
                });
            },
            .loadAddress => |inst| {
                const pop_ra = try prepareForLoc(writer, inst.to);
                if (pop_ra)
                    try writer.print("\tpush r15\n", .{});
                const pop_rb = try prepareForLoc(writer, inst.from);
                if (pop_rb)
                    try writer.print("\tpush r15\n", .{});
                if (pop_ra or pop_rb)
                    try writer.print("\tpop r15\n", .{});

                try writer.print("\tlea {s}, [{s}]\n", .{ dumpLocation(inst.to, 8), dumpLocation(inst.from, 8) });
            },
            .IntLit => |inst| {
                _ = try prepareForLoc(writer, inst.to);

                try writer.print("\tmov {s}, {d}\n", .{
                    dumpLocation(inst.to, 8),
                    inst.val,
                });
            },
            .CharLit => |inst| {
                _ = try prepareForLoc(writer, inst.to);

                try writer.print("\tmov {s}, {d}\n", .{
                    dumpLocation(inst.to, 8),
                    inst.val,
                });
            },
            .writeArrayElement => |inst| {
                _ = try prepareForLoc(writer, inst.arr);
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(inst.arr, 8)});
                const mult = Inst.getCompileSize(inst._type);
                // Fat pointer so add 8 to the index
                const decal: i64 = @intCast(inst.idx);
                try writer.print("\tlea r15, [r15 + {d}]\n", .{mult * decal + 8});
                try writer.print("\tmov {s} [r15], {s}\n", .{ getWordSize(Inst.getCompileSize(inst._type)), dumpLocation(inst.value, Inst.getCompileSize(inst._type)) }); // TODO: change qword by the wright size
            },
            .decreaseStack => |t| {
                // Removes what is on top of the stack (trusting that it is the wright type)
                if (isAlloced(t)) { // reading the size of the fat pointer
                    try writer.print("\tmov r15, qword [rsp]\n", .{});
                    try writer.print("\tadd rsp, r15\n", .{});
                } else { // else removing 8 (min size on the stack) on 64 bits cpus
                    try writer.print("\tadd rsp, 8\n", .{});
                }
            },
            .ExitWith => |inst| {
                _ = try prepareForLoc(writer, inst);

                try writer.print("\tmov rdi, {s}\n", .{
                    dumpLocation(inst, 8),
                });

                // No need to save registers because we are exiting (bye bye)
                try writer.print("\tmov rax, 0x2000001\n", .{});
                try writer.print("\tsyscall\n", .{});
            },
            .Print => |inst| {
                // Need to save rsi, rdi, rdx, rax
                try writer.print("\tpush rsi\n", .{});
                try writer.print("\tpush rdi\n", .{});
                try writer.print("\tpush rdx\n", .{});
                try writer.print("\tpush rax\n", .{});

                // Preparing for location and dumping the location
                _ = try prepareForLoc(writer, inst);

                // Moving into r15 for facilitating manipulation
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(inst, 8)});

                try writer.print("\tmov rdi, 1\n", .{});
                try writer.print("\tmov rsi, r15\n", .{});
                try writer.print("\tlea rsi, [rsi + 8]\n", .{});
                try writer.print("\tmov rdx, qword [r15]\n", .{});

                try writer.print("\tmov rax, 0x2000004\n", .{});
                try writer.print("\tsyscall\n", .{});

                try writer.print("\tpop rax\n", .{});
                try writer.print("\tpop rdx\n", .{});
                try writer.print("\tpop rdi\n", .{});
                try writer.print("\tpop rsi\n", .{});
            },
            .Return => |ret| {
                if (ret != .void) {
                    _ = try prepareForLoc(writer, ret);
                    try writer.print("\tmov rax, {s}\n", .{
                        dumpLocation(ret, 8),
                    });
                }
                try writer.print("\tret\n", .{});
            },
            .Comment => |cmt| {
                try writer.print("\t;; {s}\n", .{cmt});
            },
            .addImmediate => |adim| {
                _ = try prepareForLoc(writer, adim.x);
                try writer.print("\tadd {s}, {d}\n", .{
                    dumpLocation(adim.x, 8),
                    adim.y,
                });
            },
            .getBasePointer => |loc| {
                _ = try prepareForLoc(writer, loc);
                try writer.print("\tmov {s}, rbp\n", .{
                    dumpLocation(loc, 8),
                });
            },
            .getStackPointer => |loc| {
                _ = try prepareForLoc(writer, loc);
                try writer.print("\tmov {s}, rsp\n", .{
                    dumpLocation(loc, 8),
                });
            },
            .Funcall => |funcall| {
                for (funcall.args.items, Inst.RegIter[0..funcall.args.items.len]) |arg, destreg| {
                    _ = try prepareForLoc(writer, arg);
                    try writer.print("\tmov {s}, {s}\n", .{ reg(destreg, 8), dumpLocation(arg, 8) });
                }
                if (funcall.func == .label) {
                    try writer.print("\tcall {s}\n", .{funcall.func.label});
                } else {
                    _ = try prepareForLoc(writer, funcall.func);
                    try writer.print("\tcall {s}\n", .{dumpLocation(funcall.func, 8)});
                }
            },
            .beginVariableSection => {
                try writer.print("\nsection .data\n", .{});
            },
            .declareVariable => |_var| {
                try writer.print("{s}:\n", .{_var.name});
                for (_var.content.items) |cont| {
                    try writer.print("\t{s} {d}\n", .{ switch (cont.size) {
                        8 => "dq",
                        4 => "dd",
                        2 => "yeet",
                        1 => "db",
                        else => "yeet",
                    }, cont.content });
                }
            },
            .ConditionalJump => |inst| {
                _ = try prepareForLoc(writer, inst.value);
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(inst.value, 8)});
                try writer.print("\ttest r15, r15\n", .{});
                try writer.print("\tjnz {s}\n", .{inst.label});
            },
            .Jump => |inst| {
                try writer.print("\tjmp {s}\n", .{inst.label});
            },
            .Label => |inst| {
                try writer.print("{s}:\n", .{inst});
            },
            // Here begins operators
            .Plus => |plus| {
                _ = try prepareForLoc(writer, plus.x);
                _ = try prepareForLoc(writer, plus.y);
                // Assuming inst.to and inst.from are not both on the stack
                if (plus.x == .stack and plus.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }

                try writer.print(
                    "\tadd {s}, {s}\n",
                    .{ dumpLocation(plus.x, 8), dumpLocation(plus.y, 8) },
                );
            },
            .Minus => |minus| {
                _ = try prepareForLoc(writer, minus.x);
                _ = try prepareForLoc(writer, minus.y);
                if (minus.x == .stack and minus.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print(
                    "\tsub {s}, {s}\n",
                    .{ dumpLocation(minus.x, 8), dumpLocation(minus.y, 8) },
                );
            },
            .Multiply => |mul| {
                _ = try prepareForLoc(writer, mul.x);
                _ = try prepareForLoc(writer, mul.y);
                if (mul.x == .stack and mul.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print(
                    "\timul {s}, {s}\n",
                    .{ dumpLocation(mul.x, 8), dumpLocation(mul.y, 8) },
                );
            },
            .Divide => |div| {
                _ = try prepareForLoc(writer, div.x);
                _ = try prepareForLoc(writer, div.y);
                if (div.x == .stack and div.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                // x86 division: dividend in rax, divisor in reg/mem, result in rax, remainder in rdx
                // Move dividend to rax
                if (div.x != .register or div.x.register != .r0)
                    try writer.print("\tpush rax\n", .{});
                try writer.print("\tpush rdx\n", .{});
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(div.y, 8)});
                if (div.x != .register or div.x.register != .r0)
                    try writer.print("\tmov rax, {s}\n", .{dumpLocation(div.x, 8)});
                try writer.print("\tcqo\n", .{});
                try writer.print("\tidiv r15\n", .{});
                try writer.print("\tmov {s}, rax\n", .{dumpLocation(div.x, 8)}); // res <- rax, because it is in rax
                try writer.print("\tpop rdx\n", .{});
                if (div.x != .register or div.x.register != .r0)
                    try writer.print("\tpop rax\n", .{});
            },
            .Modulo => |mod| {
                _ = try prepareForLoc(writer, mod.x);
                _ = try prepareForLoc(writer, mod.y);
                if (mod.x == .stack and mod.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                // x86 division: dividend in rax, divisor in reg/mem, result in rax, remainder in rdx
                // Move dividend to rax
                if (mod.x != .register or mod.x.register != .r0)
                    try writer.print("\tpush rax\n", .{});
                try writer.print("\tpush rdx\n", .{});
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(mod.y, 8)});
                if (mod.x != .register or mod.x.register != .r0)
                    try writer.print("\tmov rax, {s}\n", .{dumpLocation(mod.x, 8)});
                try writer.print("\tcqo\n", .{});
                try writer.print("\tidiv r15\n", .{});
                try writer.print("\tmov {s}, rdx\n", .{dumpLocation(mod.x, 8)});
                try writer.print("\tpop rdx\n", .{});
                if (mod.x != .register or mod.x.register != .r0)
                    try writer.print("\tpop rax\n", .{});
            },
            .Equal => |eq| {
                _ = try prepareForLoc(writer, eq.x);
                _ = try prepareForLoc(writer, eq.y);
                if (eq.x == .stack and eq.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(eq.x, 8), dumpLocation(eq.y, 8) });
                try writer.print("\tsete al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(eq.x, 8)});
            },
            .NotEqual => |neq| {
                _ = try prepareForLoc(writer, neq.x);
                _ = try prepareForLoc(writer, neq.y);
                if (neq.x == .stack and neq.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(neq.x, 8), dumpLocation(neq.y, 8) });
                try writer.print("\tsetne al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(neq.x, 8)});
            },
            .LessThan => |lt| {
                _ = try prepareForLoc(writer, lt.x);
                _ = try prepareForLoc(writer, lt.y);
                if (lt.x == .stack and lt.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(lt.x, 8), dumpLocation(lt.y, 8) });
                try writer.print("\tsetl al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(lt.x, 8)});
            },
            .LessEqual => |le| {
                _ = try prepareForLoc(writer, le.x);
                _ = try prepareForLoc(writer, le.y);
                if (le.x == .stack and le.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(le.x, 8), dumpLocation(le.y, 8) });
                try writer.print("\tsetle al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(le.x, 8)});
            },
            .GreaterThan => |gt| {
                _ = try prepareForLoc(writer, gt.x);
                _ = try prepareForLoc(writer, gt.y);
                if (gt.x == .stack and gt.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(gt.x, 8), dumpLocation(gt.y, 8) });
                try writer.print("\tsetg al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(gt.x, 8)});
            },
            .GreaterEqual => |ge| {
                _ = try prepareForLoc(writer, ge.x);
                _ = try prepareForLoc(writer, ge.y);
                if (ge.x == .stack and ge.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(ge.x, 8), dumpLocation(ge.y, 8) });
                try writer.print("\tsetge al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(ge.x, 8)});
            },
            .Not => |not| {
                _ = try prepareForLoc(writer, not);
                try writer.print("\txor {s}, 1\n", .{dumpLocation(not, 8)});
            },
            else => unreachable,
        }
    }
}
