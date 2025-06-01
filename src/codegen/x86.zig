const std = @import("std");

const Inst = @import("instructions.zig");

const Allocator = std.mem.Allocator;

const FileWriter = std.io.Writer(std.fs.File, std.fs.File.WriteError, std.fs.File.write);

pub fn reg(r: Inst.Registers) []const u8 {
    return switch (r) {
        .r0 => "rax",
        .r1 => "rbx",
        .r2 => "rcx",
        .r3 => "rdx",
        .r4 => "rsi",
        .r5 => "rdi",
        .r6 => "rsp",
        .r7 => "rbp",
        .r8 => "r8",
        .r9 => "r9",
        .r10 => "r10",
        .r11 => "r11",
        .r12 => "r12",
        .r13 => "r13",
        .r14 => "r14",
    };
}

fn isAlloced(t: Inst.Type) bool {
    // Returns wether the type is alloced in memory, or can just be used in registers
    return switch (t) {
        .arrayLike, .structType => true,
        .charType, .intType, .boolType, .pointer, .function, .voidType, .stringType => false,
    };
}

fn prepareForLoc(writer: FileWriter, loc: Inst.Location) !bool {
    switch (loc) {
        .stack => |d| {
            var size_accumulator: i64 = @intCast(0);
            try writer.print("\txor r15, r15\n", .{});
            for (d.idx..d.stack_state.items.len) |i| {
                const ttype = d.stack_state.items[d.stack_state.items.len - 1 - i];
                if (isAlloced(ttype)) {
                    if (size_accumulator != 0)
                        try writer.print("\tadd r15, {d}\n", .{size_accumulator});

                    try writer.print("\tadd r15, qword [rsp + r15]\n", .{});
                    size_accumulator += 8; // The size written
                } else {
                    size_accumulator += Inst.getCompileSize(ttype);
                }
            }
            if (size_accumulator != 0)
                try writer.print("\tadd r15, {d}\n", .{size_accumulator});

            try writer.print("\tpush r15\n", .{});
            return true;
        },
        .register => {},
        .label => {},
        .void => unreachable,
    }
    return false;
}

fn dumpLocation(loc: Inst.Location) []const u8 {
    return switch (loc) {
        .register => |_r| reg(_r),
        .stack => "qword [rsp + r15]",
        .label => |l| l,
        .void => unreachable,
    };
}

pub fn dumpAssemblyX86(builder: *const Inst.Builder) !void {
    const file = try std.fs.cwd().createFile(
        "output.asm",
        .{ .read = false, .truncate = true },
    );
    defer file.close();

    const writer = file.writer();

    //_ = try writer.write("%macro LOAD_ADDRESS 2\n\tadrp %1, %2@PAGE\n\tadd  %1, %1, %2@PAGEOFF\n%endmacro\n");
    _ = try writer.write("default rel\nsection .text align=16\n\tglobal main\n");
    for (builder.code.items) |instruction| {
        switch (instruction) {
            //.Plus => |inst| writer.writeAll("\tPlus({}, {})\n"),
            //.Minus => |inst| writer.writeAll("\tMinus({}, {})\n"),
            .Function => |fname| {
                try writer.print("\n{s}:\n", .{fname});
            },
            .reserveStack => |inst| {
                try writer.print("\tadd rsp, {d}\n", .{-inst});
            },
            .Move => |inst| {
                if (inst.to == .register and inst.from == .label) {
                    try writer.print("\tlea {s}, {s}\n", .{ reg(inst.to.register), inst.from.label });
                } else {
                    const pop_ra = try prepareForLoc(writer, inst.to);
                    const pop_rb = try prepareForLoc(writer, inst.from);
                    // Assuming inst.to and inst.from are not both on the stack
                    if (inst.to == .stack and inst.from == .stack) {
                        return error.UnsupportedBothStackValues; // TODO : implemente the error
                    }
                    if (pop_ra or pop_rb)
                        try writer.print("\tpop r15\n", .{});
                    try writer.print("\tmov {s}, {s}\n", .{
                        dumpLocation(inst.to),
                        dumpLocation(inst.from),
                    });
                }
            },
            .IntLit => |inst| {
                if (try prepareForLoc(writer, inst.to))
                    try writer.print("\tpop r15\n", .{});

                try writer.print("\tmov {s}, {d}\n", .{
                    dumpLocation(inst.to),
                    inst.val,
                });
            },
            .CharLit => |inst| {
                if (try prepareForLoc(writer, inst.to))
                    try writer.print("\tpop r15\n", .{});

                try writer.print("\tmov {s}, {d}\n", .{
                    dumpLocation(inst.to),
                    inst.val,
                });
            },
            .writeArrayElement => |inst| {
                if (try prepareForLoc(writer, inst.arr))
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(inst.arr)});
                const mult = Inst.getCompileSize(inst._type);
                // Fat pointer so add 8 to the index
                const decal: i64 = @intCast(inst.idx);
                try writer.print("\tlea r15, [r15 + {d}]\n", .{mult * decal + 8});
                try writer.print("\tmov qword [r15], {s}\n", .{dumpLocation(inst.value)}); // TODO: change qword by the wright size
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
                if (try prepareForLoc(writer, inst))
                    try writer.print("\tpop r15\n", .{});

                try writer.print("\tmov rdi, {s}\n", .{
                    dumpLocation(inst),
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
                if (try prepareForLoc(writer, inst))
                    try writer.print("\tpop r15\n", .{});

                // Moving into r15 for facilitating manipulation
                try writer.print("\tmov r15, {s}\n", .{dumpLocation(inst)});

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
                if (try prepareForLoc(writer, ret))
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tmov rax, {s}\n", .{
                    dumpLocation(ret),
                });
                try writer.print("\tret\n", .{});
            },
            .Comment => {},
            .addImmediate => |adim| {
                if (try prepareForLoc(writer, adim.x))
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tadd {s}, {d}\n", .{
                    dumpLocation(adim.x),
                    adim.y,
                });
            },
            .getBasePointer => |loc| {
                if (try prepareForLoc(writer, loc))
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tmov {s}, rbp\n", .{
                    dumpLocation(loc),
                });
            },
            .getStackPointer => |loc| {
                if (try prepareForLoc(writer, loc))
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tmov {s}, rsp\n", .{
                    dumpLocation(loc),
                });
            },
            .Funcall => |funcall| {
                for (funcall.args.items, Inst.RegIter[0..funcall.args.items.len]) |arg, destreg| {
                    if (try prepareForLoc(writer, arg))
                        try writer.print("\tpop r15\n", .{});
                    try writer.print("\tmov {s}, {s}\n", .{ reg(destreg), dumpLocation(arg) });
                }
                if (try prepareForLoc(writer, funcall.func))
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcall {s}\n", .{dumpLocation(funcall.func)});
            },
            .Plus => |plus| {
                const pop_rx = try prepareForLoc(writer, plus.x);
                const pop_ry = try prepareForLoc(writer, plus.y);
                // Assuming inst.to and inst.from are not both on the stack
                if (plus.x == .stack and plus.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});

                try writer.print(
                    "\tadd {s}, {s}\n",
                    .{ dumpLocation(plus.x), dumpLocation(plus.y) },
                );
            },
            .Minus => |minus| {
                const pop_rx = try prepareForLoc(writer, minus.x);
                const pop_ry = try prepareForLoc(writer, minus.y);
                if (minus.x == .stack and minus.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print(
                    "\tsub {s}, {s}\n",
                    .{ dumpLocation(minus.x), dumpLocation(minus.y) },
                );
            },
            .Multiply => |mul| {
                const pop_rx = try prepareForLoc(writer, mul.x);
                const pop_ry = try prepareForLoc(writer, mul.y);
                if (mul.x == .stack and mul.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print(
                    "\timul {s}, {s}\n",
                    .{ dumpLocation(mul.x), dumpLocation(mul.y) },
                );
            },
            .Divide => |div| {
                const pop_rx = try prepareForLoc(writer, div.x);
                const pop_ry = try prepareForLoc(writer, div.y);
                if (div.x == .stack and div.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                // x86 division: dividend in rax, divisor in reg/mem, result in rax, remainder in rdx
                // Move dividend to rax
                try writer.print("\tmov rax, {s}\n", .{dumpLocation(div.x)});
                try writer.print("\tcqo\n", .{}); // Sign-extend rax into rdx:rax
                try writer.print("\tidiv {s}\n", .{dumpLocation(div.y)});
                // Move result back to destination
                try writer.print("\tmov {s}, rax\n", .{dumpLocation(div.x)});
            },
            .Modulo => |mod| {
                const pop_rx = try prepareForLoc(writer, mod.x);
                const pop_ry = try prepareForLoc(writer, mod.y);
                if (mod.x == .stack and mod.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tmov rax, {s}\n", .{dumpLocation(mod.x)});
                try writer.print("\tcqo\n", .{});
                try writer.print("\tidiv {s}\n", .{dumpLocation(mod.y)});
                try writer.print("\tmov {s}, rdx\n", .{dumpLocation(mod.x)});
            },
            .Equal => |eq| {
                const pop_rx = try prepareForLoc(writer, eq.x);
                const pop_ry = try prepareForLoc(writer, eq.y);
                if (eq.x == .stack and eq.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(eq.x), dumpLocation(eq.y) });
                try writer.print("\tsete al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(eq.x)});
            },
            .NotEqual => |neq| {
                const pop_rx = try prepareForLoc(writer, neq.x);
                const pop_ry = try prepareForLoc(writer, neq.y);
                if (neq.x == .stack and neq.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(neq.x), dumpLocation(neq.y) });
                try writer.print("\tsetne al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(neq.x)});
            },
            .LessThan => |lt| {
                const pop_rx = try prepareForLoc(writer, lt.x);
                const pop_ry = try prepareForLoc(writer, lt.y);
                if (lt.x == .stack and lt.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(lt.x), dumpLocation(lt.y) });
                try writer.print("\tsetl al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(lt.x)});
            },
            .LessEqual => |le| {
                const pop_rx = try prepareForLoc(writer, le.x);
                const pop_ry = try prepareForLoc(writer, le.y);
                if (le.x == .stack and le.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(le.x), dumpLocation(le.y) });
                try writer.print("\tsetle al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(le.x)});
            },
            .GreaterThan => |gt| {
                const pop_rx = try prepareForLoc(writer, gt.x);
                const pop_ry = try prepareForLoc(writer, gt.y);
                if (gt.x == .stack and gt.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(gt.x), dumpLocation(gt.y) });
                try writer.print("\tsetg al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(gt.x)});
            },
            .GreaterEqual => |ge| {
                const pop_rx = try prepareForLoc(writer, ge.x);
                const pop_ry = try prepareForLoc(writer, ge.y);
                if (ge.x == .stack and ge.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                if (pop_rx or pop_ry)
                    try writer.print("\tpop r15\n", .{});
                try writer.print("\tcmp {s}, {s}\n", .{ dumpLocation(ge.x), dumpLocation(ge.y) });
                try writer.print("\tsetge al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{dumpLocation(ge.x)});
            },
        }
    }
}
