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
            .r13 => "r15",
        };
    }
    unreachable;
}

fn getWordSize(size: i64) []const u8 {
    if (size == 8)
        return "qword";
    if (size == 1)
        return "byte";
    unreachable;
}

const isAlloced = Inst.isAlloced;

fn prepareForLoc(loc: Inst.Location) !bool {
    switch (loc) {
        .register => {},
        .void => unreachable,
    }
    return false;
}

fn dumpLocation(loc: Inst.Location, size: i64, alloc: Allocator) ![]const u8 {
    return switch (loc) {
        .register => |_r| reg(_r, size),
        .stack => |decal| try std.fmt.allocPrint(alloc, "qword [rbp - {d}]", .{decal * 8}),
        .label => |lab| try std.fmt.allocPrint(alloc, "qword [{s}]", .{lab}),
        .argument => |arg| {
            // arguments:
            //      rdi, rsi, rcx, rdx, r8, r9, stack + 0, stack + 1, stack + 2 ...
            //      0    1    2    3    4   5   6          7          8         ...
            // However because of the calling convention, rbp is saved on the stack as well as the return address
            // and there's a additionnal 8 + 8 = 16 bytes that should be added
            //      stack: | ...arguments...  |  function return address  |  rbp  | ....
            //                                                                    ^_rbp
            // and rbp points to the top of the stack
            if (arg < 6) {
                return ([6][]const u8{ "rdi", "rsi", "rcx", "rdx", "r8", "r9" })[arg];
            }
            return "qword [rbp - %%]";
        },
        .void => unreachable,
    };
}

pub fn dumpAssemblyX86(builder: *const Inst.Builder, entry_point: []const u8) !void {
    const file = try std.fs.cwd().createFile(
        "output.asm",
        .{ .read = false, .truncate = true },
    );
    defer file.close();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const writer = file.writer();

    _ = try writer.print("default rel\nsection .text align=16\n\tglobal {s}\nextern _malloc\nextern _free\n", .{entry_point});
    for (builder.code.items) |instruction| {
        switch (instruction) {
            //.Plus => |inst| writer.writeAll("\tPlus({}, {})\n"),
            //.Minus => |inst| writer.writeAll("\tMinus({}, {})\n"),
            .Function => |fname| {
                try writer.print("\n{s}:\n", .{fname});
            },
            .BeginFunction => {
                // At the beggining of the function, first instructions
                try writer.print("\tpush rbp\n", .{});
                try writer.print("\tmov rbp, rsp\n", .{});
            },
            .EndFunction => {
                // At the end of the function, last instructions
                try writer.print("\tmov rsp, rbp \n", .{});
                try writer.print("\tpop rbp\n", .{});
            },
            .reserveStack => |inst| {
                try writer.print("\tadd rsp, {d}\n", .{-inst});
            },
            .Move => |inst| {
                if (inst._type == .voidType)
                    continue;
                if (inst.to == .register and inst.from == .label) {
                    try writer.print("\tlea {s}, {s}\n", .{ reg(inst.to.register, 8), inst.from.label });
                } else {
                    // Assuming inst.to and inst.from are not both on the stack
                    if (inst.to == .stack and inst.from == .stack) {
                        return error.UnsupportedBothStackValues;
                    }
                    try writer.print("\tmov {s}, {s}\n", .{
                        try dumpLocation(inst.to, Inst.getCompileSize(inst._type), alloc),
                        try dumpLocation(inst.from, Inst.getCompileSize(inst._type), alloc),
                    });
                }
            },
            .Load => |inst| {
                try writer.print("\tmov {s}, {s}\n", .{
                    try dumpLocation(inst.to, 8, alloc),
                    try dumpLocation(inst.from, 8, alloc),
                });
            },
            .loadAddress => |inst| {
                try writer.print("\tlea {s}, [{s}]\n", .{
                    try dumpLocation(inst.to, 8, alloc),
                    if (inst.from == .stack)
                        try std.fmt.allocPrint(alloc, "rbp - {d}", .{(inst.from.stack) * 8})
                    else
                        try dumpLocation(inst.from, 8, alloc),
                });
            },
            .IntLit => |inst| {
                try writer.print("\tmov {s}, {d}\n", .{
                    try dumpLocation(inst.to, 8, alloc),
                    inst.val,
                });
            },
            .CharLit => |inst| {
                try writer.print("\tmov {s}, {d}\n", .{
                    try dumpLocation(inst.to, 8, alloc),
                    inst.val,
                });
            },
            .writeArrayElement => |inst| {
                try writer.print("\tmov r15, {s}\n", .{try dumpLocation(inst.arr, 8, alloc)});
                const mult = Inst.getCompileSize(inst._type);
                // Fat pointer so add 8 to the index
                const decal: i64 = @intCast(inst.idx);
                try writer.print("\tlea r15, [r15 + {d}]\n", .{mult * decal + 8});
                try writer.print("\tmov {s} [r15], {s}\n", .{
                    getWordSize(Inst.getCompileSize(inst._type)),
                    try dumpLocation(inst.value, Inst.getCompileSize(inst._type), alloc),
                });
            },
            .decreaseStack => |_| {
                try writer.print("\tadd rsp, 8\n", .{});
            },
            .ExitWith => |inst| {
                try writer.print("\tmov rdi, {s}\n", .{
                    try dumpLocation(inst, 8, alloc),
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

                // Moving into r15 for facilitating manipulation
                try writer.print("\tmov r15, {s}\n", .{
                    try dumpLocation(inst, 8, alloc),
                });

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
                    try writer.print("\tmov rax, {s}\n", .{
                        try dumpLocation(ret, 8, alloc),
                    });
                }
                try writer.print("\tret\n", .{});
            },
            .Comment => |cmt| {
                try writer.print("\t;; {s}\n", .{cmt});
            },
            .addImmediate => |adim| {
                try writer.print("\tadd {s}, {d}\n", .{
                    try dumpLocation(adim.x, 8, alloc),
                    adim.y,
                });
            },
            .getBasePointer => |loc| {
                try writer.print("\tmov {s}, rbp\n", .{
                    try dumpLocation(loc, 8, alloc),
                });
            },
            .getStackPointer => |loc| {
                try writer.print("\tmov {s}, rsp\n", .{
                    try dumpLocation(loc, 8, alloc),
                });
            },
            .Funcall => |funcall| {
                // first thing : we push the current

                // x86 calling convention :
                // registers rcx -> r9 used for the first arguments then everything else on the stack
                // rdi, rsi, rcx, rdx, r8, r9, stack, stack + 1, stack + 2...
                const registers: [6][]const u8 = .{ "rdi", "rsi", "rcx", "rdx", "r8", "r9" };

                // Because of the codegen, all the arguments are on the stack
                // in the reverse order (last one on the top)
                for (funcall.args.items, 0..) |arg, idx| {
                    if (idx < 6) {
                        try writer.print("\tmov {s}, {s}\n", .{
                            registers[idx],
                            try dumpLocation(arg, 8, alloc),
                        });
                    } else {
                        try writer.print("\tpush {s}\n", .{try dumpLocation(arg, 8, alloc)});
                    }
                }
                if (funcall.func == .label) {
                    try writer.print("\tcall {s}\n", .{funcall.func.label});
                } else {
                    try writer.print("\tcall {s}\n", .{
                        try dumpLocation(funcall.func, 8, alloc),
                    });
                }
                for (funcall.args.items, 0..) |_, idx| {
                    if (idx >= 6) {
                        try writer.print("\tpop\n", .{});
                    }
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
                try writer.print("\ttest {s}, {s}\n", .{
                    try dumpLocation(inst.value, 8, alloc),
                    try dumpLocation(inst.value, 8, alloc),
                });
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
                // Assuming inst.to and inst.from are not both on the stack
                if (plus.x == .stack and plus.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }

                try writer.print(
                    "\tadd {s}, {s}\n",
                    .{
                        try dumpLocation(plus.x, 8, alloc),
                        try dumpLocation(plus.y, 8, alloc),
                    },
                );
            },
            .Minus => |minus| {
                if (minus.x == .stack and minus.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print(
                    "\tsub {s}, {s}\n",
                    .{
                        try dumpLocation(minus.x, 8, alloc),
                        try dumpLocation(minus.y, 8, alloc),
                    },
                );
            },
            .Multiply => |mul| {
                if (mul.x == .stack and mul.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print(
                    "\timul {s}, {s}\n",
                    .{
                        try dumpLocation(mul.x, 8, alloc),
                        try dumpLocation(mul.y, 8, alloc),
                    },
                );
            },
            .Divide => |div| {
                if (div.x == .stack and div.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                // x86 division: dividend in rax, divisor in reg/mem, result in rax, remainder in rdx
                // Move dividend to rax
                if (div.x != .register or div.x.register != .r0)
                    try writer.print("\tpush rax\n", .{});
                try writer.print("\tpush rdx\n", .{});
                try writer.print("\tmov r15, {s}\n", .{
                    try dumpLocation(div.y, 8, alloc),
                });
                if (div.x != .register or div.x.register != .r0)
                    try writer.print("\tmov rax, {s}\n", .{
                        try dumpLocation(div.x, 8, alloc),
                    });
                try writer.print("\tcqo\n", .{});
                try writer.print("\tidiv r15\n", .{});
                try writer.print("\tmov {s}, rax\n", .{
                    try dumpLocation(div.x, 8, alloc),
                }); // res <- rax, because it is in rax
                try writer.print("\tpop rdx\n", .{});
                if (div.x != .register or div.x.register != .r0)
                    try writer.print("\tpop rax\n", .{});
            },
            .Modulo => |mod| {
                if (mod.x == .stack and mod.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                // x86 division: dividend in rax, divisor in reg/mem, result in rax, remainder in rdx
                // Move dividend to rax
                if (mod.x != .register or mod.x.register != .r0)
                    try writer.print("\tpush rax\n", .{});
                try writer.print("\tpush rdx\n", .{});
                try writer.print("\tmov r15, {s}\n", .{
                    try dumpLocation(mod.y, 8, alloc),
                });
                if (mod.x != .register or mod.x.register != .r0)
                    try writer.print("\tmov rax, {s}\n", .{
                        try dumpLocation(mod.x, 8, alloc),
                    });
                try writer.print("\tcqo\n", .{});
                try writer.print("\tidiv r15\n", .{});
                try writer.print("\tmov {s}, rdx\n", .{
                    try dumpLocation(mod.x, 8, alloc),
                });
                try writer.print("\tpop rdx\n", .{});
                if (mod.x != .register or mod.x.register != .r0)
                    try writer.print("\tpop rax\n", .{});
            },
            .Equal => |eq| {
                if (eq.x == .stack and eq.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{
                    try dumpLocation(eq.x, 8, alloc),
                    try dumpLocation(eq.y, 8, alloc),
                });
                try writer.print("\tsete al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{
                    try dumpLocation(eq.x, 8, alloc),
                });
            },
            .NotEqual => |neq| {
                if (neq.x == .stack and neq.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{
                    try dumpLocation(neq.x, 8, alloc),
                    try dumpLocation(neq.y, 8, alloc),
                });
                try writer.print("\tsetne al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{
                    try dumpLocation(neq.x, 8, alloc),
                });
            },
            .LessThan => |lt| {
                if (lt.x == .stack and lt.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{
                    try dumpLocation(lt.x, 8, alloc),
                    try dumpLocation(lt.y, 8, alloc),
                });
                try writer.print("\tsetl al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{
                    try dumpLocation(lt.x, 8, alloc),
                });
            },
            .LessEqual => |le| {
                if (le.x == .stack and le.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{
                    try dumpLocation(le.x, 8, alloc),
                    try dumpLocation(le.y, 8, alloc),
                });
                try writer.print("\tsetle al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{
                    try dumpLocation(le.x, 8, alloc),
                });
            },
            .GreaterThan => |gt| {
                if (gt.x == .stack and gt.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{
                    try dumpLocation(gt.x, 8, alloc),
                    try dumpLocation(gt.y, 8, alloc),
                });
                try writer.print("\tsetg al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{
                    try dumpLocation(gt.x, 8, alloc),
                });
            },
            .GreaterEqual => |ge| {
                if (ge.x == .stack and ge.y == .stack) {
                    return error.UnsupportedBothStackValues;
                }
                try writer.print("\tcmp {s}, {s}\n", .{
                    try dumpLocation(ge.x, 8, alloc),
                    try dumpLocation(ge.y, 8, alloc),
                });
                try writer.print("\tsetge al\n", .{});
                try writer.print("\tmovzx {s}, al\n", .{
                    try dumpLocation(ge.x, 8, alloc),
                });
            },
            .Not => |not| {
                try writer.print("\txor {s}, 1\n", .{
                    try dumpLocation(not, 8, alloc),
                });
            },
            .heapAlloc => |_alloc| {
                if (_alloc.dest != .register or _alloc.dest.register != .r0)
                    try writer.print("\tpush rax\n", .{});
                if (_alloc.dest != .register or _alloc.dest.register != .r5)
                    try writer.print("\tpush rdi\n", .{});

                try writer.print("\tmov rdi, {d}\n", .{8 * _alloc.size});
                try writer.print("\tcall _malloc\n", .{});
                try writer.print("\tmov {s}, rax\n", .{
                    try dumpLocation(_alloc.dest, 8, alloc),
                });

                if (_alloc.dest != .register or _alloc.dest.register != .r5)
                    try writer.print("\tpop rdi\n", .{});
                if (_alloc.dest != .register or _alloc.dest.register != .r0)
                    try writer.print("\tpop rax\n", .{});
            },
            .heapFree => |dest| {
                if (dest != .register or dest.register != .r5)
                    try writer.print("\tpush rdi\n", .{});
                try writer.print("\tmov rdi, {s}\n", .{try dumpLocation(
                    dest,
                    8,
                    alloc,
                )});
                try writer.print("\tcall _free\n", .{});
                if (dest != .register or dest.register != .r5)
                    try writer.print("\tpop rdi\n", .{});
            },
            .writeToPointer => |ptr_write| {
                try writer.print("\tmov QWORD [{s} + {d}], {s}\n", .{
                    try dumpLocation(ptr_write.dest, 8, alloc),
                    8 * ptr_write.decal,
                    try dumpLocation(ptr_write.content, 8, alloc),
                });
            },
            .readFromPointer => |ptr_read| {
                try writer.print("\tmov {s}, QWORD [{s} + {d}]\n", .{
                    try dumpLocation(ptr_read.dest, 8, alloc),
                    try dumpLocation(ptr_read.origin, 8, alloc),
                    8 * ptr_read.decal,
                });
            },
            .incrementeReferenceCounter => |value| {
                try writer.print("\tmov rax, QWORD [{s}]\n", .{
                    try dumpLocation(value, 8, alloc),
                });
                try writer.print("\tadd rax, 1\n", .{});
                try writer.print("\tmov QWORD [{s}], rax\n", .{
                    try dumpLocation(value, 8, alloc),
                });
            },
            .decrementReferenceCounter => |value| {
                try writer.print("\tmov rax, QWORD [{s}]\n", .{
                    try dumpLocation(value, 8, alloc),
                });
                try writer.print("\tsub rax, 1\n", .{});
                try writer.print("\tmov QWORD [{s}], rax\n", .{
                    try dumpLocation(value, 8, alloc),
                });
            },
            .pushValue => |value| {
                try writer.print("\tpush {s}\n", .{
                    try dumpLocation(value, 8, alloc),
                });
            },
            .popValue => |value| {
                try writer.print("\tpop {s}\n", .{
                    try dumpLocation(value, 8, alloc),
                });
            },

            else => unreachable,
        }
    }
}
