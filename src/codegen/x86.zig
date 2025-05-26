const std = @import("std");

const Inst = @import("instructions.zig");

const Allocator = std.mem.Allocator;

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
        .r15 => "r15",
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
            .Function => |fname| try writer.print("\n{s}:\n", .{fname}),
            .reserveStack => |inst| {
                try writer.print("\tadd rsp, {d}\n", .{-inst});
            },
            .Move => |inst| {
                if (inst.to == .register and inst.from == .label) {
                    try writer.print("\tlea {s}, {s}\n", .{ reg(inst.to.register), inst.from.label });
                } else {
                    try writer.print("\tmov ", .{});
                    switch (inst.to) {
                        .register => |r| try writer.print("{s}, ", .{reg(r)}),
                        .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                        .label => |l| try writer.print("qword {s}", .{l}),
                        .void => unreachable,
                    }
                    switch (inst.from) {
                        .register => |r| try writer.print("{s}\n", .{reg(r)}),
                        .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                        .label => |l| try writer.print("{s}\n", .{l}),
                        .void => unreachable,
                    }
                }
            },
            .IntLit => |inst| {
                switch (inst.to) {
                    .register => |r| try writer.print("\tmov {s}, {d}\n", .{ reg(r), inst.val }),
                    .stack => |d| try writer.print("\tmov qword [rbp - {d}], {d}\n", .{ d, inst.val }),
                    .label => |l| try writer.print("\tmov qword {s}, {d}\n", .{ l, inst.val }),
                    .void => unreachable,
                }
            },
            .ExitWith => |inst| {
                switch (inst) {
                    .register => |r| try writer.print("\tmov rdi, {s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("\tmov rdi, qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("\tmov rdi, {s}\n", .{l}),
                    .void => unreachable,
                }
                try writer.print("\tmov rax, 0x2000001\n", .{});
                try writer.print("\tsyscall\n", .{});
            },
            .Return => |ret| {
                switch (ret) {
                    .register => |r| try writer.print("\tmov rax, {s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("\tmov rax, qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("\tmov rax, {s}\n", .{l}),
                    .void => unreachable,
                }
                _ = try writer.write("\tret\n");
            },
            .Comment => {},
            .Funcall => |funcall| {
                for (funcall.args.items, Inst.RegIter[0..funcall.args.items.len]) |arg, destreg| {
                    try writer.print("\tmov {s}, ", .{reg(destreg)});
                    switch (arg) {
                        .register => |r| try writer.print("{s}\n", .{reg(r)}),
                        .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                        .label => |l| try writer.print("qword {s}\n", .{l}),
                        .void => unreachable,
                    }
                }
                switch (funcall.func) {
                    .label => |l| try writer.print("\tcall {s}\n", .{l}),
                    .register => |r| try writer.print("\tcall {s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("\tcall qword [rbp - {d}]\n", .{d}),
                    else => unreachable,
                }
            },
            .Plus => |plus| {
                try writer.print("\tadd ", .{});
                switch (plus.x) {
                    .label => |l| try writer.print("{s}, ", .{l}),
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    else => unreachable,
                }
                switch (plus.y) {
                    .label => |l| try writer.print("{s}\n", .{l}),
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    else => unreachable,
                }
            },
            .Minus => |minus| {
                try writer.print("\tsub ", .{});
                switch (minus.x) {
                    .label => |l| try writer.print("{s}, ", .{l}),
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    else => unreachable,
                }
                switch (minus.y) {
                    .label => |l| try writer.print("{s}\n", .{l}),
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    else => unreachable,
                }
            },
            .Multiply => |mul| {
                try writer.print("\timul ", .{});
                switch (mul.x) {
                    .label => |l| try writer.print("{s}, ", .{l}),
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    else => unreachable,
                }
                switch (mul.y) {
                    .label => |l| try writer.print("{s}\n", .{l}),
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    else => unreachable,
                }
            },
            .Divide => |div| {
                // x86-64 division: dividend in rax, divisor as operand, result in rax, remainder in rdx
                // Move dividend to rax
                switch (div.x) {
                    .register => |r| try writer.print("\tmov rax, {s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("\tmov rax, qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("\tmov rax, {s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tcqo\n", .{}); // sign-extend rax into rdx:rax
                try writer.print("\tidiv ", .{});
                switch (div.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                // Result is in rax
            },
            .Modulo => |mod| {
                // x86-64 division: dividend in rax, divisor as operand, result in rax, remainder in rdx
                switch (mod.x) {
                    .register => |r| try writer.print("\tmov rax, {s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("\tmov rax, qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("\tmov rax, {s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tcqo\n", .{});
                try writer.print("\tidiv ", .{});
                switch (mod.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                // Remainder is in rdx
            },
            .Equal => |eq| {
                try writer.print("\tcmp ", .{});
                switch (eq.x) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    .label => |l| try writer.print("{s}, ", .{l}),
                    else => unreachable,
                }
                switch (eq.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tsete al\n\tmovzx rax, al\n", .{});
            },
            .NotEqual => |ne| {
                try writer.print("\tcmp ", .{});
                switch (ne.x) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    .label => |l| try writer.print("{s}, ", .{l}),
                    else => unreachable,
                }
                switch (ne.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tsetne al\n\tmovzx rax, al\n", .{});
            },
            .LessThan => |lt| {
                try writer.print("\tcmp ", .{});
                switch (lt.x) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    .label => |l| try writer.print("{s}, ", .{l}),
                    else => unreachable,
                }
                switch (lt.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tsetl al\n\tmovzx rax, al\n", .{});
            },
            .LessEqual => |le| {
                try writer.print("\tcmp ", .{});
                switch (le.x) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    .label => |l| try writer.print("{s}, ", .{l}),
                    else => unreachable,
                }
                switch (le.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tsetle al\n\tmovzx rax, al\n", .{});
            },
            .GreaterThan => |gt| {
                try writer.print("\tcmp ", .{});
                switch (gt.x) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    .label => |l| try writer.print("{s}, ", .{l}),
                    else => unreachable,
                }
                switch (gt.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tsetg al\n\tmovzx rax, al\n", .{});
            },
            .GreaterEqual => |ge| {
                try writer.print("\tcmp ", .{});
                switch (ge.x) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}], ", .{d}),
                    .label => |l| try writer.print("{s}, ", .{l}),
                    else => unreachable,
                }
                switch (ge.y) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rbp - {d}]\n", .{d}),
                    .label => |l| try writer.print("{s}\n", .{l}),
                    else => unreachable,
                }
                try writer.print("\tsetge al\n\tmovzx rax, al\n", .{});
            },
        }
    }
}
