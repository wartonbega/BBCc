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

    _ = try writer.write("default rel\n\nsection .text\n\tglobal _main\n");
    for (builder.code.items) |instruction| {
        switch (instruction) {
            //.Plus => |inst| writer.writeAll("\tPlus({}, {})\n"),
            //.Minus => |inst| writer.writeAll("\tMinus({}, {})\n"),
            .Function => |fname| try writer.print("{s}:\n", .{fname}),
            .reserveStack => |inst| try writer.print("\tadd rsp, {d}\n", .{-inst}),
            .Move => |inst| {
                try writer.print("\tmov ", .{});
                switch (inst.to) {
                    .register => |r| try writer.print("{s}, ", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rsp + {d}], ", .{d}),
                    .void => unreachable,
                }
                switch (inst.from) {
                    .register => |r| try writer.print("{s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("qword [rsp + {d}]\n", .{d}),
                    .void => unreachable,
                }
            },
            .IntLit => |inst| {
                switch (inst.to) {
                    .register => |r| try writer.print("\tmov {s}, {d}\n", .{ reg(r), inst.val }),
                    .stack => |d| try writer.print("\tmov qword [rsp + {d}], {d}\n", .{ d, inst.val }),
                    .void => unreachable,
                }
            },
            .ExitWith => |inst| {
                switch (inst) {
                    .register => |r| try writer.print("\tmov rdi, {s}\n", .{reg(r)}),
                    .stack => |d| try writer.print("\tmov rdi, qword [rsp + {d}]\n", .{d}),
                    .void => unreachable,
                }
                try writer.print("\tmov rax, 0x2000001\n", .{});
                try writer.print("\tsyscall\n", .{});
            },
            .Comment => {},
            else => unreachable,
        }
    }
}
