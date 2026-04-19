const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");
const errors = @import("../../../errors.zig");

const Inst = @import("../../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../../compiler.zig").Compiler;

const codegen = @import("../codegenprog.zig");

pub fn codegenBoollit(b: bool, compiler: *Compiler, cctx: *analyser.Context) !void {
    _ = cctx;
    const retidx = try compiler.registerTable.allocate(compiler);
    const reg = try compiler.registerTable.getValue(retidx, compiler);
    if (b) {
        try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 1 }, .to = reg } });
    } else {
        try compiler.addInstruction(.{ .load = .{ .from = .{ .immediate = 0 }, .to = reg } });
    }
}
