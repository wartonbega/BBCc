const std = @import("std");
const analyser = @import("../../../analyser.zig");
const Ast = @import("../../../ast.zig");
const bbcTypes = @import("../../../types.zig");

const Inst = @import("../../instructions.zig");
const Compiler = @import("../../compiler.zig").Compiler;
const gc = @import("../gc.zig");

const codegen = @import("../codegenprog.zig");

// free(val) — decrements refcount; calls the struct's free function if count hits zero.
pub fn codegenFreeKeyword(fk: anytype, compiler: *Compiler, cctx: *analyser.Context) !void {
    // Evaluate the value to get the struct pointer
    try codegen.value.codegenValue(fk.val, compiler, cctx);
    const ptr_idx = compiler.registerTable.lastReg().?;
    const ptr_reg = try compiler.registerTable.getValue(ptr_idx, compiler);

    // Determine the free function label (works for named structs, String, and Buffer).
    const val_type = try bbcTypes.getTypeOfValue(fk.val, cctx, compiler.allocator);
    const free_fn = try gc.getFreeFnLabel(val_type, cctx, compiler.allocator);

    try gc.emitGcDec(ptr_reg, free_fn, compiler);

    try compiler.registerTable.free(ptr_idx);
}
