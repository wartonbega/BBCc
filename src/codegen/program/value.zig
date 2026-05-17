const std = @import("std");
const analyser = @import("../../analyser.zig");
const Ast = @import("../../ast.zig");
const bbcTypes = @import("../../types.zig");
const errors = @import("../../errors.zig");

const Inst = @import("../instructions.zig");
const Instruction = Inst.Instruction;

const Compiler = @import("../compiler.zig").Compiler;
const regTable = @import("../registerTable.zig");

const codegen = @import("codegenprog.zig");

const genValues = @import("values/values.zig");

pub fn codegenValue(value: *const Ast.Value, compiler: *Compiler, cctx: *analyser.Context) (std.mem.Allocator.Error || errors.bbcErrors || regTable.regtableError)!void {
    switch (value.*) {
        .intLit => |i| {
            try genValues.intlit.codegenIntlit(i.value, compiler, cctx);
        },
        .floatLit => |f| {
            try genValues.floatlit.codegenFloatLit(f.value, compiler, cctx);
        },
        .boolLit => |b| {
            try genValues.boollit.codegenBoollit(b.value, compiler, cctx);
        },
        .scope => |s| {
            try codegen.scope.codegenScope(s, compiler, false);
        },
        .binaryOperator => |binop| {
            try genValues.binop.codegenBinop(binop, compiler, cctx);
        },
        .assignement => |assign| {
            try genValues.assign.codegenAssignement(assign, compiler, cctx);
        },
        .identifier => |ident| {
            try genValues.identifier.codegenIdentifier(ident, compiler, cctx);
        },
        .If => |i| {
            try genValues.ifstatement.codegenIfStatement(i, compiler, cctx);
        },
        .parenthesis => |p| {
            try codegenValue(p, compiler, cctx);
        },
        .varDec => {},
        .funcall => |f| {
            try genValues.funcall.codegenFuncall(f, compiler, cctx);
        },
        .structInit => |s| {
            try genValues.structinit.codegenStructinit(s, compiler, cctx);
        },
        .unaryOperatorRight => |uopr| {
            try genValues.unaryoperatorright.codegenUopRight(uopr, compiler, cctx);
        },
        .Print => |p| {
            try genValues.print.codegenPrint(p, compiler, cctx);
        },
        .While => |wl| {
            try genValues.whileloop.codegenWhileLoop(wl, compiler, cctx);
        },
        .For => |fl| {
            try genValues.forloop.codegenForLoop(fl, compiler, cctx);
        },
        .notOp => |n| {
            try genValues.notop.codegenNotOp(n, compiler, cctx);
        },
        .stringLit => |s| {
            try genValues.stringlit.codegenStringLit(s.value, compiler, cctx);
        },
        .charLit => |c| {
            try genValues.charlit.codegenCharlit(c.value, compiler, cctx);
        },
        .bufferLit => |bl| {
            try genValues.bufferlit.codegenBufferLit(bl, compiler, cctx);
        },
        .bufferIndex => |bi| {
            try genValues.bufferindex.codegenBufferIndex(bi, compiler, cctx);
        },
        .bufferAlloc => |ba| {
            try genValues.bufferalloc.codegenBufferAlloc(ba, compiler, cctx);
        },
        .freeKeyword => |fk| {
            try genValues.freekeyword.codegenFreeKeyword(fk, compiler, cctx);
        },
        .errorCheck => |ec| {
            try genValues.errorcheck.codegenErrorCheck(ec, compiler, cctx);
        },
        .function => |func| {
            try genValues.identifier.codegenFunctionValue(func, compiler, cctx);
        },
        else => unreachable,
    }
}

pub fn codegenValueAdr(value: *const Ast.Value, compiler: *Compiler, cctx: *analyser.Context) (std.mem.Allocator.Error || errors.bbcErrors || regTable.regtableError)!void {
    switch (value.*) {
        .varDec => |vardec| {
            try genValues.vardec.codegenVardecAdr(vardec, compiler, cctx);
        },
        .identifier => |ident| {
            try genValues.identifier.codegenIdentifierAdr(ident, compiler, cctx);
        },
        .unaryOperatorRight => |uopr| {
            try genValues.unaryoperatorright.codegenUopRightAdr(uopr, compiler, cctx);
        },
        .bufferIndex => |bi| {
            try genValues.bufferindex.codegenBufferIndexAdr(bi, compiler, cctx);
        },
        else => unreachable,
    }
}
