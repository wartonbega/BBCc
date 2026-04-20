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
        .floatLit => {},
        .boolLit => |b| {
            try genValues.boollit.codegenBoollit(b.value, compiler, cctx);
        },
        .scope => |s| {
            try codegen.scope.codegenScope(s, compiler, cctx);
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
        else => unreachable,
    }
}
