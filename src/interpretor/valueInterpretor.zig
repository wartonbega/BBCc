const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("print.zig");
const Values = @import("values.zig");
const Itpr = @import("interpretor.zig");

const Context = Itpr.Context;
const Value = Values.Value;

fn setVariableRhsAssign(lhs: *Ast.Value, ctx: *Context, value: Value) !void {
    switch (lhs.*) {
        .identifier => |ident| {
            try ctx.setVariable(ident.name, value);
        },
        .varDec => |vardec| {
            try ctx.setVariable(vardec.name, value);
        },
        else => {
            std.debug.print("uniplemented {}", .{lhs.*});
            unreachable;
        },
    }
}

pub fn interpreteBinOp(binop: *Ast.binaryOperation, ctx: *Context) !Values.Value {
    const lhs_value = try interpreteValue(binop.lhs, ctx);
    const rhs_value = try interpreteValue(binop.rhs, ctx);
    switch (binop.operator) {
        .Plus => {
            return Values.Plus(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Minus => {
            return Values.Minus(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Times => {
            return Values.Times(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Div => {
            return Values.Div(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Modulus => {
            return Values.Modulus(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Lt => {
            return Values.Lt(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Gt => {
            return Values.Gt(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Le => {
            return Values.Le(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Ge => {
            return Values.Ge(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Equal => {
            return Values.Equal(lhs_value, rhs_value, ctx, binop.reference);
        },
        .NotEqual => {
            return Values.NotEqual(lhs_value, rhs_value, ctx, binop.reference);
        },
        .Or => {
            return Values.Or(lhs_value, rhs_value, ctx, binop.reference);
        },
        .And => {
            return Values.And(lhs_value, rhs_value, ctx, binop.reference);
        },
    }
    return .{ .Null = {} };
}

pub fn interpreteValue(value: *Ast.Value, ctx: *Context) (Itpr.ContextualError || std.mem.Allocator.Error)!Values.Value {
    switch (value.*) {
        //.assignement => |assign| {},
        //.binaryOperator => |binop| {},
        .intLit => |i| return Value{ .Int = i.value },
        .boolLit => |b| return Value{ .Bool = b.value },
        .nullLit => return Value{ .Null = {} },
        .errorCheck => |err_c| {
            const ret = try interpreteValue(err_c.value, ctx);
            if (ret != .Error)
                return ret;
            return try interpreteValue(err_c.scope, ctx);
        },
        .scope => |scope| {
            var child_ctx = ctx.createChild();
            return try Itpr.interpreteScope(scope, &child_ctx);
        },
        .varDec => {},
        .assignement => |assign| {
            const right_value = try interpreteValue(assign.rhs, ctx);
            try setVariableRhsAssign(assign.lhs, ctx, right_value);
        },
        .identifier => |ident| return ctx.getVariable(ident.name),
        .binaryOperator => |binop| return interpreteBinOp(binop, ctx),
        .funcall => |funcall| {
            const function = try interpreteValue(funcall.func, ctx);
            if (function != .Function)
                return Itpr.ContextualError.UnknownFunction;
            var arguments = std.ArrayList(Values.Value).init(ctx.GPAlloc);
            defer arguments.deinit();
            for (funcall.args.items) |arg| {
                try arguments.append(try interpreteValue(arg, ctx));
            }
            return try Itpr.interpreteFunction(function.Function, arguments, ctx);
        },
        .If => |ifstmt| {
            for (ifstmt.conditions.items, ifstmt.scopes.items) |condition, scope| {
                const cond_val = try interpreteValue(condition, ctx);
                if (cond_val.Bool) {
                    return try interpreteValue(scope, ctx);
                }
            }
            if (ifstmt.elsescope) |elsescope|
                return try interpreteValue(elsescope, ctx);
        },
        .parenthesis => |par| return interpreteValue(par, ctx),
        else => {
            std.debug.print("uniplemented {}", .{value.*});
            unreachable;
        },
    }
    return Value{ .Null = {} };
}
