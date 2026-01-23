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
        .unaryOperatorRight => |uopright| {
            const operand = try interpreteValue(uopright.expr, ctx);
            operand.Object.setHabitant(uopright.operator.pointAttr, value, ctx.heap);
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
    defer lhs_value.checkReference(ctx.heap);
    defer rhs_value.checkReference(ctx.heap);
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
        .charLit => |c| return Value{ .Char = c.value },
        .stringLit => |s| {
            var s_list = std.ArrayList(u8).init(ctx.heap);
            for (s.value) |c| {
                s_list.append(c) catch {};
            }
            const s_obj = try ctx.heap.create(Values.StringObj);
            s_obj.* = .{ .content = s_list, .references = 0 };
            return Value{ .String = s_obj };
        },
        .nullLit => return Value{ .Null = {} },
        .errorCheck => |err_c| {
            const ret = try interpreteValue(err_c.value, ctx);
            if (ret != .Error)
                return ret;
            return try interpreteValue(err_c.scope, ctx);
        },
        .scope => |scope| {
            var child_ctx = ctx.createChild();
            defer child_ctx.deinit();
            return try Itpr.interpreteScope(scope, &child_ctx);
        },
        .varDec => |variable| {
            const name = variable.name;
            try ctx.setVariable(name, .{ .Null = {} });
        },
        .assignement => |assign| {
            const right_value = try interpreteValue(assign.rhs, ctx);
            try setVariableRhsAssign(assign.lhs, ctx, right_value);
        },
        .identifier => |ident| return ctx.getVariable(ident.name),
        .binaryOperator => |binop| return interpreteBinOp(binop, ctx),
        .funcall => |funcall| {
            const function = try interpreteValue(funcall.func, ctx);
            defer function.decrementReference(ctx.heap);
            if (function != .Function)
                return Itpr.ContextualError.UnknownFunction;
            var arguments = std.ArrayList(Values.Value).init(ctx.heap);
            defer arguments.deinit();
            for (funcall.args.items) |arg| {
                try arguments.append(try interpreteValue(arg, ctx));
            }
            if (function.Function.parentObj) |pobj| {
                return try Itpr.interpreteFunction(function.Function.func, arguments, pobj, ctx);
            } else return try Itpr.interpreteFunction(function.Function.func, arguments, null, ctx);
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
        .Print => |p| {
            if (p.ln) {
                for (p.args.items) |arg| {
                    const val = try interpreteValue(arg, ctx);
                    Print.print(val);
                    val.checkReference(ctx.heap);
                }
                std.debug.print("\n", .{});
            } else {
                for (p.args.items) |arg| {
                    const val = try interpreteValue(arg, ctx);
                    Print.print(val);
                    val.checkReference(ctx.heap);
                }
            }
        },
        .unaryOperatorRight => |uop| {
            const operand = try interpreteValue(uop.expr, ctx);
            defer operand.checkReference(ctx.heap);
            return operand.getHabitant(uop.operator.pointAttr);
        },
        .While => |_while| {
            while ((try interpreteValue(_while.condition, ctx)).Bool) {
                const ret = try interpreteValue(_while.exec, ctx); // Should be null
                if (ret != .Null) {
                    return ret;
                }
            }
        },
        .structInit => |stc_init| {
            var obj_ret = try ctx.heap.create(Values.Object);
            obj_ret.* = .{
                .habitants = .init(ctx.heap),
                .name = stc_init.name,
                .references = 0,
            };
            var field_it = stc_init.habitants.iterator();
            while (field_it.next()) |hab| {
                const habitant = try interpreteValue(hab.value_ptr.*, ctx);
                obj_ret.setHabitant(hab.key_ptr.*, habitant, ctx.heap);
            }
            var meth_it = ctx.codeContext.getTypeDef(stc_init.name).methods.iterator();
            while (meth_it.next()) |meth| {
                obj_ret.setHabitant(
                    meth.key_ptr.*,
                    Values.Value{
                        .Function = .{
                            .func = meth.value_ptr.*,
                            .parentObj = null,
                        },
                    },
                    ctx.heap,
                );
            }
            return Values.Value{ .Object = obj_ret };
        },
        .function => |func| {
            return Value{ .Function = .{ .func = func, .parentObj = null } };
        },
        else => {
            std.debug.print("uniplemented {}", .{value.*});
            unreachable;
        },
    }
    return Value{ .Null = {} };
}
