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
        .bufferIndex => |buff_idx| {
            const buff = try interpreteValue(buff_idx.buffer, ctx);
            const idx = try interpreteValue(buff_idx.index, ctx);
            buff.Buffer.setElement(@intCast(idx.Int), value, ctx.heap);
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
        .floatLit => |f| return Value{ .Float = f.value },
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
            const scope_ret = try Itpr.interpreteScope(scope, &child_ctx);
            scope_ret.incrementReference();
            child_ctx.deinit();
            scope_ret.decrementReferenceNoCheck();
            return scope_ret;
        },
        .varDec => |variable| {
            const name = variable.name;
            try ctx.setVariable(name, .{ .Null = {} });
        },
        .assignement => |assign| {
            const right_value = try interpreteValue(assign.rhs, ctx);
            if (right_value == .Error) {
                return right_value;
            }
            try setVariableRhsAssign(assign.lhs, ctx, right_value);
        },
        .identifier => |ident| return ctx.getVariable(ident.name),
        .binaryOperator => |binop| return interpreteBinOp(binop, ctx),
        .funcall => |funcall| {
            const function = try interpreteValue(funcall.func, ctx);
            if (function != .Function)
                return Itpr.ContextualError.UnknownFunction;
            var arguments = std.ArrayList(Values.Value).init(ctx.heap);
            defer arguments.deinit();
            for (funcall.args.items) |arg| {
                try arguments.append(try interpreteValue(arg, ctx));
            }
            if (function.Function.parentObj) |pobj| {
                const ret = try Itpr.interpreteFunction(function.Function.func, arguments, pobj, ctx);
                if (ret == .Object and ret.Object == pobj) {
                    function.decrementReferenceNoCheck();
                } else {
                    function.decrementReference(ctx.heap);
                }
                return ret;
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
                    if (val == .Error) {
                        return val;
                    }
                    Print.print(val);
                    val.checkReference(ctx.heap);
                }
                std.debug.print("\n", .{});
            } else {
                for (p.args.items) |arg| {
                    const val = try interpreteValue(arg, ctx);
                    if (val == .Error) {
                        return val;
                    }
                    Print.print(val);
                    val.checkReference(ctx.heap);
                }
            }
        },
        .unaryOperatorRight => |uop| {
            // Namespace access: ns.name — look up "ns.name" as a flat variable
            if (uop.expr.* == .identifier) {
                var buf: [512]u8 = undefined;
                const qualified = std.fmt.bufPrint(&buf, "{s}.{s}", .{
                    uop.expr.identifier.name,
                    uop.operator.pointAttr,
                }) catch "";
                if (ctx.variableExist(qualified))
                    return ctx.getVariable(qualified);
            }
            const operand = try interpreteValue(uop.expr, ctx);
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
        .For => |for_loop| {
            const iter_val = try interpreteValue(for_loop.iterable, ctx);
            iter_val.incrementReference();
            defer iter_val.decrementReference(ctx.heap);

            var empty_args = std.ArrayList(Values.Value).init(ctx.heap);
            defer empty_args.deinit();
            while (true) {
                const is_last_fn = iter_val.getHabitant("isLast");
                const is_last = try Itpr.interpreteFunction(is_last_fn.Function.func, empty_args, is_last_fn.Function.parentObj, ctx);
                is_last_fn.decrementReference(ctx.heap);
                if (is_last.Bool) break;

                const next_fn = iter_val.getHabitant("next");
                const next_val = try Itpr.interpreteFunction(next_fn.Function.func, empty_args, next_fn.Function.parentObj, ctx);
                next_fn.decrementReference(ctx.heap);

                var child_ctx = ctx.createChild();
                try child_ctx.setVariable(for_loop.var_name, next_val);
                _ = try interpreteValue(for_loop.exec, &child_ctx);
                child_ctx.deinit();
            }
        },
        .notOp => |notop| {
            const val = try interpreteValue(notop.expr, ctx);
            if (val == .Error) {
                return val;
            }
            return Value{ .Bool = !val.Bool };
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
        .bufferAlloc => |buff_alloc| {
            const size_value = try interpreteValue(buff_alloc.size, ctx);
            defer size_value.checkReference(ctx.heap);
            const buf_ret = try ctx.heap.create(Values.BufferObj);
            buf_ret.* = Values.BufferObj{
                .content = try ctx.heap.alloc(?Values.Value, @intCast(size_value.Int)),
                .buffAllocator = ctx.heap,
                .references = 0,
                .size = @intCast(size_value.Int),
            };
            @memset(buf_ret.content, null);
            return Value{ .Buffer = buf_ret };
        },
        .bufferLit => |buff_lit| {
            const size_value = buff_lit.elements.items.len;
            const buf_ret = try ctx.heap.create(Values.BufferObj);
            buf_ret.* = Values.BufferObj{
                .content = try ctx.heap.alloc(?Values.Value, size_value),
                .buffAllocator = ctx.heap,
                .references = 0,
                .size = size_value,
            };
            @memset(buf_ret.content, null);
            for (0..buff_lit.elements.items.len) |i| {
                buf_ret.setElement(
                    i,
                    try interpreteValue(buff_lit.elements.items[i], ctx),
                    ctx.heap,
                );
            }
            return Value{ .Buffer = buf_ret };
        },
        .bufferIndex => |buff_idx| {
            const buff = try interpreteValue(buff_idx.buffer, ctx);
            const idx = try interpreteValue(buff_idx.index, ctx);
            switch (buff) {
                .Buffer => {
                    if (buff.Buffer.content[@intCast(idx.Int)]) |ret| {
                        return ret;
                    } else {
                        return Values.Value{ .Error = .{
                            .message = "Undefined value in Buffer at this index",
                            .reference = buff_idx.index.getReference(),
                        } };
                    }
                },
                .String => |s| {
                    const i: usize = @intCast(idx.Int);
                    if (i >= s.content.items.len)
                        return Values.Value{ .Error = .{
                            .message = "String index out of bounds",
                            .reference = buff_idx.index.getReference(),
                        } };
                    return Values.Value{ .Char = s.content.items[i] };
                },
                else => unreachable,
            }
        },
        else => {
            std.debug.print("uniplemented {}", .{value.*});
            unreachable;
        },
    }
    return Value{ .Null = {} };
}
