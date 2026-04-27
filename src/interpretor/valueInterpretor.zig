const std = @import("std");
const analyser = @import("../analyser.zig");
const Ast = @import("../ast.zig");
const bbcTypes = @import("../types.zig");
const errors = @import("../errors.zig");
const Print = @import("print.zig");
const Values = @import("values.zig");
const Itpr = @import("interpretor.zig");
const Parser = @import("../parser.zig");
const Inbuilt = @import("inbuilt.zig");

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
            switch (buff) {
                .Buffer => buff.Buffer.setElement(@intCast(idx.Int), value, ctx.heap),
                .Object => {
                    const set_fn = buff.getHabitant("set");
                    var args = std.ArrayList(Values.Value).init(ctx.heap);
                    defer args.deinit();
                    try args.append(idx);
                    try args.append(value);
                    _ = try Itpr.interpreteFunction(set_fn.Function.func, args, set_fn.Function.parentObj, ctx);
                    set_fn.decrementReference(ctx.heap);
                },
                else => unreachable,
            }
        },
        else => {
            std.debug.print("uniplemented {}", .{lhs.*});
            unreachable;
        },
    }
}

pub fn interpreteBinOp(binop: *Ast.binaryOperation, ctx: *Context) !Values.Value {
    const lhs_value = try interpreteValue(binop.lhs, ctx);

    if (binop.operator == .And or binop.operator == .Or) {
        return switch (binop.operator) {
            .Or => {
                if (lhs_value.Bool)
                    return lhs_value;
                const rhs_value = try interpreteValue(binop.rhs, ctx);
                if (rhs_value.Bool)
                    return rhs_value;
                return Value{ .Bool = false };
            },
            .And => {
                if (!lhs_value.Bool)
                    return lhs_value;
                const rhs_value = try interpreteValue(binop.rhs, ctx);
                if (rhs_value.Bool)
                    return rhs_value;
                return Value{ .Bool = false };
            },
            else => unreachable,
        };
    }
    const rhs_value = try interpreteValue(binop.rhs, ctx);
    defer lhs_value.checkReference(ctx.heap);
    defer rhs_value.checkReference(ctx.heap);
    return switch (binop.operator) {
        .Plus => try Values.Plus(lhs_value, rhs_value, ctx, binop.reference),
        .Minus => try Values.Minus(lhs_value, rhs_value, ctx, binop.reference),
        .Times => try Values.Times(lhs_value, rhs_value, ctx, binop.reference),
        .Div => try Values.Div(lhs_value, rhs_value, ctx, binop.reference),
        .Modulus => try Values.Modulus(lhs_value, rhs_value, ctx, binop.reference),
        .Lt => try Values.Lt(lhs_value, rhs_value, ctx, binop.reference),
        .Gt => try Values.Gt(lhs_value, rhs_value, ctx, binop.reference),
        .Le => try Values.Le(lhs_value, rhs_value, ctx, binop.reference),
        .Ge => try Values.Ge(lhs_value, rhs_value, ctx, binop.reference),
        .Equal => try Values.Equal(lhs_value, rhs_value, ctx, binop.reference),
        .NotEqual => try Values.NotEqual(lhs_value, rhs_value, ctx, binop.reference),
        else => unreachable,
    };
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
            try s_list.appendSlice(s.value);
            const s_obj = try ctx.heap.create(Values.StringObj);
            s_obj.* = .{ .content = s_list, .references = 0 };
            return Value{ .String = s_obj };
        },
        .nullLit => return Value{ .Null = {} },
        .errorCheck => |err_c| {
            const ret = try interpreteValue(err_c.value, ctx);
            if (ret != .Error)
                return ret;
            ret.checkReference(ctx.heap);
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
            if (function == .BuiltinFunction) {
                var arguments = std.ArrayList(Values.Value).init(ctx.heap);
                defer arguments.deinit();
                for (funcall.args.items) |arg|
                    try arguments.append(try interpreteValue(arg, ctx));
                const builtin_ret = try Inbuilt.dispatchBuiltin(function.BuiltinFunction, arguments, ctx, funcall.func.getReference());
                for (arguments.items) |arg| arg.checkReference(ctx.heap);
                return builtin_ret;
            }
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
                if (ctx.getVariableOpt(qualified)) |v| return v;
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

            const is_last_fn = iter_val.getHabitant("isLast");
            is_last_fn.decrementReference(ctx.heap);
            const next_fn = iter_val.getHabitant("next");
            next_fn.decrementReference(ctx.heap);

            var child_ctx = ctx.createChild();
            try child_ctx.variables.ensureTotalCapacity(1);
            defer child_ctx.deinit();
            while (true) {
                const is_last = try Itpr.interpreteFunction(is_last_fn.Function.func, empty_args, is_last_fn.Function.parentObj, ctx);

                if (is_last.Bool) break;

                const next_val = try Itpr.interpreteFunction(next_fn.Function.func, empty_args, next_fn.Function.parentObj, ctx);

                // Clear previous iteration's variables, retaining the backing allocation
                {
                    var it = child_ctx.variables.iterator();
                    while (it.next()) |v| v.value_ptr.decrementReference(child_ctx.heap);
                    child_ctx.variables.clearRetainingCapacity();
                }
                try child_ctx.setVariable(for_loop.var_name, next_val);
                _ = try interpreteValue(for_loop.exec, &child_ctx);
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
            const type_def = ctx.codeContext.getTypeDef(stc_init.name);
            var obj_ret = try ctx.heap.create(Values.Object);
            obj_ret.* = .{
                .habitants = .init(ctx.heap),
                .name = stc_init.name,
                .references = 0,
            };
            try obj_ret.habitants.ensureTotalCapacity(
                @intCast(stc_init.habitants.count() + type_def.methods.count()),
            );
            var field_it = stc_init.habitants.iterator();
            while (field_it.next()) |hab| {
                const habitant = try interpreteValue(hab.value_ptr.*, ctx);
                obj_ret.setHabitant(hab.key_ptr.*, habitant, ctx.heap);
            }
            var meth_it = type_def.methods.iterator();
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
                    const buf_i: usize = @intCast(idx.Int);
                    if (buf_i >= buff.Buffer.size)
                        return try Values.makeError(ctx.heap, buff_idx.index.getReference(), "Buffer index {d} out of bounds (size: {d})", .{ buf_i, buff.Buffer.size });
                    if (buff.Buffer.content[buf_i]) |ret| {
                        return ret;
                    } else {
                        return try Values.makeError(ctx.heap, buff_idx.index.getReference(), "Buffer index {d} has undefined value", .{buf_i});
                    }
                },
                .String => |s| {
                    const i: usize = @intCast(idx.Int);
                    if (i >= s.content.items.len)
                        return try Values.makeError(ctx.heap, buff_idx.index.getReference(), "String index {d} out of bounds (size: {d})", .{ i, s.content.items.len });
                    return Values.Value{ .Char = s.content.items[i] };
                },
                .Object => |_| {
                    const idx_fn = buff.getHabitant("index");
                    var empty_args = std.ArrayList(Values.Value).init(ctx.heap);
                    try empty_args.append(idx);
                    defer empty_args.deinit();
                    const next_val = try Itpr.interpreteFunction(idx_fn.Function.func, empty_args, idx_fn.Function.parentObj, ctx);
                    idx_fn.decrementReference(ctx.heap);
                    return next_val;
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
