const std = @import("std");
const analyser = @import("analyser.zig");
const Ast = @import("ast.zig");
const bbcTypes = @import("types.zig");
const errors = @import("errors.zig");
const Inst = @import("codegen/instructions.zig");

const Allocator = std.mem.Allocator;
const VarHashmap = std.hash_map.StringHashMap(bbcTypes.Type);
const VarPos = std.hash_map.StringHashMap(Inst.Location);
const Arraylist = std.ArrayList;

const Context = struct {
    codeContext: *analyser.Context,
    allocator: std.mem.Allocator,
    builder: Inst.Builder,
    registers: Inst.RegisterTable,
};

const ScopeContext = struct {
    vars: VarPos,
    simstack: Arraylist(Inst.Type),
    context: *analyser.Context,

    pub fn init(alloc: Allocator) !*ScopeContext {
        const self = try alloc.create(ScopeContext);
        self.* = ScopeContext{ .vars = VarPos.init(alloc), .simstack = Arraylist(Inst.Type).inti(alloc) };
        return self;
    }

    pub fn getVariable(self: *const ScopeContext, name: []const u8) Inst.Location {
        const loc = self.vars.get(name).?;
        switch (loc) {
            .stack => |s| {
                return Inst.Location{ .stack = .{
                    .idx = s.idx,
                    .stack_state = self.getCurrentStackState(),
                } };
            },
            .label => return loc,
            else => unreachable,
        }
    }

    //pub fn getVariables(vars: VarHashmap, ctx: *Context) !void {}

    pub fn getCurrentStackIndex(self: *const ScopeContext) usize {
        return self.simstack.items.len;
    }

    pub fn getCurrentStackState(self: *const ScopeContext) Arraylist(Inst.Type) {
        return self.simstack.clone() catch {
            return Arraylist(Inst.Type).init(self.simstack.allocator);
        };
    }
};

pub fn getCompileType(t: *Ast.Type) Inst.Type {
    switch (t.base) {
        .function => return Inst.Type{ .function = {} },
        .name => |name| {
            if (std.mem.eql(u8, name, "Int"))
                return Inst.Type{ .intType = {} };
            if (std.mem.eql(u8, name, "String"))
                return Inst.Type{ .stringType = {} };
        },
    }
    return Inst.Type{ .intType = {} };
}

const getCompileSize = Inst.getCompileSize;

pub fn generateArguments(args: Arraylist(*Ast.Arguments), ctx: *Context, scopeCtx: *ScopeContext) !void {
    var size: i64 = @intCast(0);
    if (args.items.len == 0) // No need to do anything then
        return;
    for (args.items) |arg| {
        size += getCompileSize(getCompileType(arg._type));
    }

    try ctx.builder.reserveStack(@intCast(size));
    for (args.items, Inst.RegIter[0..args.items.len]) |arg, reg| {
        try scopeCtx.simstack.append(getCompileType(arg._type));
        try ctx.builder.moveInst(
            Inst.Location{ .register = reg },
            Inst.Location{ .stack = .{
                .idx = scopeCtx.getCurrentStackIndex(),
                .stack_state = scopeCtx.getCurrentStackState(),
            } },
        );
        try scopeCtx.vars.put(arg.name, Inst.Location{ .stack = .{
            .idx = scopeCtx.getCurrentStackIndex(),
            .stack_state = scopeCtx.getCurrentStackState(),
        } });
    }
}

pub fn chooseWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext) Inst.Location {
    const a = blk: for (Inst.RegIter, ctx.registers.utilised) |reg, utilised| {
        if (!utilised) {
            break :blk Inst.Location{ .register = reg };
        }
    } else {
        break :blk Inst.Location{
            .stack = .{
                .idx = scopeCtx.getCurrentStackIndex(),
                .stack_state = scopeCtx.getCurrentStackState(),
            },
        };
    };
    return a;
}

pub fn pickWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext, t: Inst.Type) !Inst.Location {
    // Allocates memory on the stack if necessary, and returns the location
    const loc = chooseWiselyLocation(ctx, scopeCtx);
    switch (loc) {
        .stack => {
            // we add a pointer on the stack
            try scopeCtx.simstack.append(t);
            try ctx.builder.reserveStack(@intCast(getCompileSize(t)));
        },
        .register => |reg| {
            ctx.registers.setRegister(reg, true);
        },
        .label => unreachable,
        .void => {},
    }
    return loc;
}

pub fn freeLocation(loc: Inst.Location, scopeCtx: *ScopeContext, ctx: *Context) !void {
    _ = scopeCtx;
    switch (loc) {
        .register => |reg| ctx.registers.setRegister(reg, false),
        .stack => {},
        .label => unreachable,
        .void => {},
    }
}

pub fn saveRegistersFuncall(ctx: *Context, scopeCtx: *ScopeContext) !void {
    var size: i64 = 0;
    for (ctx.registers.utilised) |b| {
        size += @intFromBool(b);
    }
    size *= 8;
    try ctx.builder.reserveStack(@intCast(size));

    for (Inst.RegIter, ctx.registers.utilised) |reg, utilised| {
        if (utilised) {
            try scopeCtx.simstack.append(Inst.Type{ .intType = {} });
            try ctx.builder.moveInst(Inst.Location{ .register = reg }, Inst.Location{ .stack = reg });
        }
    }
}

pub fn loadRegistersFuncall(ctx: *Context, scopeCtx: *ScopeContext) !void {
    for (Inst.RegIter, ctx.registers.utilised) |reg, utilised| {
        if (utilised) {
            try ctx.builder.moveInst(.{ .register = reg }, .{
                .stack = .{
                    .idx = scopeCtx.getCurrentStackIndex(),
                    .stack_state = scopeCtx.getCurrentStackState(),
                },
            });
            scopeCtx.simstack.pop();
        }
    }

    var size: i64 = 0;
    for (ctx.registers.utilised) |b| {
        size += @intFromBool(b);
    }
    size *= 8;
    try ctx.builder.reserveStack(@intCast(-size));
}

pub fn generateValueAssignement(value: *const Ast.Value, scopeCtx: *ScopeContext) Inst.Location {
    return switch (value.*) {
        .identifier => |ident| return scopeCtx.getVariable(ident),
        .varDec => |vardec| return scopeCtx.getVariable(vardec.name),
        else => Inst.Location{ .void = {} },
    };
}

pub fn generateBinOperation(binop: *const Ast.binaryOperation, scopeCtx: *ScopeContext, ctx: *Context) !Inst.Location {
    const lhs_loc = try generateValue(binop.lhs, scopeCtx, ctx);
    const rhs_loc = try generateValue(binop.rhs, scopeCtx, ctx);
    const lhs_type = (try bbcTypes.getTypeOfValue(binop.lhs, scopeCtx.context, ctx.allocator)).decided;
    const rhs_type = (try bbcTypes.getTypeOfValue(binop.rhs, scopeCtx.context, ctx.allocator)).decided;
    // Assuming lhs and rhs have decided types
    switch (lhs_type.base) {
        .function => errors.bbcErrorExit("Can't use operator on function (signature: {s})", .{lhs_type.toString(ctx.allocator)}, ""),
        .name => {},
    }
    switch (rhs_type.base) {
        .function => errors.bbcErrorExit("Can't use operator on function (signature: {s})", .{rhs_type.toString(ctx.allocator)}, ""),
        .name => {},
    }
    if (std.mem.eql(u8, lhs_type.base.name, "Int") and std.mem.eql(u8, rhs_type.base.name, "Int")) {
        // Both sides are Int, the result is Int too
        try freeLocation(rhs_loc, scopeCtx, ctx);
        const res_loc = lhs_loc; // the result is stored in the lhs location
        switch (binop.operator) {
            .Plus => try ctx.builder.plus(lhs_loc, rhs_loc),
            .Minus => try ctx.builder.minus(lhs_loc, rhs_loc),
            .Times => try ctx.builder.multiply(lhs_loc, rhs_loc),
            .Div => try ctx.builder.divide(lhs_loc, rhs_loc),
            .Modulus => try ctx.builder.modulo(lhs_loc, rhs_loc),
            .Equal => try ctx.builder.equal(lhs_loc, rhs_loc),
            .NotEqual => try ctx.builder.notEqual(lhs_loc, rhs_loc),
            .Lt => try ctx.builder.lessThan(lhs_loc, rhs_loc),
            .Le => try ctx.builder.lessEqual(lhs_loc, rhs_loc),
            .Gt => try ctx.builder.greaterThan(lhs_loc, rhs_loc),
            .Ge => try ctx.builder.greaterEqual(lhs_loc, rhs_loc),
            else => unreachable,
        }
        return res_loc;
    } else if (std.mem.eql(u8, lhs_type.base.name, "Int") and std.mem.eql(u8, rhs_type.base.name, "Bool")) {
        // Both sides are Int, the result is Int too
        try freeLocation(rhs_loc, scopeCtx, ctx);
        const res_loc = lhs_loc; // the result is stored in the lhs location
        switch (binop.operator) {
            .Plus => try ctx.builder.plus(lhs_loc, rhs_loc),
            .Minus => try ctx.builder.minus(lhs_loc, rhs_loc),
            .Times => try ctx.builder.multiply(lhs_loc, rhs_loc),
            else => unreachable,
        }
        return res_loc;
    }
    errors.bbcErrorExit("Can't compile operator {s} with types {s} and {s}", .{
        Ast.reprBinOp(binop.operator),
        lhs_type.toString(ctx.allocator),
        rhs_type.toString(ctx.allocator),
    }, "");
    return chooseWiselyLocation(ctx, scopeCtx);
}

pub fn generateValue(value: *const Ast.Value, scopeCtx: *ScopeContext, ctx: *Context) Allocator.Error!Inst.Location {
    switch (value.*) {
        .intLit => |intlit| {
            const dest = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = {} });
            try ctx.builder.intLit(@intCast(intlit), dest);
            return dest;
        },
        .identifier => |ident| {
            // We move the value idealy to one of the registers, but otherwise on the stack
            std.debug.print("{s}\n", .{ident});
            const origin = scopeCtx.getVariable(ident);
            const compile_type = getCompileType(scopeCtx.context.getVariable(ident).decided);
            const dest = try pickWiselyLocation(ctx, scopeCtx, compile_type);
            try ctx.builder.moveInst(origin, dest);
            return dest;
        },
        .assignement => |assign| {
            // "dest = origin"
            const origin = try generateValue(assign.rhs, scopeCtx, ctx);
            const dest = generateValueAssignement(assign.lhs, scopeCtx);
            try ctx.builder.moveInst(origin, dest);
            try freeLocation(origin, scopeCtx, ctx);
            return Inst.Location{ .void = {} };
        },
        .varDec => {
            return Inst.Location{ .void = {} };
        },
        .scope => |scope| {
            return try generateScope(scope, scopeCtx, ctx);
        },
        .funcall => |funcall| {
            const func = try generateValue(funcall.func, scopeCtx, ctx);
            try scopeCtx.simstack.append(Inst.Type{ .function = {} });
            const func_stack_lock = Inst.Location{
                .stack = .{
                    .idx = scopeCtx.getCurrentStackIndex(),
                    .stack_state = scopeCtx.getCurrentStackState(),
                },
            };
            try ctx.builder.reserveStack(@intCast(8));
            try ctx.builder.moveInst(func, func_stack_lock);
            try freeLocation(func, scopeCtx, ctx);

            var args = Arraylist(Inst.Location).init(ctx.allocator);
            for (funcall.args.items) |arg| {
                const base_arg = try generateValue(arg, scopeCtx, ctx);
                try scopeCtx.simstack.append(getCompileType((try bbcTypes.getTypeOfValue(arg, scopeCtx.context, ctx.allocator)).decided));
                const stack_lock = Inst.Location{
                    .stack = .{
                        .idx = scopeCtx.getCurrentStackIndex(),
                        .stack_state = scopeCtx.getCurrentStackState(),
                    },
                };
                try ctx.builder.reserveStack(@intCast(8));
                try ctx.builder.moveInst(base_arg, stack_lock);
                try freeLocation(base_arg, scopeCtx, ctx);
                try args.append(stack_lock);
            }
            //try saveRegistersFuncall(ctx, scopeCtx);

            const funcretype = (try bbcTypes.getTypeOfValue(funcall.func, scopeCtx.context, ctx.allocator)).decided.base.function.retype;
            try ctx.builder.funcall(func_stack_lock, args);

            const retloc = try pickWiselyLocation(ctx, scopeCtx, getCompileType(funcretype));
            try ctx.builder.moveInst(.{ .register = Inst.Registers.r0 }, retloc);

            //try loadRegistersFuncall(ctx, scopeCtx);

            for (args.items) |arg| {
                try freeLocation(arg, scopeCtx, ctx);
            }
            try freeLocation(func_stack_lock, scopeCtx, ctx);
            return retloc;
        },
        .binaryOperator => |binop| {
            return try generateBinOperation(binop, scopeCtx, ctx);
        },
        .stringLit => |stringlit| {
            // Add the amout of memory on the stack ( len(stringlit) + (8 - len(stringlit)%8) + 8 )
            // And place each characters and the len (on the first byte)
            // String_stack_size is only the size of the string itself
            // Memory layout :  | __SIZE__ | __STRING_LIT...__ |
            //                  |    8     |        ...        |
            const string_stack_size = stringlit.len + 8 - (stringlit.len % 8);

            try scopeCtx.simstack.append(Inst.Type{ .arrayLike = {} });
            try ctx.builder.reserveStack(@intCast(string_stack_size + 8));

            // We write the size on the stack
            try ctx.builder.intLit(@intCast(string_stack_size), Inst.Location{
                .stack = .{
                    .idx = scopeCtx.getCurrentStackIndex(),
                    .stack_state = scopeCtx.getCurrentStackState(),
                },
            });

            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .pointer = {} });
            try ctx.builder.getStackPointer(ret_loc);

            // Then write the rest of the string
            for (stringlit, 0..) |char, idx| {
                const char_loc = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .charType = {} });
                try ctx.builder.charLit(char, char_loc);
                try ctx.builder.writeArrayElement(ret_loc, idx, char_loc, Inst.Type{ .charType = {} });
                try freeLocation(char_loc, scopeCtx, ctx);
            }
            try ctx.builder.print(ret_loc);
            return ret_loc;
        },
        else => {
            std.debug.print("Unimplemented: {}\n", .{value.*});
            unreachable;
        },
    }
}

pub fn generateScope(scope: *const Ast.Scope, scopeCtx: *ScopeContext, ctx: *Context) !Inst.Location {
    // declaring all the variables inside the scope
    var newScopeCtx = try ctx.allocator.create(ScopeContext);
    newScopeCtx.vars = try VarPos.clone(scopeCtx.vars);
    newScopeCtx.context = scope.ctx;
    newScopeCtx.simstack = Arraylist(Inst.Type).init(ctx.allocator);
    defer newScopeCtx.simstack.deinit();

    var it = scope.ctx.variables.iterator();
    var allocsize: i64 = @intCast(0);
    while (it.next()) |variable| {
        const vartype = getCompileType(variable.value_ptr.*.decided);
        allocsize += getCompileSize(vartype);
        try newScopeCtx.simstack.append(vartype);
        try newScopeCtx.vars.put(variable.key_ptr.*, Inst.Location{
            .stack = .{
                .idx = newScopeCtx.getCurrentStackIndex(),
                .stack_state = newScopeCtx.getCurrentStackState(),
            },
        });
    }
    // allocating the variables declared in the scope

    try ctx.builder.reserveStack(@intCast(allocsize));

    // the first n-1 elements can be discarded if it does not contain any error
    for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
        _ = try generateValue(value, newScopeCtx, ctx);
    }

    try ctx.builder.comment("Return value of scope");
    const ret = try generateValue(scope.code.items[scope.code.items.len - 1], newScopeCtx, ctx);

    while (newScopeCtx.simstack.items.len > 0) {
        try ctx.builder.decreaseStack(newScopeCtx.simstack.pop().?);
    }
    return ret;
}

pub fn generateFunction(func: *const Ast.funcDef, ctx: *Context, baseVarTable: VarPos) !void {
    // declare function (add label)
    try ctx.builder.functionDec(func.name);

    // creating the context for the scope
    var scopeCtx = try ctx.allocator.create(ScopeContext);
    scopeCtx.vars = try baseVarTable.clone();
    scopeCtx.context = func.code.ctx;
    scopeCtx.simstack = Arraylist(Inst.Type).init(ctx.allocator);
    defer scopeCtx.simstack.deinit();

    // generating the arguements and putting them on the stack
    try generateArguments(func.arguments, ctx, scopeCtx);

    const return_val = try generateScope(func.code, scopeCtx, ctx);

    // Clearing the stack of arguments, no need to do it if it is already empy
    while (scopeCtx.simstack.items.len > 0) {
        try ctx.builder.decreaseStack(scopeCtx.simstack.pop().?);
    }

    // if function is main, then exit with the return value
    if (std.mem.eql(u8, func.name, "main"))
        try ctx.builder.exitWith(return_val);

    if (!func.return_type.match((try bbcTypes.CreateTypeVoid(ctx.allocator, false)).decided))
        try ctx.builder.returnInst(return_val);

    try freeLocation(return_val, scopeCtx, ctx);
}

pub fn generateProgram(ast: *Ast.Program, cctx: *analyser.Context, alloc: Allocator) !Inst.Builder {
    // Here: building context, setting up everything needed
    var ctx = Context{
        .allocator = alloc,
        .builder = Inst.Builder.init(alloc),
        .codeContext = cctx,
        .registers = Inst.RegisterTable.init(),
    };
    var baseVarTable = VarPos.init(ctx.allocator);
    for (ast.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |func| {
                // Declaring the function as a value in a label
                try baseVarTable.put(func.name, Inst.Location{ .label = func.name });
                try generateFunction(func, &ctx, baseVarTable);
            },
        }
    }

    for (ctx.builder.code.items) |inst| {
        inst.print();
    }
    return ctx.builder;
}
