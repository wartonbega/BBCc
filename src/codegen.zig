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
    stacktop: i64,
    context: *analyser.Context,

    pub fn init(alloc: Allocator) !*ScopeContext {
        const self = try alloc.create(ScopeContext);
        self.* = ScopeContext{ .vars = VarPos.init(alloc), .stacktop = @intCast(0) };
        return self;
    }
};

pub fn getCompileType(t: *Ast.Type) Inst.Type {
    switch (t.base) {
        .function => return Inst.Type{ .declarefunc = {} },
        .name => |name| {
            if (std.mem.eql(u8, name, "Int"))
                return Inst.Type{ .intType = {} };
        },
    }
    return Inst.Type{ .intType = {} };
}

pub fn getCompileSize(t: Inst.Type) i64 {
    // Returns the size in bytes of the Compile Type
    return switch (t) {
        .intType => 8,
        else => unreachable,
    };
}

//pub fn declareVariables(vars: VarHashmap, ctx: *Context) !void {}

pub fn generateArguments(args: Arraylist(*Ast.Arguments), ctx: *Context, scopeCtx: *ScopeContext) !void {
    var size: i64 = @intCast(scopeCtx.stacktop);
    if (args.items.len == 0) // No need to do anything then
        return;
    for (args.items) |arg| {
        size += getCompileSize(getCompileType(arg._type));
    }

    try ctx.builder.reserveStack(@intCast(size));
    size = @intCast(scopeCtx.stacktop);
    for (args.items, Inst.RegIter[0..args.items.len]) |arg, reg| {
        try ctx.builder.moveInst(Inst.Location{ .register = reg }, Inst.Location{ .stack = size });
        try scopeCtx.vars.put(arg.name, Inst.Location{ .stack = size });
        size += getCompileSize(getCompileType(arg._type));
    }
    scopeCtx.stacktop += size;
}

pub fn chooseWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext) Inst.Location {
    const a = blk: for (Inst.RegIter, ctx.registers.utilised) |reg, utilised| {
        if (!utilised) {
            break :blk Inst.Location{ .register = reg };
        }
    } else {
        break :blk Inst.Location{ .stack = scopeCtx.stacktop };
    };
    return a;
}

pub fn pickWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext, t: Inst.Type) !Inst.Location {
    // Allocates memory on the stack if necessary, and returns the location
    const loc = chooseWiselyLocation(ctx, scopeCtx);
    switch (loc) {
        .stack => {
            // we add a pointer on the stack
            scopeCtx.stacktop += getCompileSize(t);
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

//pub fn storeValue(ctx: *Context, scopeCtx: *ScopeContext, t: Inst.Type) !Inst.Location {
//    // Memory disposition (on the stack) :
//    // (-)  stack end <-- | __SIZE__ | MEMORY ON STACK |  <-- stack begining  (+)
//    // Returns a pointer to that
//}

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
            scopeCtx.stacktop += 8;
            try ctx.builder.moveInst(Inst.Location{ .register = reg }, Inst.Location{ .register = reg });
        }
    }
}

pub fn loadRegistersFuncall(ctx: *Context, scopeCtx: *ScopeContext) !void {
    for (Inst.RegIter, ctx.registers.utilised) |reg, utilised| {
        if (utilised) {
            try ctx.builder.moveInst(.{ .register = reg }, .{ .stack = scopeCtx.stacktop });
            scopeCtx.stacktop -= 8;
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
        .identifier => |ident| return scopeCtx.vars.get(ident).?,
        .varDec => |vardec| return scopeCtx.vars.get(vardec.name).?,
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
        try ctx.builder.plus(lhs_loc, rhs_loc);
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
            const origin = scopeCtx.vars.get(ident).?;
            const compile_type = getCompileType(scopeCtx.context.getVariable(ident).decided);
            const dest = try pickWiselyLocation(ctx, scopeCtx, compile_type);
            try ctx.builder.moveInst(origin, dest);
            return dest;
        },
        .assignement => |assign| {
            // "dest = origin"
            const dest = generateValueAssignement(assign.lhs, scopeCtx);
            const origin = try generateValue(assign.rhs, scopeCtx, ctx);
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
            scopeCtx.stacktop += 8;
            const func_stack_lock = Inst.Location{ .stack = scopeCtx.stacktop };
            try ctx.builder.reserveStack(@intCast(8));
            try ctx.builder.moveInst(func, func_stack_lock);
            try freeLocation(func, scopeCtx, ctx);

            var args = Arraylist(Inst.Location).init(ctx.allocator);
            for (funcall.args.items) |arg| {
                scopeCtx.stacktop += 8;
                const base_arg = try generateValue(arg, scopeCtx, ctx);
                const stack_lock = Inst.Location{ .stack = scopeCtx.stacktop };
                try ctx.builder.reserveStack(@intCast(8));
                try ctx.builder.moveInst(base_arg, stack_lock);
                try freeLocation(base_arg, scopeCtx, ctx);
                try args.append(stack_lock);
            }
            //std.debug.print("{d}\n", .{scopeCtx.stacktop});
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
        else => {
            std.debug.print("Unimplemented: {}\n", .{value.*});
            unreachable;
        },
    }
}

pub fn generateScope(scope: *const Ast.Scope, scopeCtx: *ScopeContext, ctx: *Context) !Inst.Location {
    // declaring all the variables inside the scope
    var it = scope.ctx.variables.iterator();
    var allocsize: i64 = @intCast(0);
    while (it.next()) |variable| {
        const vartype = getCompileType(variable.value_ptr.*.decided);
        try scopeCtx.vars.put(variable.key_ptr.*, Inst.Location{ .stack = allocsize + scopeCtx.stacktop });
        allocsize += getCompileSize(vartype);
    }
    // allocating the variables declared in the scope
    try ctx.builder.reserveStack(@intCast(allocsize));
    scopeCtx.stacktop += allocsize;

    var newScopeCtx = try ctx.allocator.create(ScopeContext);
    newScopeCtx.context = scope.ctx;
    newScopeCtx.stacktop = @intCast(0);
    newScopeCtx.vars = try VarPos.clone(scopeCtx.vars);

    // the first n-1 elements can be discarded if it does not contain any error
    for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
        _ = try generateValue(value, newScopeCtx, ctx);
    }

    try ctx.builder.comment("Return value of scope");
    const ret = try generateValue(scope.code.items[scope.code.items.len - 1], newScopeCtx, ctx);

    // deallocating the variables
    try ctx.builder.reserveStack(@intCast(-allocsize));
    scopeCtx.stacktop -= allocsize;
    return ret;
}

pub fn generateFunction(func: *const Ast.funcDef, ctx: *Context, baseVarTable: VarPos) !void {
    // declare function (add label)
    try ctx.builder.functionDec(func.name);

    // creating the context for the scope
    var scopeCtx = try ctx.allocator.create(ScopeContext);
    scopeCtx.vars = try baseVarTable.clone();
    scopeCtx.context = func.code.ctx;
    scopeCtx.stacktop = @intCast(0);

    // generating the arguements and putting them on the stack
    try generateArguments(func.arguments, ctx, scopeCtx);

    const return_val = try generateScope(func.code, scopeCtx, ctx);

    // Clearing the stack of arguments, no need to do it if it is already empy
    if (scopeCtx.stacktop != 0)
        try ctx.builder.reserveStack(@intCast(-scopeCtx.stacktop));

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
