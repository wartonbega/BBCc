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

// here are defined global variables for the code generation
const error_union_function = "_error_union_callback";
const error_union_label = "_error_union";
const reference_counter = "_reference_counter";
pub const main_function_wrapper_name = "main_wrapper";

const Context = struct {
    codeContext: *analyser.Context,
    allocator: std.mem.Allocator,
    builder: Inst.Builder,

    registers: Inst.RegisterTable,
    registerTrace: Inst.RegisterTable, // The trace of the registers used in a function

    func_id_table_counter: std.hash_map.StringHashMap(usize),
    func_uid_list: Arraylist(struct { // Allows, to get, with only the version, the uid
        uid: []const u8,
        version: analyser.functionVersion,
    }),
    labels: std.hash_map.StringHashMap(usize), // General purpose label (if statements, loops, etc)
    dec_strings: Arraylist(struct {
        name: []const u8,
        content: []const u8,
    }),

    pub fn getUid(self: *Context, version: analyser.functionVersion) []const u8 {
        base_for: for (self.func_uid_list.items) |func| {
            if (std.mem.eql(u8, func.version.name, version.name)) {
                if (func.version.version.count() != version.version.count())
                    continue;
                var it = func.version.version.iterator();
                while (it.next()) |vers| {
                    if (!version.version.contains(vers.key_ptr.*))
                        continue :base_for;
                    const arg_type = version.version.get(vers.key_ptr.*).?;
                    if (!arg_type.matchType(vers.value_ptr.*))
                        continue :base_for;
                }
                return func.uid;
            }
        }
        std.debug.print("[INTERN ERROR]: Couldnt find version for {s}\n", .{version.name});
        return version.name;
    }

    pub fn generateLabel(self: *Context, base: []const u8) ![]const u8 {
        var count: usize = @intCast(0);
        if (self.labels.contains(base)) {
            count = self.labels.get(base).? + 1;
            try self.labels.put(base, count + 1);
        } else {
            try self.labels.put(base, count);
        }
        return try std.fmt.allocPrint(self.allocator, "autogen@{s}_{d}", .{ base, count });
    }

    pub fn eraseRegisterTrace(self: *Context) void {
        for (Inst.RegIter) |reg| {
            self.registerTrace.setRegister(reg, false);
        }
    }
};

const ScopeContext = struct {
    vars: VarPos,
    simstack: Arraylist(Inst.Type),
    context: *analyser.Context,
    version: std.hash_map.StringHashMap(bbcTypes.Type), // A version for type aliasing

    pub fn init(alloc: Allocator) !*ScopeContext {
        const self = try alloc.create(ScopeContext);
        self.* = ScopeContext{
            .vars = VarPos.init(alloc),
            .simstack = Arraylist(Inst.Type).inti(alloc),
        };

        return self;
    }

    pub fn getVariable(self: *const ScopeContext, name: []const u8) Inst.Location {
        const loc = self.vars.get(name).?;
        switch (loc) {
            .stack => return loc,
            .label => return loc,
            else => unreachable,
        }
    }

    //pub fn getVariables(vars: VarHashmap, ctx: *Context) !void {}

    pub fn getCurrentStackIndex(self: *const ScopeContext) usize {
        return self.simstack.items.len;
    }
};

pub fn getCompileType(t: *const Ast.Type, version: std.hash_map.StringHashMap(bbcTypes.Type), ctx: *ScopeContext, allocator: Allocator) Inst.Type {
    switch (t.base) {
        .function => return Inst.Type{ .function = {} },
        .name => |name| {
            if (ctx.context.typeDefExist(name)) {
                return Inst.Type{ .pointer = false };
                //var habs = Arraylist(Inst.Type).init(allocator);
                //var hab_it = ctx.context.getTypeDef(name).habitants.iterator();
                //while (hab_it.next()) |habitant| {
                //    habs.append(getCompileType(habitant.value_ptr.*, version, ctx, allocator)) catch {};
                //}
                //return Inst.Type{ .structType = habs };
            }
            if (version.contains(name))
                return getCompileType(version.get(name).?.decided, version, ctx, allocator);
            if (std.mem.eql(u8, name, "Int"))
                return Inst.Type{ .intType = t.err };
            if (std.mem.eql(u8, name, "Bool"))
                return Inst.Type{ .intType = t.err };
            if (std.mem.eql(u8, name, "String"))
                return Inst.Type{ .stringType = t.err };
            if (std.mem.eql(u8, name, "Void"))
                return Inst.Type{ .voidType = t.err };
        },
    }
    return Inst.Type{ .intType = false };
}

const getCompileSize = Inst.getCompileSize;

pub fn saveRegisterFuncall(ctx: *Context) !void {
    for (ctx.registers.utilised, Inst.RegIter) |utilised, reg| {
        if (utilised)
            try ctx.builder.pushValue(.{ .register = reg });
    }
}

pub fn loadRegisterFuncall(ctx: *Context) !void {
    var i = ctx.registers.utilised.len;

    while (i > 0) : (i -= 1) {
        const utilised = ctx.registers.utilised[i - 1];
        const reg = Inst.RegIter[i - 1];
        if (utilised)
            try ctx.builder.popValue(.{ .register = reg });
    }
}

pub fn generateArguments(args: Arraylist(*Ast.Arguments), ctx: *Context, scopeCtx: *ScopeContext) !void {
    var size: i64 = @intCast(0);
    if (args.items.len == 0) // No need to do anything then
        return;
    for (args.items) |arg| {
        size += getCompileSize(getCompileType(arg._type, scopeCtx.version, scopeCtx, ctx.allocator));
    }

    try ctx.builder.reserveStack(@intCast(size));
    for (args.items, 0..) |arg, i| {
        const compile_arg_type = getCompileType(arg._type, scopeCtx.version, scopeCtx, ctx.allocator);
        try scopeCtx.simstack.append(compile_arg_type);
        try ctx.builder.moveInst(
            Inst.Location{ .argument = i },
            Inst.Location{
                .stack = scopeCtx.getCurrentStackIndex(),
            },
            compile_arg_type,
        );
        try scopeCtx.vars.put(arg.name, Inst.Location{
            .stack = scopeCtx.getCurrentStackIndex(),
        });
    }
}

pub fn freeArguments(args: Arraylist(*Ast.Arguments), ctx: *Context, scopeCtx: *ScopeContext, funcname: []const u8) !void {
    var size: i64 = @intCast(0);
    if (args.items.len == 0) // No need to do anything then
        return;
    for (args.items) |arg| {
        size += getCompileSize(getCompileType(arg._type, scopeCtx.version, scopeCtx, ctx.allocator));
    }
    try ctx.builder.comment("Freeing the arguements");
    for (args.items) |arg| {
        const emplacement = scopeCtx.vars.get(arg.name).?;
        const compile_arg_type = getCompileType(arg._type, scopeCtx.version, scopeCtx, ctx.allocator);
        const gen_funcname = try std.fmt.allocPrint(ctx.allocator, "{s}Free", .{arg._type.base.name});
        if (compile_arg_type == .pointer and !std.mem.eql(u8, funcname, gen_funcname)) {
            const loc = try pickWiselyLocation(ctx, scopeCtx, compile_arg_type, .mid);

            try ctx.builder.readFromPointer(loc, 0, emplacement);

            try ctx.builder.decrementReferenceCounter(loc);
            try callFreeFunction(ctx, scopeCtx, loc, bbcTypes.Type{ .decided = arg._type });
            try freeLocation(loc, scopeCtx, ctx);
        }

        try ctx.builder.decreaseStack(.{ .intType = false }); // Don't care about the real type
    }
    try ctx.builder.comment("");
}

pub fn callFreeFunction(ctx: *Context, scopeCtx: *ScopeContext, loc: Inst.Location, _type: bbcTypes.Type) !void {
    const funcname = try std.fmt.allocPrint(ctx.allocator, "{s}Free", .{_type.decided.base.name});
    var functype = Ast.TypeFunc{
        .argtypes = .init(ctx.allocator),
        .fname = funcname,
        .retype = (try bbcTypes.CreateTypeVoid(ctx.allocator, false)).decided,
        .typeparam = .init(ctx.allocator),
    };
    try functype.argtypes.append((try bbcTypes.CreateTypeInt(ctx.allocator, false)).decided);
    const label = ctx.getUid(analyser.functionVersion{ .name = funcname, .signature = functype, .version = .init(ctx.allocator) });

    const func = Inst.Location{ .label = label };

    var arguments = Arraylist(Inst.Location).init(ctx.allocator);
    try arguments.append(.{ .stack = scopeCtx.getCurrentStackIndex() });

    try ctx.builder.pushValue(loc);

    try ctx.builder.funcall(func, arguments);
    try ctx.builder.popValue(loc);
}

pub fn chooseWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext, duration: Inst.DurationPriority) Inst.Location {
    const a = if (duration == .mid) blk: {
        for (Inst.CalleeSave) |reg| {
            const utilised = ctx.registers.getRegister(reg);
            if (!utilised) {
                break :blk Inst.Location{ .register = reg };
            }
        } else {
            break :blk Inst.Location{
                .stack = scopeCtx.getCurrentStackIndex(),
            };
        }
    } else if (duration == .scratch) blk: {
        for (Inst.ScratchReg) |reg| {
            const utilised = ctx.registers.getRegister(reg);
            if (!utilised) {
                break :blk Inst.Location{ .register = reg };
            }
        } else {
            break :blk Inst.Location{
                .stack = scopeCtx.getCurrentStackIndex(),
            };
        }
    } else Inst.Location{
        .stack = scopeCtx.getCurrentStackIndex(),
    };
    return a;
}

pub fn pickWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext, t: Inst.Type, duration: Inst.DurationPriority) !Inst.Location {
    // Allocates memory on the stack if necessary, and returns the location
    const loc = chooseWiselyLocation(ctx, scopeCtx, duration);
    switch (loc) {
        .stack => {
            // we add a pointer on the stack
            try scopeCtx.simstack.append(t);
            try ctx.builder.reserveStack(@intCast(getCompileSize(t)));
        },
        .register => |reg| {
            ctx.registers.setRegister(reg, true);
            // We need to trace what registers have been used in the function
            // so they can be saved (caller saved - like)
            ctx.registerTrace.setRegister(reg, true);
        },
        .argument => unreachable, // Can't pick an argument
        .label => unreachable,
        .void => unreachable,
    }
    return loc;
}

pub fn freeLocation(loc: Inst.Location, scopeCtx: *ScopeContext, ctx: *Context) !void {
    _ = scopeCtx;
    switch (loc) {
        .register => |reg| ctx.registers.setRegister(reg, false),
        .stack => {},
        .label => {},
        .argument => {},
        .void => {},
    }
}

pub fn generateValueAssignement(value: *const Ast.Value, ctx: *Context, scopeCtx: *ScopeContext) !Inst.Location {
    switch (value.*) {
        .identifier => |ident| {
            const varpos = scopeCtx.getVariable(ident.name);
            const ret = try pickWiselyLocation(ctx, scopeCtx, .{ .pointer = false }, .mid);
            try ctx.builder.loadAddress(varpos, ret);
            return ret;
        },
        .varDec => |vardec| {
            const varpos = scopeCtx.getVariable(vardec.name);
            const ret = try pickWiselyLocation(ctx, scopeCtx, .{ .pointer = false }, .mid);
            try ctx.builder.loadAddress(varpos, ret);
            return ret;
        },
        else => return Inst.Location{ .void = {} },
    }
}

pub fn generateBinOperation(binop: *const Ast.binaryOperation, scopeCtx: *ScopeContext, ctx: *Context) !Inst.Location {
    const lhs_loc = try generateValue(binop.lhs, scopeCtx, ctx);
    const rhs_loc = try generateValue(binop.rhs, scopeCtx, ctx);
    const lhs_type = getCompileType((try bbcTypes.getTypeOfValue(binop.lhs, scopeCtx.context, ctx.allocator)).decided, scopeCtx.version, scopeCtx, ctx.allocator);
    const rhs_type = getCompileType((try bbcTypes.getTypeOfValue(binop.rhs, scopeCtx.context, ctx.allocator)).decided, scopeCtx.version, scopeCtx, ctx.allocator);
    // Assuming lhs and rhs have decided types and both can't be functions (eliminated option in the analyser)
    if (lhs_type == .intType and rhs_type == .intType) {
        // Both sides are Int, the result is Int too
        try freeLocation(rhs_loc, scopeCtx, ctx);
        const res_loc = lhs_loc; // the result is stored in the lhs location
        switch (binop.operator) {
            .Plus => try ctx.builder.plus(lhs_loc, rhs_loc),
            .Minus => try ctx.builder.minus(lhs_loc, rhs_loc),
            .Times => try ctx.builder.multiply(lhs_loc, rhs_loc),
            .Modulus => try ctx.builder.modulo(lhs_loc, rhs_loc),
            .Equal => try ctx.builder.equal(lhs_loc, rhs_loc),
            .NotEqual => try ctx.builder.notEqual(lhs_loc, rhs_loc),
            .Lt => try ctx.builder.lessThan(lhs_loc, rhs_loc),
            .Le => try ctx.builder.lessEqual(lhs_loc, rhs_loc),
            .Gt => try ctx.builder.greaterThan(lhs_loc, rhs_loc),
            .Ge => try ctx.builder.greaterEqual(lhs_loc, rhs_loc),
            .Div => { // Div can return an error
                const div_label = try ctx.generateLabel("div");
                const div_err_label = try ctx.generateLabel("div_err");
                try ctx.builder.conditionalJump(rhs_loc, div_label);
                try ctx.builder.intLit(1, .{ .register = .r0 });
                try ctx.builder.moveInst(Inst.Location{ .register = .r0 }, .{ .label = &error_union_label.* }, .{ .intType = false });
                try ctx.builder.jump(div_err_label); // Avoid division by zero
                try ctx.builder.labelDec(div_label);
                try ctx.builder.divide(lhs_loc, rhs_loc);
                try ctx.builder.labelDec(div_err_label);
            },
            else => unreachable,
        }
        return res_loc;
    }
    if (rhs_type == .voidType) {
        try freeLocation(rhs_loc, scopeCtx, ctx);
        const res_loc = lhs_loc; // the result is stored in the lhs location
        switch (binop.operator) {
            .Equal => try ctx.builder.equal(lhs_loc, rhs_loc),
            .NotEqual => try ctx.builder.notEqual(lhs_loc, rhs_loc),
            else => unreachable,
        }
        return res_loc;
    }
    errors.bbcErrorExit("Can't compile operator {s} with types {s} and {s}", .{
        Ast.reprBinOp(binop.operator),
        @tagName(lhs_type),
        @tagName(rhs_type),
    }, "");
    return Inst.Location{ .void = {} };
}

pub fn generateValue(value: *const Ast.Value, scopeCtx: *ScopeContext, ctx: *Context) Allocator.Error!Inst.Location {
    defer {
        var defer_err_type = Ast.Type{ .base = Ast.TypeBase{ .name = "Int" }, .err = false, .references = 0 };
        const eval_type = bbcTypes.getTypeOfValue(@constCast(value), scopeCtx.context, ctx.allocator) catch blk: {
            break :blk bbcTypes.Type{ .decided = &defer_err_type };
        };
        const comp_type = getCompileType(eval_type.decided, scopeCtx.version, scopeCtx, ctx.allocator);
        if (comp_type.hasError())
            ctx.builder.conditionalJump(Inst.Location{ .label = &error_union_label.* }, &error_union_function.*) catch {};
    }
    switch (value.*) {
        .intLit => |intlit| {
            const dest = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = false }, .mid);
            try ctx.builder.intLit(@intCast(intlit.value), dest);
            return dest;
        },
        .boolLit => |boollit| {
            const dest = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = false }, .mid);
            try ctx.builder.charLit(if (boollit.value) 1 else 0, dest);
            return dest;
        },
        .nullLit => {
            const dest = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = false }, .mid);
            try ctx.builder.intLit(0, dest);
            return dest;
        },
        .identifier => |ident| {
            // We move the value idealy to one of the registers, but otherwise on the stack
            const origin = scopeCtx.getVariable(ident.name);
            const _type = scopeCtx.context.getVariable(ident.name).decided;
            if (_type.base == .function)
                return origin;
            const compile_type = getCompileType(_type, scopeCtx.version, scopeCtx, ctx.allocator);
            const dest = try pickWiselyLocation(ctx, scopeCtx, compile_type, .mid);

            try ctx.builder.moveInst(origin, dest, compile_type);

            return dest;
        },
        .assignement => |assign| {
            // "dest = origin"
            const origin = try generateValue(assign.rhs, scopeCtx, ctx);
            const dest = try generateValueAssignement(assign.lhs, ctx, scopeCtx);
            const _type = (try bbcTypes.getTypeOfValue(assign.rhs, scopeCtx.context, ctx.allocator)).decided;
            const compile_type = getCompileType(
                _type,
                scopeCtx.version,
                scopeCtx,
                ctx.allocator,
            );
            if (compile_type == .pointer) {
                try ctx.builder.incrementeReferenceCounter(origin);

                try ctx.builder.comment("Decrementing ref counter and free");
                const emplacement = try pickWiselyLocation(ctx, scopeCtx, .{ .pointer = false }, .mid);
                try ctx.builder.readFromPointer(emplacement, 0, dest);
                try ctx.builder.decrementReferenceCounter(emplacement);

                try callFreeFunction(ctx, scopeCtx, emplacement, bbcTypes.Type{ .decided = _type });
                try freeLocation(emplacement, scopeCtx, ctx);
                try ctx.builder.comment(" -- ");
            }
            try ctx.builder.comment("Writing variable");
            try ctx.builder.writeToPointer(dest, 0, origin);
            try ctx.builder.comment("--");
            try freeLocation(origin, scopeCtx, ctx);
            try freeLocation(dest, scopeCtx, ctx);
            return Inst.Location{ .void = {} };
        },
        .varDec => {
            return Inst.Location{ .void = {} };
        },
        .scope => |scope| {
            return try generateScope(scope, scopeCtx, ctx);
        },
        .funcall => |funcall| {
            var func = try generateValue(funcall.func, scopeCtx, ctx);
            const functype = (try bbcTypes.getTypeOfValue(funcall.func, scopeCtx.context, ctx.allocator)).decided.base.function;
            // creating the local version
            var func_version = std.hash_map.StringHashMap(bbcTypes.Type).init(ctx.allocator);
            for (funcall.args.items, functype.argtypes.items) |a, b| {
                const argtype = try bbcTypes.getTypeOfValue(a, scopeCtx.context, ctx.allocator);
                // If there's a type alias, then we get to assign it
                if (analyser.typeparamContains(functype.typeparam, b.base.name)) {
                    try func_version.put(b.base.name, argtype);
                }
            }
            // func should have the location 'label'
            func.label = ctx.getUid(analyser.functionVersion{ .name = func.label, .signature = functype, .version = func_version });
            //try saveRegisterFuncall(ctx);

            var args = Arraylist(Inst.Location).init(ctx.allocator);
            for (funcall.args.items) |arg| {
                // All the arguments are placed on the stack => we can have an inifite ammount of arguments at the same place
                // and calling conventions are easyier if everything is temporally stored on the stack
                const base_arg = try generateValue(arg, scopeCtx, ctx);
                const arg_compile_type = getCompileType(
                    (try bbcTypes.getTypeOfValue(arg, scopeCtx.context, ctx.allocator)).decided,
                    scopeCtx.version,
                    scopeCtx,
                    ctx.allocator,
                );
                try scopeCtx.simstack.append(arg_compile_type);
                const stack_lock = Inst.Location{
                    .stack = scopeCtx.getCurrentStackIndex(),
                };
                try ctx.builder.pushValue(base_arg);
                try freeLocation(base_arg, scopeCtx, ctx);
                try args.append(stack_lock);
            }

            try ctx.builder.funcall(func, args);

            const funcretype = (try bbcTypes.getTypeOfValue(funcall.func, scopeCtx.context, ctx.allocator)).decided.base.function.retype;
            const ret_compile_type = getCompileType(
                funcretype,
                scopeCtx.version,
                scopeCtx,
                ctx.allocator,
            );
            const retloc = try pickWiselyLocation(ctx, scopeCtx, ret_compile_type, .mid);
            try ctx.builder.moveInst(.{ .register = .r0 }, retloc, ret_compile_type);

            for (0..funcall.args.items.len) |i_arg| {
                const arg_compile_type = getCompileType(
                    (try bbcTypes.getTypeOfValue(funcall.args.items[funcall.args.items.len - 1 - i_arg], scopeCtx.context, ctx.allocator)).decided,
                    scopeCtx.version,
                    scopeCtx,
                    ctx.allocator,
                );
                try ctx.builder.decreaseStack(arg_compile_type);
                _ = scopeCtx.simstack.pop().?;
            }

            //try loadRegisterFuncall(ctx);

            return retloc;
        },
        .binaryOperator => |binop| {
            return try generateBinOperation(binop, scopeCtx, ctx);
        },
        .stringLit => |stringlit| {
            // Add the amout of memory on the stack ( len(stringlit) + (8 - len(stringlit)%8) + 8 )
            // in order to keep the stack 8bits aligned
            // And place each characters and the len (on the first byte)
            // String_stack_size is only the size of the string itself
            // Memory layout :  | __SIZE__ | __STRING_LIT...__ |
            //                  |    8     |        ...        |
            const string_name = try ctx.generateLabel("stringlit");
            try ctx.dec_strings.append(.{
                .content = stringlit.value,
                .name = string_name,
            });
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .pointer = false }, .mid);
            try ctx.builder.moveInst(.{ .label = string_name }, ret_loc, Inst.Type{ .pointer = false });
            try ctx.builder.print(ret_loc);
            return ret_loc;
        },
        .If => |ifstmt| {
            const end_label = try ctx.generateLabel("end_if");
            const compile_type = getCompileType(
                (try bbcTypes.getTypeOfValue(ifstmt.scopes.items[0], scopeCtx.context, ctx.allocator)).decided,
                scopeCtx.version,
                scopeCtx,
                ctx.allocator,
            );
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, compile_type, .mid);

            // We can generate each scope one by one
            for (ifstmt.conditions.items, ifstmt.scopes.items) |cond, scope| {
                const next_label = try ctx.generateLabel("ifstmt_cond");
                const _test = try generateValue(cond, scopeCtx, ctx);
                try ctx.builder.not(_test);
                try ctx.builder.conditionalJump(_test, next_label);
                const scope_ret_loc = try generateValue(scope, scopeCtx, ctx);
                if (compile_type != .voidType)
                    try ctx.builder.moveInst(scope_ret_loc, ret_loc, compile_type);
                try ctx.builder.jump(end_label);
                try ctx.builder.labelDec(next_label);
            }

            // Then if it exists, the else statement
            if (ifstmt.elsescope) |else_scope| {
                const else_scope_ret_loc = try generateValue(else_scope, scopeCtx, ctx);
                if (compile_type != .voidType)
                    try ctx.builder.moveInst(else_scope_ret_loc, ret_loc, compile_type);
            }
            try ctx.builder.labelDec(end_label);
            return ret_loc;
        },
        .parenthesis => |val| {
            return try generateValue(val, scopeCtx, ctx);
        },
        .errorCheck => |errcheck| {
            // Principle of the compilation
            // We create a pseudo-function that we call, if there's an error, the functions returns
            // otherwise the function continues normally:
            // Layout
            //      ...
            //      call function
            //      ;; here the function has returned => error
            //      move and evaluate the scope value
            //      jump to end
            //   function:
            //      do what the error-unioned code do
            //      and put it in the right spot
            //   end:
            //      ...
            // there's no need of actually creating the pseudo-function, just the label in sufficient

            const pseudo_func = try ctx.generateLabel("error_checking");
            const end_label = try ctx.generateLabel("error_check_end");

            const eval_type = try bbcTypes.getTypeOfValue(errcheck.value, scopeCtx.context, ctx.allocator);
            const comp_type = getCompileType(
                eval_type.decided,
                scopeCtx.version,
                scopeCtx,
                ctx.allocator,
            );
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, comp_type, .mid);

            try ctx.builder.funcall(Inst.Location{ .label = pseudo_func }, .init(ctx.allocator));

            // Generating the default value if there's an error
            const scope_loc = try generateValue(errcheck.scope, scopeCtx, ctx);
            try ctx.builder.moveInst(scope_loc, ret_loc, comp_type);
            try freeLocation(scope_loc, scopeCtx, ctx); // We can free this location, not needed
            try ctx.builder.jump(end_label);

            try ctx.builder.labelDec(pseudo_func);
            try ctx.builder.beginFunction();
            try scopeCtx.simstack.append(.{ .pointer = false }); // Pushing a pointer because of the function call

            // The original value, that may return with an error (in that case it jumps back to the funcall)
            const value_loc = try generateValue(errcheck.value, scopeCtx, ctx);
            try ctx.builder.moveInst(value_loc, ret_loc, comp_type);
            try freeLocation(value_loc, scopeCtx, ctx); // We can free this location, not needed
            // We can remove the function from the top of the stack
            try ctx.builder.endFunction();
            _ = scopeCtx.simstack.pop();
            try ctx.builder.decreaseStack(Inst.Type{ .pointer = false });

            // End label
            try ctx.builder.labelDec(end_label);

            return ret_loc;
        },
        .While => |whileloop| {
            const w_begin = try ctx.generateLabel("while_begin");
            const w_end = try ctx.generateLabel("while_end");
            try ctx.builder.labelDec(w_begin);
            const cond = try generateValue(whileloop.condition, scopeCtx, ctx);
            try ctx.builder.not(cond);
            try ctx.builder.conditionalJump(cond, w_end);
            _ = try generateValue(whileloop.exec, scopeCtx, ctx);
            try ctx.builder.jump(w_begin);
            try ctx.builder.labelDec(w_end);
            return Inst.Location{ .void = {} };
        },
        .structInit => |stc_init| {
            // See standards to understand how the memory is layed out
            const stc_loc = try pickWiselyLocation(ctx, scopeCtx, .{ .pointer = false }, .mid);
            const stc_type = Ast.Type{
                .base = .{ .name = stc_init.name },
                .err = false,
                .references = 0,
            };
            const stc_c_type = getCompileType(
                &stc_type,
                scopeCtx.version,
                scopeCtx,
                ctx.allocator,
            );
            const stc_size = getCompileSize(stc_c_type);

            try ctx.builder.heapAlloc(stc_size, stc_loc);

            //const stc_def = scopeCtx.context.getTypeDef(stc_init.name);
            var hab_it = stc_init.habitants.iterator();
            while (hab_it.next()) |habitant| {
                const idx = scopeCtx.context.getStructHabitantIndex(
                    stc_init.name,
                    habitant.key_ptr.*,
                );
                const hab_type = scopeCtx.context.getTypeDef(stc_init.name).getHabitant(habitant.key_ptr.*);
                const hab_compile_type = getCompileType(hab_type, scopeCtx.version, scopeCtx, ctx.allocator);
                const content_loc = try generateValue(habitant.value_ptr.*, scopeCtx, ctx);

                try ctx.builder.writeToPointer(stc_loc, idx, content_loc);

                const emplacement = try pickWiselyLocation(ctx, scopeCtx, .{ .pointer = false }, .mid);
                try ctx.builder.readFromPointer(emplacement, idx, stc_loc);
                if (hab_compile_type == .pointer) {
                    try ctx.builder.incrementeReferenceCounter(content_loc);
                    try ctx.builder.decrementReferenceCounter(emplacement);
                    try callFreeFunction(ctx, scopeCtx, emplacement, bbcTypes.Type{ .decided = hab_type });
                }
                try freeLocation(emplacement, scopeCtx, ctx);
                try freeLocation(content_loc, scopeCtx, ctx);
            }
            const zero_count = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = false }, .scratch);
            try ctx.builder.intLit(0, zero_count);
            try ctx.builder.writeToPointer(stc_loc, 0, zero_count); // Coounter and size are 0 initialized
            try ctx.builder.writeToPointer(stc_loc, 1, zero_count);
            try freeLocation(zero_count, scopeCtx, ctx);

            return stc_loc;
        },
        .unaryOperatorRight => |uop_right| {
            const value_loc = try generateValue(uop_right.expr, scopeCtx, ctx);
            const value_type = try bbcTypes.getTypeOfValue(uop_right.expr, scopeCtx.context, ctx.allocator);
            const ret_type = try bbcTypes.getTypeOfValue(@constCast(value), scopeCtx.context, ctx.allocator);
            const compile_type = getCompileType(ret_type.decided, scopeCtx.version, scopeCtx, ctx.allocator);
            const idx = scopeCtx.context.getStructHabitantIndex(
                value_type.decided.base.name,
                uop_right.operator.pointAttr,
            );
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, compile_type, .mid);
            try ctx.builder.readFromPointer(ret_loc, idx, value_loc);
            try freeLocation(value_loc, scopeCtx, ctx);
            return ret_loc;
        },
        .freeKeyword => |freekwd| {
            const location = try generateValue(freekwd.val, scopeCtx, ctx);
            try ctx.builder.heapFree(location);
            return Inst.Location{ .void = {} };
        },
        else => {
            std.debug.print("Unimplemented: {}\n", .{value.*});
            unreachable;
        },
    }
}

pub fn generateScope(scope: *const Ast.Scope, scopeCtx: *ScopeContext, ctx: *Context) !Inst.Location {
    if (scope.code.items.len == 0)
        return try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .voidType = false }, .scratch);
    // declaring all the variables inside the scope
    var newScopeCtx = try ctx.allocator.create(ScopeContext);
    newScopeCtx.vars = try VarPos.clone(scopeCtx.vars);
    newScopeCtx.context = scope.ctx;
    newScopeCtx.simstack = try scopeCtx.simstack.clone();
    newScopeCtx.version = try scopeCtx.version.clone();
    const init_stack_size = scopeCtx.simstack.items.len;
    defer newScopeCtx.simstack.deinit();

    // We can reserve some place on the stack
    // for all the variables declared in this scope
    // so it's easyer to free everything at the end, plus
    // we can know at all time where they are
    var it = scope.ctx.variables.iterator();
    var allocsize: i64 = @intCast(0);
    var variables = Arraylist(bbcTypes.Type).init(ctx.allocator);
    while (it.next()) |variable| {
        const vartype = getCompileType(variable.value_ptr.*.decided, scopeCtx.version, scopeCtx, ctx.allocator);
        allocsize += getCompileSize(vartype);
        try newScopeCtx.simstack.append(vartype);
        try newScopeCtx.vars.put(variable.key_ptr.*, Inst.Location{
            .stack = scopeCtx.getCurrentStackIndex(),
        });
        try variables.append(variable.value_ptr.*);
    }
    // allocating the variables declared in the scope

    try ctx.builder.reserveStack(@intCast(allocsize));

    // the first n-1 elements can be discarded if it does not contain any error
    for (scope.code.items[0 .. scope.code.items.len - 1]) |value| {
        //  const valtype = getCompileType((try bbcTypes.getTypeOfValue(value, scopeCtx.context, ctx.allocator)).decided, newScopeCtx.version);
        try ctx.builder.comment("Instruction");

        _ = try generateValue(value, newScopeCtx, ctx);
    }

    try ctx.builder.comment("Return value of scope");
    const ret = try generateValue(scope.code.getLast(), newScopeCtx, ctx);
    while (newScopeCtx.simstack.items.len - init_stack_size > 0) {
        const ast_type = variables.pop().?;
        const _type = newScopeCtx.simstack.pop().?;
        if (_type == .pointer) {
            const loc = try pickWiselyLocation(ctx, scopeCtx, .{ .pointer = false }, .mid);
            try ctx.builder.popValue(loc);

            try ctx.builder.decrementReferenceCounter(loc);
            try callFreeFunction(ctx, scopeCtx, loc, ast_type);

            try freeLocation(loc, scopeCtx, ctx);
        } else {
            try ctx.builder.decreaseStack(newScopeCtx.simstack.pop().?);
        }
    }
    return ret;
}

pub fn generateFunction(func: *const analyser.functionVersion, function_uid: []const u8, ctx: *Context, baseVarTable: VarPos) !void {
    // declare function (add label)
    try ctx.builder.functionDec(function_uid);
    try ctx.builder.beginFunction();

    const funcdef = ctx.codeContext.functions.get(func.name).?;

    // creating the context for the scope
    var scopeCtx = try ctx.allocator.create(ScopeContext);
    scopeCtx.vars = try baseVarTable.clone();
    scopeCtx.context = funcdef.code.ctx;
    scopeCtx.simstack = Arraylist(Inst.Type).init(ctx.allocator);
    scopeCtx.version = try func.version.clone();
    defer scopeCtx.simstack.deinit();

    // We can get the position of the builder before and after the scope build
    // to shift the stack because we're saving the registers used in the function
    // on the stack
    const begin_func_pos = ctx.builder.getPosition();

    // generating the arguements and putting them on the stack
    try generateArguments(funcdef.arguments, ctx, scopeCtx);

    const return_val = try generateScope(funcdef.code, scopeCtx, ctx);
    if (getCompileType((try bbcTypes.getTypeOfScope(funcdef.code, scopeCtx.context, ctx.allocator)).decided, func.version, scopeCtx, ctx.allocator) != .voidType)
        try ctx.builder.moveInst(return_val, .{ .register = .r0 }, .{ .intType = false });
    try freeLocation(return_val, scopeCtx, ctx);

    // All the registers used
    var reg_pushed = Arraylist(Inst.Registers).init(ctx.allocator);
    defer reg_pushed.deinit();
    ctx.builder.setPosition(begin_func_pos); // Setting the builder to a previous position before the scope
    try ctx.builder.comment("Saving the callee saved registers");
    for (ctx.registerTrace.utilised, 0..) |utilised, i| {
        const reg = Inst.RegIter[i];
        if (utilised and Inst.in(reg, &Inst.CalleeSave)) {
            try reg_pushed.insert(0, reg);
            try ctx.builder.pushValue(.{ .register = reg });
        }
    }
    try ctx.builder.comment("");

    const second_func_pos = ctx.builder.getPosition();

    ctx.builder.setPosition(.{ .last = {} }); // Now we can set it to the end of the code

    // clearing the arguments on the stack
    try freeArguments(funcdef.arguments, ctx, scopeCtx, func.name);

    // We can shift the stack in the scope
    ctx.builder.shiftStackBetween(second_func_pos, .{ .last = {} }, @intCast(reg_pushed.items.len + 1));
    // Now we can restore the registers
    try ctx.builder.comment("Loading the callee saved registers");
    for (reg_pushed.items) |reg| {
        try ctx.builder.popValue(.{ .register = reg });
    }
    try ctx.builder.comment("");

    // We can declare the function's ending , seting rbp, and all
    try ctx.builder.endFunction();
    try ctx.builder.returnInst(.{ .register = .r0 });

    try freeLocation(return_val, scopeCtx, ctx);
    ctx.eraseRegisterTrace();
}

pub fn generateFuncUID(funcname: []const u8, f_table: *std.hash_map.StringHashMap(usize), alloc: Allocator) ![]const u8 {
    // Main can't have a different ID than 'main'
    if (std.mem.eql(u8, funcname, "main"))
        return funcname;

    if (!f_table.contains(funcname))
        try f_table.put(funcname, 0);

    const func_num = f_table.get(funcname).?;

    const id = try std.fmt.allocPrint(alloc, "{s}@{d}", .{ funcname, func_num });
    try f_table.put(funcname, func_num + 1);
    return id;
}

pub fn generateProgram(ast: *Ast.Program, cctx: *analyser.Context, alloc: Allocator) !Inst.Builder {
    // Here: building context, setting up everything needed
    var ctx = Context{
        .allocator = alloc,
        .builder = Inst.Builder.init(alloc),
        .codeContext = cctx,
        .registers = Inst.RegisterTable.init(),
        .func_id_table_counter = std.hash_map.StringHashMap(usize).init(alloc),
        .func_uid_list = .init(alloc),
        .labels = .init(alloc),
        .dec_strings = .init(alloc),
        .registerTrace = .init(),
    };

    var functions_uid = Arraylist([]const u8).init(alloc);
    for (cctx.functions_to_compile.items) |func| {
        const function_uid = try generateFuncUID(func.name, &ctx.func_id_table_counter, ctx.allocator);
        try ctx.func_uid_list.append(.{
            .uid = function_uid,
            .version = func,
        });
        try functions_uid.append(function_uid);
    }

    _ = ast;
    var baseVarTable = VarPos.init(ctx.allocator);
    while (cctx.functions_to_compile.items.len > 0) {
        const func = cctx.functions_to_compile.pop().?;
        const f_uid = functions_uid.pop().?;
        std.debug.print("#[Generating {s}]\n", .{f_uid});
        try baseVarTable.put(func.name, Inst.Location{ .label = func.name });
        try generateFunction(&func, f_uid, &ctx, baseVarTable);
    }

    // Building the wrapper to main
    try ctx.builder.functionDec(&main_function_wrapper_name.*);
    try ctx.builder.moveInst(Inst.Location{ .label = "main" }, .{ .register = .r0 }, .{ .function = {} });
    try ctx.builder.funcall(.{ .register = .r0 }, Arraylist(Inst.Location).init(alloc));
    try ctx.builder.exitWith(Inst.Location{ .register = .r0 });

    // Building the error function callback
    try ctx.builder.functionDec(&error_union_function.*);
    try ctx.builder.endFunction(); // the end of all function :,(
    try ctx.builder.returnInst(Inst.Location{ .void = {} });

    // we can begin the variable section
    try ctx.builder.beginVariableSection();
    // content: []const (struct{size:usize, content:[]const u8})
    var contents_data = Inst.varContentType.init(alloc);
    try contents_data.append(.{ .content = 0, .size = 8 });
    try ctx.builder.declareVariable(&error_union_label.*, &contents_data);

    for (ctx.dec_strings.items) |string| {
        var content = Inst.varContentType.init(ctx.allocator);
        try content.append(.{
            .content = @intCast(string.content.len),
            .size = 8,
        });
        for (0..string.content.len) |i_char| {
            try content.append(.{
                .content = string.content[i_char],
                .size = 1,
            });
        }
        try ctx.builder.declareVariable(string.name, &content);
    }

    return ctx.builder;
}
