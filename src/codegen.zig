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
pub const main_function_wrapper_name = "main_wrapper";

const Context = struct {
    codeContext: *analyser.Context,
    allocator: std.mem.Allocator,
    builder: Inst.Builder,
    registers: Inst.RegisterTable,
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
            std.debug.print("-->#{s}\n", .{func.version.name});
            if (std.mem.eql(u8, func.version.name, version.name)) {
                if (func.version.version.count() != version.version.count())
                    continue;
                var it = func.version.version.iterator();
                while (it.next()) |vers| {
                    std.debug.print("-->({s}\n", .{vers.key_ptr.*});
                    if (!version.version.contains(vers.key_ptr.*))
                        continue :base_for;
                    const arg_type = version.version.get(vers.key_ptr.*).?;
                    std.debug.print("-->({s}\n", .{arg_type.toString(self.allocator)});
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
            count = self.labels.get(base).?;
        } else {
            try self.labels.put(base, count);
        }
        return try std.fmt.allocPrint(self.allocator, "autogen@{s}_{d}", .{ base, count });
    }
};

const ScopeContext = struct {
    vars: VarPos,
    simstack: Arraylist(Inst.Type),
    context: *analyser.Context,
    version: std.hash_map.StringHashMap(bbcTypes.Type), // A version for type aliasing

    pub fn init(alloc: Allocator) !*ScopeContext {
        const self = try alloc.create(ScopeContext);
        self.* = ScopeContext{ .vars = VarPos.init(alloc), .simstack = Arraylist(Inst.Type).inti(alloc) };
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

pub fn getCompileType(t: *Ast.Type, version: std.hash_map.StringHashMap(bbcTypes.Type)) Inst.Type {
    switch (t.base) {
        .function => return Inst.Type{ .function = {} },
        .name => |name| {
            if (version.contains(name))
                return getCompileType(version.get(name).?.decided, version);
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

pub fn generateArguments(args: Arraylist(*Ast.Arguments), ctx: *Context, scopeCtx: *ScopeContext) !void {
    var size: i64 = @intCast(0);
    if (args.items.len == 0) // No need to do anything then
        return;
    for (args.items) |arg| {
        size += getCompileSize(getCompileType(arg._type, scopeCtx.version));
    }

    try ctx.builder.reserveStack(@intCast(size));
    for (args.items, 0..) |arg, i| {
        const compile_arg_type = getCompileType(arg._type, scopeCtx.version);
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

pub fn chooseWiselyLocation(ctx: *Context, scopeCtx: *ScopeContext) Inst.Location {
    const a = blk: for (Inst.RegIter, ctx.registers.utilised) |reg, utilised| {
        if (!utilised) {
            break :blk Inst.Location{ .register = reg };
        }
    } else {
        break :blk Inst.Location{
            .stack = scopeCtx.getCurrentStackIndex(),
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
        .argument => unreachable, // Can't pick an argument
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
        .label => {},
        .argument => {},
        .void => {},
    }
}

pub fn generateValueAssignement(value: *const Ast.Value, scopeCtx: *ScopeContext) Inst.Location {
    return switch (value.*) {
        .identifier => |ident| return scopeCtx.getVariable(ident.name),
        .varDec => |vardec| return scopeCtx.getVariable(vardec.name),
        else => Inst.Location{ .void = {} },
    };
}

pub fn generateBinOperation(binop: *const Ast.binaryOperation, scopeCtx: *ScopeContext, ctx: *Context) !Inst.Location {
    const lhs_loc = try generateValue(binop.lhs, scopeCtx, ctx);
    const rhs_loc = try generateValue(binop.rhs, scopeCtx, ctx);
    const lhs_type = getCompileType((try bbcTypes.getTypeOfValue(binop.lhs, scopeCtx.context, ctx.allocator)).decided, scopeCtx.version);
    const rhs_type = getCompileType((try bbcTypes.getTypeOfValue(binop.rhs, scopeCtx.context, ctx.allocator)).decided, scopeCtx.version);
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
    errors.bbcErrorExit("Can't compile operator {s} with types {s} and {s}", .{
        Ast.reprBinOp(binop.operator),
        @tagName(lhs_type),
        @tagName(rhs_type),
    }, "");
    return chooseWiselyLocation(ctx, scopeCtx);
}

pub fn generateValue(value: *const Ast.Value, scopeCtx: *ScopeContext, ctx: *Context) Allocator.Error!Inst.Location {
    defer {
        var defer_err_type = Ast.Type{ .base = Ast.TypeBase{ .name = "Int" }, .err = false, .references = 0 };
        const eval_type = bbcTypes.getTypeOfValue(@constCast(value), scopeCtx.context, ctx.allocator) catch blk: {
            break :blk bbcTypes.Type{ .decided = &defer_err_type };
        };
        const comp_type = getCompileType(eval_type.decided, scopeCtx.version);
        if (comp_type.hasError())
            ctx.builder.conditionalJump(Inst.Location{ .label = &error_union_label.* }, &error_union_function.*) catch {};
    }
    switch (value.*) {
        .intLit => |intlit| {
            const dest = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = false });
            try ctx.builder.intLit(@intCast(intlit.value), dest);
            return dest;
        },
        .boolLit => |boollit| {
            const dest = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .intType = false });
            try ctx.builder.charLit(if (boollit.value) 1 else 0, dest);
            return dest;
        },
        .identifier => |ident| {
            // We move the value idealy to one of the registers, but otherwise on the stack
            const origin = scopeCtx.getVariable(ident.name);
            const _type = scopeCtx.context.getVariable(ident.name).decided;
            if (_type.base == .function)
                return origin;
            const compile_type = getCompileType(_type, scopeCtx.version);
            const dest = try pickWiselyLocation(ctx, scopeCtx, compile_type);

            try ctx.builder.moveInst(origin, dest, compile_type);

            return dest;
        },
        .assignement => |assign| {
            // "dest = origin"
            const origin = try generateValue(assign.rhs, scopeCtx, ctx);
            const dest = generateValueAssignement(assign.lhs, scopeCtx);
            const compile_type = getCompileType(
                (try bbcTypes.getTypeOfValue(assign.rhs, scopeCtx.context, ctx.allocator)).decided,
                scopeCtx.version,
            );
            try ctx.builder.moveInst(origin, dest, compile_type);
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

            //try scopeCtx.simstack.append(Inst.Type{ .function = {} });
            //const func_stack_loc_idx = scopeCtx.getCurrentStackIndex();
            //const func_stack_lock = Inst.Location{
            //    .stack = func_stack_loc_idx,
            //};
            //try ctx.builder.reserveStack(@intCast(8));
            //try ctx.builder.moveInst(func, func_stack_lock, Inst.Type{ .function = {} }); // We can reserve the function on the stack
            //try freeLocation(func, scopeCtx, ctx);

            var args = Arraylist(Inst.Location).init(ctx.allocator);
            for (funcall.args.items) |arg| {
                // All the arguments are placed on the stack => we can have an inifite ammount of arguments at the same place
                // and calling conventions are easyier if everything is temporally stored on the stack
                const base_arg = try generateValue(arg, scopeCtx, ctx);
                const arg_compile_type = getCompileType(
                    (try bbcTypes.getTypeOfValue(arg, scopeCtx.context, ctx.allocator)).decided,
                    scopeCtx.version,
                );
                try scopeCtx.simstack.append(arg_compile_type);
                const stack_lock = Inst.Location{
                    .stack = scopeCtx.getCurrentStackIndex(),
                };
                try ctx.builder.reserveStack(@intCast(8));
                try ctx.builder.moveInst(base_arg, stack_lock, arg_compile_type);
                try freeLocation(base_arg, scopeCtx, ctx);
                try args.append(stack_lock);
            }
            //try saveRegistersFuncall(ctx, scopeCtx);

            try ctx.builder.funcall(func, args);

            const funcretype = (try bbcTypes.getTypeOfValue(funcall.func, scopeCtx.context, ctx.allocator)).decided.base.function.retype;
            const ret_compile_type = getCompileType(
                funcretype,
                scopeCtx.version,
            );
            const retloc = try pickWiselyLocation(ctx, scopeCtx, ret_compile_type);
            try ctx.builder.moveInst(.{ .register = Inst.Registers.r0 }, retloc, ret_compile_type);
            //try loadRegistersFuncall(ctx, scopeCtx);

            for (0..funcall.args.items.len) |i_arg| {
                const arg_compile_type = getCompileType(
                    (try bbcTypes.getTypeOfValue(funcall.args.items[funcall.args.items.len - 1 - i_arg], scopeCtx.context, ctx.allocator)).decided,
                    scopeCtx.version,
                );
                try ctx.builder.decreaseStack(arg_compile_type);
                _ = scopeCtx.simstack.pop().?;
            }
            //try ctx.builder.decreaseStack(Inst.Type{ .pointer = false });
            //_ = scopeCtx.simstack.pop().?;
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
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .pointer = false });
            try ctx.builder.moveInst(.{ .label = string_name }, ret_loc, Inst.Type{ .pointer = false });
            try ctx.builder.print(ret_loc);
            return ret_loc;
        },
        .If => |ifstmt| {
            const end_label = try ctx.generateLabel("end_if");
            const compile_type = getCompileType((try bbcTypes.getTypeOfValue(ifstmt.scopes.items[0], scopeCtx.context, ctx.allocator)).decided, scopeCtx.version);
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, compile_type);

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
            const comp_type = getCompileType(eval_type.decided, scopeCtx.version);
            const ret_loc = try pickWiselyLocation(ctx, scopeCtx, comp_type);

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
            _ = stc_init;
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
        return pickWiselyLocation(ctx, scopeCtx, Inst.Type{ .voidType = false });
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
    while (it.next()) |variable| {
        const vartype = getCompileType(variable.value_ptr.*.decided, scopeCtx.version);
        allocsize += getCompileSize(vartype);
        try newScopeCtx.simstack.append(vartype);
        try newScopeCtx.vars.put(variable.key_ptr.*, Inst.Location{
            .stack = scopeCtx.getCurrentStackIndex(),
        });
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
        try ctx.builder.decreaseStack(newScopeCtx.simstack.pop().?);
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

    // generating the arguements and putting them on the stack
    try generateArguments(funcdef.arguments, ctx, scopeCtx);

    const return_val = try generateScope(funcdef.code, scopeCtx, ctx);

    // We can declare the function's ending , seting rbp, and all
    try ctx.builder.endFunction();

    try ctx.builder.returnInst(return_val);

    try freeLocation(return_val, scopeCtx, ctx);
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
