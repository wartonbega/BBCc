const std = @import("std");
const ast = @import("ast.zig");
const Types = @import("types.zig");
const errors = @import("errors.zig");
const Traits = @import("traits.zig");
const Parser = @import("parser.zig");
const position = @import("position.zig");
const analyser = @import("analyser.zig");

pub const hinterType = union(enum) {
    noInfo: void,
    variable: struct {
        _type: *ast.Type,
        name: []const u8,
    },
    func: struct {
        sig: *ast.Type,
        name: []const u8,
    },
    literal: []const u8,

    typeName: *ast.Type,
};

pub const hinterInfo = struct {
    ttype: hinterType,
};

// ————————————————————————————————————————————————
//      HINTER SEARCH VALUE (when hovering)
// ————————————————————————————————————————————————
fn hintSearchValue(pos: position.Position, allocator: std.mem.Allocator, value: *ast.Value, ctx: *analyser.Context) hinterInfo {
    switch (value.*) {
        .assignement => |assign| {
            if (assign.rhs.getReference().contains(pos))
                return hintSearchValue(pos, allocator, assign.rhs, ctx);
            if (assign.lhs.getReference().contains(pos))
                return hintSearchValue(pos, allocator, assign.lhs, ctx);
        },
        .boolLit => return hinterInfo{ .ttype = .{ .literal = "Bool" } },
        .intLit => return hinterInfo{ .ttype = .{ .literal = "Int" } },
        .stringLit => |s| return hinterInfo{ .ttype = .{ .literal = s.value } },
        .nullLit => return hinterInfo{ .ttype = .{ .literal = "null" } },
        .charLit => return hinterInfo{ .ttype = .{ .literal = "Char" } },
        .identifier => |ident| {
            const name = ident.name;
            if (ctx.variableExist(name))
                return hinterInfo{ .ttype = .{ .variable = .{ ._type = ctx.getVariable(name).decided, .name = name } } };
            if (ctx.functionExist(name)) {
                const _type = analyser.createFunctionSignature(ctx.getFunction(name), allocator) catch {
                    return hinterInfo{ .ttype = .{ .noInfo = {} } };
                };
                return hinterInfo{ .ttype = .{ .variable = .{
                    ._type = _type,
                    .name = name,
                } } };
            }
        },
        .varDec => |vd| {
            if (ctx.variableExist(vd.name))
                return hinterInfo{ .ttype = .{ .variable = .{ ._type = ctx.getVariable(vd.name).decided, .name = vd.name } } };
        },
        .binaryOperator => |binop| {
            if (binop.rhs.getReference().contains(pos))
                return hintSearchValue(pos, allocator, binop.rhs, ctx);
            if (binop.lhs.getReference().contains(pos))
                return hintSearchValue(pos, allocator, binop.lhs, ctx);
        },
        .errorCheck => |err| {
            if (err.scope.getReference().contains(pos))
                return hintSearchValue(pos, allocator, err.scope, ctx);
            if (err.value.getReference().contains(pos))
                return hintSearchValue(pos, allocator, err.value, ctx);
        },
        .funcall => |funcall| {
            if (funcall.func.getReference().contains(pos))
                return hintSearchValue(pos, allocator, funcall.func, ctx);
            for (funcall.args.items) |arg| {
                if (arg.getReference().contains(pos))
                    return hintSearchValue(pos, allocator, arg, ctx);
            }
        },
        .function => |func| {
            return hintSearchFuncdef(pos, allocator, func, ctx);
        },
        .parenthesis => |p| return hintSearchValue(pos, allocator, p, ctx),
        .Print => |print| {
            for (print.args.items) |arg| {
                if (arg.getReference().contains(pos))
                    return hintSearchValue(pos, allocator, arg, ctx);
            }
        },
        .scope => |s| return hintSearchScope(pos, allocator, s),
        .If => |ifs| {
            for (ifs.conditions.items) |arg| {
                if (arg.getReference().contains(pos))
                    return hintSearchValue(pos, allocator, arg, ctx);
            }
            for (ifs.scopes.items) |arg| {
                if (arg.getReference().contains(pos))
                    return hintSearchValue(pos, allocator, arg, ctx);
            }
            if (ifs.elsescope) |esc| {
                if (esc.getReference().contains(pos))
                    return hintSearchValue(pos, allocator, esc, ctx);
            }
        },
        .structInit => |stc| {
            var it = stc.habitants.iterator();
            while (it.next()) |hab| {
                if (hab.value_ptr.*.getReference().contains(pos))
                    return hintSearchValue(pos, allocator, hab.value_ptr.*, ctx);
            }
        },
        .unaryOperatorRight => |uopr| {
            if (uopr.expr.getReference().contains(pos))
                return hintSearchValue(pos, allocator, uopr.expr, ctx);
            const t = Types.getTypeOfValue(uopr.expr, ctx, allocator) catch {
                return hinterInfo{ .ttype = .{ .noInfo = {} } };
            };
            return hinterInfo{ .ttype = .{ .typeName = t.decided } };
        },
        .While => |wl| {
            if (wl.condition.getReference().contains(pos))
                return hintSearchValue(pos, allocator, wl.condition, ctx);
            if (wl.exec.getReference().contains(pos))
                return hintSearchValue(pos, allocator, wl.exec, ctx);
        },
        else => std.debug.print("unimplemented {}\n", .{value.*}),
    }
    return hinterInfo{ .ttype = .{ .noInfo = {} } };
}

fn hintSearchScope(pos: position.Position, allocator: std.mem.Allocator, scope: *ast.Scope) hinterInfo {
    for (scope.code.items) |inst| {
        if (inst.getReference().contains(pos)) {
            return hintSearchValue(pos, allocator, inst, scope.ctx);
        }
    }
    return hinterInfo{ .ttype = .{ .noInfo = {} } };
}

fn hintSearchFuncdef(pos: position.Position, allocator: std.mem.Allocator, funcDef: *ast.funcDef, ctx: *analyser.Context) hinterInfo {
    for (funcDef.arguments.items) |arg| {
        if (arg.reference.contains(pos)) {
            return hinterInfo{ .ttype = .{ .variable = .{ ._type = arg._type, .name = arg.name } } };
        }
        if (arg.typeref.contains(pos)) {
            return hinterInfo{ .ttype = .{ .typeName = arg._type } };
        }
    }
    if (funcDef.return_type_ref.contains(pos)) {
        return hinterInfo{ .ttype = .{ .typeName = funcDef.return_type } };
    }
    if (!ctx.variableExist(funcDef.name)) // La fonction n'a pas été anlysée
        return hinterInfo{ .ttype = .{ .noInfo = {} } };

    if (funcDef.code.reference.contains(pos))
        return hintSearchScope(pos, allocator, funcDef.code);
    return hinterInfo{ .ttype = .{ .func = .{ .sig = ctx.getVariable(funcDef.name).decided, .name = funcDef.name } } };
}

fn hintSearchProg(pos: position.Position, allocator: std.mem.Allocator, prog: *ast.Program, ctx: *analyser.Context) hinterInfo {
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |fd| {
                if (fd.reference.contains(pos)) {
                    return hintSearchFuncdef(pos, allocator, fd, ctx);
                }
            },
            .StructDef => |sd| {
                if (sd.reference.contains(pos)) {}
            },
        }
    }
    return hinterInfo{ .ttype = .{ .noInfo = {} } };
}

pub fn getInfos(pos: position.Position, allocator: std.mem.Allocator, prog: *ast.Program, ctx: *analyser.Context) hinterInfo {
    return hintSearchProg(pos, allocator, prog, ctx);
}

// ——————————————————————————————
//       GET REFERENCE
// ——————————————————————————————

fn hintGetDefinitionValue(pos: position.Position, allocator: std.mem.Allocator, value: *ast.Value, ctx: *analyser.Context) ?position.Location {
    switch (value.*) {
        .assignement => |assign| {
            if (assign.rhs.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, assign.rhs, ctx);
            if (assign.lhs.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, assign.lhs, ctx);
        },
        .boolLit, .intLit, .stringLit, .nullLit, .charLit => return null,
        .identifier => |ident| {
            const name = ident.name;
            if (ctx.variableExist(name))
                return ctx.getVariableDefinition(name);
            if (ctx.functionExist(ident.name))
                return ctx.getFunction(ident.name).reference;
        },
        .funcall => |funcall| {
            if (funcall.func.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, funcall.func, ctx);
            for (funcall.args.items) |arg| {
                if (arg.getReference().contains(pos))
                    return hintGetDefinitionValue(pos, allocator, arg, ctx);
            }
        },
        .scope => |scope| {
            return hintGetDefinitionScope(pos, allocator, scope);
        },
        .binaryOperator => |binop| {
            if (binop.lhs.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, binop.lhs, ctx);
            if (binop.rhs.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, binop.rhs, ctx);
        },
        .parenthesis => |val| {
            if (val.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, val, ctx);
        },
        .varDec => |vardec| {
            return vardec.reference;
        },
        .If => |ifstmt| {
            for (ifstmt.conditions.items) |cond| {
                if (cond.getReference().contains(pos))
                    return hintGetDefinitionValue(pos, allocator, cond, ctx);
            }
            for (ifstmt.scopes.items) |scope| {
                if (scope.getReference().contains(pos))
                    return hintGetDefinitionValue(pos, allocator, scope, ctx);
            }
            if (ifstmt.elsescope) |elsescpe| {
                if (elsescpe.getReference().contains(pos))
                    return hintGetDefinitionValue(pos, allocator, elsescpe, ctx);
            }
        },
        .While => |whi| {
            if (whi.condition.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, whi.condition, ctx);
            if (whi.exec.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, whi.exec, ctx);
        },
        .Print => |print| {
            for (print.args.items) |arg| {
                if (arg.getReference().contains(pos))
                    return hintGetDefinitionValue(pos, allocator, arg, ctx);
            }
        },
        .errorCheck => |err| {
            if (err.value.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, err.value, ctx);
            if (err.scope.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, err.scope, ctx);
        },
        .function => |func| {
            return hintGetDefinitionFuncdef(pos, allocator, func, ctx);
        },
        .structInit => |stc| {
            var it = stc.habitants.iterator();
            while (it.next()) |hab| {
                if (hab.value_ptr.*.getReference().contains(pos))
                    return hintGetDefinitionValue(pos, allocator, hab.value_ptr.*, ctx);
            }
            if (ctx.typeDefExist(stc.name))
                return ctx.getTypeDef(stc.name).reference;
        },
        .unaryOperatorRight => |uopr| {
            if (uopr.expr.getReference().contains(pos))
                return hintGetDefinitionValue(pos, allocator, uopr.expr, ctx);
            const t = Types.getTypeOfValue(uopr.expr, ctx, allocator) catch {
                return null;
            };
            if (t.decided.base == .name and ctx.typeDefExist(t.decided.base.name))
                return ctx.getTypeDef(t.decided.base.name).reference;
        },
        else => std.debug.print("unimplemented {}\n", .{value.*}),
    }
    return null;
}

fn hintGetDefinitionScope(pos: position.Position, allocator: std.mem.Allocator, scope: *ast.Scope) ?position.Location {
    for (scope.code.items) |inst| {
        if (inst.getReference().contains(pos)) {
            return hintGetDefinitionValue(pos, allocator, inst, scope.ctx);
        }
    }
    return null;
}

fn hintGetDefinitionFuncdef(pos: position.Position, allocator: std.mem.Allocator, funcDef: *ast.funcDef, ctx: *analyser.Context) ?position.Location {
    for (funcDef.arguments.items) |arg| {
        if (arg.typeref.contains(pos)) {
            return ctx.getTypeDefinition(arg._type);
        }
    }
    if (funcDef.return_type_ref.contains(pos)) {
        return ctx.getTypeDefinition(funcDef.return_type);
    }
    if (!ctx.variableExist(funcDef.name)) // La fonction n'a pas été anlysée
        return null;

    if (funcDef.code.reference.contains(pos))
        return hintGetDefinitionScope(pos, allocator, funcDef.code);
    return null;
}

fn hintGetDefinitionProg(pos: position.Position, allocator: std.mem.Allocator, prog: *ast.Program, ctx: *analyser.Context) ?position.Location {
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |fd| {
                if (fd.reference.contains(pos)) {
                    return hintGetDefinitionFuncdef(pos, allocator, fd, ctx);
                }
            },
            .StructDef => |sd| {
                if (sd.reference.contains(pos)) {}
            },
        }
    }
    return null;
}

pub fn getDefinition(pos: position.Position, allocator: std.mem.Allocator, prog: *ast.Program, ctx: *analyser.Context) ?position.Location {
    return hintGetDefinitionProg(pos, allocator, prog, ctx);
}
// ——————————————————————————————————————————————————
//      INLAY HINTS (parameter names & types)
// ——————————————————————————————————————————————————

const protocol = @import("../lsp/protocol.zig");

pub const InlayHintCollector = struct {
    hints: std.ArrayList(protocol.InlayHint),
    range: protocol.Range,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, range: protocol.Range) InlayHintCollector {
        return .{
            .hints = std.ArrayList(protocol.InlayHint).init(allocator),
            .range = range,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *InlayHintCollector) void {
        self.hints.deinit();
    }

    fn isInRange(self: *InlayHintCollector, pos: position.Position) bool {
        if (pos.line < self.range.start.line or pos.line > self.range.end.line) return false;
        if (pos.line == self.range.start.line and pos.character < self.range.start.character) return false;
        if (pos.line == self.range.end.line and pos.character > self.range.end.character) return false;
        return true;
    }

    fn addParameterHint(self: *InlayHintCollector, pos: position.Position, param_name: []const u8) !void {
        if (!self.isInRange(pos)) return;

        const label = try std.fmt.allocPrint(self.allocator, "{s}:", .{param_name});

        try self.hints.append(.{
            .position = .{
                .line = @intCast(pos.line),
                .character = @intCast(pos.character),
            },
            .label = label,
            .kind = .Parameter,
            .paddingRight = true,
        });
    }

    fn addTypeHint(self: *InlayHintCollector, pos: position.Position, type_name: []const u8) !void {
        if (!self.isInRange(pos)) return;

        const label = try std.fmt.allocPrint(self.allocator, ": {s}", .{type_name});

        try self.hints.append(.{
            .position = .{
                .line = @intCast(pos.line),
                .character = @intCast(pos.character),
            },
            .label = label,
            .kind = .Type,
            .paddingLeft = true,
        });
    }
};

// Main function to collect hints from a value
fn collectHintsFromValue(collector: *InlayHintCollector, value: *ast.Value, ctx: *analyser.Context) void {
    switch (value.*) {
        .assignement => |assign| {
            collectHintsFromValue(collector, assign.lhs, ctx);
            collectHintsFromValue(collector, assign.rhs, ctx);
        },
        .binaryOperator => |binop| {
            collectHintsFromValue(collector, binop.lhs, ctx);
            collectHintsFromValue(collector, binop.rhs, ctx);
        },
        .errorCheck => |err| {
            collectHintsFromValue(collector, err.value, ctx);
            collectHintsFromValue(collector, err.scope, ctx);
        },
        .funcall => |funcall| {
            // This is where we add parameter name hints!
            const sig = Types.getTypeOfValue(funcall.func, ctx, collector.allocator) catch return;

            if (sig == .decided and sig.decided.base == .function) {
                const sigfunc = sig.decided.base.function;
                if (ctx.functionExist(sigfunc.fname)) {
                    const func_def = ctx.getFunction(sigfunc.fname);

                    // For each argument, add a hint with the parameter name
                    for (funcall.args.items, 0..) |arg, i| {
                        if (i >= func_def.arguments.items.len) break;

                        const param = func_def.arguments.items[i];
                        const arg_ref = arg.getReference();

                        // Add hint at the start of the argument
                        collector.addParameterHint(
                            .{ .line = arg_ref.range.start.line, .character = arg_ref.range.start.character },
                            param.name,
                        ) catch return;

                        // Recursively process the argument
                        collectHintsFromValue(collector, arg, ctx);
                    }
                }
            }

            collectHintsFromValue(collector, funcall.func, ctx);
        },
        .function => |func| {
            collectHintsFromScope(collector, func.code);
        },
        .parenthesis => |p| {
            collectHintsFromValue(collector, p, ctx);
        },
        .Print => |print| {
            for (print.args.items) |arg| {
                collectHintsFromValue(collector, arg, ctx);
            }
        },
        .scope => |s| {
            collectHintsFromScope(collector, s);
        },
        .If => |ifs| {
            for (ifs.conditions.items) |cond| {
                collectHintsFromValue(collector, cond, ctx);
            }
            for (ifs.scopes.items) |scope| {
                collectHintsFromValue(collector, scope, ctx);
            }
            if (ifs.elsescope) |elsescope| {
                collectHintsFromValue(collector, elsescope, ctx);
            }
        },
        .structInit => |stc| {
            var it = stc.habitants.iterator();
            while (it.next()) |hab| {
                collectHintsFromValue(collector, hab.value_ptr.*, ctx);
            }
        },
        .unaryOperatorRight => |uopr| {
            collectHintsFromValue(collector, uopr.expr, ctx);
        },
        .While => |wl| {
            collectHintsFromValue(collector, wl.condition, ctx);
            collectHintsFromValue(collector, wl.exec, ctx);
        },
        .varDec => |vardec| {
            // Optionally add type hints for variable declarations without explicit types
            if (ctx.variableExist(vardec.name)) {
                const inferred_type = ctx.getVariable(vardec.name);
                const type_str = inferred_type.decided.toString(collector.allocator);
                const var_end = position.Position{
                    .line = vardec.reference.range.end.line,
                    .character = vardec.reference.range.end.character,
                };
                collector.addTypeHint(var_end, type_str) catch return;
            }
        },
        // Literals don't need hints
        .boolLit, .intLit, .stringLit, .nullLit, .charLit, .identifier => {},
        else => {},
    }
}

fn collectHintsFromScope(collector: *InlayHintCollector, scope: *ast.Scope) void {
    for (scope.code.items) |inst| {
        collectHintsFromValue(collector, inst, scope.ctx);
    }
}

fn collectHintsFromFuncdef(collector: *InlayHintCollector, funcDef: *ast.funcDef, ctx: *analyser.Context) void {
    _ = ctx; // Parent context not needed - function has its own scope context
    collectHintsFromScope(collector, funcDef.code);
}

fn collectHintsFromProg(collector: *InlayHintCollector, prog: *ast.Program, ctx: *analyser.Context) void {
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |fd| {
                collectHintsFromFuncdef(collector, fd, ctx);
            },
            .StructDef => {},
        }
    }
}

pub fn getInlayHints(
    range: protocol.Range,
    allocator: std.mem.Allocator,
    prog: *ast.Program,
    ctx: *analyser.Context,
) ![]protocol.InlayHint {
    var collector = InlayHintCollector.init(allocator, range);

    collectHintsFromProg(&collector, prog, ctx);

    return collector.hints.toOwnedSlice();
}
