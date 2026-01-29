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
