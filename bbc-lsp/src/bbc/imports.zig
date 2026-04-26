const std = @import("std");
const parser = @import("parser.zig");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const errors = @import("errors.zig");

const Allocator = std.mem.Allocator;

pub const ImportError = error{ CyclicImport, FileNotFound };

fn prefixName(allocator: Allocator, libname: []const u8, name: []const u8) ![]const u8 {
    return std.mem.concat(allocator, u8, &.{ libname, ".", name });
}

// Recursively update a Type node: any .name or .generic.name that is in localNames
// gets prefixed with libname.
fn prefixTypeRef(
    t: *ast.Type,
    libname: []const u8,
    localNames: *const std.StringHashMap(void),
    allocator: Allocator,
) !void {
    switch (t.base) {
        .name => |name| {
            if (localNames.contains(name)) {
                t.base = .{ .name = try prefixName(allocator, libname, name) };
            }
        },
        .generic => |*g| {
            if (localNames.contains(g.name)) {
                g.name = try prefixName(allocator, libname, g.name);
            }
            for (g.params.items) |param| {
                try prefixTypeRef(param, libname, localNames, allocator);
            }
        },
        .buffer => |elem| {
            try prefixTypeRef(elem, libname, localNames, allocator);
        },
        .function => |*f| {
            try prefixTypeRef(f.retype, libname, localNames, allocator);
            for (f.argtypes.items) |argtype| {
                try prefixTypeRef(argtype, libname, localNames, allocator);
            }
        },
        .import_ns => {}, // namespace types are global, no prefixing needed
    }
}

// Walk a Value node and:
//  - update StructInit.name if in localTypeNames
//  - update identifier names if they are unbound local-function references
// boundNames tracks parameter and let-bound names in scope to avoid false renames.
fn prefixValueNode(
    val: *ast.Value,
    libname: []const u8,
    localTypeNames: *const std.StringHashMap(void),
    localFuncNames: *const std.StringHashMap(void),
    boundNames: *std.StringHashMap(void),
    allocator: Allocator,
) anyerror!void {
    switch (val.*) {
        .structInit => |stc| {
            if (localTypeNames.contains(stc.name)) {
                stc.name = try prefixName(allocator, libname, stc.name);
            }
            var it = stc.habitants.iterator();
            while (it.next()) |hab| {
                try prefixValueNode(hab.value_ptr.*, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
        },
        .identifier => |*ident| {
            if (!boundNames.contains(ident.name) and localFuncNames.contains(ident.name)) {
                ident.name = try prefixName(allocator, libname, ident.name);
            }
        },
        .binaryOperator => |binop| {
            try prefixValueNode(binop.lhs, libname, localTypeNames, localFuncNames, boundNames, allocator);
            try prefixValueNode(binop.rhs, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .funcall => |funcall| {
            try prefixValueNode(funcall.func, libname, localTypeNames, localFuncNames, boundNames, allocator);
            for (funcall.args.items) |arg| {
                try prefixValueNode(arg, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
        },
        .assignement => |assign| {
            try prefixValueNode(assign.lhs, libname, localTypeNames, localFuncNames, boundNames, allocator);
            try prefixValueNode(assign.rhs, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .varDec => |decl| {
            // Add bound variable so later uses in the same scope aren't renamed
            try boundNames.put(decl.name, {});
        },
        .If => |ifstmt| {
            for (ifstmt.conditions.items) |cond| {
                try prefixValueNode(cond, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
            for (ifstmt.scopes.items) |scope_val| {
                try prefixValueNode(scope_val, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
            if (ifstmt.elsescope) |els| {
                try prefixValueNode(els, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
        },
        .While => |wl| {
            try prefixValueNode(wl.condition, libname, localTypeNames, localFuncNames, boundNames, allocator);
            try prefixValueNode(wl.exec, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .For => |fl| {
            try prefixValueNode(fl.iterable, libname, localTypeNames, localFuncNames, boundNames, allocator);
            // fl.var_name is bound inside the loop body
            var loopBound = try boundNames.clone();
            defer loopBound.deinit();
            try loopBound.put(fl.var_name, {});
            try prefixValueNode(fl.exec, libname, localTypeNames, localFuncNames, &loopBound, allocator);
        },
        .scope => |scope| {
            try prefixScope(scope, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .parenthesis => |par| {
            try prefixValueNode(par, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .unaryOperatorRight => |uop| {
            try prefixValueNode(uop.expr, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .notOp => |notop| {
            try prefixValueNode(notop.expr, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .errorCheck => |errcheck| {
            try prefixValueNode(errcheck.value, libname, localTypeNames, localFuncNames, boundNames, allocator);
            try prefixValueNode(errcheck.scope, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .function => |func| {
            // Inline anonymous function: treat params as bound names
            var innerBound = std.StringHashMap(void).init(allocator);
            defer innerBound.deinit();
            for (func.arguments.items) |arg| {
                try innerBound.put(arg.name, {});
                try prefixTypeRef(arg._type, libname, localTypeNames, allocator);
            }
            try prefixTypeRef(func.return_type, libname, localTypeNames, allocator);
            try prefixScope(func.code, libname, localTypeNames, localFuncNames, &innerBound, allocator);
        },
        .freeKeyword => |fk| {
            try prefixValueNode(fk.val, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .Print => |p| {
            for (p.args.items) |arg| {
                try prefixValueNode(arg, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
        },
        .bufferAlloc => |ba| {
            try prefixTypeRef(ba.elem_type, libname, localTypeNames, allocator);
            try prefixValueNode(ba.size, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        .bufferLit => |bl| {
            for (bl.elements.items) |elem| {
                try prefixValueNode(elem, libname, localTypeNames, localFuncNames, boundNames, allocator);
            }
        },
        .bufferIndex => |bi| {
            try prefixValueNode(bi.buffer, libname, localTypeNames, localFuncNames, boundNames, allocator);
            try prefixValueNode(bi.index, libname, localTypeNames, localFuncNames, boundNames, allocator);
        },
        // Leaf nodes with no sub-values to walk
        .intLit, .floatLit, .boolLit, .stringLit, .charLit, .nullLit, .NULL => {},
    }
}

fn prefixScope(
    scope: *ast.Scope,
    libname: []const u8,
    localTypeNames: *const std.StringHashMap(void),
    localFuncNames: *const std.StringHashMap(void),
    boundNames: *std.StringHashMap(void),
    allocator: Allocator,
) anyerror!void {
    for (scope.code.items) |val| {
        try prefixValueNode(val, libname, localTypeNames, localFuncNames, boundNames, allocator);
    }
}

fn applyNamespace(prog: *ast.Program, libname: []const u8, allocator: Allocator) !void {
    // Pass 1: collect all locally-defined type and function names (before renaming).
    var localTypeNames = std.StringHashMap(void).init(allocator);
    var localFuncNames = std.StringHashMap(void).init(allocator);

    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .StructDef => |stct| try localTypeNames.put(stct.name, {}),
            .TraitDef => |trait| try localTypeNames.put(trait.name, {}),
            .FuncDef => |func| try localFuncNames.put(func.name, {}),
            .TraitImpl, .ImportDef => {},
        }
    }

    // Pass 2: update all internal references, then rename the definitions themselves.
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |func| {
                // Update return type and argument types
                try prefixTypeRef(func.return_type, libname, &localTypeNames, allocator);
                for (func.arguments.items) |arg| {
                    try prefixTypeRef(arg._type, libname, &localTypeNames, allocator);
                }
                // Walk function body; params are bound and must not be renamed
                var bound = std.StringHashMap(void).init(allocator);
                defer bound.deinit();
                for (func.arguments.items) |arg| {
                    try bound.put(arg.name, {});
                }
                try prefixScope(func.code, libname, &localTypeNames, &localFuncNames, &bound, allocator);
                // Rename the function itself last
                func.name = try prefixName(allocator, libname, func.name);
            },
            .StructDef => |stct| {
                // Update field types
                var it = stct.habitants.iterator();
                while (it.next()) |hab| {
                    try prefixTypeRef(hab.value_ptr.*, libname, &localTypeNames, allocator);
                }
                stct.name = try prefixName(allocator, libname, stct.name);
            },
            .TraitDef => |trait| {
                var it = trait.methods.iterator();
                while (it.next()) |meth| {
                    try prefixTypeRef(meth.value_ptr.*.return_type, libname, &localTypeNames, allocator);
                    for (meth.value_ptr.*.arguments.items) |arg| {
                        try prefixTypeRef(arg._type, libname, &localTypeNames, allocator);
                    }
                }
                trait.name = try prefixName(allocator, libname, trait.name);
            },
            .TraitImpl => |impl| {
                for (impl.methods.items) |method| {
                    try prefixTypeRef(method.return_type, libname, &localTypeNames, allocator);
                    for (method.arguments.items) |arg| {
                        try prefixTypeRef(arg._type, libname, &localTypeNames, allocator);
                    }
                    if (method.parent) |parent| {
                        try prefixTypeRef(parent, libname, &localTypeNames, allocator);
                    }
                    var bound = std.StringHashMap(void).init(allocator);
                    defer bound.deinit();
                    for (method.arguments.items) |arg| {
                        try bound.put(arg.name, {});
                    }
                    try prefixScope(method.code, libname, &localTypeNames, &localFuncNames, &bound, allocator);
                }
                impl.trait_name = try prefixName(allocator, libname, impl.trait_name);
                impl.type_name = try prefixName(allocator, libname, impl.type_name);
            },
            .ImportDef => {},
        }
    }
}

fn resolveImportsInternal(
    prog: *ast.Program,
    base_dir: []const u8,
    arena: *std.heap.ArenaAllocator,
    visiting: *std.StringHashMap(void),
) !void {
    const allocator = arena.allocator();
    var i: usize = 0;
    while (i < prog.instructions.items.len) {
        const inst = prog.instructions.items[i];
        switch (inst.*) {
            .ImportDef => |imp| {
                const abs_path = try std.fs.path.resolve(allocator, &.{ base_dir, imp.path });

                if (visiting.contains(abs_path)) {
                    errors.bbcError("Cyclic import detected: '{s}'", .{abs_path}, imp.reference);
                    return ImportError.CyclicImport;
                }

                try visiting.put(abs_path, {});

                const tokens = parser.parse(abs_path, arena) catch {
                    errors.bbcError("Cannot open import '{s}'", .{abs_path}, imp.reference);
                    return ImportError.FileNotFound;
                };
                const imported_prog = try lexer.lexeProgram(tokens, allocator, abs_path);

                const imported_dir = std.fs.path.dirname(abs_path) orelse ".";
                try resolveImportsInternal(imported_prog, imported_dir, arena, visiting);

                if (imp.libname) |libname| {
                    try applyNamespace(imported_prog, libname, allocator);
                }

                // Replace import node with the imported definitions (inline)
                _ = prog.instructions.orderedRemove(i);
                for (imported_prog.instructions.items, 0..) |imported_inst, j| {
                    try prog.instructions.insert(i + j, imported_inst);
                }

                _ = visiting.remove(abs_path);
                // Don't advance i — re-examine the slot (now holds the first imported node)
                continue;
            },
            else => {},
        }
        i += 1;
    }
}

/// Resolve all imports in `prog`, inlining their definitions in place.
/// `source_file` is the path of the file that contains `prog` (used to anchor relative paths).
/// The same `arena` used to parse `prog` is reused for imported files.
pub fn resolveAllImports(
    prog: *ast.Program,
    source_file: []const u8,
    arena: *std.heap.ArenaAllocator,
) !void {
    const allocator = arena.allocator();
    const abs_source = try std.fs.path.resolve(allocator, &.{source_file});
    const base_dir = std.fs.path.dirname(abs_source) orelse ".";

    var visiting = std.StringHashMap(void).init(allocator);
    defer visiting.deinit();
    try visiting.put(abs_source, {}); // prevent self-import

    try resolveImportsInternal(prog, base_dir, arena, &visiting);
}
