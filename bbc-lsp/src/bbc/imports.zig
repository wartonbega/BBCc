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

fn applyNamespace(prog: *ast.Program, libname: []const u8, allocator: Allocator) !void {
    for (prog.instructions.items) |inst| {
        switch (inst.*) {
            .FuncDef => |func| {
                func.name = try prefixName(allocator, libname, func.name);
            },
            .StructDef => |stct| {
                stct.name = try prefixName(allocator, libname, stct.name);
            },
            .TraitDef => |trait| {
                trait.name = try prefixName(allocator, libname, trait.name);
            },
            .TraitImpl => |impl| {
                impl.trait_name = try prefixName(allocator, libname, impl.trait_name);
                impl.type_name = try prefixName(allocator, libname, impl.type_name);
                for (impl.methods.items) |method| {
                    if (method.parent) |parent| {
                        parent.base.name = try prefixName(allocator, libname, parent.base.name);
                    }
                }
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
