const std = @import("std");
const parser = @import("../bbc/parser.zig");
const lexer = @import("../bbc/lexer.zig");
const imports = @import("../bbc/imports.zig");
const ast = @import("../bbc/ast.zig");
const analyser = @import("../bbc/analyser.zig");
const protocol = @import("protocol.zig");

const Allocator = std.mem.Allocator;

/// A direct import statement found in the source before resolution.
pub const ImportDecl = struct {
    path: []const u8,           // path as written in source
    target_uri: []const u8,     // resolved file:// URI of the imported file
    libname: ?[]const u8,       // alias after "as", or null
    reference: parser.Location, // position of the import(...) statement in this file
};

pub const Document = struct {
    uri: []const u8,
    text: []const u8,
    version: i32,

    // Parsed data
    tokens: ?std.ArrayList(parser.Token),
    program: ?*ast.Program,
    ctx: ?*analyser.Context,
    diagnostics: std.ArrayList(protocol.Diagnostic),
    /// Direct imports found in this file before resolution (arena-allocated).
    import_decls: []ImportDecl,

    arena: std.heap.ArenaAllocator,
    allocator: Allocator,

    pub fn init(allocator: Allocator, uri: []const u8, text: []const u8, version: i32) !*Document {
        const doc = try allocator.create(Document);
        doc.* = .{
            .uri = try allocator.dupe(u8, uri),
            .text = try allocator.dupe(u8, text),
            .version = version,
            .tokens = null,
            .program = null,
            .diagnostics = std.ArrayList(protocol.Diagnostic).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
            .ctx = null,
            .import_decls = &[_]ImportDecl{},
        };
        return doc;
    }

    pub fn deinit(self: *Document) void {
        self.allocator.free(self.uri);
        self.allocator.free(self.text);
        self.diagnostics.deinit();
        self.arena.deinit();
        self.allocator.destroy(self);
    }

    pub fn update(self: *Document, new_text: []const u8, new_version: i32) !void {
        self.allocator.free(self.text);
        self.text = try self.allocator.dupe(u8, new_text);
        self.version = new_version;

        // Clear old parsed data (arena holds tokens/program/ctx/import_decls)
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.allocator);
        self.tokens = null;
        self.program = null;
        self.ctx = null;
        self.import_decls = &[_]ImportDecl{};
        self.diagnostics.clearRetainingCapacity();
    }

    /// Re-parse and re-analyse from the current text without changing the text.
    /// Used to refresh analysis when an imported file changes.
    pub fn reanalyze(self: *Document) !void {
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.allocator);
        self.tokens = null;
        self.program = null;
        self.ctx = null;
        self.import_decls = &[_]ImportDecl{};
        self.diagnostics.clearRetainingCapacity();
        try self.analyze();
    }

    /// Parse and analyze the document
    pub fn analyze(self: *Document) !void {
        self.diagnostics.clearRetainingCapacity();

        var reader = parser.Reader{
            .content = self.text,
            .pos = 0,
            .filename = self.uri,
            .lastPos = .{ .character = 0, .line = 0 },
            .allocator = self.arena.allocator(),
        };

        // Step 1: Tokenize from memory text
        const tokens = parser.parseFromText(&self.arena, &reader) catch |err| {
            try self.diagnostics.append(.{
                .range = .{
                    .start = reader.lastPos,
                    .end = reader.getCurrentPos(),
                },
                .severity = .Error,
                .source = "bbc-lexer",
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "Lexer error: {}",
                    .{err},
                ),
            });
            return;
        };
        self.tokens = tokens;

        var tokReader = lexer.tokenReader.new(tokens, self.uri);
        // Step 2: Parse into AST
        const prog = lexer.lexeProgramWithReader(
            &tokReader,
            self.arena.allocator(),
        ) catch {
            try self.diagnostics.append(.{
                .range = tokReader.errorLocation.range,
                .severity = .Error,
                .source = "bbc-parser",
                .message = try std.fmt.allocPrint(
                    self.allocator,
                    "Parser error: {s}",
                    .{tokReader.errorMessage},
                ),
            });
            return;
        };
        self.program = prog;

        // Step 2b: Collect import declarations before they are inlined/removed.
        {
            const file_path_base = if (std.mem.startsWith(u8, self.uri, "file://"))
                self.uri[7..]
            else
                self.uri;
            const abs_source = std.fs.path.resolve(self.arena.allocator(), &.{file_path_base}) catch file_path_base;
            const base_dir = std.fs.path.dirname(abs_source) orelse ".";
            var decls = std.ArrayList(ImportDecl).init(self.arena.allocator());
            for (prog.instructions.items) |inst| {
                if (inst.* == .ImportDef) {
                    const imp = inst.*.ImportDef;
                    const abs_path = std.fs.path.resolve(self.arena.allocator(), &.{ base_dir, imp.path }) catch continue;
                    const target_uri = std.mem.concat(self.arena.allocator(), u8, &.{ "file://", abs_path }) catch continue;
                    decls.append(.{
                        .path = imp.path,
                        .target_uri = target_uri,
                        .libname = imp.libname,
                        .reference = imp.reference,
                    }) catch continue;
                }
            }
            self.import_decls = decls.items;
        }

        // Step 2c: Resolve imports (file:// URI → path)
        const file_path = if (std.mem.startsWith(u8, self.uri, "file://"))
            self.uri[7..]
        else
            self.uri;
        imports.resolveAllImports(prog, file_path, &self.arena) catch |err| {
            if (err == imports.ImportError.CyclicImport or err == imports.ImportError.FileNotFound) {
                // bbcError already printed; continue with what we have
            } else return err;
        };

        // Step 3: Analyze
        const ctx = try analyser.Context.init(self.arena.allocator());
        self.ctx = ctx;
        analyser.analyse(prog, ctx, self.arena.allocator()) catch {
            for (ctx.error_locations.items, ctx.error_messages.items) |loc, message| {
                try self.diagnostics.append(.{
                    .range = loc.range,
                    .severity = .Error,
                    .source = "bbc-analyzer",
                    .message = try std.fmt.allocPrint(
                        self.allocator,
                        "Analyzer error: {s}",
                        .{message},
                    ),
                });
            }
            return;
        };
    }
};

pub const DocumentStore = struct {
    documents: std.StringHashMap(*Document),
    allocator: Allocator,

    pub fn init(allocator: Allocator) DocumentStore {
        var documents = std.StringHashMap(*Document).init(allocator);
        // Ensure capacity so get() doesn't crash on empty map
        documents.ensureTotalCapacity(4) catch {};

        return .{
            .documents = documents,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *DocumentStore) void {
        var it = self.documents.valueIterator();
        while (it.next()) |doc| {
            doc.*.deinit();
        }
        self.documents.deinit();
    }

    pub fn open(self: *DocumentStore, uri: []const u8, text: []const u8, version: i32) !*Document {
        const doc = try Document.init(self.allocator, uri, text, version);
        try self.documents.put(doc.uri, doc);
        try doc.analyze();
        return doc;
    }

    pub fn get(self: *DocumentStore, uri: []const u8) ?*Document {
        // Use iterator instead of .get() to avoid capacity=0 crash
        var it = self.documents.iterator();
        while (it.next()) |entry| {
            if (std.mem.eql(u8, entry.key_ptr.*, uri)) {
                return entry.value_ptr.*;
            }
        }
        return null;
    }

    pub fn close(self: *DocumentStore, uri: []const u8) void {
        if (self.documents.fetchRemove(uri)) |kv| {
            kv.value.deinit();
        }
    }

    pub fn update(self: *DocumentStore, uri: []const u8, text: []const u8, version: i32) !void {
        if (self.get(uri)) |doc| {
            try doc.update(text, version);
            try doc.analyze();
        }

        // Re-analyse any open document that imports the file we just changed.
        var it = self.documents.valueIterator();
        while (it.next()) |doc_ptr| {
            const other = doc_ptr.*;
            if (std.mem.eql(u8, other.uri, uri)) continue;
            for (other.import_decls) |decl| {
                if (std.mem.eql(u8, decl.target_uri, uri)) {
                    other.reanalyze() catch {};
                    break;
                }
            }
        }
    }

    /// Returns a slice of all currently-open document URIs (arena-allocated).
    pub fn allUris(self: *DocumentStore, arena: Allocator) ![][]const u8 {
        var list = std.ArrayList([]const u8).init(arena);
        var it = self.documents.valueIterator();
        while (it.next()) |doc_ptr| {
            try list.append(doc_ptr.*.uri);
        }
        return list.toOwnedSlice();
    }
};
