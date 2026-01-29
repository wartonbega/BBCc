const std = @import("std");
const parser = @import("../bbc/parser.zig");
const lexer = @import("../bbc/lexer.zig");
const ast = @import("../bbc/ast.zig");
const analyser = @import("../bbc/analyser.zig");
const protocol = @import("protocol.zig");

const Allocator = std.mem.Allocator;

pub const Document = struct {
    uri: []const u8,
    text: []const u8,
    version: i32,

    // Parsed data
    tokens: ?std.ArrayList(parser.Token),
    program: ?*ast.Program,
    ctx: ?*analyser.Context,
    diagnostics: std.ArrayList(protocol.Diagnostic),

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

        // Clear old parsed data
        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.allocator);
        self.tokens = null;
        self.program = null;
        self.diagnostics.clearRetainingCapacity();
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
    }
};
