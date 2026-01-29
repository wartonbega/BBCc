const std = @import("std");
const protocol = @import("protocol.zig");
const document = @import("document.zig");

const context_hinter = @import("../bbc/context_hinter.zig");
const ast = @import("../bbc/ast.zig");

const Allocator = std.mem.Allocator;

pub const Handlers = struct {
    store: *document.DocumentStore,
    allocator: Allocator,
    initialized: bool = false,

    pub fn init(allocator: Allocator, store: *document.DocumentStore) Handlers {
        return .{
            .store = store,
            .allocator = allocator,
        };
    }

    /// Handle initialize request
    pub fn handleInitialize(
        self: *Handlers,
        params: protocol.InitializeParams,
        arena: Allocator,
    ) !protocol.InitializeResult {
        _ = params;

        self.initialized = true;

        // Create trigger characters as a slice
        const trigger_chars = try arena.alloc([]const u8, 2);
        trigger_chars[0] = ".";
        trigger_chars[1] = "@";

        return .{
            .capabilities = .{
                .textDocumentSync = .Full,
                .hoverProvider = true,
                .completionProvider = .{
                    .triggerCharacters = trigger_chars,
                },
                .definitionProvider = true,
            },
            .serverInfo = .{
                .name = "bbc-lsp",
                .version = "0.1.0",
            },
        };
    }

    pub fn handleDidOpen(self: *Handlers, params: protocol.DidOpenTextDocumentParams) !void {
        std.log.info("Received didOpen notification", .{});
        std.log.info("Opening file: {s}", .{params.textDocument.uri});
        std.log.info("File text length: {d}", .{params.textDocument.text.len});

        const doc = self.store.open(
            params.textDocument.uri,
            params.textDocument.text,
            params.textDocument.version,
        ) catch |err| {
            std.log.err("Failed to open document: {}", .{err});
            return err;
        };

        std.log.info("Document opened successfully, URI: {s}", .{doc.uri});
        std.log.info("Documents in store: {d}", .{self.store.documents.count()});
    }

    /// Handle textDocument/didChange notification
    pub fn handleDidChange(self: *Handlers, params: protocol.DidChangeTextDocumentParams) !void {
        if (params.contentChanges.len == 0) return;
        std.log.info("Document reloaded", .{});

        // We use Full sync, so just take the last change
        const change = params.contentChanges[params.contentChanges.len - 1];

        try self.store.update(
            params.textDocument.uri,
            change.text,
            params.textDocument.version,
        );
    }

    /// Handle textDocument/didClose notification
    pub fn handleDidClose(
        self: *Handlers,
        params: protocol.DidCloseTextDocumentParams,
    ) void {
        self.store.close(params.textDocument.uri);
    }

    /// Handle textDocument/hover request
    pub fn handleHover(self: *Handlers, params: protocol.HoverParams, arena: Allocator) !?protocol.Hover {
        std.log.info("Looking for document: {s}", .{params.textDocument.uri});

        const doc = self.store.get(params.textDocument.uri) orelse {
            std.log.info("Document not found in store", .{});
            return null;
        };

        std.log.info("Document found", .{});

        if (doc.program) |prog| {
            if (doc.ctx) |ctx| {
                const infos = context_hinter.getInfos(params.position, arena, prog, ctx);

                return protocol.Hover{
                    .contents = .{
                        .kind = .markdown,
                        .value = switch (infos.ttype) {
                            .variable => |v| try std.fmt.allocPrint(arena, "{s}: {s}", .{ v.name, v._type.toString(arena) }),
                            .func => |f| try std.fmt.allocPrint(arena, "{s}: {s}", .{ f.name, f.sig.toString(arena) }),
                            .typeName => |tn| tn.toString(arena),
                            .literal => |lit| lit,
                            .noInfo => "",
                        },
                    },
                };
            }
        }

        // const hover_text = try std.fmt.allocPrint(
        //     arena,
        //     "BBC Language Document\nVersion: {d}",
        //     .{doc.version},
        // );

        return protocol.Hover{
            .contents = .{
                .kind = .markdown,
                .value = "",
            },
        };
    }

    /// Handle textDocument/definition request
    pub fn handleDefinition(
        self: *Handlers,
        params: protocol.DefinitionParams,
        arena: Allocator,
    ) !?protocol.Location {
        _ = arena;

        std.log.info("Looking for definition at {s}:{}:{}", .{
            params.textDocument.uri,
            params.position.line,
            params.position.character,
        });

        const doc = self.store.get(params.textDocument.uri) orelse {
            std.log.info("Document not found", .{});
            return null;
        };

        if (doc.program) |prog| {
            if (doc.ctx) |ctx|
                return context_hinter.getDefinition(params.position, self.allocator, prog, ctx);
        }
        return null;
    }

    /// Get diagnostics for a document (to be published)
    pub fn getDiagnostics(
        self: *Handlers,
        uri: []const u8,
    ) ?[]const protocol.Diagnostic {
        const doc = self.store.get(uri) orelse return null;
        return doc.diagnostics.items;
    }

    /// Handle textDocument/completion request
    pub fn handleCompletion(
        self: *Handlers,
        params: protocol.CompletionParams,
        arena: Allocator,
    ) !?protocol.CompletionList {
        std.log.info("Completion requested at {}:{}", .{
            params.position.line,
            params.position.character,
        });

        const doc = self.store.get(params.textDocument.uri) orelse {
            std.log.info("Document not found", .{});
            return null;
        };

        var items = std.ArrayList(protocol.CompletionItem).init(arena);

        // Add BBC language keywords
        const keywords = [_][]const u8{
            "func",    "let",  "if",   "elif",  "else", "while",  "for",
            "struct",  "enum", "true", "false", "null", "return", "print",
            "println", "free",
        };

        for (keywords) |keyword| {
            try items.append(.{
                .label = keyword,
                .kind = .Keyword,
                .detail = "BBC keyword",
            });
        }

        // Add built-in types
        const types = [_][]const u8{ "Int", "Bool", "Char", "Void" };
        for (types) |typename| {
            try items.append(.{
                .label = typename,
                .kind = .Class,
                .detail = "Built-in type",
            });
        }

        // If we have a parsed program, add functions and structs
        if (doc.program) |prog| {
            for (prog.instructions.items) |inst| {
                switch (inst.*) {
                    .FuncDef => |func| {
                        try items.append(.{
                            .label = func.name,
                            .kind = .Function,
                            .detail = "Function",
                        });
                    },
                    .StructDef => |structdef| {
                        try items.append(.{
                            .label = structdef.name,
                            .kind = .Struct,
                            .detail = "Struct",
                        });

                        // If triggered by '.', add struct fields
                        if (params.context) |ctx| {
                            if (ctx.triggerKind == .TriggerCharacter and
                                ctx.triggerCharacter != null and
                                std.mem.eql(u8, ctx.triggerCharacter.?, "."))
                            {
                                for (structdef.fields.items) |field_name| {
                                    const field_type = structdef.habitants.get(field_name);
                                    const detail = if (field_type) |t|
                                        try std.fmt.allocPrint(arena, "Field: {s}", .{t.toString(arena)})
                                    else
                                        "Field";

                                    try items.append(.{
                                        .label = field_name,
                                        .kind = .Field,
                                        .detail = detail,
                                    });
                                }
                            }
                        }
                    },
                }
            }
        }

        std.log.info("Returning {} completion items", .{items.items.len});

        return protocol.CompletionList{
            .isIncomplete = false,
            .items = items.items,
        };
    }
};
