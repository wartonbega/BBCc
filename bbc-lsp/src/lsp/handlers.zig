const std = @import("std");
const protocol = @import("protocol.zig");
const document = @import("document.zig");

const context_hinter = @import("../bbc/context_hinter.zig");
const ast = @import("../bbc/ast.zig");

const Allocator = std.mem.Allocator;

fn isIdentChar(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or
        (c >= '0' and c <= '9') or c == '_';
}

/// Extracts the identifier immediately before the `.` trigger at (line, char).
/// Returns null if no identifier precedes the dot.
fn getNamespacePrefix(text: []const u8, line: u32, char: u32) ?[]const u8 {
    if (char == 0) return null;
    var line_iter = std.mem.splitScalar(u8, text, '\n');
    var cur: u32 = 0;
    while (line_iter.next()) |line_text| {
        defer cur += 1;
        if (cur != line) continue;
        const dot_idx = char - 1;
        if (dot_idx >= line_text.len or line_text[dot_idx] != '.') return null;
        var start: usize = dot_idx;
        while (start > 0 and isIdentChar(line_text[start - 1])) start -= 1;
        if (start == dot_idx) return null;
        return line_text[start..dot_idx];
    }
    return null;
}

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

    pub fn handleInitialize(
        self: *Handlers,
        params: protocol.InitializeParams,
        arena: Allocator,
    ) !protocol.InitializeResult {
        _ = params;

        self.initialized = true;

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
                .inlayHintProvider = true, // ← Add this
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

    /// Handle textDocument/inlayHint request
    pub fn handleInlayHint(
        self: *Handlers,
        params: protocol.InlayHintParams,
        arena: Allocator,
    ) ![]protocol.InlayHint {
        std.log.info("Inlay hints requested for range {}:{} to {}:{}", .{
            params.range.start.line,
            params.range.start.character,
            params.range.end.line,
            params.range.end.character,
        });

        const doc = self.store.get(params.textDocument.uri) orelse {
            std.log.info("Document not found", .{});
            return &[_]protocol.InlayHint{};
        };

        const prog = doc.program orelse return &[_]protocol.InlayHint{};
        const ctx = doc.ctx orelse return &[_]protocol.InlayHint{};

        const hints = try context_hinter.getInlayHints(
            params.range,
            arena,
            prog,
            ctx,
            doc.uri,
        );

        std.log.info("Returning {} inlay hints", .{hints.len});
        return hints;
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

        // Hovering over an import statement → show the resolved path.
        for (doc.import_decls) |decl| {
            if (decl.reference.contains(params.position)) {
                const hover_text = if (decl.libname) |ln|
                    try std.fmt.allocPrint(arena, "import `{s}` as `{s}`\n\n→ `{s}`", .{ decl.path, ln, decl.target_uri })
                else
                    try std.fmt.allocPrint(arena, "import `{s}`\n\n→ `{s}`", .{ decl.path, decl.target_uri });
                return protocol.Hover{
                    .contents = .{ .kind = .markdown, .value = hover_text },
                };
            }
        }

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

        // Clicking on an import statement → jump to the top of the imported file.
        for (doc.import_decls) |decl| {
            if (decl.reference.contains(params.position)) {
                std.log.info("Definition is import: {s}", .{decl.target_uri});
                return protocol.Location{
                    .uri = decl.target_uri,
                    .range = .{
                        .start = .{ .line = 0, .character = 0 },
                        .end = .{ .line = 0, .character = 0 },
                    },
                };
            }
        }

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
            "func",    "let",  "if",    "elif",  "else",  "while",  "for",
            "struct",  "enum", "true",  "false", "null",  "return", "print",
            "println", "free", "input", "read",  "error", "zeroOf", "unitOf",
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

        // When '.' is the trigger, check for namespace prefix (e.g. "import_file.")
        if (params.context) |ctx_params| {
            if (ctx_params.triggerKind == .TriggerCharacter and
                ctx_params.triggerCharacter != null and
                std.mem.eql(u8, ctx_params.triggerCharacter.?, "."))
            {
                if (getNamespacePrefix(doc.text, params.position.line, params.position.character)) |ns_prefix| {
                    const prefix_dot = try std.fmt.allocPrint(arena, "{s}.", .{ns_prefix});
                    var ns_items = std.ArrayList(protocol.CompletionItem).init(arena);

                    // Functions from the analysed context
                    if (doc.ctx) |analysis_ctx| {
                        var it = analysis_ctx.functions.iterator();
                        while (it.next()) |entry| {
                            const fname = entry.key_ptr.*;
                            if (!std.mem.startsWith(u8, fname, prefix_dot)) continue;
                            const suffix = fname[prefix_dot.len..];
                            // Skip method names (ns.Type.method — contain another dot)
                            if (std.mem.indexOfScalar(u8, suffix, '.') != null) continue;
                            try ns_items.append(.{
                                .label = suffix,
                                .kind = .Function,
                                .detail = "Function",
                            });
                        }
                    }

                    // Structs and traits from the program
                    if (doc.program) |prog| {
                        for (prog.instructions.items) |inst| {
                            switch (inst.*) {
                                .StructDef => |sd| {
                                    if (!std.mem.startsWith(u8, sd.name, prefix_dot)) continue;
                                    const suffix = sd.name[prefix_dot.len..];
                                    if (std.mem.indexOfScalar(u8, suffix, '.') != null) continue;
                                    try ns_items.append(.{
                                        .label = suffix,
                                        .kind = .Struct,
                                        .detail = "Struct",
                                    });
                                },
                                .TraitDef => |td| {
                                    if (!std.mem.startsWith(u8, td.name, prefix_dot)) continue;
                                    const suffix = td.name[prefix_dot.len..];
                                    if (std.mem.indexOfScalar(u8, suffix, '.') != null) continue;
                                    try ns_items.append(.{
                                        .label = suffix,
                                        .kind = .Interface,
                                        .detail = "Trait",
                                    });
                                },
                                else => {},
                            }
                        }
                    }

                    if (ns_items.items.len > 0) {
                        std.log.info("Returning {} namespace completion items for '{s}'", .{ ns_items.items.len, ns_prefix });
                        return protocol.CompletionList{
                            .isIncomplete = false,
                            .items = ns_items.items,
                        };
                    }
                }
            }
        }

        // If we have a parsed program, add functions and structs
        if (doc.program) |prog| {
            for (prog.instructions.items) |inst| {
                switch (inst.*) {
                    .FuncDef => |func| {
                        // Skip namespaced names (imported with alias) in the flat list
                        if (std.mem.indexOfScalar(u8, func.name, '.') != null) continue;
                        try items.append(.{
                            .label = func.name,
                            .kind = .Function,
                            .detail = "Function",
                        });
                    },
                    .StructDef => |structdef| {
                        // Skip namespaced names
                        if (std.mem.indexOfScalar(u8, structdef.name, '.') != null) continue;
                        try items.append(.{
                            .label = structdef.name,
                            .kind = .Struct,
                            .detail = "Struct",
                        });

                        // If triggered by '.', add struct fields (for variable.field access)
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
                    else => {},
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
