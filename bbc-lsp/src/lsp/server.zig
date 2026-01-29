const std = @import("std");
const protocol = @import("protocol.zig");
const transport = @import("transport.zig");
const document = @import("document.zig");
const handlers = @import("handlers.zig");

const Allocator = std.mem.Allocator;

pub const Server = struct {
    transport: transport.Transport,
    store: *document.DocumentStore,
    handlers: handlers.Handlers,
    allocator: Allocator,
    running: bool = false,

    pub fn init(allocator: Allocator) !Server {
        const store = try allocator.create(document.DocumentStore);
        store.* = document.DocumentStore.init(allocator);
        return .{
            .transport = transport.Transport.init(allocator),
            .store = store,
            .handlers = handlers.Handlers.init(allocator, store),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Server) void {
        self.store.deinit();
    }

    pub fn run(self: *Server) !void {
        self.running = true;

        std.log.info("BBC LSP Server starting...", .{});

        while (self.running) {
            var arena = std.heap.ArenaAllocator.init(self.allocator);
            defer arena.deinit();
            const message = self.transport.readMessage(arena.allocator()) catch |err| {
                std.log.err("Failed to read message: {}", .{err});
                continue;
            };

            self.handleMessage(message, arena.allocator()) catch |err| {
                std.log.err("Failed to handle message: {}", .{err});
            };
        }
    }

    fn handleMessage(self: *Server, message: []const u8, arena: Allocator) !void {
        std.log.info("Received message length: {d}\n", .{message.len});
        std.log.info("Message content: {s}\n", .{message});

        // Try to parse with more lenient options
        const parsed = std.json.parseFromSlice(
            std.json.Value,
            arena,
            message,
            .{
                .ignore_unknown_fields = true,
                .allocate = .alloc_always,
            },
        ) catch |err| {
            std.log.err("Failed to parse JSON: {any}", .{err});
            std.log.err("Message was: {s}", .{message});
            return;
        };

        std.log.info("JSON parsed successfully", .{});

        const root = parsed.value;

        // Check if it's a request or notification
        const has_id = root.object.get("id") != null;
        const method_value = root.object.get("method") orelse {
            std.log.err("No method in request", .{});
            return;
        };
        const method = method_value.string;

        std.log.info("Method: {s}, has_id: {}", .{ method, has_id });

        if (has_id) {
            try self.handleRequest(root, method, arena);
        } else {
            try self.handleNotification(root, method, arena);
        }
    }

    fn handleRequest(self: *Server, root: std.json.Value, method: []const u8, arena: Allocator) !void {
        std.log.info("Handling request: {s}", .{method});

        const id_value = root.object.get("id").?;
        const id: protocol.RequestId = if (id_value == .integer)
            .{ .number = id_value.integer }
        else
            .{ .string = id_value.string };

        if (std.mem.eql(u8, method, "initialize")) {
            std.log.info("Processing initialize request", .{});

            const params_value = root.object.get("params") orelse return error.MissingParams;
            const params = try std.json.parseFromValue(
                protocol.InitializeParams,
                arena,
                params_value,
                .{ .ignore_unknown_fields = true },
            );

            std.log.info("Calling handleInitialize", .{});
            const result = try self.handlers.handleInitialize(params.value, arena);

            std.log.info("Serializing result", .{});
            // Serialize result to JSON
            var result_string = std.ArrayList(u8).init(arena);
            try std.json.stringify(result, .{}, result_string.writer());

            std.log.info("Result JSON: {s}", .{result_string.items});

            const result_parsed = try std.json.parseFromSlice(
                std.json.Value,
                arena,
                result_string.items,
                .{},
            );

            const response = protocol.Response{
                .id = id,
                .result = result_parsed.value,
            };

            std.log.info("Sending response", .{});
            try self.transport.sendResponse(response, arena);
            std.log.info("Response sent successfully", .{});
        } else if (std.mem.eql(u8, method, "textDocument/hover")) {
            std.log.info("Processing hover request", .{});

            const params_value = root.object.get("params") orelse return error.MissingParams;
            const params = try std.json.parseFromValue(
                protocol.HoverParams,
                arena,
                params_value,
                .{ .ignore_unknown_fields = true },
            );

            const hover_result = try self.handlers.handleHover(params.value, arena);

            const response = if (hover_result) |result| blk: {
                var result_string = std.ArrayList(u8).init(arena);
                try std.json.stringify(result, .{}, result_string.writer());
                const result_parsed = try std.json.parseFromSlice(
                    std.json.Value,
                    arena,
                    result_string.items,
                    .{},
                );

                break :blk protocol.Response{
                    .id = id,
                    .result = result_parsed.value,
                };
            } else protocol.Response{
                .id = id,
                .result = .null,
            };

            try self.transport.sendResponse(response, arena);
        } else if (std.mem.eql(u8, method, "textDocument/definition")) {
            std.log.info("Processing definition request", .{});

            const params_value = root.object.get("params") orelse return error.MissingParams;
            const params = try std.json.parseFromValue(
                protocol.DefinitionParams,
                arena,
                params_value,
                .{ .ignore_unknown_fields = true },
            );

            const definition_result = try self.handlers.handleDefinition(params.value, arena);

            const response = if (definition_result) |result| blk: {
                var result_string = std.ArrayList(u8).init(arena);
                try std.json.stringify(result, .{}, result_string.writer());
                const result_parsed = try std.json.parseFromSlice(
                    std.json.Value,
                    arena,
                    result_string.items,
                    .{},
                );

                break :blk protocol.Response{
                    .id = id,
                    .result = result_parsed.value,
                };
            } else protocol.Response{
                .id = id,
                .result = .null,
            };

            try self.transport.sendResponse(response, arena);
        } else if (std.mem.eql(u8, method, "shutdown")) {
            std.log.info("Received shutdown request", .{});
            const response = protocol.Response{
                .id = id,
                .result = .null,
            };
            try self.transport.sendResponse(response, arena);
            // Don't set running = false here, wait for exit notification
        } else if (std.mem.eql(u8, method, "textDocument/completion")) {
            std.log.info("Processing completion request", .{});

            // Return empty completion list for now
            const response = protocol.Response{
                .id = id,
                .result = .null,
            };

            try self.transport.sendResponse(response, arena);
        } else if (std.mem.eql(u8, method, "exit")) {
            std.log.info("Received exit notification, shutting down", .{});
            self.running = false;
            // Exit cleanly
        } else {
            // Method not found
            const response = protocol.Response{
                .id = id,
                .@"error" = .{
                    .code = protocol.ErrorCodes.MethodNotFound,
                    .message = "Method not found",
                },
            };
            try self.transport.sendResponse(response, arena);
        }
    }

    fn handleNotification(self: *Server, root: std.json.Value, method: []const u8, arena: Allocator) !void {
        std.log.info("Got notification {s}", .{method});
        if (std.mem.eql(u8, method, "initialized")) {
            // Nothing to do
        } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            std.log.info("Received didOpen notification", .{});

            const params_value = root.object.get("params") orelse return error.MissingParams;
            const params = try std.json.parseFromValue(
                protocol.DidOpenTextDocumentParams,
                arena,
                params_value,
                .{ .ignore_unknown_fields = true },
            );

            std.log.info("Opening file: {s}", .{params.value.textDocument.uri});

            try self.handlers.handleDidOpen(params.value);

            std.log.info("File opened, sending diagnostics", .{});

            // Send diagnostics
            try self.publishDiagnostics(params.value.textDocument.uri, arena);
        } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
            const params_value = root.object.get("params") orelse return error.MissingParams;
            const params = try std.json.parseFromValue(
                protocol.DidChangeTextDocumentParams,
                arena,
                params_value,
                .{ .ignore_unknown_fields = true },
            );

            try self.handlers.handleDidChange(params.value);

            // Send diagnostics
            try self.publishDiagnostics(params.value.textDocument.uri, arena);
        } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
            const params_value = root.object.get("params") orelse return error.MissingParams;
            const params = try std.json.parseFromValue(
                protocol.DidCloseTextDocumentParams,
                arena,
                params_value,
                .{ .ignore_unknown_fields = true },
            );

            self.handlers.handleDidClose(params.value);
        } else if (std.mem.eql(u8, method, "exit")) {
            std.process.exit(0);
        }
    }

    fn publishDiagnostics(self: *Server, uri: []const u8, arena: Allocator) !void {
        const diagnostics = self.handlers.getDiagnostics(uri) orelse return;

        const params = protocol.PublishDiagnosticsParams{
            .uri = uri,
            .diagnostics = diagnostics,
        };

        // Serialize params to JSON
        var params_string = std.ArrayList(u8).init(arena);
        try std.json.stringify(params, .{}, params_string.writer());
        const params_parsed = try std.json.parseFromSlice(
            std.json.Value,
            arena,
            params_string.items,
            .{},
        );

        const notification = protocol.Notification{
            .method = "textDocument/publishDiagnostics",
            .params = params_parsed.value,
        };

        try self.transport.sendNotification(notification, arena);
    }
};
