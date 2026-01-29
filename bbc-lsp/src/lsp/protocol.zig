const std = @import("std");
const position = @import("../bbc/position.zig");

// ============================================================================
// JSON-RPC 2.0 Base Types
// ============================================================================

pub const JsonRpcVersion = "2.0";

pub const RequestId = union(enum) {
    number: i64,
    string: []const u8,

    pub fn jsonStringify(self: RequestId, out: anytype) !void {
        switch (self) {
            .number => |n| try out.write(n),
            .string => |s| try out.write(s),
        }
    }
};
pub const Request = struct {
    jsonrpc: []const u8 = JsonRpcVersion,
    id: RequestId,
    method: []const u8,
    params: ?std.json.Value = null,
};

pub const Response = struct {
    jsonrpc: []const u8 = JsonRpcVersion,
    id: ?RequestId,
    result: ?std.json.Value = null,
    @"error": ?ResponseError = null,
};

pub const ResponseError = struct {
    code: i32,
    message: []const u8,
    data: ?std.json.Value = null,
};

pub const Notification = struct {
    jsonrpc: []const u8 = JsonRpcVersion,
    method: []const u8,
    params: ?std.json.Value = null,
};

// ============================================================================
// LSP Error Codes
// ============================================================================

pub const ErrorCodes = struct {
    pub const ParseError: i32 = -32700;
    pub const InvalidRequest: i32 = -32600;
    pub const MethodNotFound: i32 = -32601;
    pub const InvalidParams: i32 = -32602;
    pub const InternalError: i32 = -32603;
    pub const ServerNotInitialized: i32 = -32002;
    pub const UnknownErrorCode: i32 = -32001;
    pub const RequestCancelled: i32 = -32800;
    pub const ContentModified: i32 = -32801;
};

// ============================================================================
// LSP Basic Types
// ============================================================================

pub const Position = position.Position;
pub const Range = position.Range;
pub const Location = position.Location;

pub const TextDocumentIdentifier = struct {
    uri: []const u8,
};

pub const VersionedTextDocumentIdentifier = struct {
    uri: []const u8,
    version: i32,
};

pub const TextDocumentItem = struct {
    uri: []const u8,
    languageId: []const u8,
    version: i32,
    text: []const u8,
};

// ============================================================================
// Diagnostic (Errors/Warnings)
// ============================================================================

pub const DiagnosticSeverity = enum(u8) {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
};

pub const Diagnostic = struct {
    range: Range,
    severity: ?DiagnosticSeverity = null,
    code: ?[]const u8 = null,
    source: ?[]const u8 = null,
    message: []const u8,
    relatedInformation: ?[]DiagnosticRelatedInformation = null,
};

pub const DiagnosticRelatedInformation = struct {
    location: Location,
    message: []const u8,
};

// ============================================================================
// Initialize Request/Response
// ============================================================================

pub const InitializeParams = struct {
    processId: ?i32 = null,
    rootUri: ?[]const u8 = null,
    capabilities: ClientCapabilities,
    workspaceFolders: ?[]WorkspaceFolder = null,
};

pub const ClientCapabilities = struct {
    workspace: ?WorkspaceClientCapabilities = null,
    textDocument: ?TextDocumentClientCapabilities = null,
};

pub const WorkspaceClientCapabilities = struct {};
pub const TextDocumentClientCapabilities = struct {};

pub const WorkspaceFolder = struct {
    uri: []const u8,
    name: []const u8,
};

pub const InitializeResult = struct {
    capabilities: ServerCapabilities,
    serverInfo: ?ServerInfo = null,
};

pub const ServerInfo = struct {
    name: []const u8,
    version: ?[]const u8 = null,
};

pub const ServerCapabilities = struct {
    textDocumentSync: ?TextDocumentSyncKind = null,
    hoverProvider: ?bool = null,
    completionProvider: ?CompletionOptions = null,
    definitionProvider: ?bool = null,
    referencesProvider: ?bool = null,
    documentSymbolProvider: ?bool = null,
};

pub const TextDocumentSyncKind = enum(u8) {
    None = 0,
    Full = 1,
    Incremental = 2,

    pub fn jsonStringify(self: TextDocumentSyncKind, out: anytype) !void {
        try out.write(@intFromEnum(self));
    }
};

pub const CompletionOptions = struct {
    triggerCharacters: ?[][]const u8 = null,
};

// ============================================================================
// Text Document Notifications
// ============================================================================

pub const DidOpenTextDocumentParams = struct {
    textDocument: TextDocumentItem,
};

pub const DidChangeTextDocumentParams = struct {
    textDocument: VersionedTextDocumentIdentifier,
    contentChanges: []TextDocumentContentChangeEvent,
};

pub const TextDocumentContentChangeEvent = struct {
    range: ?Range = null,
    rangeLength: ?u32 = null,
    text: []const u8,
};

pub const DidCloseTextDocumentParams = struct {
    textDocument: TextDocumentIdentifier,
};

pub const DidSaveTextDocumentParams = struct {
    textDocument: TextDocumentIdentifier,
    text: ?[]const u8 = null,
};

// ============================================================================
// Hover Request
// ============================================================================

pub const HoverParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
};

pub const Hover = struct {
    contents: MarkupContent,
    range: ?Range = null,
};

pub const MarkupContent = struct {
    kind: MarkupKind,
    value: []const u8,
};

pub const MarkupKind = enum {
    plaintext,
    markdown,

    pub fn jsonStringify(self: MarkupKind, out: anytype) !void {
        try out.write(switch (self) {
            .plaintext => "plaintext",
            .markdown => "markdown",
        });
    }
};

pub const DefinitionParams = struct {
    textDocument: TextDocumentIdentifier,
    position: Position,
};

// ============================================================================
// PublishDiagnostics Notification
// ============================================================================

pub const PublishDiagnosticsParams = struct {
    uri: []const u8,
    version: ?i32 = null,
    diagnostics: []const Diagnostic,
};
