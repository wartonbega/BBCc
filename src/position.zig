const std = @import("std");

/// Represents a position in a text document
pub const Position = struct {
    line: u32, // 0-based
    character: u32, // 0-based (UTF-16 code units for LSP compatibility)
};

/// Represents a range in a text document
pub const Range = struct {
    start: Position,
    end: Position,
};

/// Represents a location in a source file
pub const Location = struct {
    uri: []const u8, // file:// URI
    range: Range,

    pub fn toString(self: *const Location) []const u8 {
        const alloc = std.heap.page_allocator;
        return std.fmt.allocPrint(alloc, "{s}:{d}:{d}", .{ self.uri, self.range.start.line, self.range.start.character }) catch {
            return "";
        };
    }
};
