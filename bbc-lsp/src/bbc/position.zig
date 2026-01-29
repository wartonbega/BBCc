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

    pub fn isLower(self: *const Location, other: *const Location) bool {
        if (self.range.start.line != other.range.start.line) {
            return self.range.start.line < other.range.start.line;
        }
        return self.range.start.character < other.range.start.character;
    }

    pub fn isGreater(self: *const Location, other: *const Location) bool {
        if (self.range.start.line != other.range.start.line) {
            return self.range.start.line > other.range.start.line;
        }
        return self.range.start.character > other.range.start.character;
    }

    pub fn isEqual(self: *const Location, other: *const Location) bool {
        return self.range.start.line == other.range.start.line and
            self.range.start.character == other.range.start.character;
    }

    pub fn isIn(self: *const Location, range: *const Range) bool {
        if (self.range.start.line < range.start.line or self.range.start.line > range.end.line) {
            return false;
        }
        if (self.range.start.line == range.start.line and self.range.start.character < range.start.character) {
            return false;
        }
        if (self.range.start.line == range.end.line and self.range.start.character > range.end.character) {
            return false;
        }
        return true;
    }

    pub fn unionWith(self: *const Location, other: Location) Location {
        const start = self.range.start;
        const end = other.range.end;

        return Location{
            .uri = self.uri,
            .range = Range{ .start = .{ .character = start.character, .line = start.line }, .end = .{ .character = end.character, .line = end.line } },
        };
    }

    pub fn contains(self: *const Location, other: Position) bool {
        if (other.line < self.range.start.line or other.line > self.range.end.line) {
            return false;
        }
        if (other.line == self.range.start.line and other.character < self.range.start.character) {
            return false;
        }
        if (other.line == self.range.end.line and other.character > self.range.end.character) {
            return false;
        }
        return true;
    }
};
