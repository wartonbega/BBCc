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

pub fn getInbuiltLocation() Location {
    return Location{ .range = .{
        .start = .{ .character = 0, .line = 0 },
        .end = .{ .character = 0, .line = 0 },
    }, .uri = "inbuild" };
}

/// Helper to track source positions
pub const SourceLocation = struct {
    line: u32,
    column: u32,
    offset: usize, // byte offset in file

    pub fn toPosition(self: SourceLocation) Position {
        return .{
            .line = self.line,
            .character = self.column,
        };
    }
};

pub const TokenType = enum {
    NULL, // Null token for errors
    ANYTYPE, // Matches anything
    // Operators
    EQUAL,
    DBL_EQUAL, // ==
    NOT_EQUAL, // !=
    LESS_THAN, // <
    MORE_THAN, // >
    LESS_THAN_EQ, // <=
    MORE_THAN_EQ, // >=
    TIMES,
    PLUS,
    DIV,
    MINUS,

    DOT, // .
    COMMA, // ,
    EXCLAM, // !
    QUEST, // ?
    ARROW, // ->
    AT, // @

    // Keywords
    FN_DEC, // 'fn'
    LET, // 'let a = b'
    IF,
    ELIF,
    ELSE,
    WHILE,
    FOR,
    STRUCT,
    ENUM,
    FREE,
    PRINT,
    PRINTLN,
    IMPORT,

    IDENT, // identifier
    INTLIT, // Integer literal
    CHARLIT,
    STRINGLIT,
    TRUE, // Boolean literal
    FALSE,
    NULL_KW, // Null value

    COLON, // :

    // Parenthesis-like
    O_PAR,
    C_PAR,
    O_BRA,
    C_BRA,
    O_CUR,
    C_CUR,
};

// A token is
//  The type of the token
//  The value string literal
//  The position in the document (e.g. "main.bbc:12:1")
pub const Token = struct { type: TokenType, value: []const u8, location: Location };

// Return the type of the token, wether the current identifier is a simple
// identifier or wether it is a Keyword. In that case, it returns the wright
// keyword type
fn getIdentType(ident: []const u8) TokenType {
    if (std.mem.eql(u8, ident, "func")) {
        return TokenType.FN_DEC;
    }
    if (std.mem.eql(u8, ident, "if")) {
        return TokenType.IF;
    }
    if (std.mem.eql(u8, ident, "elif")) {
        return TokenType.ELIF;
    }
    if (std.mem.eql(u8, ident, "else")) {
        return TokenType.ELSE;
    }
    if (std.mem.eql(u8, ident, "let")) {
        return TokenType.LET;
    }
    if (std.mem.eql(u8, ident, "while")) {
        return TokenType.WHILE;
    }
    if (std.mem.eql(u8, ident, "for")) {
        return TokenType.FOR;
    }
    if (std.mem.eql(u8, ident, "true")) {
        return TokenType.TRUE;
    }
    if (std.mem.eql(u8, ident, "false")) {
        return TokenType.FALSE;
    }
    if (std.mem.eql(u8, ident, "struct")) {
        return TokenType.STRUCT;
    }
    if (std.mem.eql(u8, ident, "enum")) {
        return TokenType.ENUM;
    }
    if (std.mem.eql(u8, ident, "free")) {
        return TokenType.FREE;
    }
    if (std.mem.eql(u8, ident, "null")) {
        return TokenType.NULL_KW;
    }
    if (std.mem.eql(u8, ident, "print")) {
        return TokenType.PRINT;
    }
    if (std.mem.eql(u8, ident, "println")) {
        return TokenType.PRINTLN;
    }
    if (std.mem.eql(u8, ident, "import")) {
        return TokenType.IMPORT;
    }
    return TokenType.IDENT;
}

// Alpha caracters : alphabet lower & uppercase and underscore
fn isAlpha(char: u8) bool {
    return (@as(u8, 'a') <= char and char <= @as(u8, 'z')) or
        (@as(u8, 'A') <= char and char <= @as(u8, 'Z')) or
        char == @as(u8, '_');
}

fn isNumeric(char: u8) bool {
    return @as(u8, '0') <= char and char <= @as(u8, '9');
}

fn isAlphaNumeric(char: u8) bool {
    return isNumeric(char) or isAlpha(char);
}

fn isWhiteSpace(char: u8) bool {
    return char == ' ' or char == '\t' or char == '\n';
}

// Returns wether the char is a director of
// one of the operator (eg + , =, -, <, >, ... )
fn isDirOperator(char: u8) bool {
    return char == '+' or
        char == '-' or
        char == ':' or
        char == '?' or
        char == '/' or
        char == '*' or
        char == '=' or
        char == '<' or
        char == '>' or
        char == '!' or
        char == '@' or
        char == '.';
}

// The parser errors
const parser_error = error{ IntegerLiteralTooLong, IdentifierTooLong, UnexpectedToken, UnknownOperator, ExpectedToken };

const Reader = struct {
    content: []const u8,
    pos: u32,
    filename: []const u8,
    lastPos: Position,
    allocator: std.mem.Allocator,

    pub fn new(allocator: *std.heap.ArenaAllocator, filename: []const u8) !Reader {
        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        const contents = try file.readToEndAlloc(allocator.allocator(), 1 << 20); // max 1 MB
        return Reader{
            .content = contents,
            .pos = 0,
            .filename = filename,
            .lastPos = .{ .character = 0, .line = 0 },
            .allocator = allocator.allocator(),
        };
    }

    pub fn canPeek(self: *Reader, amount: u32) bool {
        return amount + self.pos <= self.content.len;
    }

    pub fn peek(self: *Reader, amount: u32) []const u8 {
        if (self.pos + amount >= self.content.len) {
            return self.content[self.pos..self.content.len];
        }
        return self.content[self.pos .. self.pos + amount];
    }

    pub fn consume(self: *Reader, amount: u32) []const u8 {
        defer self.pos += amount;
        self.lastPos = getCurrentPos(self);
        if (self.pos + amount >= self.content.len) {
            return self.content[self.pos..self.content.len];
        }
        return self.content[self.pos .. self.pos + amount];
    }

    pub fn getCurrentPos(self: *Reader) Position {
        var line = @as(u32, 0);
        var col = @as(u32, 0);
        for (0..self.pos) |i| {
            if (self.content[i] == '\n') {
                col = 0;
                line += 1;
            } else {
                col += 1;
            }
        }
        return Position{
            .character = col,
            .line = line,
        };
    }

    pub fn getCurrentPosition(self: *Reader) Location {
        return Location{
            .range = Range{ .start = self.lastPos, .end = self.getCurrentPos() },
            .uri = self.filename,
        };
    }

    // Returns the current position in the document as :
    //      line << 32 || column
    pub fn current_pos(self: *Reader) u64 {
        var line = 0;
        var col = 0;
        for (0..self.pos) |i| {
            if (self.content[i] == '\n') {
                col = 0;
                line += 1;
            } else {
                col += 1;
            }
        }
        return (line << 32) || col;
    }
};

fn print_toks(list: std.ArrayList(Token)) void {
    std.debug.print("[", .{});
    for (list.items) |value| {
        std.debug.print("{}({s}), ", .{ value.type, value.value });
    }
    std.debug.print("]\n", .{});
}

fn parseIntegerLiteral(reader: *Reader, allocator: std.mem.Allocator) !Token {
    var buffer = try allocator.alloc(u8, 100);
    var pos = @as(u8, 0);
    while (pos < 100 and reader.canPeek(1) and isNumeric(reader.peek(1)[0])) {
        buffer[pos] = reader.consume(1)[0];
        pos += 1;
    }
    if (pos == 100) {
        return parser_error.IntegerLiteralTooLong;
    }
    const value = buffer[0..pos];
    return .{ .type = TokenType.INTLIT, .value = value, .location = reader.getCurrentPosition() };
}

fn parseIdentifier(reader: *Reader, allocator: std.mem.Allocator) !Token {
    var buffer = try allocator.alloc(u8, 100);
    var pos = @as(u8, 0);
    while (pos < 100 and reader.canPeek(1) and isAlphaNumeric(reader.peek(1)[0])) {
        buffer[pos] = reader.consume(1)[0];
        pos += 1;
    }
    if (pos == 100) {
        return parser_error.IdentifierTooLong;
    }
    const value = buffer[0..pos];
    return .{ .type = getIdentType(value), .value = value, .location = reader.getCurrentPosition() };
}

fn parseChar(reader: *Reader) !Token {
    _ = reader.consume(1);
    const char = reader.consume(1);
    const closing_quote = reader.consume(1)[0];
    if (closing_quote != '\'')
        return parser_error.ExpectedToken;
    return .{ .type = TokenType.CHARLIT, .value = char, .location = reader.getCurrentPosition() };
}

fn parseString(reader: *Reader) !Token {
    _ = reader.consume(1);
    var i: u32 = @intCast(1);
    while (reader.peek(i)[reader.peek(i).len - 1] != '"') {
        i += 1;
    }
    const ret = reader.consume(i - 1);
    _ = reader.consume(1);
    return .{ .type = TokenType.STRINGLIT, .value = ret, .location = reader.getCurrentPosition() };
}

fn ignoreLineComment(reader: *Reader) void {
    // the two first '//'
    while (reader.canPeek(1) and reader.consume(1)[0] != '\n') {}
}

fn parseOperator(reader: *Reader) !Token {
    // 2-char long operators :
    // -> == <= >= ...
    if (reader.canPeek(2) and std.mem.eql(u8, reader.peek(2), "->")) {
        _ = reader.consume(2);
        return .{ .type = TokenType.ARROW, .value = "->", .location = reader.getCurrentPosition() };
    } else if (reader.canPeek(2) and std.mem.eql(u8, reader.peek(2), "==")) {
        _ = reader.consume(2);
        return .{ .type = TokenType.DBL_EQUAL, .value = "==", .location = reader.getCurrentPosition() };
    } else if (reader.canPeek(2) and std.mem.eql(u8, reader.peek(2), "!=")) {
        _ = reader.consume(2);
        return .{ .type = TokenType.NOT_EQUAL, .value = "!=", .location = reader.getCurrentPosition() };
    } else if (reader.canPeek(2) and std.mem.eql(u8, reader.peek(2), "<=")) {
        _ = reader.consume(2);
        return .{ .type = TokenType.LESS_THAN_EQ, .value = "<=", .location = reader.getCurrentPosition() };
    } else if (reader.canPeek(2) and std.mem.eql(u8, reader.peek(2), ">=")) {
        _ = reader.consume(2);
        return .{ .type = TokenType.MORE_THAN_EQ, .value = ">=", .location = reader.getCurrentPosition() };
    }
    // 1-char long operators :
    // + - / * . : ?

    // if (reader.canPeek(1)) no need to check : there is an operator recognized
    else {
        const char = reader.peek(1)[0];
        const ret = switch (char) {
            '=' => Token{ .type = TokenType.EQUAL, .value = "=", .location = reader.getCurrentPosition() },
            '.' => Token{ .type = TokenType.DOT, .value = ".", .location = reader.getCurrentPosition() },
            ':' => Token{ .type = TokenType.COLON, .value = ":", .location = reader.getCurrentPosition() },
            '?' => Token{ .type = TokenType.QUEST, .value = "?", .location = reader.getCurrentPosition() },
            '*' => Token{ .type = TokenType.TIMES, .value = "*", .location = reader.getCurrentPosition() },
            '/' => Token{ .type = TokenType.DIV, .value = "/", .location = reader.getCurrentPosition() },
            '+' => Token{ .type = TokenType.PLUS, .value = "+", .location = reader.getCurrentPosition() },
            '-' => Token{ .type = TokenType.MINUS, .value = "-", .location = reader.getCurrentPosition() },
            '>' => Token{ .type = TokenType.MORE_THAN, .value = ">", .location = reader.getCurrentPosition() },
            '<' => Token{ .type = TokenType.LESS_THAN, .value = "<", .location = reader.getCurrentPosition() },
            '!' => Token{ .type = TokenType.EXCLAM, .value = "!", .location = reader.getCurrentPosition() },
            '@' => Token{ .type = TokenType.AT, .value = "@", .location = reader.getCurrentPosition() },
            else => parser_error.UnknownOperator,
        };
        _ = reader.consume(1);
        return ret;
    }
}

pub fn parse(filename: []const u8, arena: *std.heap.ArenaAllocator) !std.ArrayList(Token) {
    const allocator = arena.allocator();

    // The reader of the file. Can perfom peek, consume and canPeek
    var reader = try Reader.new(arena, filename);

    // The final list of all tokens
    var tokens = std.ArrayList(Token).init(arena.allocator());

    while (true) {
        // Check if it is eof
        if (!reader.canPeek(1)) {
            break;
        }

        // Otherwise, we can read the first character
        const char = reader.peek(1)[0];
        if (reader.canPeek(2) and std.mem.eql(u8, reader.peek(2), "//")) { // line comment
            ignoreLineComment(&reader);
        } else if (isNumeric(char)) {
            try tokens.append(try parseIntegerLiteral(&reader, allocator));
        } else if (isAlpha(char)) {
            try tokens.append(try parseIdentifier(&reader, allocator));
        } else if (isDirOperator(char)) {
            try tokens.append(try parseOperator(&reader));
        } else if (char == '\'') {
            try tokens.append(try parseChar(&reader));
        } else if (char == '"') {
            try tokens.append(try parseString(&reader));
        } else if (char == ',') {
            try tokens.append(.{ .type = TokenType.COMMA, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (char == '{') {
            try tokens.append(.{ .type = TokenType.O_CUR, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (char == '}') {
            try tokens.append(.{ .type = TokenType.C_CUR, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (char == '(') {
            try tokens.append(.{ .type = TokenType.O_PAR, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (char == ')') {
            try tokens.append(.{ .type = TokenType.C_PAR, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (char == '[') {
            try tokens.append(.{ .type = TokenType.O_BRA, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (char == ')') {
            try tokens.append(.{ .type = TokenType.C_BRA, .value = reader.consume(1), .location = reader.getCurrentPosition() });
        } else if (isWhiteSpace(char)) {
            _ = reader.consume(1);
        } else {
            std.debug.print("Unexpected token {c}\n", .{char});
            return parser_error.UnexpectedToken;
        }
    }
    //print_toks(tokens);
    return tokens;
}
