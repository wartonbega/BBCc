const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const errors = @import("errors.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const TokenType = parser.TokenType;

var stdout = std.io.getStdOut().writer();
var stderr = std.io.getStdErr().writer();

const lexerError = error{ noFollowingToken, unexpectedToken, unimplementedValueType };

pub const tokenReader = struct {
    pos: i32,
    tokens: ArrayList(parser.Token),

    pub fn new(toks: ArrayList(parser.Token)) tokenReader {
        return .{ .pos = 0, .tokens = toks };
    }

    pub fn next(self: *tokenReader) !parser.Token {
        if (self.pos < self.tokens.items.len) {
            defer self.pos += 1;
            return self.tokens.items[@intCast(self.pos)];
        }
        return lexerError.noFollowingToken;
    }

    pub fn consume(self: *tokenReader, ttype: TokenType) parser.Token {
        if (self.pos < self.tokens.items.len) {
            defer self.pos += 1;
            const ret = self.tokens.items[@intCast(self.pos)];
            if (ret.type != ttype and ttype != TokenType.ANYTYPE)
                errors.bbcErrorExit("Unexpected token '{s}', expected '{}'\n", .{ ret.value, ttype }, ret.pos);
            return ret;
        }
        errors.bbcErrorExit("Expected token '{}' found nothing\n", .{ttype}, self.current().pos);
        return .{ .type = TokenType.NULL, .value = "", .pos = "" };
    }

    pub fn peek(self: *tokenReader) !parser.Token {
        if (self.pos < self.tokens.items.len) {
            return self.tokens.items[@intCast(self.pos)];
        }
        return lexerError.noFollowingToken;
    }

    pub fn current(self: *tokenReader) parser.Token {
        if (self.pos < self.tokens.items.len) {
            return self.tokens.items[@intCast(self.pos)];
        }
        return self.tokens.items[@intCast(self.tokens.items.len - 1)];
    }

    pub fn canPeek(self: *tokenReader) bool {
        return self.pos < self.tokens.items.len;
    }
};

pub fn lexeType(reader: *tokenReader, allocator: Allocator) !*ast.Type {
    // Type :
    //    | typefunction
    //    | (!)? (*)* name
    const opt_error = (try reader.peek()).type == parser.TokenType.EXCLAM;
    if (opt_error)
        _ = try reader.next();
    var references_counter = @as(i32, 0);
    while (reader.canPeek() and (try reader.peek()).type == parser.TokenType.TIMES) {
        _ = try reader.next();
        references_counter += 1;
    }
    const base_name = reader.consume(TokenType.IDENT).value;

    const ret = try allocator.create(ast.Type);
    ret.* = ast.Type{ .base = ast.TypeBase{ .name = base_name }, .err = opt_error, .references = references_counter };
    return ret;
}

pub fn lexeListedValue(reader: *tokenReader, allocator: Allocator) !ArrayList(*ast.Value) {
    // rule:
    // | (val, val, val,...)
    var ret = ArrayList(*ast.Value).init(allocator);
    _ = reader.consume(TokenType.O_PAR);
    while (reader.canPeek()) {
        const val = try lexeValue0(reader, allocator);
        try ret.append(val);
        if ((try reader.peek()).type == TokenType.C_PAR)
            break;
        _ = reader.consume(TokenType.COMMA);
    }
    _ = reader.consume(TokenType.C_PAR);
    return ret;
}

pub fn lexeValue7(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value7:
    //  | IF value0 {scope}
    //  | LET ident
    //  | ident
    //  | "stringlit" [TODO]
    //  | 'c' (charlit)
    //  | intlit
    //  | (value)
    //  | {scope}
    //  | func (args) retype -> {}

    switch ((try reader.peek()).type) {
        TokenType.IF => {
            _ = reader.consume(TokenType.IF);
            const condition = try lexeValue0(reader, allocator);
            const scope = try lexeScope(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.IfStmt);
            operation.* = ast.IfStmt{ .condition = condition, .scope = scope };
            ret.* = ast.Value{ .If = operation };
            return ret;
        },
        TokenType.LET => {
            _ = reader.consume(TokenType.LET);
            const name = reader.consume(TokenType.IDENT);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.VarDeclaration);
            operation.* = ast.VarDeclaration{ .mutable = false, .name = name.value };
            ret.* = ast.Value{ .varDec = operation };
            return ret;
        },
        TokenType.IDENT => {
            const name = reader.consume(TokenType.IDENT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .identifier = name.value };
            return ret;
        },
        TokenType.INTLIT => {
            const value = reader.consume(TokenType.INTLIT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .intLit = try std.fmt.parseInt(i32, value.value, 10) };
            return ret;
        },
        TokenType.O_CUR => {
            const scope = try lexeScope(reader, allocator);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .scope = scope };
            return ret;
        },
        TokenType.O_PAR => {
            _ = reader.consume(TokenType.O_PAR);
            const val = try lexeValue0(reader, allocator);
            _ = reader.consume(TokenType.C_PAR);

            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .parenthesis = val };
            return ret;
        },
        TokenType.CHARLIT => {
            const val = reader.consume(TokenType.CHARLIT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .charLit = val.value[0] };
            return ret;
        },
        TokenType.STRINGLIT => {
            const val = reader.consume(TokenType.STRINGLIT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .stringLit = val.value };
            return ret;
        },
        TokenType.FN_DEC => {
            _ = reader.consume(TokenType.FN_DEC);
            _ = reader.consume(TokenType.O_PAR);
            const args = try lexeArguments(reader, allocator);
            const rettype = try lexeType(reader, allocator);
            _ = reader.consume(TokenType.ARROW);
            const code = try lexeScope(reader, allocator);

            const ret = try allocator.create(ast.Value);
            const function = try allocator.create(ast.Function);
            function.* = ast.Function{
                .arguments = args,
                .code = code,
                .return_type = rettype,
            };
            ret.* = ast.Value{ .function = function };
            return ret;
        },
        else => {
            std.debug.print("{s}\n", .{@tagName((try reader.peek()).type)});
            return lexerError.unimplementedValueType;
        },
    }
}

pub fn lexeValue6(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value6:
    //  | Value7 ? err : {code}
    //  | Value7
    const lhs = try lexeValue7(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.QUEST => |optype| {
            _ = reader.consume(optype);
            //const rhs = lexeValue3(reader, allocator);
            _ = reader.consume(TokenType.IDENT);
            _ = reader.consume(TokenType.COLON);
            _ = try lexeScope(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.ErrCheck);
            //operation.* = ast.ErrCheck{
            // [TODO]
            //};

            ret.* = ast.Value{ .errorCheck = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue5(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value5:
    //  | Value6 (.IDENT)* // for the future it will be (.IDENT | [value0])*

    // lhs is a variable because the system works by aggregating
    // all the attributes together.
    // For example, a.b.c derives in .c => (.b => (a))
    var lhs = try lexeValue6(reader, allocator);
    if (!reader.canPeek())
        return lhs;

    while (reader.canPeek() and switch ((try reader.peek()).type) {
        TokenType.DOT => |optype| blk: {
            _ = reader.consume(optype);
            const ident = reader.consume(TokenType.IDENT);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.UnaryOperatorRight);
            operation.* = ast.UnaryOperatorRight{ .expr = lhs, .operator = ast.RightUnaryOperators{ .pointAttr = ident.value } };
            ret.* = ast.Value{ .unaryOperatorRight = operation };
            lhs = ret;
            break :blk true;
        },
        else => false,
    }) {}
    return lhs;
}

pub fn lexeValue4(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value4:
    //  | Value5(args) # funcall
    //  | Value5
    const lhs = try lexeValue5(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.O_PAR => {
            const args = try lexeListedValue(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.Funcall);
            operation.* = ast.Funcall{
                .args = args,
                .func = lhs,
            };
            ret.* = ast.Value{ .funcall = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}
pub fn lexeValue3(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value3:
    //  | Value4 TIMES Value3
    //  | Value4 DIV   Value3
    //  | Value4
    const lhs = try lexeValue4(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.TIMES, TokenType.DIV => |optype| {
            _ = reader.consume(optype);
            const rhs = try lexeValue3(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.binaryOperation);
            operation.* = ast.binaryOperation{
                .lhs = lhs,
                .rhs = rhs,
                .operator = switch (optype) {
                    TokenType.DIV => ast.binOperator.Div,
                    TokenType.TIMES => ast.binOperator.Times,
                    else => ast.binOperator.Times, // We need a default value
                },
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue2(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value2:
    //  | Value3 PLUS  Value2
    //  | Value3 MINUS Value2
    //  | Value3
    const lhs = try lexeValue3(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.PLUS, TokenType.MINUS => |optype| {
            _ = reader.consume(optype);
            const rhs = try lexeValue2(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.binaryOperation);
            operation.* = ast.binaryOperation{
                .lhs = lhs,
                .rhs = rhs,
                .operator = switch (optype) {
                    TokenType.PLUS => ast.binOperator.Plus,
                    TokenType.MINUS => ast.binOperator.Minus,
                    else => ast.binOperator.Minus, // We need a default value
                },
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue1(reader: *tokenReader, allocator: Allocator) (lexerError || std.mem.Allocator.Error || std.fmt.ParseIntError)!*ast.Value {
    // value1:
    //  | Value2 == value1
    //  | Value2 != value1
    //  | Value2 <= value1
    //  | Value2 >= value1
    //  | Value2 < value1
    //  | Value2 > value1
    //  | value2
    const lhs = try lexeValue2(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.DBL_EQUAL, TokenType.NOT_EQUAL, TokenType.LESS_THAN, TokenType.MORE_THAN, TokenType.MORE_THAN_EQ, TokenType.LESS_THAN_EQ => |optype| {
            _ = reader.consume(optype);
            const rhs = try lexeValue1(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.binaryOperation);
            operation.* = ast.binaryOperation{
                .lhs = lhs,
                .rhs = rhs,
                .operator = switch (optype) {
                    TokenType.DBL_EQUAL => ast.binOperator.Equal,
                    TokenType.NOT_EQUAL => ast.binOperator.NotEqual,
                    TokenType.LESS_THAN => ast.binOperator.Lt,
                    TokenType.MORE_THAN => ast.binOperator.Gt,
                    TokenType.MORE_THAN_EQ => ast.binOperator.Ge,
                    TokenType.LESS_THAN_EQ => ast.binOperator.Le,
                    else => ast.binOperator.Equal,
                },
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue0(reader: *tokenReader, allocator: Allocator) (lexerError || std.mem.Allocator.Error || std.fmt.ParseIntError)!*ast.Value {
    // value0: less priority
    //      assignations '='
    // value0:
    //  | Value1 = value0
    //  | value1
    const lhs = try lexeValue1(reader, allocator);
    if (reader.canPeek() and (try reader.peek()).type == TokenType.EQUAL) {
        _ = reader.consume(TokenType.EQUAL);
        const rhs = try lexeValue0(reader, allocator);
        const ret = try allocator.create(ast.Value);
        const assignement = try allocator.create(ast.Assignement);
        assignement.* = ast.Assignement{ .lhs = lhs, .rhs = rhs };
        ret.* = ast.Value{ .assignement = assignement };
        return ret;
    }
    return lhs;
}

pub fn lexeScope(reader: *tokenReader, allocator: Allocator) !*ast.Scope {
    // Used for when there's a multitude of values
    // Like in a scope
    // Rule :
    //      { values * }

    // Consume opening {
    _ = reader.consume(TokenType.O_CUR);

    var values = ArrayList(*ast.Value).init(allocator);
    while (reader.canPeek() and (try reader.peek()).type != TokenType.C_CUR) {
        try values.append(try lexeValue0(reader, allocator));
    }
    _ = reader.consume(TokenType.C_CUR);

    const ret = try ast.Scope.init(values, allocator);
    return ret;
}

pub fn lexeArguments(reader: *tokenReader, allocator: Allocator) !ArrayList(*ast.Arguments) {
    _ = reader.consume(TokenType.O_PAR);
    var args = ArrayList(*ast.Arguments).init(allocator);
    // First checking if it is not an empty parenthesis
    if ((try reader.peek()).type != TokenType.C_PAR)
        while (true) {
            const argtype = try lexeType(reader, allocator);
            const arg_name = reader.consume(TokenType.IDENT);

            const arg = try allocator.create(ast.Arguments);
            arg.name = arg_name.value;
            arg._type = argtype;
            try args.append(arg);

            if ((try reader.peek()).type != TokenType.COMMA)
                break;
            _ = reader.consume(TokenType.COMMA);
        };
    _ = reader.consume(TokenType.C_PAR);
    return args;
}

pub fn lexeTypeParametrisation(reader: *tokenReader, allocator: Allocator) !ArrayList(ast.TypeParam) {
    // <Name: trait>
    var ret = ArrayList(ast.TypeParam).init(allocator);
    if (!reader.canPeek() or (try reader.peek()).type != .LESS_THAN)
        return ret;

    _ = reader.consume(.LESS_THAN);
    while (true) {
        const name = reader.consume(.IDENT);
        _ = reader.consume(.COLON);
        // The traits list
        var traits = ArrayList([]const u8).init(allocator);
        // Two ways for reading traits (with or without Parenthesis):
        // <Type1: (Add, Sub, ...), Type2: ..;> or <Type1: Add, Type2: ...>
        if (reader.canPeek() and (try reader.peek()).type == .O_PAR) {
            _ = reader.consume(.O_PAR);
            const first_trait = reader.consume(.IDENT);
            try traits.append(first_trait.value);
            while (reader.canPeek() and (try reader.peek()).type == .COMMA) {
                _ = reader.consume(.COMMA);
                const trait = reader.consume(.IDENT);
                try traits.append(trait.value);
            }
            _ = reader.consume(.C_PAR);
        } else {
            const trait = reader.consume(.IDENT);
            try traits.append(trait.value);
        }
        try ret.append(.{
            .name = name.value,
            .traits = traits,
        });
        if (reader.canPeek() and (try reader.peek()).type != .COMMA) {
            break;
        } else _ = reader.consume(.COMMA);
    }
    _ = reader.consume(.MORE_THAN);
    return ret;
}

pub fn lexeFuncdef(reader: *tokenReader, allocator: Allocator) !*ast.funcDef {
    // Rule:
    //      func <typeimple>? name ( args* ) retype { code }
    //          with { code } witch is parsed by scope
    _ = reader.consume(.FN_DEC); // func keyword

    const type_param = try lexeTypeParametrisation(reader, allocator);

    const name = reader.consume(TokenType.IDENT);

    const args = try lexeArguments(reader, allocator);

    // now parsing return type
    const rettype = try lexeType(reader, allocator);

    // now parsing scope
    const scope = try lexeScope(reader, allocator);

    const ret = try allocator.create(ast.funcDef);

    ret.* = ast.funcDef{
        .arguments = args,
        .name = name.value,
        .return_type = rettype,
        .code = scope,
        .typeparam = type_param,
    };
    return ret;
}

pub fn lexeProgram(tokens: ArrayList(parser.Token), allocator: Allocator) !*ast.Program {
    // Program:
    //   | FUNC name ( args* ) retype {code}
    var reader = tokenReader.new(tokens);
    var ret = try allocator.create(ast.Program);
    ret.* = ast.Program{ .instructions = ArrayList(*ast.ProgInstructions).init(allocator) };
    while (reader.canPeek()) {
        const token = try reader.peek();
        switch (token.type) {
            TokenType.FN_DEC => {
                const fndef = try allocator.create(ast.ProgInstructions);
                fndef.* = ast.ProgInstructions{ .FuncDef = try lexeFuncdef(&reader, allocator) };
                try ret.instructions.append(fndef);
            },
            else => {
                errors.bbcErrorExit("Unexpected token {}", .{token.type}, token.pos);
            },
        }
    }
    //ret.print();
    return ret;
}
