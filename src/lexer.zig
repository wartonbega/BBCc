const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const errors = @import("errors.zig");
const types = @import("types.zig");

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
    ret.* = ast.Type{
        .base = ast.TypeBase{ .name = base_name },
        .err = opt_error,
        .references = references_counter,
    };
    return ret;
}

pub fn lexeListedValue(reader: *tokenReader, allocator: Allocator) !ArrayList(*ast.Value) {
    // rule:
    // | (val, val, val,...)
    var ret = ArrayList(*ast.Value).init(allocator);
    _ = reader.consume(TokenType.O_PAR);
    while (reader.canPeek() and (try reader.peek()).type != TokenType.C_PAR) {
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
    //  | IF value0 value0
    //  | WHILE value0 value0
    //  | LET ident
    //  | ident
    //  | @ ident? {( value0 ,)*}
    //  | "stringlit" [TODO]
    //  | 'c' (charlit)
    //  | intlit
    //  | (value)
    //  | {scope}
    //  | func (args) retype -> {}
    //  | free value

    switch ((try reader.peek()).type) {
        TokenType.IF => {
            var conditions = ArrayList(*ast.Value).init(allocator);
            var scopes = ArrayList(*ast.Value).init(allocator);
            var else_scope: ?*ast.Value = null;

            const if_tok = reader.consume(TokenType.IF);
            const code_ref = if_tok.pos;
            try conditions.append(try lexeValue0(reader, allocator));
            try scopes.append(try lexeValue0(reader, allocator));

            while (reader.canPeek() and (try reader.peek()).type == .ELIF) {
                _ = reader.consume(.ELIF);
                try conditions.append(try lexeValue0(reader, allocator));
                try scopes.append(try lexeValue0(reader, allocator));
            }
            if (reader.canPeek() and (try reader.peek()).type == .ELSE) {
                _ = reader.consume(.ELSE);
                else_scope = try lexeValue0(reader, allocator);
            }

            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.IfStmt);
            operation.* = ast.IfStmt{
                .conditions = conditions,
                .scopes = scopes,
                .elsescope = else_scope,
                .reference = code_ref,
            };
            ret.* = ast.Value{ .If = operation };
            return ret;
        },
        TokenType.WHILE => {
            const while_tok = reader.consume(TokenType.WHILE);
            const code_ref = while_tok.pos;

            const condition = try lexeValue0(reader, allocator);
            const exec = try lexeValue0(reader, allocator);

            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.WhileLoop);
            operation.* = ast.WhileLoop{
                .condition = condition,
                .exec = exec,
                .reference = code_ref,
            };
            ret.* = ast.Value{ .While = operation };
            return ret;
        },
        TokenType.LET => {
            _ = reader.consume(TokenType.LET);
            const name = reader.consume(TokenType.IDENT);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.VarDeclaration);
            operation.* = ast.VarDeclaration{
                .mutable = false,
                .name = name.value,
                .reference = name.pos,
            };
            ret.* = ast.Value{ .varDec = operation };
            return ret;
        },
        TokenType.IDENT => {
            const name = reader.consume(TokenType.IDENT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .identifier = .{
                .name = name.value,
                .reference = name.pos,
            } };
            return ret;
        },
        TokenType.AT => { // Struct usage
            const at_op = reader.consume(TokenType.AT);

            const ret = try allocator.create(ast.Value);

            const stc_init = try allocator.create(ast.StructInit);
            ret.* = ast.Value{ .structInit = stc_init };
            stc_init.habitants = .init(allocator);
            stc_init.reference = at_op.pos;

            ret.structInit = stc_init;
            // The name is optionnal
            const stc_name = if (reader.canPeek() and (try reader.peek()).type == .IDENT) reader.consume(.IDENT).value else @as([]const u8, "");
            stc_init.name = stc_name;

            _ = reader.consume(.O_CUR);
            while ((reader.canPeek()) and (try reader.peek()).type != .C_CUR) {
                // {a = 1, b = 2 ...}
                const name = reader.consume(.IDENT);
                _ = reader.consume(.EQUAL);
                const hab = try lexeValue0(reader, allocator);
                try stc_init.habitants.put(name.value, hab);
                if (reader.canPeek() and (try reader.peek()).type != .C_CUR)
                    _ = reader.consume(.COMMA);
            }
            _ = reader.consume(.C_CUR);
            return ret;
        },
        TokenType.INTLIT => {
            const value = reader.consume(TokenType.INTLIT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .intLit = .{
                .value = try std.fmt.parseInt(i32, value.value, 10),
                .reference = value.pos,
            } };
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
            ret.* = ast.Value{ .charLit = .{
                .value = val.value[0],
                .reference = val.pos,
            } };
            return ret;
        },
        TokenType.STRINGLIT => {
            const val = reader.consume(TokenType.STRINGLIT);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .stringLit = .{
                .value = val.value,
                .reference = val.pos,
            } };
            return ret;
        },
        TokenType.TRUE, TokenType.FALSE => |boollit| {
            const val = reader.consume(boollit);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .boolLit = .{
                .value = boollit == .TRUE,
                .reference = val.pos,
            } };
            return ret;
        },
        TokenType.FN_DEC => {
            const fn_dec_tok = reader.consume(TokenType.FN_DEC);
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
                .reference = fn_dec_tok.pos,
            };
            ret.* = ast.Value{ .function = function };
            return ret;
        },
        TokenType.FREE => {
            const free_tok = reader.consume(.FREE);
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .freeKeyword = .{
                .val = try lexeValue7(reader, allocator),
                .reference = free_tok.pos,
            } };
            return ret;
        },
        else => {
            std.debug.print("{s}\n", .{@tagName((try reader.peek()).type)});
            return lexerError.unimplementedValueType;
        },
    }
}

// TODO: inverser value6 et value5

pub fn lexeValue6(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value6:
    //  | value7 (.IDENT)* // for the future it will be (.IDENT | [value0])*

    // lhs is a variable because the system works by aggregating
    // all the attributes together.
    // For example, a.b.c derives in .c => (.b => (a))
    var lhs = try lexeValue7(reader, allocator);
    if (!reader.canPeek())
        return lhs;

    while (reader.canPeek() and switch ((try reader.peek()).type) {
        TokenType.DOT => |optype| blk: {
            _ = reader.consume(optype);
            const ident = reader.consume(TokenType.IDENT);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.UnaryOperatorRight);
            operation.* = ast.UnaryOperatorRight{
                .expr = lhs,
                .operator = ast.RightUnaryOperators{ .pointAttr = ident.value },
                .reference = ident.pos,
            };
            ret.* = ast.Value{ .unaryOperatorRight = operation };
            lhs = ret;
            break :blk true;
        },
        else => false,
    }) {}
    return lhs;
}

pub fn lexeValue5(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value5:
    //  | Value6(args) # funcall
    //  | Value6
    const lhs = try lexeValue6(reader, allocator);
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

pub fn lexeValue4(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value4:
    //  | Value5 TIMES Value4
    //  | Value5 DIV   Value4
    //  | Value5
    const lhs = try lexeValue5(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.TIMES, TokenType.DIV => |optype| {
            const operator = reader.consume(optype);
            const rhs = try lexeValue4(reader, allocator);
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
                .reference = operator.pos,
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue3(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value3:
    //  | Value4 PLUS  Value3
    //  | Value4 MINUS Value3
    //  | Value4
    const lhs = try lexeValue4(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.PLUS, TokenType.MINUS => |optype| {
            const operator = reader.consume(optype);
            const rhs = try lexeValue3(reader, allocator);
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
                .reference = operator.pos,
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue2(reader: *tokenReader, allocator: Allocator) (lexerError || std.mem.Allocator.Error || std.fmt.ParseIntError)!*ast.Value {
    // Value2:
    //  | Value3 == Value2
    //  | Value3 != Value2
    //  | Value3 <= Value2
    //  | Value3 >= Value2
    //  | Value3 < Value2
    //  | Value3 > Value2
    //  | Value3
    const lhs = try lexeValue3(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.DBL_EQUAL, TokenType.NOT_EQUAL, TokenType.LESS_THAN, TokenType.MORE_THAN, TokenType.MORE_THAN_EQ, TokenType.LESS_THAN_EQ => |optype| {
            const operator = reader.consume(optype);
            const rhs = try lexeValue2(reader, allocator);
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
                .reference = operator.pos,
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue1(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // value1:
    //  | value2 ? err  {code}
    //  | value2
    const lhs = try lexeValue2(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.QUEST => |optype| {
            const operator = reader.consume(optype);

            const error_name = reader.consume(TokenType.IDENT).value;
            const scope = try lexeValue0(reader, allocator);

            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.ErrCheck);
            operation.* = ast.ErrCheck{
                .err = error_name,
                .scope = scope,
                .value = lhs,
                .reference = operator.pos,
            };

            ret.* = ast.Value{ .errorCheck = operation };
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
        const assignation_op = reader.consume(TokenType.EQUAL);
        const rhs = try lexeValue0(reader, allocator);
        const ret = try allocator.create(ast.Value);
        const assignement = try allocator.create(ast.Assignement);
        assignement.* = ast.Assignement{
            .lhs = lhs,
            .rhs = rhs,
            .reference = assignation_op.pos,
        };
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
    const ocur = reader.consume(TokenType.O_CUR);

    var values = ArrayList(*ast.Value).init(allocator);
    while (reader.canPeek() and (try reader.peek()).type != TokenType.C_CUR) {
        try values.append(try lexeValue0(reader, allocator));
    }
    _ = reader.consume(TokenType.C_CUR);

    const ret = try ast.Scope.init(values, ocur.pos, allocator);
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
            arg.reference = arg_name.pos;
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
        if (reader.canPeek() and (try reader.peek()).type == .COLON) {
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
                .reference = name.pos,
            });
        } else {
            // No traits specified
            try ret.append(.{
                .name = name.value,
                .traits = ArrayList([]const u8).init(allocator),
                .reference = name.pos,
            });
        }
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
        .reference = name.pos,
    };
    return ret;
}

pub fn lexeStructDef(reader: *tokenReader, allocator: Allocator) !*ast.structDef {
    // Struct def:
    // struct name { (Type name)* }
    const struct_keyword = reader.consume(.STRUCT); // func keyword

    const st_name = reader.consume(.IDENT);

    _ = reader.consume(.O_CUR);

    const ret = try allocator.create(ast.structDef);
    ret.habitants = std.hash_map.StringHashMap(*ast.Type).init(allocator);
    ret.reference = struct_keyword.pos;
    ret.name = st_name.value;
    ret.order = std.ArrayList([]const u8).init(allocator);

    try ret.habitants.put("_count", (try types.CreateTypeInt(allocator, false)).decided);
    try ret.habitants.put("_size", (try types.CreateTypeInt(allocator, false)).decided);

    try ret.order.append("_count");
    try ret.order.append("_size"); // For now unused

    while ((reader.canPeek()) and (try reader.peek()).type != .C_CUR) {
        const ttype = try lexeType(reader, allocator);
        const name = reader.consume(.IDENT);
        try ret.habitants.put(name.value, ttype);
        try ret.order.append(name.value);
        if (reader.canPeek() and (try reader.peek()).type != .C_CUR)
            _ = reader.consume(.COMMA);
    }
    _ = reader.consume(.C_CUR);
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
            TokenType.STRUCT => {
                const structdef = try allocator.create(ast.ProgInstructions);
                structdef.* = ast.ProgInstructions{ .StructDef = try lexeStructDef(&reader, allocator) };
                try ret.instructions.append(structdef);
            },
            else => {
                errors.bbcErrorExit("Unexpected token {}", .{token.type}, token.pos);
            },
        }
    }
    //ret.print();
    return ret;
}
