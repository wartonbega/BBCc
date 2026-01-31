const std = @import("std");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const errors = @import("errors.zig");
const types = @import("types.zig");
const analyser = @import("analyser.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const TokenType = parser.TokenType;

var stdout = std.io.getStdOut().writer();
var stderr = std.io.getStdErr().writer();

var globalAnonymousFunctionCounter: i32 = @intCast(0);

const lexerErrors = error{ noFollowingToken, unexpectedToken, unimplementedValueType };

pub fn lexerError(comptime val: []const u8, args: anytype, pos: parser.Location, tokReader: *tokenReader) !void {
    tokReader.errorLocation = pos;
    _ = std.fmt.bufPrint(&tokReader.errorMessage, val, args) catch {};
    try errors.bbcErrorExit(val, args, pos);
}

pub const tokenReader = struct {
    pos: i32,
    tokens: ArrayList(parser.Token),
    uri: []const u8,
    errorLocation: parser.Location,
    errorMessage: [1024]u8,

    pub fn new(toks: ArrayList(parser.Token), uri: []const u8) tokenReader {
        return .{
            .pos = 0,
            .tokens = toks,
            .uri = uri,
            .errorLocation = parser.getInbuiltLocation(),
            .errorMessage = [_]u8{0} ** 1024, // Allocate a bit of memory for error messages
        };
    }

    pub fn next(self: *tokenReader) !parser.Token {
        if (self.pos < self.tokens.items.len) {
            defer self.pos += 1;
            return self.tokens.items[@intCast(self.pos)];
        }
        return lexerErrors.noFollowingToken;
    }

    pub fn consume(self: *tokenReader, ttype: TokenType) !parser.Token {
        if (self.pos < self.tokens.items.len) {
            defer self.pos += 1;
            const ret = self.tokens.items[@intCast(self.pos)];
            if (ret.type != ttype and ttype != TokenType.ANYTYPE)
                try lexerError("Unexpected token '{s}', expected '{s}'\n", .{ ret.value, ttype.toString() }, ret.location, self);
            return ret;
        }
        try lexerError("Expected token '{s}' found nothing\n", .{ttype.toString()}, self.current().location, self);
        return .{ .type = TokenType.NULL, .value = "", .location = .{ .uri = self.uri, .range = .{
            .start = .{
                .line = 0,
                .character = 0,
            },
            .end = .{
                .line = 0,
                .character = 0,
            },
        } } };
    }

    pub fn peek(self: *tokenReader) !parser.Token {
        if (self.pos < self.tokens.items.len) {
            return self.tokens.items[@intCast(self.pos)];
        }
        return lexerErrors.noFollowingToken;
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
    //    | ((Type ident, )*) -> Type name
    //    | (!)? (*)* name
    const opt_error = (try reader.peek()).type == parser.TokenType.EXCLAM;
    if (opt_error)
        _ = try reader.next();
    var references_counter = @as(i32, 0);
    while (reader.canPeek() and (try reader.peek()).type == parser.TokenType.TIMES) {
        _ = try reader.next();
        references_counter += 1;
    }

    const base_name = (try reader.consume(TokenType.IDENT)).value;

    const ret = try allocator.create(ast.Type);
    ret.* = ast.Type{
        .base = ast.TypeBase{ .name = base_name },
        .err = opt_error,
        .references = references_counter,
    };
    return ret;
}

pub fn lexeTypeWithLocation(reader: *tokenReader, allocator: Allocator) !struct { t: *ast.Type, loc: parser.Location } {
    // Type :
    //    | ((Type ident, )*) -> Type name
    //    | (!)? (*)* name
    const opt_error = (try reader.peek()).type == parser.TokenType.EXCLAM;
    if (opt_error)
        _ = try reader.next();
    var references_counter = @as(i32, 0);
    while (reader.canPeek() and (try reader.peek()).type == parser.TokenType.TIMES) {
        _ = try reader.next();
        references_counter += 1;
    }

    const tok_name = try reader.consume(TokenType.IDENT);
    const base_name = tok_name.value;

    const ret = try allocator.create(ast.Type);
    ret.* = ast.Type{
        .base = ast.TypeBase{ .name = base_name },
        .err = opt_error,
        .references = references_counter,
    };
    return .{ .t = ret, .loc = tok_name.location };
}

pub fn lexeListedValue(reader: *tokenReader, allocator: Allocator) !ArrayList(*ast.Value) {
    // rule:
    // | (val, val, val,...)
    var ret = ArrayList(*ast.Value).init(allocator);
    _ = (try reader.consume(TokenType.O_PAR));
    while (reader.canPeek() and (try reader.peek()).type != TokenType.C_PAR) {
        const val = try lexeValue0(reader, allocator);
        try ret.append(val);

        //if ((try reader.peek()).type == TokenType.C_PAR)
        if ((try reader.peek()).type != TokenType.COMMA)
            break;
        _ = (try reader.consume(TokenType.COMMA));
    }
    _ = (try reader.consume(TokenType.C_PAR));
    return ret;
}

pub fn lexeValue7(reader: *tokenReader, allocator: Allocator) (std.mem.Allocator.Error || lexerErrors || std.fmt.ParseIntError || errors.bbcErrors)!*ast.Value {
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
    //  | print(value*)
    //  | println(value*)

    switch ((try reader.peek()).type) {
        TokenType.IF => {
            var conditions = ArrayList(*ast.Value).init(allocator);
            var scopes = ArrayList(*ast.Value).init(allocator);
            var else_scope: ?*ast.Value = null;

            const if_tok = (try reader.consume(TokenType.IF));
            const code_ref = if_tok.location;
            try conditions.append(try lexeValue0(reader, allocator));
            try scopes.append(try lexeValue0(reader, allocator));

            while (reader.canPeek() and (try reader.peek()).type == .ELIF) {
                _ = (try reader.consume(.ELIF));
                try conditions.append(try lexeValue0(reader, allocator));
                try scopes.append(try lexeValue0(reader, allocator));
            }
            if (reader.canPeek() and (try reader.peek()).type == .ELSE) {
                _ = (try reader.consume(.ELSE));
                else_scope = try lexeValue0(reader, allocator);
            }

            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.IfStmt);
            operation.* = ast.IfStmt{
                .conditions = conditions,
                .scopes = scopes,
                .elsescope = else_scope,
                .reference = code_ref.unionWith(if (else_scope) |escp| escp.getReference() else scopes.getLast().getReference()),
            };
            ret.* = ast.Value{ .If = operation };
            return ret;
        },
        TokenType.WHILE => {
            const while_tok = (try reader.consume(TokenType.WHILE));
            const code_ref = while_tok.location;

            const condition = try lexeValue0(reader, allocator);
            const exec = try lexeValue0(reader, allocator);

            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.WhileLoop);
            operation.* = ast.WhileLoop{
                .condition = condition,
                .exec = exec,
                .reference = code_ref.unionWith(exec.getReference()),
            };
            ret.* = ast.Value{ .While = operation };
            return ret;
        },
        TokenType.LET => {
            const let_kw = (try reader.consume(TokenType.LET));
            const name = (try reader.consume(TokenType.IDENT));
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.VarDeclaration);
            operation.* = ast.VarDeclaration{
                .mutable = false,
                .name = name.value,
                .reference = let_kw.location.unionWith(name.location),
            };
            ret.* = ast.Value{ .varDec = operation };
            return ret;
        },
        TokenType.IDENT => {
            const name = (try reader.consume(TokenType.IDENT));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .identifier = .{
                .name = name.value,
                .reference = name.location,
            } };
            return ret;
        },
        TokenType.AT => { // Struct usage
            const at_op = (try reader.consume(TokenType.AT));

            const ret = try allocator.create(ast.Value);

            const stc_init = try allocator.create(ast.StructInit);
            ret.* = ast.Value{ .structInit = stc_init };
            stc_init.habitants = .init(allocator);

            ret.structInit = stc_init;
            // The name is optionnal
            const stc_name = if (reader.canPeek() and (try reader.peek()).type == .IDENT) (try reader.consume(.IDENT)).value else @as([]const u8, "");
            stc_init.name = stc_name;

            _ = (try reader.consume(.O_CUR));
            while ((reader.canPeek()) and (try reader.peek()).type != .C_CUR) {
                // {a = 1, b = 2 ...}
                const name = (try reader.consume(.IDENT));
                _ = (try reader.consume(.EQUAL));
                const hab = try lexeValue0(reader, allocator);
                try stc_init.habitants.put(name.value, hab);
                if (reader.canPeek() and (try reader.peek()).type != .C_CUR)
                    _ = (try reader.consume(.COMMA));
            }
            const c_cur = (try reader.consume(.C_CUR));
            stc_init.reference = at_op.location.unionWith(c_cur.location);
            return ret;
        },
        TokenType.INTLIT => {
            const value = (try reader.consume(TokenType.INTLIT));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .intLit = .{
                .value = try std.fmt.parseInt(i32, value.value, 10),
                .reference = value.location,
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
            _ = (try reader.consume(TokenType.O_PAR));
            const val = try lexeValue0(reader, allocator);
            _ = (try reader.consume(TokenType.C_PAR));

            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .parenthesis = val };
            return ret;
        },
        TokenType.CHARLIT => {
            const val = (try reader.consume(TokenType.CHARLIT));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .charLit = .{
                .value = val.value[0],
                .reference = val.location,
            } };
            return ret;
        },
        TokenType.STRINGLIT => {
            const val = (try reader.consume(TokenType.STRINGLIT));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .stringLit = .{
                .value = val.value,
                .reference = val.location,
            } };
            return ret;
        },
        TokenType.TRUE, TokenType.FALSE => |boollit| {
            const val = (try reader.consume(boollit));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .boolLit = .{
                .value = boollit == .TRUE,
                .reference = val.location,
            } };
            return ret;
        },
        TokenType.NULL_KW => {
            const val = (try reader.consume(TokenType.NULL_KW));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .nullLit = .{ .reference = val.location } };
            return ret;
        },
        TokenType.FN_DEC => {
            const fn_dec_tok = (try reader.consume(TokenType.FN_DEC));
            const args = try lexeArguments(reader, allocator);
            const rettype_tuple = try lexeTypeWithLocation(reader, allocator);
            const rettype = rettype_tuple.t;
            const rettype_loc = rettype_tuple.loc;
            _ = (try reader.consume(TokenType.ARROW));
            const code = try lexeScope(reader, allocator);

            const ret = try allocator.create(ast.Value);
            const function = try allocator.create(ast.funcDef);
            function.* = ast.funcDef{
                .name = try std.fmt.allocPrint(allocator, "anonymousFunctionObject@{d}", .{globalAnonymousFunctionCounter}),
                .arguments = args,
                .code = code,
                .return_type = rettype,
                .return_type_ref = rettype_loc,
                .typeparam = .init(allocator),
                .reference = fn_dec_tok.location.unionWith(code.reference),
                .parent = null,
            };
            globalAnonymousFunctionCounter += 1;
            ret.* = ast.Value{ .function = function };
            return ret;
        },
        TokenType.FREE => {
            const free_tok = (try reader.consume(.FREE));
            const ret = try allocator.create(ast.Value);
            ret.* = ast.Value{ .freeKeyword = .{
                .val = try lexeValue7(reader, allocator),
                .reference = free_tok.location,
            } };
            return ret;
        },
        TokenType.PRINT, TokenType.PRINTLN => |tok| {
            const print_token = (try reader.consume(tok));
            const ret = try allocator.create(ast.Value);
            const args = try lexeListedValue(reader, allocator);
            ret.* = ast.Value{ .Print = .{
                .args = args,
                .ln = tok == .PRINTLN,
                .reference = if (args.items.len != 0) print_token.location.unionWith(args.getLast().getReference()) else print_token.location,
            } };
            return ret;
        },
        else => {
            std.debug.print("{s}\n", .{@tagName((try reader.peek()).type)});
            try lexerError("Unexpected token '{s}'", .{(try reader.peek()).type.toString()}, (try reader.peek()).location, reader);
            return lexerErrors.unexpectedToken;
        },
    }
}

pub fn lexeValue6(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value6:
    //  | value7 ((.IDENT) | (args...))* // for the future it will be (.IDENT | [value0])*

    // lhs is a variable because the system works by aggregating
    // all the attributes together.
    // For example, a.b.c derives in .c => (.b => (a))
    var lhs = try lexeValue7(reader, allocator);
    if (!reader.canPeek())
        return lhs;

    while (reader.canPeek() and switch ((try reader.peek()).type) {
        TokenType.DOT => |optype| blk: {
            _ = (try reader.consume(optype));
            const ident = (try reader.consume(TokenType.IDENT));
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.UnaryOperatorRight);
            operation.* = ast.UnaryOperatorRight{
                .expr = lhs,
                .operator = ast.RightUnaryOperators{ .pointAttr = ident.value },
                .reference = lhs.getReference().unionWith(ident.location),
            };
            ret.* = ast.Value{ .unaryOperatorRight = operation };
            lhs = ret;
            break :blk true;
        },
        TokenType.O_PAR => blk: {
            const args = try lexeListedValue(reader, allocator);
            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.Funcall);
            operation.* = ast.Funcall{
                .args = args,
                .func = lhs,
            };
            ret.* = ast.Value{ .funcall = operation };
            lhs = ret;
            break :blk true;
        },
        else => false,
    }) {}
    return lhs;
}

pub fn lexeValue4(reader: *tokenReader, allocator: Allocator) !*ast.Value {
    // Value4:
    //  | Value5 TIMES Value4
    //  | Value5 DIV   Value4
    //  | Value5
    const lhs = try lexeValue6(reader, allocator);
    if (!reader.canPeek())
        return lhs;
    switch ((try reader.peek()).type) {
        TokenType.TIMES, TokenType.DIV => |optype| {
            _ = (try reader.consume(optype));
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
                .reference = lhs.getReference().unionWith(rhs.getReference()),
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
            _ = (try reader.consume(optype));
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
                .reference = lhs.getReference().unionWith(rhs.getReference()),
            };
            ret.* = ast.Value{ .binaryOperator = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue2(reader: *tokenReader, allocator: Allocator) (lexerErrors || std.mem.Allocator.Error || std.fmt.ParseIntError || errors.bbcErrors)!*ast.Value {
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
            _ = (try reader.consume(optype));
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
                .reference = lhs.getReference().unionWith(rhs.getReference()),
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
            _ = (try reader.consume(optype));

            const error_name = (try reader.consume(TokenType.IDENT)).value;
            const scope = try lexeValue0(reader, allocator);

            const ret = try allocator.create(ast.Value);
            const operation = try allocator.create(ast.ErrCheck);
            operation.* = ast.ErrCheck{
                .err = error_name,
                .scope = scope,
                .value = lhs,
                .reference = lhs.getReference().unionWith(scope.getReference()),
            };

            ret.* = ast.Value{ .errorCheck = operation };
            return ret;
        },
        else => return lhs,
    }
    return lhs;
}

pub fn lexeValue0(reader: *tokenReader, allocator: Allocator) (lexerErrors || std.mem.Allocator.Error || std.fmt.ParseIntError || errors.bbcErrors)!*ast.Value {
    // value0: less priority
    //      assignations '='
    // value0:
    //  | Value1 = value0
    //  | value1
    const lhs = try lexeValue1(reader, allocator);
    if (reader.canPeek() and (try reader.peek()).type == TokenType.EQUAL) {
        _ = (try reader.consume(TokenType.EQUAL));
        const rhs = try lexeValue0(reader, allocator);
        const ret = try allocator.create(ast.Value);
        const assignement = try allocator.create(ast.Assignement);
        assignement.* = ast.Assignement{
            .lhs = lhs,
            .rhs = rhs,
            .reference = lhs.getReference().unionWith(rhs.getReference()),
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
    const ocur = (try reader.consume(TokenType.O_CUR));

    var values = ArrayList(*ast.Value).init(allocator);
    while (reader.canPeek() and (try reader.peek()).type != TokenType.C_CUR) {
        try values.append(try lexeValue0(reader, allocator));
    }
    const ccur = (try reader.consume(TokenType.C_CUR));

    const ret = try ast.Scope.init(
        values,
        ocur.location.unionWith(ccur.location),
        allocator,
    );
    return ret;
}

pub fn lexeArguments(reader: *tokenReader, allocator: Allocator) !ArrayList(*ast.Arguments) {
    _ = (try reader.consume(TokenType.O_PAR));
    var args = ArrayList(*ast.Arguments).init(allocator);
    // First checking if it is not an empty parenthesis
    if ((try reader.peek()).type != TokenType.C_PAR)
        while (true) {
            const argtype_tuple = try lexeTypeWithLocation(reader, allocator);
            const argtype = argtype_tuple.t;
            const argtype_ref = argtype_tuple.loc;
            const arg_name = (try reader.consume(TokenType.IDENT));

            const arg = try allocator.create(ast.Arguments);
            arg.name = arg_name.value;
            arg._type = argtype;
            arg.typeref = argtype_ref;
            arg.reference = arg_name.location;
            try args.append(arg);

            if ((try reader.peek()).type != TokenType.COMMA)
                break;
            _ = (try reader.consume(TokenType.COMMA));
        };
    _ = (try reader.consume(TokenType.C_PAR));
    return args;
}

pub fn lexeTypeParametrisation(reader: *tokenReader, allocator: Allocator) !ArrayList(ast.TypeParam) {
    // <Name: trait>
    var ret = ArrayList(ast.TypeParam).init(allocator);
    if (!reader.canPeek() or (try reader.peek()).type != .LESS_THAN)
        return ret;

    while (true) {
        const name = (try reader.consume(.IDENT));
        if (reader.canPeek() and (try reader.peek()).type == .COLON) {
            _ = (try reader.consume(.COLON));
            // The traits list
            var traits = ArrayList([]const u8).init(allocator);
            // Two ways for reading traits (with or without Parenthesis):
            // <Type1: (Add, Sub, ...), Type2: ..;> or <Type1: Add, Type2: ...>
            if (reader.canPeek() and (try reader.peek()).type == .O_PAR) {
                _ = (try reader.consume(.O_PAR));
                const first_trait = (try reader.consume(.IDENT));
                try traits.append(first_trait.value);
                while (reader.canPeek() and (try reader.peek()).type == .COMMA) {
                    _ = (try reader.consume(.COMMA));
                    const trait = (try reader.consume(.IDENT));
                    try traits.append(trait.value);
                }
                _ = (try reader.consume(.C_PAR));
            } else {
                const trait = (try reader.consume(.IDENT));
                try traits.append(trait.value);
            }
            try ret.append(.{
                .name = name.value,
                .traits = traits,
                .reference = name.location,
            });
        } else {
            // No traits specified
            try ret.append(.{
                .name = name.value,
                .traits = ArrayList([]const u8).init(allocator),
                .reference = name.location,
            });
        }
        if (reader.canPeek() and (try reader.peek()).type != .COMMA) {
            break;
        } else _ = (try reader.consume(.COMMA));
    }

    return ret;
}

pub fn lexeFuncdef(reader: *tokenReader, allocator: Allocator) !*ast.funcDef {
    // Rule:
    //      func <typeimple>? name ( args* ) retype { code }
    //          with { code } witch is parsed by scope
    const fn_kwd = (try reader.consume(.FN_DEC)); // func keyword

    const type_param = try lexeTypeParametrisation(reader, allocator);

    if (reader.canPeek() and ((try reader.peek()).type == .PRINT or (try reader.peek()).type == .PRINTLN)) {
        try lexerError("Unauthorized name '{s}' for a function, try '_{s}' if you really want to use that name", .{ (try reader.peek()).value, (try reader.peek()).value }, fn_kwd.location, reader);
    }

    const name = (try reader.consume(TokenType.IDENT));

    const args = try lexeArguments(reader, allocator);

    // now parsing return type
    const retype_tuple = try lexeTypeWithLocation(reader, allocator);
    const rettype = retype_tuple.t;
    const rettype_loc = retype_tuple.loc;

    // now parsing scope
    const scope = try lexeScope(reader, allocator);

    const ret = try allocator.create(ast.funcDef);

    ret.* = ast.funcDef{
        .arguments = args,
        .name = name.value,
        .return_type = rettype,
        .return_type_ref = rettype_loc,
        .code = scope,
        .typeparam = type_param,
        .reference = name.location.unionWith(scope.reference),
        .parent = null,
    };
    return ret;
}

pub fn lexeStructDef(reader: *tokenReader, allocator: Allocator) !*ast.structDef {
    // Struct def:
    // struct name { (Type name)* }
    const struct_keyword = (try reader.consume(.STRUCT)); // func keyword

    const st_name = (try reader.consume(.IDENT));

    _ = (try reader.consume(.O_CUR));

    const ret = try allocator.create(ast.structDef);
    ret.habitants = .init(allocator);

    ret.name = st_name.value;
    ret.methods = .init(allocator);
    ret.fields = .init(allocator);

    try ret.habitants.put("_count", (try types.CreateTypeInt(allocator, false)).decided);
    try ret.habitants.put("_size", (try types.CreateTypeInt(allocator, false)).decided);

    try ret.fields.append("_count");
    try ret.fields.append("_size"); // For now unused

    const stc_type = try allocator.create(ast.Type);
    stc_type.* = ast.Type{
        .base = .{ .name = st_name.value },
        .err = false,
        .references = 0,
    };

    while ((reader.canPeek()) and (try reader.peek()).type != .C_CUR) {
        switch ((try reader.peek()).type) {
            .FN_DEC => {
                var method = try lexeFuncdef(reader, allocator);
                method.parent = stc_type;
                try ret.habitants.put(method.name, try analyser.createFunctionSignature(method, allocator));
                try ret.fields.append(method.name);
                try ret.methods.put(method.name, method);
            },
            else => {
                const ttype = try lexeType(reader, allocator);
                const name = (try reader.consume(.IDENT));
                try ret.habitants.put(name.value, ttype);
                try ret.fields.append(name.value);
                if (reader.canPeek() and (try reader.peek()).type != .C_CUR)
                    _ = (try reader.consume(.COMMA));
            },
        }
    }
    const curl = (try reader.consume(.C_CUR));
    ret.reference = struct_keyword.location.unionWith(curl.location);
    return ret;
}

pub fn lexeProgram(tokens: ArrayList(parser.Token), allocator: Allocator, uri: []const u8) !*ast.Program {
    // Program:
    //   | FUNC name ( args* ) retype {code}
    var reader = tokenReader.new(tokens, uri);
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
            // TokenType.IMPORT => {
            //     // Preprocessor inst => just copies the imported files' code here
            //     const import_token = (try reader.consume(.IMPORT));
            //     const file_name = (try reader.consume(.STRINGLIT)).value;
            //     // File name can be the name of the file (relative path to the file)
            //     // or the name of a file in the include dir
            // },
            else => {
                try lexerError("Unexpected token {s}", .{token.type.toString()}, token.location, &reader);
            },
        }
    }
    //ret.print();
    return ret;
}

pub fn lexeProgramWithReader(reader: *tokenReader, allocator: Allocator) !*ast.Program {
    // Program:
    //   | FUNC name ( args* ) retype {code}

    var ret = try allocator.create(ast.Program);
    ret.* = ast.Program{ .instructions = ArrayList(*ast.ProgInstructions).init(allocator) };
    while (reader.canPeek()) {
        const token = try reader.peek();
        switch (token.type) {
            TokenType.FN_DEC => {
                const fndef = try allocator.create(ast.ProgInstructions);
                fndef.* = ast.ProgInstructions{ .FuncDef = try lexeFuncdef(reader, allocator) };
                try ret.instructions.append(fndef);
            },
            TokenType.STRUCT => {
                const structdef = try allocator.create(ast.ProgInstructions);
                structdef.* = ast.ProgInstructions{ .StructDef = try lexeStructDef(reader, allocator) };
                try ret.instructions.append(structdef);
            },
            else => {
                try lexerError("Unexpected token {s}", .{token.type.toString()}, token.location, reader);
            },
        }
    }
    return ret;
}
