const std = @import("std");
const lexer = @import("lexer.zig");
const ast = @import("ast.zig");
const Token = lexer.Token;
const TokenTag = lexer.TokenTag;
const Lexer = lexer.Lexer;

/// Parser error types
pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEof,
    InvalidType,
    InvalidAttribute,
    OutOfMemory,
};

/// Parser for BAML source code
pub const Parser = struct {
    tokens: []const Token,
    index: usize,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(ParserError),

    /// Initialize a parser with a token stream
    pub fn init(allocator: std.mem.Allocator, tokens: []const Token) Parser {
        return Parser{
            .tokens = tokens,
            .index = 0,
            .allocator = allocator,
            .errors = std.ArrayList(ParserError).init(allocator),
        };
    }

    /// Clean up parser resources
    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    /// Peek at the current token without consuming it
    pub fn peek(self: *const Parser) ?Token {
        if (self.index >= self.tokens.len) {
            return null;
        }
        return self.tokens[self.index];
    }

    /// Peek ahead at token at offset from current position
    pub fn peekAt(self: *const Parser, offset: usize) ?Token {
        const pos = self.index + offset;
        if (pos >= self.tokens.len) {
            return null;
        }
        return self.tokens[pos];
    }

    /// Consume and return the current token
    pub fn advance(self: *Parser) ?Token {
        if (self.index >= self.tokens.len) {
            return null;
        }
        const token = self.tokens[self.index];
        self.index += 1;
        return token;
    }

    /// Check if current token matches the given tag
    pub fn check(self: *const Parser, tag: TokenTag) bool {
        if (self.peek()) |token| {
            return token.tag == tag;
        }
        return false;
    }

    /// Check if current token matches any of the given tags
    pub fn checkAny(self: *const Parser, tags: []const TokenTag) bool {
        if (self.peek()) |token| {
            for (tags) |tag| {
                if (token.tag == tag) {
                    return true;
                }
            }
        }
        return false;
    }

    /// Consume token if it matches the given tag, otherwise return null
    pub fn match(self: *Parser, tag: TokenTag) ?Token {
        if (self.check(tag)) {
            return self.advance();
        }
        return null;
    }

    /// Consume token if it matches any of the given tags
    pub fn matchAny(self: *Parser, tags: []const TokenTag) ?Token {
        if (self.peek()) |token| {
            for (tags) |tag| {
                if (token.tag == tag) {
                    return self.advance();
                }
            }
        }
        return null;
    }

    /// Expect a token with the given tag, error if not found
    pub fn expect(self: *Parser, tag: TokenTag) ParseError!Token {
        if (self.match(tag)) |token| {
            return token;
        }

        const current = self.peek();
        if (current) |tok| {
            try self.addError("Expected {s}, got {s}", .{ @tagName(tag), @tagName(tok.tag) }, tok.line, tok.column);
        } else {
            try self.addError("Expected {s}, got EOF", .{@tagName(tag)}, 0, 0);
        }

        return ParseError.UnexpectedToken;
    }

    /// Skip newlines and comments (optionally capture docstring)
    pub fn skipTrivia(self: *Parser) void {
        while (self.peek()) |token| {
            switch (token.tag) {
                .newline, .comment, .docstring, .block_comment => {
                    _ = self.advance();
                },
                else => break,
            }
        }
    }

    /// Capture and skip trivia, returning last docstring if present
    pub fn skipTriviaCapturingDocstring(self: *Parser) ?[]const u8 {
        var last_docstring: ?[]const u8 = null;

        while (self.peek()) |token| {
            switch (token.tag) {
                .docstring => {
                    last_docstring = token.lexeme;
                    _ = self.advance();
                },
                .newline, .comment, .block_comment => {
                    _ = self.advance();
                },
                else => break,
            }
        }

        return last_docstring;
    }

    /// Add a parser error
    fn addError(self: *Parser, comptime fmt: []const u8, args: anytype, line: usize, column: usize) !void {
        const msg = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.errors.append(ParserError{
            .message = msg,
            .line = line,
            .column = column,
        });
    }

    /// Check if we're at the end of input
    pub fn isAtEnd(self: *const Parser) bool {
        return self.index >= self.tokens.len or self.check(.eof);
    }

    /// Parse a type expression
    pub fn parseTypeExpr(self: *Parser) ParseError!*ast.TypeExpr {
        self.skipTrivia();
        return self.parseUnionType();
    }

    /// Parse union type (Type | Type | ...)
    fn parseUnionType(self: *Parser) ParseError!*ast.TypeExpr {
        const left = try self.parsePostfixType();

        // Check for union operator |
        var types = std.ArrayList(*ast.TypeExpr).init(self.allocator);
        errdefer {
            for (types.items) |t| {
                t.deinit(self.allocator);
                self.allocator.destroy(t);
            }
            types.deinit();
        }

        try types.append(left);

        while (self.match(.pipe)) |_| {
            self.skipTrivia();
            const right = try self.parsePostfixType();
            try types.append(right);
        }

        // If we only have one type, return it directly
        if (types.items.len == 1) {
            const single = types.items[0];
            types.deinit();
            return single;
        }

        // Create union type
        const union_type = try self.allocator.create(ast.TypeExpr);
        union_type.* = ast.TypeExpr{
            .union_type = ast.UnionType{
                .types = types,
            },
        };
        return union_type;
    }

    /// Parse postfix type (array[], optional?)
    fn parsePostfixType(self: *Parser) ParseError!*ast.TypeExpr {
        var base = try self.parsePrimaryType();

        while (true) {
            self.skipTrivia();

            if (self.match(.lbracket)) |_| {
                // Array type: Type[]
                _ = try self.expect(.rbracket);
                const array_type = try self.allocator.create(ast.TypeExpr);
                array_type.* = ast.TypeExpr{ .array = base };
                base = array_type;
            } else if (self.match(.question)) |_| {
                // Optional type: Type?
                const optional_type = try self.allocator.create(ast.TypeExpr);
                optional_type.* = ast.TypeExpr{ .optional = base };
                base = optional_type;
            } else {
                break;
            }
        }

        return base;
    }

    /// Parse primary type (primitives, named types, map, literals)
    fn parsePrimaryType(self: *Parser) ParseError!*ast.TypeExpr {
        self.skipTrivia();

        const current = self.peek() orelse {
            try self.addError("Expected type expression, got EOF", .{}, 0, 0);
            return ParseError.UnexpectedEof;
        };

        // Primitive types
        if (self.matchPrimitiveType()) |prim_type| {
            const type_expr = try self.allocator.create(ast.TypeExpr);
            type_expr.* = ast.TypeExpr{ .primitive = prim_type };
            return type_expr;
        }

        // Map type: map<K, V>
        if (self.match(.type_map)) |_| {
            return self.parseMapType();
        }

        // Literal types (string, int, float, bool)
        if (self.check(.string_literal) or self.check(.int_literal) or
            self.check(.float_literal) or self.check(.bool_literal))
        {
            return self.parseLiteralType();
        }

        // Named type (identifier)
        if (self.match(.identifier)) |token| {
            const type_expr = try self.allocator.create(ast.TypeExpr);
            type_expr.* = ast.TypeExpr{ .named = token.lexeme };
            return type_expr;
        }

        try self.addError("Expected type expression", .{}, current.line, current.column);
        return ParseError.InvalidType;
    }

    /// Match and return primitive type if current token is a primitive type
    fn matchPrimitiveType(self: *Parser) ?ast.PrimitiveType {
        const current = self.peek() orelse return null;

        const prim_type = switch (current.tag) {
            .type_string => ast.PrimitiveType.string,
            .type_int => ast.PrimitiveType.int,
            .type_float => ast.PrimitiveType.float,
            .type_bool => ast.PrimitiveType.bool,
            .type_null => ast.PrimitiveType.null_type,
            .type_image => ast.PrimitiveType.image,
            .type_audio => ast.PrimitiveType.audio,
            .type_video => ast.PrimitiveType.video,
            .type_pdf => ast.PrimitiveType.pdf,
            else => return null,
        };

        _ = self.advance();
        return prim_type;
    }

    /// Parse map type: map<K, V>
    fn parseMapType(self: *Parser) ParseError!*ast.TypeExpr {
        self.skipTrivia();
        _ = try self.expect(.less_than);
        self.skipTrivia();

        const key_type = try self.parseTypeExpr();
        errdefer {
            key_type.deinit(self.allocator);
            self.allocator.destroy(key_type);
        }

        self.skipTrivia();
        _ = try self.expect(.comma);
        self.skipTrivia();

        const value_type = try self.parseTypeExpr();
        errdefer {
            value_type.deinit(self.allocator);
            self.allocator.destroy(value_type);
        }

        self.skipTrivia();
        _ = try self.expect(.greater_than);

        const map_type = try self.allocator.create(ast.TypeExpr);
        map_type.* = ast.TypeExpr{
            .map = ast.MapType{
                .key_type = key_type,
                .value_type = value_type,
            },
        };
        return map_type;
    }

    /// Parse literal type ("value" | 123 | true)
    fn parseLiteralType(self: *Parser) ParseError!*ast.TypeExpr {
        const token = self.advance() orelse {
            try self.addError("Expected literal value", .{}, 0, 0);
            return ParseError.UnexpectedEof;
        };

        const literal = switch (token.tag) {
            .string_literal => ast.LiteralValue{ .string = token.lexeme },
            .int_literal => blk: {
                const value = std.fmt.parseInt(i64, token.lexeme, 10) catch {
                    try self.addError("Invalid integer literal: {s}", .{token.lexeme}, token.line, token.column);
                    return ParseError.InvalidType;
                };
                break :blk ast.LiteralValue{ .int = value };
            },
            .float_literal => blk: {
                const value = std.fmt.parseFloat(f64, token.lexeme) catch {
                    try self.addError("Invalid float literal: {s}", .{token.lexeme}, token.line, token.column);
                    return ParseError.InvalidType;
                };
                break :blk ast.LiteralValue{ .float = value };
            },
            .bool_literal => blk: {
                const value = std.mem.eql(u8, token.lexeme, "true");
                break :blk ast.LiteralValue{ .bool = value };
            },
            else => {
                try self.addError("Expected literal value, got {s}", .{@tagName(token.tag)}, token.line, token.column);
                return ParseError.InvalidType;
            },
        };

        const type_expr = try self.allocator.create(ast.TypeExpr);
        type_expr.* = ast.TypeExpr{ .literal = literal };
        return type_expr;
    }

    /// Parse attribute: @name(...) or @@name(...)
    pub fn parseAttribute(self: *Parser) ParseError!ast.Attribute {
        self.skipTrivia();

        // Check for @ or @@
        const is_class_level = if (self.match(.double_at)) |_| true else if (self.match(.at)) |_| false else {
            const current = self.peek() orelse {
                try self.addError("Expected @ or @@", .{}, 0, 0);
                return ParseError.UnexpectedEof;
            };
            try self.addError("Expected @ or @@", .{}, current.line, current.column);
            return ParseError.InvalidAttribute;
        };

        const location_token = self.peek() orelse {
            try self.addError("Expected attribute name", .{}, 0, 0);
            return ParseError.UnexpectedEof;
        };

        const location = ast.Location{
            .line = location_token.line,
            .column = location_token.column,
        };

        // Get attribute name
        const name_token = try self.expect(.identifier);

        var args = std.ArrayList(ast.Value).init(self.allocator);
        errdefer {
            for (args.items) |*arg| {
                arg.deinit(self.allocator);
            }
            args.deinit();
        }

        self.skipTrivia();

        // Parse optional arguments: (arg1, arg2, ...)
        if (self.match(.lparen)) |_| {
            self.skipTrivia();

            // Empty argument list
            if (self.match(.rparen)) |_| {
                return ast.Attribute{
                    .name = name_token.lexeme,
                    .is_class_level = is_class_level,
                    .args = args,
                    .location = location,
                };
            }

            // Parse arguments
            while (true) {
                self.skipTrivia();
                const arg = try self.parseValue();
                try args.append(arg);

                self.skipTrivia();
                if (self.match(.rparen)) |_| {
                    break;
                }

                _ = try self.expect(.comma);
            }
        }

        return ast.Attribute{
            .name = name_token.lexeme,
            .is_class_level = is_class_level,
            .args = args,
            .location = location,
        };
    }

    /// Parse a value (string, number, bool, array, object, env var)
    pub fn parseValue(self: *Parser) ParseError!ast.Value {
        self.skipTrivia();

        const current = self.peek() orelse {
            try self.addError("Expected value", .{}, 0, 0);
            return ParseError.UnexpectedEof;
        };

        switch (current.tag) {
            .string_literal => {
                const token = self.advance().?;
                return ast.Value{ .string = token.lexeme };
            },
            .int_literal => {
                const token = self.advance().?;
                const value = std.fmt.parseInt(i64, token.lexeme, 10) catch {
                    try self.addError("Invalid integer: {s}", .{token.lexeme}, token.line, token.column);
                    return ParseError.InvalidType;
                };
                return ast.Value{ .int = value };
            },
            .float_literal => {
                const token = self.advance().?;
                const value = std.fmt.parseFloat(f64, token.lexeme) catch {
                    try self.addError("Invalid float: {s}", .{token.lexeme}, token.line, token.column);
                    return ParseError.InvalidType;
                };
                return ast.Value{ .float = value };
            },
            .bool_literal => {
                const token = self.advance().?;
                const value = std.mem.eql(u8, token.lexeme, "true");
                return ast.Value{ .bool = value };
            },
            .type_null => {
                _ = self.advance();
                return ast.Value{ .null_value = {} };
            },
            .lbracket => {
                return self.parseArrayValue();
            },
            .lbrace => {
                return self.parseObjectValue();
            },
            .env => {
                return self.parseEnvVar();
            },
            else => {
                try self.addError("Expected value, got {s}", .{@tagName(current.tag)}, current.line, current.column);
                return ParseError.UnexpectedToken;
            },
        }
    }

    /// Parse array value: [val1, val2, ...]
    fn parseArrayValue(self: *Parser) ParseError!ast.Value {
        _ = try self.expect(.lbracket);
        self.skipTrivia();

        var items = std.ArrayList(ast.Value).init(self.allocator);
        errdefer {
            for (items.items) |*item| {
                item.deinit(self.allocator);
            }
            items.deinit();
        }

        // Empty array
        if (self.match(.rbracket)) |_| {
            return ast.Value{ .array = items };
        }

        // Parse items
        while (true) {
            self.skipTrivia();
            const item = try self.parseValue();
            try items.append(item);

            self.skipTrivia();
            if (self.match(.rbracket)) |_| {
                break;
            }

            _ = try self.expect(.comma);
        }

        return ast.Value{ .array = items };
    }

    /// Parse object value: { key: val, ... }
    fn parseObjectValue(self: *Parser) ParseError!ast.Value {
        _ = try self.expect(.lbrace);
        self.skipTrivia();

        var obj = std.StringHashMap(ast.Value).init(self.allocator);
        errdefer {
            var it = obj.iterator();
            while (it.next()) |entry| {
                var value = entry.value_ptr.*;
                value.deinit(self.allocator);
            }
            obj.deinit();
        }

        // Empty object
        if (self.match(.rbrace)) |_| {
            return ast.Value{ .object = obj };
        }

        // Parse key-value pairs
        while (true) {
            self.skipTrivia();

            // Key can be identifier or string
            const key = if (self.match(.identifier)) |tok|
                tok.lexeme
            else if (self.match(.string_literal)) |tok|
                tok.lexeme
            else {
                const current = self.peek() orelse {
                    try self.addError("Expected object key", .{}, 0, 0);
                    return ParseError.UnexpectedEof;
                };
                try self.addError("Expected object key", .{}, current.line, current.column);
                return ParseError.UnexpectedToken;
            };

            self.skipTrivia();
            _ = try self.expect(.colon);
            self.skipTrivia();

            const value = try self.parseValue();
            try obj.put(key, value);

            self.skipTrivia();
            if (self.match(.rbrace)) |_| {
                break;
            }

            _ = try self.expect(.comma);
        }

        return ast.Value{ .object = obj };
    }

    /// Parse environment variable: env.VAR_NAME
    fn parseEnvVar(self: *Parser) ParseError!ast.Value {
        _ = try self.expect(.env);

        // In BAML, env variables are written as env.VAR_NAME
        // We need to handle this as two tokens: "env" and identifier
        // For now, we'll store just the identifier part
        const var_name = try self.expect(.identifier);
        return ast.Value{ .env_var = var_name.lexeme };
    }

    /// Parse class declaration: class Name { ... }
    pub fn parseClassDecl(self: *Parser) ParseError!ast.ClassDecl {
        // Capture docstring before class keyword
        const docstring = self.skipTriviaCapturingDocstring();

        const class_token = try self.expect(.keyword_class);
        const location = ast.Location{
            .line = class_token.line,
            .column = class_token.column,
        };

        self.skipTrivia();
        const name_token = try self.expect(.identifier);

        self.skipTrivia();
        _ = try self.expect(.lbrace);

        var class_decl = ast.ClassDecl.init(self.allocator, name_token.lexeme, location);
        class_decl.docstring = docstring;

        errdefer class_decl.deinit(self.allocator);

        // Parse properties and class-level attributes
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            self.skipTrivia();

            if (self.check(.rbrace)) break;

            // Check for class-level attribute (@@)
            if (self.check(.double_at)) {
                const attr = try self.parseAttribute();
                try class_decl.attributes.append(attr);
                continue;
            }

            // Otherwise, parse property
            const prop = try self.parseProperty();
            try class_decl.properties.append(prop);
        }

        self.skipTrivia();
        _ = try self.expect(.rbrace);

        return class_decl;
    }

    /// Parse class property: name Type @attr1 @attr2
    fn parseProperty(self: *Parser) ParseError!ast.Property {
        // Capture docstring before property
        const docstring = self.skipTriviaCapturingDocstring();

        const name_token = try self.expect(.identifier);
        const location = ast.Location{
            .line = name_token.line,
            .column = name_token.column,
        };

        self.skipTrivia();
        const type_expr = try self.parseTypeExpr();
        errdefer {
            type_expr.deinit(self.allocator);
            self.allocator.destroy(type_expr);
        }

        var attributes = std.ArrayList(ast.Attribute).init(self.allocator);
        errdefer {
            for (attributes.items) |*attr| {
                attr.deinit(self.allocator);
            }
            attributes.deinit();
        }

        // Parse property-level attributes
        while (self.check(.at)) {
            self.skipTrivia();
            const attr = try self.parseAttribute();
            try attributes.append(attr);
            self.skipTrivia();
        }

        return ast.Property{
            .name = name_token.lexeme,
            .type_expr = type_expr,
            .attributes = attributes,
            .docstring = docstring,
            .location = location,
        };
    }

    /// Parse enum declaration: enum Name { ... }
    pub fn parseEnumDecl(self: *Parser) ParseError!ast.EnumDecl {
        // Capture docstring before enum keyword
        const docstring = self.skipTriviaCapturingDocstring();

        const enum_token = try self.expect(.keyword_enum);
        const location = ast.Location{
            .line = enum_token.line,
            .column = enum_token.column,
        };

        self.skipTrivia();
        const name_token = try self.expect(.identifier);

        self.skipTrivia();
        _ = try self.expect(.lbrace);

        var enum_decl = ast.EnumDecl.init(self.allocator, name_token.lexeme, location);
        enum_decl.docstring = docstring;

        errdefer enum_decl.deinit(self.allocator);

        // Parse enum values and enum-level attributes
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            self.skipTrivia();

            if (self.check(.rbrace)) break;

            // Check for enum-level attribute (@@)
            if (self.check(.double_at)) {
                const attr = try self.parseAttribute();
                try enum_decl.attributes.append(attr);
                continue;
            }

            // Otherwise, parse enum value
            const val = try self.parseEnumValue();
            try enum_decl.values.append(val);
        }

        self.skipTrivia();
        _ = try self.expect(.rbrace);

        return enum_decl;
    }

    /// Parse enum value: ValueName @attr1 @attr2
    fn parseEnumValue(self: *Parser) ParseError!ast.EnumValue {
        // Capture docstring before enum value
        const docstring = self.skipTriviaCapturingDocstring();

        const name_token = try self.expect(.identifier);
        const location = ast.Location{
            .line = name_token.line,
            .column = name_token.column,
        };

        var attributes = std.ArrayList(ast.Attribute).init(self.allocator);
        errdefer {
            for (attributes.items) |*attr| {
                attr.deinit(self.allocator);
            }
            attributes.deinit();
        }

        // Parse value-level attributes
        while (self.check(.at)) {
            self.skipTrivia();
            const attr = try self.parseAttribute();
            try attributes.append(attr);
            self.skipTrivia();
        }

        return ast.EnumValue{
            .name = name_token.lexeme,
            .attributes = attributes,
            .docstring = docstring,
            .location = location,
        };
    }

    /// Parse function declaration: function Name(params) -> ReturnType { client ... prompt ... }
    pub fn parseFunctionDecl(self: *Parser) ParseError!ast.FunctionDecl {
        // Capture docstring before function keyword
        const docstring = self.skipTriviaCapturingDocstring();

        const function_token = try self.expect(.keyword_function);
        const location = ast.Location{
            .line = function_token.line,
            .column = function_token.column,
        };

        self.skipTrivia();
        const name_token = try self.expect(.identifier);

        var function_decl = ast.FunctionDecl.init(self.allocator, name_token.lexeme, location);
        function_decl.docstring = docstring;

        errdefer function_decl.deinit(self.allocator);

        // Parse parameters: (param1: Type, param2: Type)
        self.skipTrivia();
        _ = try self.expect(.lparen);

        // Parse parameter list
        while (!self.check(.rparen) and !self.isAtEnd()) {
            self.skipTrivia();

            if (self.check(.rparen)) break;

            const param = try self.parseParameter();
            try function_decl.parameters.append(param);

            self.skipTrivia();
            if (self.match(.comma)) |_| {
                continue;
            } else if (self.check(.rparen)) {
                break;
            } else {
                const current = self.peek() orelse {
                    try self.addError("Expected ',' or ')' in parameter list", .{}, 0, 0);
                    return ParseError.UnexpectedEof;
                };
                try self.addError("Expected ',' or ')' in parameter list", .{}, current.line, current.column);
                return ParseError.UnexpectedToken;
            }
        }

        self.skipTrivia();
        _ = try self.expect(.rparen);

        // Parse return type: -> Type
        self.skipTrivia();
        _ = try self.expect(.arrow);
        self.skipTrivia();

        const return_type = try self.parseTypeExpr();
        function_decl.return_type = return_type;

        // Parse function body: { client ... prompt ... }
        self.skipTrivia();
        _ = try self.expect(.lbrace);

        // Parse client and prompt (and optionally attributes)
        while (!self.check(.rbrace) and !self.isAtEnd()) {
            self.skipTrivia();

            if (self.check(.rbrace)) break;

            // Check for function-level attribute (@)
            if (self.check(.at)) {
                const attr = try self.parseAttribute();
                try function_decl.attributes.append(attr);
                continue;
            }

            // Check for 'client' keyword
            if (self.match(.keyword_client)) |_| {
                self.skipTrivia();
                const client_token = try self.expect(.string_literal);
                function_decl.client = client_token.lexeme;
                continue;
            }

            // Check for 'prompt' keyword
            if (self.match(.keyword_prompt)) |_| {
                self.skipTrivia();
                const prompt_token = try self.expect(.block_string);
                function_decl.prompt = prompt_token.lexeme;
                continue;
            }

            // Unknown token in function body
            const current = self.peek() orelse {
                try self.addError("Expected 'client', 'prompt', or '@' in function body", .{}, 0, 0);
                return ParseError.UnexpectedEof;
            };
            try self.addError("Expected 'client', 'prompt', or '@' in function body, got {s}", .{@tagName(current.tag)}, current.line, current.column);
            return ParseError.UnexpectedToken;
        }

        self.skipTrivia();
        _ = try self.expect(.rbrace);

        return function_decl;
    }

    /// Parse function parameter: name: Type
    fn parseParameter(self: *Parser) ParseError!ast.Parameter {
        self.skipTrivia();

        const name_token = try self.expect(.identifier);
        const location = ast.Location{
            .line = name_token.line,
            .column = name_token.column,
        };

        self.skipTrivia();
        _ = try self.expect(.colon);
        self.skipTrivia();

        const type_expr = try self.parseTypeExpr();

        return ast.Parameter{
            .name = name_token.lexeme,
            .type_expr = type_expr,
            .location = location,
        };
    }
};

/// Parser error information
pub const ParserError = struct {
    message: []const u8,
    line: usize,
    column: usize,
};

// ============================================================================
// TESTS
// ============================================================================

test "Parser: Initialize and cleanup" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 6 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    try std.testing.expect(parser.tokens.len == 2);
    try std.testing.expect(parser.index == 0);
}

test "Parser: peek and advance" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 13 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const first = parser.peek().?;
    try std.testing.expect(first.tag == .keyword_class);

    _ = parser.advance();
    const second = parser.peek().?;
    try std.testing.expect(second.tag == .identifier);
}

test "Parser: check and match" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 13 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    try std.testing.expect(parser.check(.keyword_class));
    try std.testing.expect(!parser.check(.identifier));

    const matched = parser.match(.keyword_class);
    try std.testing.expect(matched != null);
    try std.testing.expect(matched.?.tag == .keyword_class);

    try std.testing.expect(parser.check(.identifier));
}

test "Parser: Parse primitive type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .type_string, .lexeme = "string", .line = 1, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 7 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .primitive);
    try std.testing.expect(type_expr.primitive == .string);
}

test "Parser: Parse array type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .type_int, .lexeme = "int", .line = 1, .column = 1 },
        Token{ .tag = .lbracket, .lexeme = "[", .line = 1, .column = 4 },
        Token{ .tag = .rbracket, .lexeme = "]", .line = 1, .column = 5 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 6 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .array);
    try std.testing.expect(type_expr.array.* == .primitive);
    try std.testing.expect(type_expr.array.primitive == .int);
}

test "Parser: Parse optional type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .type_string, .lexeme = "string", .line = 1, .column = 1 },
        Token{ .tag = .question, .lexeme = "?", .line = 1, .column = 7 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 8 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .optional);
    try std.testing.expect(type_expr.optional.* == .primitive);
    try std.testing.expect(type_expr.optional.primitive == .string);
}

test "Parser: Parse union type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .type_string, .lexeme = "string", .line = 1, .column = 1 },
        Token{ .tag = .pipe, .lexeme = "|", .line = 1, .column = 8 },
        Token{ .tag = .type_int, .lexeme = "int", .line = 1, .column = 10 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 13 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .union_type);
    try std.testing.expect(type_expr.union_type.types.items.len == 2);
}

test "Parser: Parse map type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .type_map, .lexeme = "map", .line = 1, .column = 1 },
        Token{ .tag = .less_than, .lexeme = "<", .line = 1, .column = 4 },
        Token{ .tag = .type_string, .lexeme = "string", .line = 1, .column = 5 },
        Token{ .tag = .comma, .lexeme = ",", .line = 1, .column = 11 },
        Token{ .tag = .type_int, .lexeme = "int", .line = 1, .column = 13 },
        Token{ .tag = .greater_than, .lexeme = ">", .line = 1, .column = 16 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 17 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .map);
    try std.testing.expect(type_expr.map.key_type.* == .primitive);
    try std.testing.expect(type_expr.map.key_type.primitive == .string);
    try std.testing.expect(type_expr.map.value_type.* == .primitive);
    try std.testing.expect(type_expr.map.value_type.primitive == .int);
}

test "Parser: Parse named type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 7 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .named);
    try std.testing.expectEqualStrings("Person", type_expr.named);
}

test "Parser: Parse complex type (string | int)[]?" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .type_string, .lexeme = "string", .line = 1, .column = 1 },
        Token{ .tag = .pipe, .lexeme = "|", .line = 1, .column = 8 },
        Token{ .tag = .type_int, .lexeme = "int", .line = 1, .column = 10 },
        Token{ .tag = .lbracket, .lexeme = "[", .line = 1, .column = 13 },
        Token{ .tag = .rbracket, .lexeme = "]", .line = 1, .column = 14 },
        Token{ .tag = .question, .lexeme = "?", .line = 1, .column = 15 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 16 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    // Should be: optional(array(union(string, int)))
    try std.testing.expect(type_expr.* == .optional);
    try std.testing.expect(type_expr.optional.* == .array);
    try std.testing.expect(type_expr.optional.array.* == .union_type);
}

test "Parser: Parse attribute without args" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .at, .lexeme = "@", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "skip", .line = 1, .column = 2 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 6 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var attr = try parser.parseAttribute();
    defer attr.deinit(allocator);

    try std.testing.expectEqualStrings("skip", attr.name);
    try std.testing.expect(!attr.is_class_level);
    try std.testing.expect(attr.args.items.len == 0);
}

test "Parser: Parse attribute with args" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .at, .lexeme = "@", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "alias", .line = 1, .column = 2 },
        Token{ .tag = .lparen, .lexeme = "(", .line = 1, .column = 7 },
        Token{ .tag = .string_literal, .lexeme = "full_name", .line = 1, .column = 8 },
        Token{ .tag = .rparen, .lexeme = ")", .line = 1, .column = 19 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 20 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var attr = try parser.parseAttribute();
    defer attr.deinit(allocator);

    try std.testing.expectEqualStrings("alias", attr.name);
    try std.testing.expect(!attr.is_class_level);
    try std.testing.expect(attr.args.items.len == 1);
    try std.testing.expect(attr.args.items[0] == .string);
    try std.testing.expectEqualStrings("full_name", attr.args.items[0].string);
}

test "Parser: Parse class-level attribute" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .double_at, .lexeme = "@@", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "dynamic", .line = 1, .column = 3 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 10 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var attr = try parser.parseAttribute();
    defer attr.deinit(allocator);

    try std.testing.expectEqualStrings("dynamic", attr.name);
    try std.testing.expect(attr.is_class_level);
}

test "Parser: Parse string literal type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .string_literal, .lexeme = "active", .line = 1, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 9 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .literal);
    try std.testing.expect(type_expr.literal == .string);
    try std.testing.expectEqualStrings("active", type_expr.literal.string);
}

test "Parser: Parse int literal type" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .int_literal, .lexeme = "42", .line = 1, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 3 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const type_expr = try parser.parseTypeExpr();
    defer {
        type_expr.deinit(allocator);
        allocator.destroy(type_expr);
    }

    try std.testing.expect(type_expr.* == .literal);
    try std.testing.expect(type_expr.literal == .int);
    try std.testing.expect(type_expr.literal.int == 42);
}

test "Parser: Parse array value" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .lbracket, .lexeme = "[", .line = 1, .column = 1 },
        Token{ .tag = .int_literal, .lexeme = "1", .line = 1, .column = 2 },
        Token{ .tag = .comma, .lexeme = ",", .line = 1, .column = 3 },
        Token{ .tag = .int_literal, .lexeme = "2", .line = 1, .column = 5 },
        Token{ .tag = .comma, .lexeme = ",", .line = 1, .column = 6 },
        Token{ .tag = .int_literal, .lexeme = "3", .line = 1, .column = 8 },
        Token{ .tag = .rbracket, .lexeme = "]", .line = 1, .column = 9 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 10 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var value = try parser.parseValue();
    defer value.deinit(allocator);

    try std.testing.expect(value == .array);
    try std.testing.expect(value.array.items.len == 3);
    try std.testing.expect(value.array.items[0].int == 1);
    try std.testing.expect(value.array.items[1].int == 2);
    try std.testing.expect(value.array.items[2].int == 3);
}

test "Parser: Parse object value" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "name", .line = 1, .column = 3 },
        Token{ .tag = .colon, .lexeme = ":", .line = 1, .column = 7 },
        Token{ .tag = .string_literal, .lexeme = "John", .line = 1, .column = 9 },
        Token{ .tag = .comma, .lexeme = ",", .line = 1, .column = 15 },
        Token{ .tag = .identifier, .lexeme = "age", .line = 1, .column = 17 },
        Token{ .tag = .colon, .lexeme = ":", .line = 1, .column = 20 },
        Token{ .tag = .int_literal, .lexeme = "30", .line = 1, .column = 22 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 1, .column = 24 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 25 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var value = try parser.parseValue();
    defer value.deinit(allocator);

    try std.testing.expect(value == .object);
    try std.testing.expect(value.object.count() == 2);

    const name = value.object.get("name").?;
    try std.testing.expect(name == .string);
    try std.testing.expectEqualStrings("John", name.string);

    const age = value.object.get("age").?;
    try std.testing.expect(age == .int);
    try std.testing.expect(age.int == 30);
}

test "Parser: Skip trivia (newlines and comments)" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .newline, .lexeme = "\n", .line = 1, .column = 1 },
        Token{ .tag = .comment, .lexeme = " comment", .line = 2, .column = 1 },
        Token{ .tag = .newline, .lexeme = "\n", .line = 2, .column = 10 },
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 3, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 3, .column = 6 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    parser.skipTrivia();
    const token = parser.peek().?;
    try std.testing.expect(token.tag == .keyword_class);
}

test "Parser: Parse simple class" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 14 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 1, .column = 15 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 16 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Person", class_decl.name);
    try std.testing.expect(class_decl.properties.items.len == 0);
    try std.testing.expect(class_decl.attributes.items.len == 0);
}

test "Parser: Parse class with properties" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 14 },
        // name string
        Token{ .tag = .identifier, .lexeme = "name", .line = 2, .column = 3 },
        Token{ .tag = .type_string, .lexeme = "string", .line = 2, .column = 8 },
        // age int?
        Token{ .tag = .identifier, .lexeme = "age", .line = 3, .column = 3 },
        Token{ .tag = .type_int, .lexeme = "int", .line = 3, .column = 7 },
        Token{ .tag = .question, .lexeme = "?", .line = 3, .column = 10 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 4, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 4, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Person", class_decl.name);
    try std.testing.expect(class_decl.properties.items.len == 2);

    // Check first property: name string
    const prop1 = class_decl.properties.items[0];
    try std.testing.expectEqualStrings("name", prop1.name);
    try std.testing.expect(prop1.type_expr.* == .primitive);
    try std.testing.expect(prop1.type_expr.primitive == .string);

    // Check second property: age int?
    const prop2 = class_decl.properties.items[1];
    try std.testing.expectEqualStrings("age", prop2.name);
    try std.testing.expect(prop2.type_expr.* == .optional);
    try std.testing.expect(prop2.type_expr.optional.* == .primitive);
    try std.testing.expect(prop2.type_expr.optional.primitive == .int);
}

test "Parser: Parse class property with attributes" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 14 },
        // email string @alias("email_address")
        Token{ .tag = .identifier, .lexeme = "email", .line = 2, .column = 3 },
        Token{ .tag = .type_string, .lexeme = "string", .line = 2, .column = 9 },
        Token{ .tag = .at, .lexeme = "@", .line = 2, .column = 16 },
        Token{ .tag = .identifier, .lexeme = "alias", .line = 2, .column = 17 },
        Token{ .tag = .lparen, .lexeme = "(", .line = 2, .column = 22 },
        Token{ .tag = .string_literal, .lexeme = "email_address", .line = 2, .column = 23 },
        Token{ .tag = .rparen, .lexeme = ")", .line = 2, .column = 38 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 3, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 3, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expect(class_decl.properties.items.len == 1);
    const prop = class_decl.properties.items[0];
    try std.testing.expectEqualStrings("email", prop.name);
    try std.testing.expect(prop.attributes.items.len == 1);

    const attr = prop.attributes.items[0];
    try std.testing.expectEqualStrings("alias", attr.name);
    try std.testing.expect(!attr.is_class_level);
    try std.testing.expect(attr.args.items.len == 1);
    try std.testing.expectEqualStrings("email_address", attr.args.items[0].string);
}

test "Parser: Parse class with class-level attribute" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 14 },
        Token{ .tag = .identifier, .lexeme = "name", .line = 2, .column = 3 },
        Token{ .tag = .type_string, .lexeme = "string", .line = 2, .column = 8 },
        // @@dynamic
        Token{ .tag = .double_at, .lexeme = "@@", .line = 3, .column = 3 },
        Token{ .tag = .identifier, .lexeme = "dynamic", .line = 3, .column = 5 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 4, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 4, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Person", class_decl.name);
    try std.testing.expect(class_decl.properties.items.len == 1);
    try std.testing.expect(class_decl.attributes.items.len == 1);

    const attr = class_decl.attributes.items[0];
    try std.testing.expectEqualStrings("dynamic", attr.name);
    try std.testing.expect(attr.is_class_level);
}

test "Parser: Parse class with docstring" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .docstring, .lexeme = " A person entity", .line = 1, .column = 1 },
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 2, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 2, .column = 7 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 2, .column = 14 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 3, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 3, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Person", class_decl.name);
    try std.testing.expect(class_decl.docstring != null);
    try std.testing.expectEqualStrings(" A person entity", class_decl.docstring.?);
}

test "Parser: Parse simple enum" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_enum, .lexeme = "enum", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Status", .line = 1, .column = 6 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 13 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 1, .column = 14 },
        Token{ .tag = .eof, .lexeme = "", .line = 1, .column = 15 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Status", enum_decl.name);
    try std.testing.expect(enum_decl.values.items.len == 0);
    try std.testing.expect(enum_decl.attributes.items.len == 0);
}

test "Parser: Parse enum with values" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_enum, .lexeme = "enum", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Status", .line = 1, .column = 6 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 13 },
        Token{ .tag = .identifier, .lexeme = "Active", .line = 2, .column = 3 },
        Token{ .tag = .identifier, .lexeme = "Inactive", .line = 3, .column = 3 },
        Token{ .tag = .identifier, .lexeme = "Pending", .line = 4, .column = 3 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 5, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 5, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Status", enum_decl.name);
    try std.testing.expect(enum_decl.values.items.len == 3);

    try std.testing.expectEqualStrings("Active", enum_decl.values.items[0].name);
    try std.testing.expectEqualStrings("Inactive", enum_decl.values.items[1].name);
    try std.testing.expectEqualStrings("Pending", enum_decl.values.items[2].name);
}

test "Parser: Parse enum value with attributes" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_enum, .lexeme = "enum", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Status", .line = 1, .column = 6 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 13 },
        // Active @alias("currently_active") @description("Active status")
        Token{ .tag = .identifier, .lexeme = "Active", .line = 2, .column = 3 },
        Token{ .tag = .at, .lexeme = "@", .line = 2, .column = 10 },
        Token{ .tag = .identifier, .lexeme = "alias", .line = 2, .column = 11 },
        Token{ .tag = .lparen, .lexeme = "(", .line = 2, .column = 16 },
        Token{ .tag = .string_literal, .lexeme = "currently_active", .line = 2, .column = 17 },
        Token{ .tag = .rparen, .lexeme = ")", .line = 2, .column = 35 },
        Token{ .tag = .at, .lexeme = "@", .line = 2, .column = 37 },
        Token{ .tag = .identifier, .lexeme = "description", .line = 2, .column = 38 },
        Token{ .tag = .lparen, .lexeme = "(", .line = 2, .column = 49 },
        Token{ .tag = .string_literal, .lexeme = "Active status", .line = 2, .column = 50 },
        Token{ .tag = .rparen, .lexeme = ")", .line = 2, .column = 65 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 3, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 3, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expect(enum_decl.values.items.len == 1);
    const val = enum_decl.values.items[0];
    try std.testing.expectEqualStrings("Active", val.name);
    try std.testing.expect(val.attributes.items.len == 2);

    const attr1 = val.attributes.items[0];
    try std.testing.expectEqualStrings("alias", attr1.name);
    try std.testing.expectEqualStrings("currently_active", attr1.args.items[0].string);

    const attr2 = val.attributes.items[1];
    try std.testing.expectEqualStrings("description", attr2.name);
    try std.testing.expectEqualStrings("Active status", attr2.args.items[0].string);
}

test "Parser: Parse enum with enum-level attribute" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_enum, .lexeme = "enum", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Status", .line = 1, .column = 6 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 13 },
        Token{ .tag = .identifier, .lexeme = "Active", .line = 2, .column = 3 },
        // @@dynamic
        Token{ .tag = .double_at, .lexeme = "@@", .line = 3, .column = 3 },
        Token{ .tag = .identifier, .lexeme = "dynamic", .line = 3, .column = 5 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 4, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 4, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Status", enum_decl.name);
    try std.testing.expect(enum_decl.values.items.len == 1);
    try std.testing.expect(enum_decl.attributes.items.len == 1);

    const attr = enum_decl.attributes.items[0];
    try std.testing.expectEqualStrings("dynamic", attr.name);
    try std.testing.expect(attr.is_class_level);
}

test "Parser: Parse enum with docstring" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .docstring, .lexeme = " Status enumeration", .line = 1, .column = 1 },
        Token{ .tag = .keyword_enum, .lexeme = "enum", .line = 2, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Status", .line = 2, .column = 6 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 2, .column = 13 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 3, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 3, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Status", enum_decl.name);
    try std.testing.expect(enum_decl.docstring != null);
    try std.testing.expectEqualStrings(" Status enumeration", enum_decl.docstring.?);
}

test "Parser: Parse enum value with docstring" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_enum, .lexeme = "enum", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Status", .line = 1, .column = 6 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 13 },
        Token{ .tag = .docstring, .lexeme = " Active state", .line = 2, .column = 3 },
        Token{ .tag = .identifier, .lexeme = "Active", .line = 3, .column = 3 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 4, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 4, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expect(enum_decl.values.items.len == 1);
    const val = enum_decl.values.items[0];
    try std.testing.expectEqualStrings("Active", val.name);
    try std.testing.expect(val.docstring != null);
    try std.testing.expectEqualStrings(" Active state", val.docstring.?);
}

test "Parser: Parse class property with docstring" {
    const allocator = std.testing.allocator;
    const tokens = [_]Token{
        Token{ .tag = .keyword_class, .lexeme = "class", .line = 1, .column = 1 },
        Token{ .tag = .identifier, .lexeme = "Person", .line = 1, .column = 7 },
        Token{ .tag = .lbrace, .lexeme = "{", .line = 1, .column = 14 },
        Token{ .tag = .docstring, .lexeme = " The person's name", .line = 2, .column = 3 },
        Token{ .tag = .identifier, .lexeme = "name", .line = 3, .column = 3 },
        Token{ .tag = .type_string, .lexeme = "string", .line = 3, .column = 8 },
        Token{ .tag = .rbrace, .lexeme = "}", .line = 4, .column = 1 },
        Token{ .tag = .eof, .lexeme = "", .line = 4, .column = 2 },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expect(class_decl.properties.items.len == 1);
    const prop = class_decl.properties.items[0];
    try std.testing.expectEqualStrings("name", prop.name);
    try std.testing.expect(prop.docstring != null);
    try std.testing.expectEqualStrings(" The person's name", prop.docstring.?);
}

test "Parser: Integration - Parse class from lexer output" {
    const allocator = std.testing.allocator;

    // Simple BAML class
    const source =
        \\class Person {
        \\  name string
        \\  age int?
        \\  email string @alias("email_address")
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var class_decl = try parser.parseClassDecl();
    defer class_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Person", class_decl.name);
    try std.testing.expect(class_decl.properties.items.len == 3);

    // Verify properties
    try std.testing.expectEqualStrings("name", class_decl.properties.items[0].name);
    try std.testing.expectEqualStrings("age", class_decl.properties.items[1].name);
    try std.testing.expectEqualStrings("email", class_decl.properties.items[2].name);

    // Verify email has alias attribute
    try std.testing.expect(class_decl.properties.items[2].attributes.items.len == 1);
    try std.testing.expectEqualStrings("alias", class_decl.properties.items[2].attributes.items[0].name);
}

test "Parser: Integration - Parse enum from lexer output" {
    const allocator = std.testing.allocator;

    // Simple BAML enum
    const source =
        \\enum Status {
        \\  Active
        \\  Inactive
        \\  Pending
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var enum_decl = try parser.parseEnumDecl();
    defer enum_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Status", enum_decl.name);
    try std.testing.expect(enum_decl.values.items.len == 3);

    // Verify values
    try std.testing.expectEqualStrings("Active", enum_decl.values.items[0].name);
    try std.testing.expectEqualStrings("Inactive", enum_decl.values.items[1].name);
    try std.testing.expectEqualStrings("Pending", enum_decl.values.items[2].name);
}

test "Parser: Parse simple function without parameters" {
    const allocator = std.testing.allocator;

    const source =
        \\function GetGreeting() -> string {
        \\  client "openai/gpt-4"
        \\  prompt #"Say hello"#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("GetGreeting", func_decl.name);
    try std.testing.expect(func_decl.parameters.items.len == 0);
    try std.testing.expect(func_decl.return_type.* == .primitive);
    try std.testing.expect(func_decl.return_type.primitive == .string);
    try std.testing.expect(func_decl.client != null);
    try std.testing.expectEqualStrings("openai/gpt-4", func_decl.client.?);
    try std.testing.expect(func_decl.prompt != null);
    try std.testing.expectEqualStrings("Say hello", func_decl.prompt.?);
}

test "Parser: Parse function with single parameter" {
    const allocator = std.testing.allocator;

    const source =
        \\function Greet(name: string) -> string {
        \\  client "openai/gpt-4"
        \\  prompt #"Hello {{ name }}"#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Greet", func_decl.name);
    try std.testing.expect(func_decl.parameters.items.len == 1);

    const param = func_decl.parameters.items[0];
    try std.testing.expectEqualStrings("name", param.name);
    try std.testing.expect(param.type_expr.* == .primitive);
    try std.testing.expect(param.type_expr.primitive == .string);
}

test "Parser: Parse function with multiple parameters" {
    const allocator = std.testing.allocator;

    const source =
        \\function Process(text: string, count: int, flag: bool) -> string {
        \\  client "anthropic/claude"
        \\  prompt #"Process: {{ text }}"#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Process", func_decl.name);
    try std.testing.expect(func_decl.parameters.items.len == 3);

    try std.testing.expectEqualStrings("text", func_decl.parameters.items[0].name);
    try std.testing.expectEqualStrings("count", func_decl.parameters.items[1].name);
    try std.testing.expectEqualStrings("flag", func_decl.parameters.items[2].name);
}

test "Parser: Parse function with complex parameter types" {
    const allocator = std.testing.allocator;

    const source =
        \\function Extract(data: string, img: image, tags: string[]) -> Person | null {
        \\  client "anthropic/claude"
        \\  prompt #"Extract from {{ data }}"#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Extract", func_decl.name);
    try std.testing.expect(func_decl.parameters.items.len == 3);

    // Check first param: data: string
    const param1 = func_decl.parameters.items[0];
    try std.testing.expectEqualStrings("data", param1.name);
    try std.testing.expect(param1.type_expr.* == .primitive);

    // Check second param: img: image
    const param2 = func_decl.parameters.items[1];
    try std.testing.expectEqualStrings("img", param2.name);
    try std.testing.expect(param2.type_expr.* == .primitive);
    try std.testing.expect(param2.type_expr.primitive == .image);

    // Check third param: tags: string[]
    const param3 = func_decl.parameters.items[2];
    try std.testing.expectEqualStrings("tags", param3.name);
    try std.testing.expect(param3.type_expr.* == .array);
    try std.testing.expect(param3.type_expr.array.* == .primitive);
}

test "Parser: Parse function with union return type" {
    const allocator = std.testing.allocator;

    const source =
        \\function Query(q: string) -> string | int | null {
        \\  client "openai/gpt-4"
        \\  prompt #"Query: {{ q }}"#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Query", func_decl.name);
    try std.testing.expect(func_decl.return_type.* == .union_type);
    try std.testing.expect(func_decl.return_type.union_type.types.items.len == 3);
}

test "Parser: Parse function with multiline prompt" {
    const allocator = std.testing.allocator;

    const source =
        \\function Extract(text: string) -> Person {
        \\  client "anthropic/claude-sonnet-4"
        \\  prompt ##"
        \\    Extract person from: {{ text }}
        \\
        \\    {{ ctx.output_format }}
        \\  "##
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Extract", func_decl.name);
    try std.testing.expect(func_decl.prompt != null);
    // Verify multiline prompt contains expected content
    try std.testing.expect(std.mem.indexOf(u8, func_decl.prompt.?, "Extract person from") != null);
    try std.testing.expect(std.mem.indexOf(u8, func_decl.prompt.?, "ctx.output_format") != null);
}

test "Parser: Parse function with docstring" {
    const allocator = std.testing.allocator;

    const source =
        \\/// Greets a person by name
        \\function GreetPerson(p: Person) -> string {
        \\  client "openai/gpt-4"
        \\  prompt #"Hello {{ p.name }}"#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("GreetPerson", func_decl.name);
    try std.testing.expect(func_decl.docstring != null);
    try std.testing.expectEqualStrings(" Greets a person by name", func_decl.docstring.?);
}

test "Parser: Integration - Parse complete function from test.baml" {
    const allocator = std.testing.allocator;

    const source =
        \\function Greet(p: Person) -> string {
        \\  client "openai/gpt-4"
        \\  prompt #"
        \\    Say hello to {{ p.name }}
        \\  "#
        \\}
    ;

    var lex = Lexer.init(allocator, source);
    defer lex.deinit();

    const tokens = try lex.tokenize();
    defer allocator.free(tokens);

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();

    var func_decl = try parser.parseFunctionDecl();
    defer func_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Greet", func_decl.name);
    try std.testing.expect(func_decl.parameters.items.len == 1);
    try std.testing.expectEqualStrings("p", func_decl.parameters.items[0].name);
    try std.testing.expect(func_decl.return_type.* == .primitive);
    try std.testing.expect(func_decl.return_type.primitive == .string);
    try std.testing.expectEqualStrings("openai/gpt-4", func_decl.client.?);
    try std.testing.expect(std.mem.indexOf(u8, func_decl.prompt.?, "Say hello to") != null);
}
