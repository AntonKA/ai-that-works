const std = @import("std");

/// TokenTag represents all possible token types in the BAML language
pub const TokenTag = enum {
    // Keywords
    keyword_class,
    keyword_enum,
    keyword_function,
    keyword_client,
    keyword_test,
    keyword_generator,
    keyword_template_string,
    keyword_type,

    // Primitive types
    type_string,
    type_int,
    type_float,
    type_bool,
    type_null,
    type_image,
    type_audio,
    type_video,
    type_pdf,
    type_map,

    // Symbols
    at, // @
    double_at, // @@
    lbrace, // {
    rbrace, // }
    lbracket, // [
    rbracket, // ]
    lparen, // (
    rparen, // )
    pipe, // |
    question, // ?
    less_than, // <
    greater_than, // >
    colon, // :
    comma, // ,
    hash, // #
    quote, // "
    env, // env

    // Literals
    string_literal,
    int_literal,
    float_literal,
    bool_literal,
    identifier,

    // Comments
    comment,
    docstring,
    block_comment,

    // Special
    eof,
    newline,
    unknown,
};

/// Token represents a single lexical token with its metadata
pub const Token = struct {
    tag: TokenTag,
    lexeme: []const u8,
    line: usize,
    column: usize,
};

/// Lexer performs lexical analysis on BAML source code
pub const Lexer = struct {
    source: []const u8,
    index: usize,
    line: usize,
    column: usize,

    /// Initialize a new lexer with the given source code
    pub fn init(source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .index = 0,
            .line = 1,
            .column = 1,
        };
    }

    /// Peek at the current character without consuming it
    pub fn peek(self: *const Lexer) ?u8 {
        if (self.isAtEnd()) {
            return null;
        }
        return self.source[self.index];
    }

    /// Consume and return the current character
    pub fn advance(self: *Lexer) ?u8 {
        if (self.isAtEnd()) {
            return null;
        }
        const char = self.source[self.index];
        self.index += 1;
        self.column += 1;
        return char;
    }

    /// Check if we've reached the end of the source
    pub fn isAtEnd(self: *const Lexer) bool {
        return self.index >= self.source.len;
    }

    /// Skip whitespace characters (spaces and tabs, but NOT newlines)
    pub fn skipWhitespace(self: *Lexer) void {
        while (self.peek()) |char| {
            if (char == ' ' or char == '\t') {
                _ = self.advance();
            } else {
                break;
            }
        }
    }

    /// Peek ahead at the character at offset from current position
    fn peekAt(self: *const Lexer, offset: usize) ?u8 {
        const pos = self.index + offset;
        if (pos >= self.source.len) {
            return null;
        }
        return self.source[pos];
    }

    /// Scan a line comment (// or ///)
    /// Assumes current position is at the first '/' of '//' or '///'
    pub fn scanComment(self: *Lexer) Token {
        const start_line = self.line;
        const start_column = self.column;
        const start_index = self.index;

        // Advance past first '/'
        _ = self.advance();

        // Check for second '/'
        if (self.peek() != '/') {
            return Token{
                .tag = .unknown,
                .lexeme = self.source[start_index..self.index],
                .line = start_line,
                .column = start_column,
            };
        }

        // Advance past second '/'
        _ = self.advance();

        // Check for third '/' (docstring)
        const is_docstring = self.peek() == '/';
        if (is_docstring) {
            _ = self.advance();
        }

        // Mark start of content (after // or ///)
        const content_start = self.index;

        // Advance until newline or EOF
        while (self.peek()) |char| {
            if (char == '\n') {
                break;
            }
            _ = self.advance();
        }

        // Extract content without the // or /// prefix
        const lexeme = self.source[content_start..self.index];

        return Token{
            .tag = if (is_docstring) .docstring else .comment,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Scan a Jinja block comment {# ... #}
    /// Assumes current position is at the '{'
    pub fn scanBlockComment(self: *Lexer) Token {
        const start_line = self.line;
        const start_column = self.column;
        const start_index = self.index;

        // Advance past '{'
        _ = self.advance();

        // Verify next char is '#'
        if (self.peek() != '#') {
            return Token{
                .tag = .unknown,
                .lexeme = self.source[start_index..self.index],
                .line = start_line,
                .column = start_column,
            };
        }

        // Advance past '#'
        _ = self.advance();

        // Mark start of content
        const content_start = self.index;

        // Advance through content until finding '#}'
        var depth: usize = 1;
        while (self.peek()) |char| {
            if (char == '\n') {
                self.line += 1;
                self.column = 0;
                _ = self.advance();
                continue;
            }

            if (char == '#' and self.peekAt(1) == '}') {
                depth -= 1;
                if (depth == 0) {
                    // Found closing #}
                    const lexeme = self.source[content_start..self.index];
                    _ = self.advance(); // consume '#'
                    _ = self.advance(); // consume '}'
                    return Token{
                        .tag = .block_comment,
                        .lexeme = lexeme,
                        .line = start_line,
                        .column = start_column,
                    };
                }
            }

            // Check for nested block comment
            if (char == '{' and self.peekAt(1) == '#') {
                depth += 1;
                _ = self.advance(); // consume '{'
                _ = self.advance(); // consume '#'
                continue;
            }

            _ = self.advance();
        }

        // EOF before closing - return unknown token
        return Token{
            .tag = .unknown,
            .lexeme = self.source[start_index..self.index],
            .line = start_line,
            .column = start_column,
        };
    }

    /// Check if a character is a digit
    pub fn isDigit(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    /// Check if character is alphabetic (a-z, A-Z, or _)
    pub fn isAlpha(char: u8) bool {
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or char == '_';
    }

    /// Check if character is alphanumeric or underscore
    pub fn isAlphaNumeric(char: u8) bool {
        return isAlpha(char) or isDigit(char);
    }

    /// Parse integer and float literals
    /// Assumes current char is a digit or minus sign followed by digit
    pub fn scanNumber(self: *Lexer) Token {
        const start_line = self.line;
        const start_column = self.column;
        const start_index = self.index;

        // Handle negative sign
        if (self.peek()) |char| {
            if (char == '-') {
                _ = self.advance();
            }
        }

        // Scan initial digits
        while (self.peek()) |char| {
            if (isDigit(char)) {
                _ = self.advance();
            } else {
                break;
            }
        }

        // Check for decimal point followed by digits
        var is_float = false;
        if (self.peek()) |char| {
            if (char == '.') {
                if (self.peekAt(1)) |next_char| {
                    if (isDigit(next_char)) {
                        is_float = true;
                        _ = self.advance(); // consume '.'

                        // Scan digits after decimal
                        while (self.peek()) |digit_char| {
                            if (isDigit(digit_char)) {
                                _ = self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }

        const lexeme = self.source[start_index..self.index];
        const tag = if (is_float) TokenTag.float_literal else TokenTag.int_literal;

        return Token{
            .tag = tag,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Scan an identifier or keyword starting from the current position
    pub fn scanIdentifierOrKeyword(self: *Lexer) Token {
        const start_index = self.index;
        const start_line = self.line;
        const start_column = self.column;

        // Consume first character (already validated as alpha)
        _ = self.advance();

        // Continue while we have alphanumeric characters or underscores
        while (self.peek()) |char| {
            if (isAlphaNumeric(char)) {
                _ = self.advance();
            } else {
                break;
            }
        }

        // Extract the lexeme
        const lexeme = self.source[start_index..self.index];

        // Check if it's a keyword or just an identifier
        const tag = getKeyword(lexeme) orelse .identifier;

        return Token{
            .tag = tag,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Get the keyword token tag for a given lexeme, or null if it's not a keyword
    fn getKeyword(lexeme: []const u8) ?TokenTag {
        // Keywords
        if (std.mem.eql(u8, lexeme, "class")) return .keyword_class;
        if (std.mem.eql(u8, lexeme, "enum")) return .keyword_enum;
        if (std.mem.eql(u8, lexeme, "function")) return .keyword_function;
        if (std.mem.eql(u8, lexeme, "client")) return .keyword_client;
        if (std.mem.eql(u8, lexeme, "test")) return .keyword_test;
        if (std.mem.eql(u8, lexeme, "generator")) return .keyword_generator;
        if (std.mem.eql(u8, lexeme, "template_string")) return .keyword_template_string;
        if (std.mem.eql(u8, lexeme, "type")) return .keyword_type;
        if (std.mem.eql(u8, lexeme, "env")) return .env;

        // Primitive types
        if (std.mem.eql(u8, lexeme, "string")) return .type_string;
        if (std.mem.eql(u8, lexeme, "int")) return .type_int;
        if (std.mem.eql(u8, lexeme, "float")) return .type_float;
        if (std.mem.eql(u8, lexeme, "bool")) return .type_bool;
        if (std.mem.eql(u8, lexeme, "null")) return .type_null;
        if (std.mem.eql(u8, lexeme, "image")) return .type_image;
        if (std.mem.eql(u8, lexeme, "audio")) return .type_audio;
        if (std.mem.eql(u8, lexeme, "video")) return .type_video;
        if (std.mem.eql(u8, lexeme, "pdf")) return .type_pdf;
        if (std.mem.eql(u8, lexeme, "map")) return .type_map;

        // Boolean literals
        if (std.mem.eql(u8, lexeme, "true")) return .bool_literal;
        if (std.mem.eql(u8, lexeme, "false")) return .bool_literal;

        return null;
    }

    /// Parse a quoted string literal: "..."
    /// Assumes current char is "
    pub fn scanString(self: *Lexer) Token {
        const start_line = self.line;
        const start_column = self.column;

        // Advance past opening quote
        _ = self.advance();
        const start_index = self.index;

        while (self.peek()) |char| {
            if (char == '"') {
                // Found closing quote
                const lexeme = self.source[start_index..self.index];
                _ = self.advance(); // consume closing quote
                return Token{
                    .tag = .string_literal,
                    .lexeme = lexeme,
                    .line = start_line,
                    .column = start_column,
                };
            } else if (char == '\\') {
                // Handle escape sequence
                _ = self.advance(); // consume backslash
                if (self.peek()) |_| {
                    const escaped = self.advance().?;
                    if (escaped == '\n') {
                        self.line += 1;
                        self.column = 1;
                    }
                }
            } else if (char == '\n') {
                self.line += 1;
                self.column = 0; // will be incremented by advance
                _ = self.advance();
            } else {
                _ = self.advance();
            }
        }

        // EOF before closing quote - error
        const lexeme = self.source[start_index..self.index];
        return Token{
            .tag = .unknown,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Parse a block string: #"..."# or ##"..."## etc.
    /// Assumes current char is #
    pub fn scanBlockString(self: *Lexer) Token {
        const start_line = self.line;
        const start_column = self.column;

        // Count opening hashes
        var hash_count: usize = 0;
        while (self.peek()) |char| {
            if (char == '#') {
                hash_count += 1;
                _ = self.advance();
            } else {
                break;
            }
        }

        // Expect opening quote
        if (self.peek() != '"') {
            return Token{
                .tag = .unknown,
                .lexeme = self.source[start_column - 1 .. self.index],
                .line = start_line,
                .column = start_column,
            };
        }
        _ = self.advance(); // consume opening quote

        const start_index = self.index;

        // Scan until we find closing "###...
        while (!self.isAtEnd()) {
            if (self.peek() == '"') {
                _ = self.advance(); // consume quote

                // Count closing hashes
                const hash_start = self.index;
                var closing_hash_count: usize = 0;
                while (self.peek()) |char| {
                    if (char == '#') {
                        closing_hash_count += 1;
                        _ = self.advance();
                    } else {
                        break;
                    }
                }

                // Check if we have matching hash counts
                if (closing_hash_count == hash_count) {
                    const lexeme = self.source[start_index .. hash_start - 1];
                    return Token{
                        .tag = .string_literal,
                        .lexeme = lexeme,
                        .line = start_line,
                        .column = start_column,
                    };
                }
                // Not enough hashes, keep scanning
            } else {
                const char = self.advance().?;
                if (char == '\n') {
                    self.line += 1;
                    self.column = 1;
                }
            }
        }

        // EOF before proper closing
        const lexeme = self.source[start_index..self.index];
        return Token{
            .tag = .unknown,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Parse an unquoted string (stops at whitespace or special chars)
    /// Used for simple values in attribute arguments
    pub fn scanUnquotedString(self: *Lexer) Token {
        const start_line = self.line;
        const start_column = self.column;
        const start_index = self.index;

        while (self.peek()) |char| {
            // Stop at whitespace
            if (char == ' ' or char == '\t' or char == '\n' or char == '\r') {
                break;
            }
            // Stop at special characters
            if (char == '@' or char == '{' or char == '}' or
                char == '[' or char == ']' or char == '(' or
                char == ')' or char == '|' or char == '?' or
                char == '<' or char == '>' or char == ':' or
                char == ',' or char == '#' or char == '"')
            {
                break;
            }
            _ = self.advance();
        }

        const lexeme = self.source[start_index..self.index];
        return Token{
            .tag = .string_literal,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Main tokenization method that scans and returns the next token
    pub fn scanToken(self: *Lexer) Token {
        self.skipWhitespace();

        if (self.isAtEnd()) {
            return Token{
                .tag = .eof,
                .lexeme = "",
                .line = self.line,
                .column = self.column,
            };
        }

        const start_column = self.column;
        const start_line = self.line;
        const char = self.peek().?;

        // Handle alphabetic characters and underscore (identifiers/keywords)
        if (isAlpha(char)) {
            return self.scanIdentifierOrKeyword();
        }

        // Handle digits (numbers)
        if (isDigit(char)) {
            return self.scanNumber();
        }

        // Handle negative numbers
        if (char == '-') {
            if (self.peekAt(1)) |next| {
                if (isDigit(next)) {
                    return self.scanNumber();
                }
            }
            // Single '-' is unknown
            const start_index = self.index;
            _ = self.advance();
            const lexeme = self.source[start_index..self.index];
            return Token{
                .tag = .unknown,
                .lexeme = lexeme,
                .line = start_line,
                .column = start_column,
            };
        }

        // Handle strings
        if (char == '"') {
            return self.scanString();
        }

        // Handle hash symbol
        if (char == '#') {
            // Look ahead to see if this is a block string (#"..." or ##"..."## etc)
            var look_ahead: usize = 1;
            while (self.peekAt(look_ahead)) |next_char| {
                if (next_char == '#') {
                    look_ahead += 1;
                } else if (next_char == '"') {
                    // This is a block string
                    return self.scanBlockString();
                } else {
                    // Not a block string
                    break;
                }
            }
            // Single hash symbol (or hash not followed by quote)
            const start_index = self.index;
            _ = self.advance();
            const lexeme = self.source[start_index..self.index];
            return Token{
                .tag = .hash,
                .lexeme = lexeme,
                .line = start_line,
                .column = start_column,
            };
        }

        // Handle forward slash (comments)
        if (char == '/') {
            if (self.peekAt(1)) |next| {
                if (next == '/') {
                    return self.scanComment();
                }
            }
            // Single forward slash is not valid
            const start_index = self.index;
            _ = self.advance();
            const lexeme = self.source[start_index..self.index];
            return Token{
                .tag = .unknown,
                .lexeme = lexeme,
                .line = start_line,
                .column = start_column,
            };
        }

        // Handle left brace (possibly block comment)
        if (char == '{') {
            if (self.peekAt(1)) |next| {
                if (next == '#') {
                    return self.scanBlockComment();
                }
            }
            // Left brace symbol
            const start_index = self.index;
            _ = self.advance();
            const lexeme = self.source[start_index..self.index];
            return Token{
                .tag = .lbrace,
                .lexeme = lexeme,
                .line = start_line,
                .column = start_column,
            };
        }

        // Handle at symbol (@ or @@)
        if (char == '@') {
            if (self.peekAt(1)) |next| {
                if (next == '@') {
                    const start_index = self.index;
                    _ = self.advance(); // first @
                    _ = self.advance(); // second @
                    const lexeme = self.source[start_index..self.index];
                    return Token{
                        .tag = .double_at,
                        .lexeme = lexeme,
                        .line = start_line,
                        .column = start_column,
                    };
                }
            }
            // Single at symbol
            const start_index = self.index;
            _ = self.advance();
            const lexeme = self.source[start_index..self.index];
            return Token{
                .tag = .at,
                .lexeme = lexeme,
                .line = start_line,
                .column = start_column,
            };
        }

        // Handle newline
        if (char == '\n') {
            const start_index = self.index;
            _ = self.advance();
            const lexeme = self.source[start_index..self.index];
            self.line += 1;
            self.column = 1;
            return Token{
                .tag = .newline,
                .lexeme = lexeme,
                .line = start_line,
                .column = start_column,
            };
        }

        // Handle single-character symbols
        const start_index = self.index;
        const tag: TokenTag = switch (char) {
            '}' => .rbrace,
            '[' => .lbracket,
            ']' => .rbracket,
            '(' => .lparen,
            ')' => .rparen,
            '|' => .pipe,
            '?' => .question,
            '<' => .less_than,
            '>' => .greater_than,
            ':' => .colon,
            ',' => .comma,
            else => .unknown,
        };

        _ = self.advance();
        const lexeme = self.source[start_index..self.index];
        return Token{
            .tag = tag,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    /// Tokenize the entire source and return a list of tokens
    pub fn tokenize(self: *Lexer, allocator: std.mem.Allocator) !std.ArrayList(Token) {
        var tokens: std.ArrayList(Token) = .{};
        errdefer tokens.deinit(allocator);

        while (true) {
            const token = self.scanToken();
            try tokens.append(allocator, token);
            if (token.tag == .eof) {
                break;
            }
        }

        return tokens;
    }
};

// ============================================================================
// TESTS
// ============================================================================

test "Lexer initialization" {
    const source = "test input";
    const lexer = Lexer.init(source);

    try std.testing.expectEqual(@as(usize, 0), lexer.index);
    try std.testing.expectEqual(@as(usize, 1), lexer.line);
    try std.testing.expectEqual(@as(usize, 1), lexer.column);
    try std.testing.expectEqualStrings(source, lexer.source);
}

test "Lexer peek does not advance position" {
    const source = "abc";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(@as(u8, 'a'), lexer.peek().?);
    try std.testing.expectEqual(@as(u8, 'a'), lexer.peek().?);
    try std.testing.expectEqual(@as(usize, 0), lexer.index);
    try std.testing.expectEqual(@as(usize, 1), lexer.column);
}

test "Lexer advance increments position and column" {
    const source = "abc";
    var lexer = Lexer.init(source);

    try std.testing.expectEqual(@as(u8, 'a'), lexer.advance().?);
    try std.testing.expectEqual(@as(usize, 1), lexer.index);
    try std.testing.expectEqual(@as(usize, 2), lexer.column);

    try std.testing.expectEqual(@as(u8, 'b'), lexer.advance().?);
    try std.testing.expectEqual(@as(usize, 2), lexer.index);
    try std.testing.expectEqual(@as(usize, 3), lexer.column);

    try std.testing.expectEqual(@as(u8, 'c'), lexer.advance().?);
    try std.testing.expectEqual(@as(usize, 3), lexer.index);
    try std.testing.expectEqual(@as(usize, 4), lexer.column);
}

test "Lexer isAtEnd behavior" {
    const source = "a";
    var lexer = Lexer.init(source);

    try std.testing.expect(!lexer.isAtEnd());
    _ = lexer.advance();
    try std.testing.expect(lexer.isAtEnd());
    try std.testing.expectEqual(@as(?u8, null), lexer.peek());
}

test "Lexer skipWhitespace skips spaces and tabs" {
    const source = "   \t  abc";
    var lexer = Lexer.init(source);

    lexer.skipWhitespace();
    try std.testing.expectEqual(@as(u8, 'a'), lexer.peek().?);
    try std.testing.expectEqual(@as(usize, 6), lexer.index);
    try std.testing.expectEqual(@as(usize, 7), lexer.column);
}

test "Lexer skipWhitespace stops at newline and handles edge cases" {
    var lexer = Lexer.init("  \n  abc");
    lexer.skipWhitespace();
    try std.testing.expectEqual(@as(u8, '\n'), lexer.peek().?);

    lexer = Lexer.init("   ");
    lexer.skipWhitespace();
    try std.testing.expect(lexer.isAtEnd());
}

test "Token creation with all fields" {
    const token = Token{
        .tag = .identifier,
        .lexeme = "test_var",
        .line = 42,
        .column = 15,
    };

    try std.testing.expectEqual(TokenTag.identifier, token.tag);
    try std.testing.expectEqualStrings("test_var", token.lexeme);
    try std.testing.expectEqual(@as(usize, 42), token.line);
    try std.testing.expectEqual(@as(usize, 15), token.column);
}

// ============================================================================
// COMMENT TESTS
// ============================================================================

test "scanComment - simple line comment" {
    const source = "// hello world";
    var lexer = Lexer.init(source);

    const token = lexer.scanComment();

    try std.testing.expectEqual(TokenTag.comment, token.tag);
    try std.testing.expectEqualStrings(" hello world", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
    try std.testing.expectEqual(@as(usize, 1), token.column);
}

test "scanComment - docstring comment" {
    const source = "/// documentation here";
    var lexer = Lexer.init(source);

    const token = lexer.scanComment();

    try std.testing.expectEqual(TokenTag.docstring, token.tag);
    try std.testing.expectEqualStrings(" documentation here", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
}

test "scanComment - empty comment" {
    const source = "//";
    var lexer = Lexer.init(source);

    const token = lexer.scanComment();

    try std.testing.expectEqual(TokenTag.comment, token.tag);
    try std.testing.expectEqualStrings("", token.lexeme);
}

test "scanComment - empty docstring" {
    const source = "///";
    var lexer = Lexer.init(source);

    const token = lexer.scanComment();

    try std.testing.expectEqual(TokenTag.docstring, token.tag);
    try std.testing.expectEqualStrings("", token.lexeme);
}

test "scanComment - comment before newline" {
    const source = "// test\nnext line";
    var lexer = Lexer.init(source);

    const token = lexer.scanComment();

    try std.testing.expectEqual(TokenTag.comment, token.tag);
    try std.testing.expectEqualStrings(" test", token.lexeme);
    // Newline should not be consumed
    try std.testing.expectEqual(@as(u8, '\n'), lexer.peek().?);
}

test "scanComment - comment at EOF" {
    const source = "// end comment";
    var lexer = Lexer.init(source);

    const token = lexer.scanComment();

    try std.testing.expectEqual(TokenTag.comment, token.tag);
    try std.testing.expectEqualStrings(" end comment", token.lexeme);
    try std.testing.expect(lexer.isAtEnd());
}

test "scanBlockComment - simple block comment" {
    const source = "{# comment content #}";
    var lexer = Lexer.init(source);

    const token = lexer.scanBlockComment();

    try std.testing.expectEqual(TokenTag.block_comment, token.tag);
    try std.testing.expectEqualStrings(" comment content ", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
}

test "scanBlockComment - multi-line" {
    const source = "{# line 1\nline 2\nline 3 #}";
    var lexer = Lexer.init(source);

    const token = lexer.scanBlockComment();

    try std.testing.expectEqual(TokenTag.block_comment, token.tag);
    try std.testing.expectEqualStrings(" line 1\nline 2\nline 3 ", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
}

test "scanBlockComment - nested" {
    const source = "{# outer {# inner #} outer #}";
    var lexer = Lexer.init(source);

    const token = lexer.scanBlockComment();

    try std.testing.expectEqual(TokenTag.block_comment, token.tag);
    try std.testing.expectEqualStrings(" outer {# inner #} outer ", token.lexeme);
}

test "scanBlockComment - unclosed returns unknown" {
    const source = "{# unclosed comment";
    var lexer = Lexer.init(source);

    const token = lexer.scanBlockComment();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
}

test "scanBlockComment - invalid syntax returns unknown" {
    const source = "{not a comment";
    var lexer = Lexer.init(source);

    const token = lexer.scanBlockComment();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
}

test "scanBlockComment - empty" {
    const source = "{##}";
    var lexer = Lexer.init(source);

    const token = lexer.scanBlockComment();

    try std.testing.expectEqual(TokenTag.block_comment, token.tag);
    try std.testing.expectEqualStrings("", token.lexeme);
}

test "peekAt - look ahead" {
    const source = "abcdef";
    const lexer = Lexer.init(source);

    try std.testing.expectEqual(@as(u8, 'a'), lexer.peekAt(0).?);
    try std.testing.expectEqual(@as(u8, 'c'), lexer.peekAt(2).?);
    try std.testing.expectEqual(@as(?u8, null), lexer.peekAt(10));
}

// ============================================================================
// NUMBER SCANNING TESTS
// ============================================================================

test "isDigit correctly identifies digits" {
    try std.testing.expect(Lexer.isDigit('0'));
    try std.testing.expect(Lexer.isDigit('5'));
    try std.testing.expect(Lexer.isDigit('9'));
    try std.testing.expect(!Lexer.isDigit('a'));
    try std.testing.expect(!Lexer.isDigit('-'));
    try std.testing.expect(!Lexer.isDigit('.'));
}

test "scanNumber parses zero" {
    const source = "0";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("0", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
    try std.testing.expectEqual(@as(usize, 1), token.column);
}

test "scanNumber parses single digit integer" {
    const source = "1";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("1", token.lexeme);
}

test "scanNumber parses two digit integer" {
    const source = "42";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("42", token.lexeme);
}

test "scanNumber parses large integer" {
    const source = "123456";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("123456", token.lexeme);
}

test "scanNumber parses float with zero" {
    const source = "0.0";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("0.0", token.lexeme);
}

test "scanNumber parses simple float" {
    const source = "1.5";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("1.5", token.lexeme);
}

test "scanNumber parses pi approximation" {
    const source = "3.14159";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("3.14159", token.lexeme);
}

test "scanNumber parses negative single digit" {
    const source = "-1";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("-1", token.lexeme);
}

test "scanNumber parses negative two digit integer" {
    const source = "-42";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("-42", token.lexeme);
}

test "scanNumber parses negative float" {
    const source = "-1.5";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("-1.5", token.lexeme);
}

test "scanNumber parses negative pi" {
    const source = "-3.14";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("-3.14", token.lexeme);
}

test "scanNumber handles integer followed by space" {
    const source = "42 ";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("42", token.lexeme);
    try std.testing.expectEqual(@as(u8, ' '), lexer.peek().?);
}

test "scanNumber handles float followed by non-digit" {
    const source = "3.14abc";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("3.14", token.lexeme);
    try std.testing.expectEqual(@as(u8, 'a'), lexer.peek().?);
}

test "scanNumber handles integer followed by dot without digits" {
    const source = "42.";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("42", token.lexeme);
    try std.testing.expectEqual(@as(u8, '.'), lexer.peek().?);
}

test "scanNumber handles integer followed by dot and non-digit" {
    const source = "42.abc";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("42", token.lexeme);
    try std.testing.expectEqual(@as(u8, '.'), lexer.peek().?);
}

test "scanNumber handles very large integer" {
    const source = "9876543210";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("9876543210", token.lexeme);
}

test "scanNumber handles float with many decimal places" {
    const source = "123.456789";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("123.456789", token.lexeme);
}

test "scanNumber handles negative zero" {
    const source = "-0";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("-0", token.lexeme);
}

test "scanNumber handles negative float zero" {
    const source = "-0.0";
    var lexer = Lexer.init(source);
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.float_literal, token.tag);
    try std.testing.expectEqualStrings("-0.0", token.lexeme);
}

test "scanNumber preserves correct line and column" {
    const source = "   42";
    var lexer = Lexer.init(source);
    lexer.skipWhitespace();
    const token = lexer.scanNumber();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("42", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
    try std.testing.expectEqual(@as(usize, 4), token.column);
}

test "isAlpha recognizes alphabetic characters" {
    try std.testing.expect(Lexer.isAlpha('a'));
    try std.testing.expect(Lexer.isAlpha('z'));
    try std.testing.expect(Lexer.isAlpha('A'));
    try std.testing.expect(Lexer.isAlpha('Z'));
    try std.testing.expect(Lexer.isAlpha('_'));
    try std.testing.expect(!Lexer.isAlpha('0'));
    try std.testing.expect(!Lexer.isAlpha('9'));
    try std.testing.expect(!Lexer.isAlpha(' '));
    try std.testing.expect(!Lexer.isAlpha('!'));
}

test "isDigit recognizes digits" {
    try std.testing.expect(Lexer.isDigit('0'));
    try std.testing.expect(Lexer.isDigit('5'));
    try std.testing.expect(Lexer.isDigit('9'));
    try std.testing.expect(!Lexer.isDigit('a'));
    try std.testing.expect(!Lexer.isDigit('Z'));
    try std.testing.expect(!Lexer.isDigit('_'));
    try std.testing.expect(!Lexer.isDigit(' '));
}

test "isAlphaNumeric combines alpha and digit checks" {
    try std.testing.expect(Lexer.isAlphaNumeric('a'));
    try std.testing.expect(Lexer.isAlphaNumeric('Z'));
    try std.testing.expect(Lexer.isAlphaNumeric('_'));
    try std.testing.expect(Lexer.isAlphaNumeric('0'));
    try std.testing.expect(Lexer.isAlphaNumeric('9'));
    try std.testing.expect(!Lexer.isAlphaNumeric(' '));
    try std.testing.expect(!Lexer.isAlphaNumeric('!'));
}

test "scanIdentifierOrKeyword recognizes all keywords" {
    const keywords = [_]struct { source: []const u8, tag: TokenTag }{
        .{ .source = "class", .tag = .keyword_class },
        .{ .source = "enum", .tag = .keyword_enum },
        .{ .source = "function", .tag = .keyword_function },
        .{ .source = "client", .tag = .keyword_client },
        .{ .source = "test", .tag = .keyword_test },
        .{ .source = "generator", .tag = .keyword_generator },
        .{ .source = "template_string", .tag = .keyword_template_string },
        .{ .source = "type", .tag = .keyword_type },
        .{ .source = "env", .tag = .env },
    };

    for (keywords) |kw| {
        var lexer = Lexer.init(kw.source);
        const token = lexer.scanIdentifierOrKeyword();
        try std.testing.expectEqual(kw.tag, token.tag);
        try std.testing.expectEqualStrings(kw.source, token.lexeme);
    }
}

test "scanIdentifierOrKeyword recognizes all type keywords" {
    const types = [_]struct { source: []const u8, tag: TokenTag }{
        .{ .source = "string", .tag = .type_string },
        .{ .source = "int", .tag = .type_int },
        .{ .source = "float", .tag = .type_float },
        .{ .source = "bool", .tag = .type_bool },
        .{ .source = "null", .tag = .type_null },
        .{ .source = "image", .tag = .type_image },
        .{ .source = "audio", .tag = .type_audio },
        .{ .source = "video", .tag = .type_video },
        .{ .source = "pdf", .tag = .type_pdf },
        .{ .source = "map", .tag = .type_map },
    };

    for (types) |t| {
        var lexer = Lexer.init(t.source);
        const token = lexer.scanIdentifierOrKeyword();
        try std.testing.expectEqual(t.tag, token.tag);
        try std.testing.expectEqualStrings(t.source, token.lexeme);
    }
}

test "scanIdentifierOrKeyword recognizes bool literals" {
    var lexer = Lexer.init("true");
    var token = lexer.scanIdentifierOrKeyword();
    try std.testing.expectEqual(TokenTag.bool_literal, token.tag);
    try std.testing.expectEqualStrings("true", token.lexeme);

    lexer = Lexer.init("false");
    token = lexer.scanIdentifierOrKeyword();
    try std.testing.expectEqual(TokenTag.bool_literal, token.tag);
    try std.testing.expectEqualStrings("false", token.lexeme);
}

test "scanIdentifierOrKeyword recognizes different identifier styles" {
    const identifiers = [_][]const u8{
        "camelCase",
        "snake_case",
        "SCREAMING_CASE",
        "PascalCase",
        "mixedStyle_123",
    };

    for (identifiers) |id| {
        var lexer = Lexer.init(id);
        const token = lexer.scanIdentifierOrKeyword();
        try std.testing.expectEqual(TokenTag.identifier, token.tag);
        try std.testing.expectEqualStrings(id, token.lexeme);
    }
}

test "scanIdentifierOrKeyword recognizes identifiers with numbers" {
    const identifiers = [_][]const u8{
        "var123",
        "test2",
        "foo42bar",
        "a1b2c3",
    };

    for (identifiers) |id| {
        var lexer = Lexer.init(id);
        const token = lexer.scanIdentifierOrKeyword();
        try std.testing.expectEqual(TokenTag.identifier, token.tag);
        try std.testing.expectEqualStrings(id, token.lexeme);
    }
}

test "scanIdentifierOrKeyword recognizes single letter identifiers" {
    const identifiers = [_][]const u8{ "a", "x", "Z", "_" };

    for (identifiers) |id| {
        var lexer = Lexer.init(id);
        const token = lexer.scanIdentifierOrKeyword();
        try std.testing.expectEqual(TokenTag.identifier, token.tag);
        try std.testing.expectEqualStrings(id, token.lexeme);
    }
}

test "scanIdentifierOrKeyword stops at non-alphanumeric" {
    const source = "myVar.property";
    var lexer = Lexer.init(source);
    const token = lexer.scanIdentifierOrKeyword();
    try std.testing.expectEqual(TokenTag.identifier, token.tag);
    try std.testing.expectEqualStrings("myVar", token.lexeme);
    try std.testing.expectEqual(@as(u8, '.'), lexer.peek().?);
}

test "scanIdentifierOrKeyword preserves line and column info" {
    const source = "  identifier";
    var lexer = Lexer.init(source);
    lexer.skipWhitespace();
    const token = lexer.scanIdentifierOrKeyword();
    try std.testing.expectEqual(@as(usize, 1), token.line);
    try std.testing.expectEqual(@as(usize, 3), token.column);
}

// ============================================================================
// SYMBOL AND OPERATOR TOKENIZATION TESTS
// ============================================================================

test "scanToken - all single-character symbols" {
    const symbols = [_]struct { source: []const u8, tag: TokenTag }{
        .{ .source = "}", .tag = .rbrace },
        .{ .source = "[", .tag = .lbracket },
        .{ .source = "]", .tag = .rbracket },
        .{ .source = "(", .tag = .lparen },
        .{ .source = ")", .tag = .rparen },
        .{ .source = "|", .tag = .pipe },
        .{ .source = "?", .tag = .question },
        .{ .source = "<", .tag = .less_than },
        .{ .source = ">", .tag = .greater_than },
        .{ .source = ":", .tag = .colon },
        .{ .source = ",", .tag = .comma },
    };

    for (symbols) |sym| {
        var lexer = Lexer.init(sym.source);
        const token = lexer.scanToken();
        try std.testing.expectEqual(sym.tag, token.tag);
        try std.testing.expectEqualStrings(sym.source, token.lexeme);
    }
}

test "scanToken - left brace symbol" {
    const source = "{";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.lbrace, token.tag);
    try std.testing.expectEqualStrings("{", token.lexeme);
}

test "scanToken - hash symbol" {
    const source = "#";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.hash, token.tag);
    try std.testing.expectEqualStrings("#", token.lexeme);
}

test "scanToken - at symbol vs double at" {
    var lexer = Lexer.init("@");
    var token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.at, token.tag);
    try std.testing.expectEqualStrings("@", token.lexeme);

    lexer = Lexer.init("@@");
    token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.double_at, token.tag);
    try std.testing.expectEqualStrings("@@", token.lexeme);
}

test "scanToken - double at vs two single at symbols" {
    var lexer = Lexer.init("@@ @");
    
    var token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.double_at, token.tag);
    try std.testing.expectEqualStrings("@@", token.lexeme);
    
    token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.at, token.tag);
    try std.testing.expectEqualStrings("@", token.lexeme);
}

test "scanToken - newline handling" {
    const source = "\n";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.newline, token.tag);
    try std.testing.expectEqualStrings("\n", token.lexeme);
    try std.testing.expectEqual(@as(usize, 1), token.line);
    try std.testing.expectEqual(@as(usize, 1), token.column);
    try std.testing.expectEqual(@as(usize, 2), lexer.line); // Line incremented
    try std.testing.expectEqual(@as(usize, 1), lexer.column); // Column reset
}

test "scanToken - newline increments line number" {
    const source = "a\nb";
    var lexer = Lexer.init(source);
    
    var token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.identifier, token.tag);
    try std.testing.expectEqual(@as(usize, 1), token.line);
    
    token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.newline, token.tag);
    try std.testing.expectEqual(@as(usize, 1), token.line);
    
    token = lexer.scanToken();
    try std.testing.expectEqual(TokenTag.identifier, token.tag);
    try std.testing.expectEqual(@as(usize, 2), token.line);
}

test "scanToken - unknown characters" {
    const unknowns = [_][]const u8{ "!", "$", "%", "&", "*", "~", "`", "^", "=" };

    for (unknowns) |unknown| {
        var lexer = Lexer.init(unknown);
        const token = lexer.scanToken();
        try std.testing.expectEqual(TokenTag.unknown, token.tag);
        try std.testing.expectEqualStrings(unknown, token.lexeme);
    }
}

test "scanToken - single forward slash is unknown" {
    const source = "/";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
    try std.testing.expectEqualStrings("/", token.lexeme);
}

test "scanToken - single minus is unknown" {
    const source = "- ";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
    try std.testing.expectEqualStrings("-", token.lexeme);
}

test "scanToken - EOF token" {
    const source = "";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.eof, token.tag);
    try std.testing.expectEqualStrings("", token.lexeme);
}

test "scanToken - whitespace is skipped" {
    const source = "   {";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.lbrace, token.tag);
    try std.testing.expectEqualStrings("{", token.lexeme);
}

test "scanToken - dispatches to scanIdentifierOrKeyword" {
    const source = "myVar";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.identifier, token.tag);
    try std.testing.expectEqualStrings("myVar", token.lexeme);
}

test "scanToken - dispatches to scanNumber" {
    const source = "123";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.int_literal, token.tag);
    try std.testing.expectEqualStrings("123", token.lexeme);
}

test "scanToken - dispatches to scanString" {
    const source = "\"hello\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("\"hello\"", token.lexeme);
}

test "scanToken - dispatches to scanComment" {
    const source = "// comment";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.comment, token.tag);
    try std.testing.expectEqualStrings(" comment", token.lexeme);
}

test "scanToken - dispatches to scanBlockComment" {
    const source = "{# comment #}";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.block_comment, token.tag);
    try std.testing.expectEqualStrings(" comment ", token.lexeme);
}

test "scanToken - dispatches to scanBlockString" {
    const source = "#\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.unknown, token.tag); // stub returns unknown
    try std.testing.expectEqualStrings("#\"", token.lexeme);
}

test "tokenize - empty source" {
    const source = "";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 1), tokens.items.len);
    try std.testing.expectEqual(TokenTag.eof, tokens.items[0].tag);
}

test "tokenize - source with only whitespace" {
    const source = "   \t  \t   ";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 1), tokens.items.len);
    try std.testing.expectEqual(TokenTag.eof, tokens.items[0].tag);
}

test "tokenize - simple BAML snippet" {
    const source = "class MyClass { name string }";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .keyword_class,
        .identifier,
        .lbrace,
        .identifier,
        .type_string,
        .rbrace,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - function declaration snippet" {
    const source = "function GetData(id: int) -> string";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .keyword_function,
        .identifier,
        .lparen,
        .identifier,
        .colon,
        .type_int,
        .rparen,
        .unknown, // '-' is unknown when not followed by digit
        .greater_than,
        .type_string,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - mixed symbols and identifiers" {
    const source = "@prompt(var: string | int)";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .at,
        .identifier,
        .lparen,
        .identifier,
        .colon,
        .type_string,
        .pipe,
        .type_int,
        .rparen,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - array type syntax" {
    const source = "items: string[]";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .identifier,
        .colon,
        .type_string,
        .lbracket,
        .rbracket,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - optional and map types" {
    const source = "field?: map<string, int>";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .identifier,
        .question,
        .colon,
        .type_map,
        .less_than,
        .type_string,
        .comma,
        .type_int,
        .greater_than,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - with newlines" {
    const source = "class Foo\n{\nname string\n}";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .keyword_class,
        .identifier,
        .newline,
        .lbrace,
        .newline,
        .identifier,
        .type_string,
        .newline,
        .rbrace,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - preserves line and column information" {
    const source = "a\nb";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 4), tokens.items.len);
    
    // First token 'a' on line 1
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[0].tag);
    try std.testing.expectEqual(@as(usize, 1), tokens.items[0].line);
    
    // Newline on line 1
    try std.testing.expectEqual(TokenTag.newline, tokens.items[1].tag);
    try std.testing.expectEqual(@as(usize, 1), tokens.items[1].line);
    
    // Second token 'b' on line 2
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[2].tag);
    try std.testing.expectEqual(@as(usize, 2), tokens.items[2].line);
}

// ============================================================================
// STRING SCANNING TESTS
// ============================================================================

test "scanString - simple quoted string" {
    const source = "\"hello\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("hello", token.lexeme);
}

test "scanString - empty string" {
    const source = "\"\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("", token.lexeme);
}

test "scanString - with escape sequences" {
    const source = "\"hello\\nworld\\t!\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("hello\\nworld\\t!", token.lexeme);
}

test "scanString - with escaped quotes" {
    const source = "\"say \\\"hello\\\"\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("say \\\"hello\\\"", token.lexeme);
}

test "scanString - multiline string" {
    const source = "\"line1\nline2\nline3\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("line1\nline2\nline3", token.lexeme);
}

test "scanString - unclosed string returns unknown" {
    const source = "\"hello";
    var lexer = Lexer.init(source);
    const token = lexer.scanString();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
    try std.testing.expectEqualStrings("hello", token.lexeme);
}

test "scanBlockString - simple block string" {
    const source = "#\"hello\"#";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("hello", token.lexeme);
}

test "scanBlockString - double hash" {
    const source = "##\"content\"##";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("content", token.lexeme);
}

test "scanBlockString - with nested quotes" {
    const source = "##\"outer \"inner\" outer\"##";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("outer \"inner\" outer", token.lexeme);
}

test "scanBlockString - multiline" {
    const source = "#\"line1\nline2\nline3\"#";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("line1\nline2\nline3", token.lexeme);
    try std.testing.expectEqual(@as(usize, 4), lexer.line); // Should be on line 4 after 3 newlines
}

test "scanBlockString - with single hash inside" {
    const source = "##\"contains #\"text\"# inside\"##";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("contains #\"text\"# inside", token.lexeme);
}

test "scanBlockString - unclosed returns unknown" {
    const source = "#\"hello";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
}

test "scanBlockString - mismatched hash count" {
    const source = "##\"hello\"#";
    var lexer = Lexer.init(source);
    const token = lexer.scanBlockString();

    try std.testing.expectEqual(TokenTag.unknown, token.tag);
}

test "scanUnquotedString - simple word" {
    const source = "hello";
    var lexer = Lexer.init(source);
    const token = lexer.scanUnquotedString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("hello", token.lexeme);
}

test "scanUnquotedString - stops at whitespace" {
    const source = "hello world";
    var lexer = Lexer.init(source);
    const token = lexer.scanUnquotedString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("hello", token.lexeme);
}

test "scanUnquotedString - stops at special chars" {
    const tests = [_]struct { source: []const u8, expected: []const u8 }{
        .{ .source = "value@", .expected = "value" },
        .{ .source = "value{", .expected = "value" },
        .{ .source = "value}", .expected = "value" },
        .{ .source = "value[", .expected = "value" },
        .{ .source = "value]", .expected = "value" },
        .{ .source = "value(", .expected = "value" },
        .{ .source = "value)", .expected = "value" },
        .{ .source = "value|", .expected = "value" },
        .{ .source = "value?", .expected = "value" },
        .{ .source = "value<", .expected = "value" },
        .{ .source = "value>", .expected = "value" },
        .{ .source = "value:", .expected = "value" },
        .{ .source = "value,", .expected = "value" },
        .{ .source = "value#", .expected = "value" },
        .{ .source = "value\"", .expected = "value" },
    };

    for (tests) |t| {
        var lexer = Lexer.init(t.source);
        const token = lexer.scanUnquotedString();
        try std.testing.expectEqualStrings(t.expected, token.lexeme);
    }
}

test "scanUnquotedString - alphanumeric with underscores" {
    const source = "test_value_123";
    var lexer = Lexer.init(source);
    const token = lexer.scanUnquotedString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("test_value_123", token.lexeme);
}

test "scanUnquotedString - dots and dashes" {
    const source = "test-value.txt";
    var lexer = Lexer.init(source);
    const token = lexer.scanUnquotedString();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("test-value.txt", token.lexeme);
}

// ============================================================================
// COMPREHENSIVE INTEGRATION TESTS
// ============================================================================

test "tokenize - complete BAML class with attributes" {
    const source =
        \\/// A person entity
        \\class Person {
        \\  name string @alias("full_name")
        \\  age int?
        \\  status Status
        \\  @@dynamic
        \\}
    ;
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    // Verify we have tokens (at minimum: docstring, class, identifier, lbrace,
    // multiple fields, double_at, identifier, rbrace, eof)
    try std.testing.expect(tokens.items.len > 20);

    // Verify first few tokens
    try std.testing.expectEqual(TokenTag.docstring, tokens.items[0].tag);
    try std.testing.expectEqual(TokenTag.keyword_class, tokens.items[1].tag);
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[2].tag);
    try std.testing.expectEqualStrings("Person", tokens.items[2].lexeme);
}

test "tokenize - enum with attributes" {
    const source =
        \\enum Status {
        \\  Active @alias("currently_active")
        \\  Inactive
        \\}
    ;
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expect(tokens.items.len > 10);
    try std.testing.expectEqual(TokenTag.keyword_enum, tokens.items[0].tag);
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[1].tag);
    try std.testing.expectEqualStrings("Status", tokens.items[1].lexeme);
}

test "tokenize - function with block string prompt" {
    const source =
        \\function Greet(p: Person) -> string {
        \\  client "openai/gpt-4"
        \\  prompt #"Hello {{ p.name }}"#
        \\}
    ;
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expect(tokens.items.len > 15);
    try std.testing.expectEqual(TokenTag.keyword_function, tokens.items[0].tag);
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[1].tag);
    try std.testing.expectEqualStrings("Greet", tokens.items[1].lexeme);
}

test "tokenize - client declaration with env variable" {
    const source =
        \\client<llm> MyClient {
        \\  provider "openai"
        \\  options {
        \\    api_key env.OPENAI_API_KEY
        \\  }
        \\}
    ;
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expect(tokens.items.len > 15);
    try std.testing.expectEqual(TokenTag.keyword_client, tokens.items[0].tag);

    // Find env token
    var found_env = false;
    for (tokens.items) |token| {
        if (token.tag == .env) {
            found_env = true;
            break;
        }
    }
    try std.testing.expect(found_env);
}

test "tokenize - test declaration" {
    const source =
        \\test MyTest {
        \\  functions [Greet]
        \\  args {
        \\    p { name "Alice" }
        \\  }
        \\}
    ;
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expect(tokens.items.len > 15);
    try std.testing.expectEqual(TokenTag.keyword_test, tokens.items[0].tag);
}

test "tokenize - union types with pipe" {
    const source = "result: string | int | null";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .identifier,
        .colon,
        .type_string,
        .pipe,
        .type_int,
        .pipe,
        .type_null,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - all primitive types" {
    const source = "string int float bool null image audio video pdf map";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .type_string,
        .type_int,
        .type_float,
        .type_bool,
        .type_null,
        .type_image,
        .type_audio,
        .type_video,
        .type_pdf,
        .type_map,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - all keywords" {
    const source = "class enum function client test generator template_string type";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .keyword_class,
        .keyword_enum,
        .keyword_function,
        .keyword_client,
        .keyword_test,
        .keyword_generator,
        .keyword_template_string,
        .keyword_type,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - nested block comment" {
    const source = "{# outer {# inner #} outer #} after";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 3), tokens.items.len);
    try std.testing.expectEqual(TokenTag.block_comment, tokens.items[0].tag);
    try std.testing.expectEqualStrings(" outer {# inner #} outer ", tokens.items[0].lexeme);
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[1].tag);
    try std.testing.expectEqualStrings("after", tokens.items[1].lexeme);
}

test "tokenize - mixed comments and code" {
    const source =
        \\// line comment
        \\/// docstring
        \\class Foo {
        \\  {# block comment #}
        \\  name string
        \\}
    ;
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    // Should have: comment, newline, docstring, newline, class, identifier, lbrace, newline,
    // block_comment, newline, identifier, type, newline, rbrace, eof
    try std.testing.expect(tokens.items.len >= 15);

    var has_comment = false;
    var has_docstring = false;
    var has_block_comment = false;

    for (tokens.items) |token| {
        if (token.tag == .comment) has_comment = true;
        if (token.tag == .docstring) has_docstring = true;
        if (token.tag == .block_comment) has_block_comment = true;
    }

    try std.testing.expect(has_comment);
    try std.testing.expect(has_docstring);
    try std.testing.expect(has_block_comment);
}

test "tokenize - complex nested structures" {
    const source = "data: map<string, Person[]>?";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    const expected_tags = [_]TokenTag{
        .identifier,
        .colon,
        .type_map,
        .less_than,
        .type_string,
        .comma,
        .identifier,
        .lbracket,
        .rbracket,
        .greater_than,
        .question,
        .eof,
    };

    try std.testing.expectEqual(expected_tags.len, tokens.items.len);
    for (expected_tags, 0..) |tag, i| {
        try std.testing.expectEqual(tag, tokens.items[i].tag);
    }
}

test "tokenize - attribute with arguments" {
    const source = "@alias(\"full_name\") @description(\"The person's name\")";
    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    // Should tokenize: @, identifier, (, string, ), @, identifier, (, string, ), eof
    try std.testing.expect(tokens.items.len >= 10);
    try std.testing.expectEqual(TokenTag.at, tokens.items[0].tag);
    try std.testing.expectEqual(TokenTag.identifier, tokens.items[1].tag);
    try std.testing.expectEqualStrings("alias", tokens.items[1].lexeme);
}

test "scanString - preserves lexeme correctly" {
    const source = "\"test string\"";
    var lexer = Lexer.init(source);
    const token = lexer.scanToken();

    try std.testing.expectEqual(TokenTag.string_literal, token.tag);
    try std.testing.expectEqualStrings("test string", token.lexeme);
}

test "complete BAML file tokenization" {
    const source =
        \\// Test file
        \\class Person {
        \\  name string
        \\  age int?
        \\}
        \\
        \\enum Status {
        \\  Active
        \\  Inactive
        \\}
        \\
        \\function Greet(p: Person) -> string {
        \\  client "openai/gpt-4"
        \\  prompt #"
        \\    Say hello to {{ p.name }}
        \\  "#
        \\}
    ;

    var lexer = Lexer.init(source);
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit(std.testing.allocator);

    // Verify tokenization completes without errors
    try std.testing.expect(tokens.items.len > 40);

    // Verify we have all major token types
    var has_class = false;
    var has_enum = false;
    var has_function = false;
    var has_comment = false;
    var has_string = false;

    for (tokens.items) |token| {
        switch (token.tag) {
            .keyword_class => has_class = true,
            .keyword_enum => has_enum = true,
            .keyword_function => has_function = true,
            .comment => has_comment = true,
            .string_literal => has_string = true,
            else => {},
        }
    }

    try std.testing.expect(has_class);
    try std.testing.expect(has_enum);
    try std.testing.expect(has_function);
    try std.testing.expect(has_comment);
    try std.testing.expect(has_string);

    // Verify EOF is last token
    try std.testing.expectEqual(TokenTag.eof, tokens.items[tokens.items.len - 1].tag);
}
