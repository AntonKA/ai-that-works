const std = @import("std");

/// Jinja token types for template parsing
pub const JinjaTokenType = enum {
    text, // Plain text outside Jinja constructs
    variable_start, // {{
    variable_end, // }}
    statement_start, // {%
    statement_end, // %}
    comment_start, // {#
    comment_end, // #}
    identifier, // Variable or function names
    dot, // . for property access
    pipe, // | for filters
    lparen, // (
    rparen, // )
    comma, // ,
    string_literal, // "..." or '...'
    number, // Integer or float
    eof,
};

/// A single token in a Jinja template
pub const JinjaToken = struct {
    type: JinjaTokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,
};

/// Lexer state for tracking context
const LexerState = enum {
    in_text,
    in_variable,
    in_statement,
    in_comment,
};

/// Jinja template lexer
pub const JinjaLexer = struct {
    source: []const u8,
    pos: usize,
    line: usize,
    column: usize,
    state: LexerState,

    pub fn init(source: []const u8) JinjaLexer {
        return JinjaLexer{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .state = .in_text,
        };
    }

    pub fn tokenize(self: *JinjaLexer, allocator: std.mem.Allocator) !std.ArrayList(JinjaToken) {
        var tokens = std.ArrayList(JinjaToken){};
        errdefer tokens.deinit(allocator);

        while (self.pos < self.source.len) {
            const token = try self.nextToken();
            try tokens.append(allocator, token);
            if (token.type == .eof) break;
        }

        return tokens;
    }

    fn nextToken(self: *JinjaLexer) !JinjaToken {
        if (self.pos >= self.source.len) {
            return JinjaToken{
                .type = .eof,
                .lexeme = "",
                .line = self.line,
                .column = self.column,
            };
        }

        // Check for Jinja delimiters
        if (self.peek2() == '{' and self.peekAhead(1) == '{') {
            self.state = .in_variable;
            return self.makeToken(.variable_start, 2);
        }
        if (self.peek2() == '}' and self.peekAhead(1) == '}') {
            self.state = .in_text;
            return self.makeToken(.variable_end, 2);
        }
        if (self.peek2() == '{' and self.peekAhead(1) == '%') {
            self.state = .in_statement;
            return self.makeToken(.statement_start, 2);
        }
        if (self.peek2() == '%' and self.peekAhead(1) == '}') {
            self.state = .in_text;
            return self.makeToken(.statement_end, 2);
        }
        if (self.peek2() == '{' and self.peekAhead(1) == '#') {
            self.state = .in_comment;
            return self.makeToken(.comment_start, 2);
        }
        if (self.peek2() == '#' and self.peekAhead(1) == '}') {
            self.state = .in_text;
            return self.makeToken(.comment_end, 2);
        }

        // Tokenize based on state
        switch (self.state) {
            .in_text => return self.scanText(),
            .in_variable, .in_statement => return self.scanExpression(),
            .in_comment => return self.scanCommentContent(),
        }
    }

    fn scanText(self: *JinjaLexer) !JinjaToken {
        const start = self.pos;
        const start_line = self.line;
        const start_column = self.column;

        // Scan until we hit a Jinja delimiter
        while (self.pos < self.source.len) {
            if (self.peek2() == '{' and
                (self.peekAhead(1) == '{' or self.peekAhead(1) == '%' or self.peekAhead(1) == '#')) {
                break;
            }
            if (self.peek2() == '\n') {
                self.line += 1;
                self.column = 1;
                self.pos += 1;
            } else {
                self.column += 1;
                self.pos += 1;
            }
        }

        const lexeme = self.source[start..self.pos];
        return JinjaToken{
            .type = .text,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    fn scanExpression(self: *JinjaLexer) !JinjaToken {
        self.skipWhitespace();

        if (self.pos >= self.source.len) {
            return JinjaToken{
                .type = .eof,
                .lexeme = "",
                .line = self.line,
                .column = self.column,
            };
        }

        const c = self.peek2();

        // Single-character tokens
        if (c == '.') return self.makeToken(.dot, 1);
        if (c == '|') return self.makeToken(.pipe, 1);
        if (c == '(') return self.makeToken(.lparen, 1);
        if (c == ')') return self.makeToken(.rparen, 1);
        if (c == ',') return self.makeToken(.comma, 1);

        // String literals
        if (c == '"' or c == '\'') return self.scanString(c);

        // Numbers
        if (std.ascii.isDigit(c)) return self.scanNumber();

        // Identifiers
        if (std.ascii.isAlphabetic(c) or c == '_') return self.scanIdentifier();

        // Unknown character - treat as text
        return self.makeToken(.text, 1);
    }

    fn scanIdentifier(self: *JinjaLexer) !JinjaToken {
        const start = self.pos;
        const start_line = self.line;
        const start_column = self.column;

        while (self.pos < self.source.len) {
            const c = self.peek2();
            if (!std.ascii.isAlphanumeric(c) and c != '_') break;
            self.advance();
        }

        const lexeme = self.source[start..self.pos];
        return JinjaToken{
            .type = .identifier,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    fn scanNumber(self: *JinjaLexer) !JinjaToken {
        const start = self.pos;
        const start_line = self.line;
        const start_column = self.column;

        while (self.pos < self.source.len and std.ascii.isDigit(self.peek2())) {
            self.advance();
        }

        // Check for decimal point
        if (self.pos < self.source.len and self.peek2() == '.' and
            self.pos + 1 < self.source.len and std.ascii.isDigit(self.peekAhead(1))) {
            self.advance(); // consume '.'
            while (self.pos < self.source.len and std.ascii.isDigit(self.peek2())) {
                self.advance();
            }
        }

        const lexeme = self.source[start..self.pos];
        return JinjaToken{
            .type = .number,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    fn scanString(self: *JinjaLexer, quote: u8) !JinjaToken {
        const start = self.pos;
        const start_line = self.line;
        const start_column = self.column;

        self.advance(); // consume opening quote

        while (self.pos < self.source.len and self.peek2() != quote) {
            if (self.peek2() == '\\' and self.pos + 1 < self.source.len) {
                self.advance(); // skip escape
            }
            self.advance();
        }

        if (self.pos < self.source.len) {
            self.advance(); // consume closing quote
        }

        const lexeme = self.source[start..self.pos];
        return JinjaToken{
            .type = .string_literal,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    fn skipWhitespace(self: *JinjaLexer) void {
        while (self.pos < self.source.len) {
            const c = self.peek2();
            if (c == ' ' or c == '\t' or c == '\r') {
                self.advance();
            } else if (c == '\n') {
                self.line += 1;
                self.column = 1;
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    fn peek2(self: *JinjaLexer) u8 {
        if (self.pos >= self.source.len) return 0;
        return self.source[self.pos];
    }

    fn peekAhead(self: *JinjaLexer, offset: usize) u8 {
        const pos = self.pos + offset;
        if (pos >= self.source.len) return 0;
        return self.source[pos];
    }

    fn advance(self: *JinjaLexer) void {
        self.pos += 1;
        self.column += 1;
    }

    fn scanCommentContent(self: *JinjaLexer) !JinjaToken {
        const start = self.pos;
        const start_line = self.line;
        const start_column = self.column;

        // Scan until we hit comment end
        while (self.pos < self.source.len) {
            if (self.peek2() == '#' and self.peekAhead(1) == '}') {
                break;
            }
            if (self.peek2() == '\n') {
                self.line += 1;
                self.column = 1;
                self.pos += 1;
            } else {
                self.column += 1;
                self.pos += 1;
            }
        }

        const lexeme = self.source[start..self.pos];
        return JinjaToken{
            .type = .text,
            .lexeme = lexeme,
            .line = start_line,
            .column = start_column,
        };
    }

    fn makeToken(self: *JinjaLexer, token_type: JinjaTokenType, len: usize) JinjaToken {
        const start = self.pos;
        const start_column = self.column;
        self.pos += len;
        self.column += len;
        return JinjaToken{
            .type = token_type,
            .lexeme = self.source[start..self.pos],
            .line = self.line,
            .column = start_column,
        };
    }
};

/// Jinja template node types
pub const JinjaNodeType = enum {
    text,
    variable, // {{ expr }}
    statement, // {% statement %}
    comment, // {# comment #}
};

/// A parsed Jinja template node
pub const JinjaNode = union(JinjaNodeType) {
    text: []const u8,
    variable: JinjaVariable,
    statement: JinjaStatement,
    comment: []const u8,

    pub fn deinit(self: *JinjaNode, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .variable => |*v| v.deinit(allocator),
            .statement => |*s| s.deinit(allocator),
            else => {},
        }
    }
};

/// A variable expression: {{ x.y.z }}
pub const JinjaVariable = struct {
    path: std.ArrayList([]const u8), // e.g., ["p", "name"]
    filters: std.ArrayList([]const u8),
    line: usize,
    column: usize,

    pub fn init(allocator: std.mem.Allocator, line: usize, column: usize) JinjaVariable {
        _ = allocator;
        return JinjaVariable{
            .path = std.ArrayList([]const u8){},
            .filters = std.ArrayList([]const u8){},
            .line = line,
            .column = column,
        };
    }

    pub fn deinit(self: *JinjaVariable, allocator: std.mem.Allocator) void {
        self.path.deinit(allocator);
        self.filters.deinit(allocator);
    }
};

/// Statement types for control flow
pub const JinjaStatementType = enum {
    for_start,
    endfor,
    if_start,
    elif,
    else_block,
    endif,
};

/// For loop statement: {% for x in items %}
pub const JinjaForStatement = struct {
    loop_var: []const u8, // "x" from "for x in items"
    iterable: []const u8, // "items" from "for x in items"
    iterable_path: std.ArrayList([]const u8), // ["items"] or ["ctx", "client", "provider"]
    line: usize,
    column: usize,

    pub fn deinit(self: *JinjaForStatement, allocator: std.mem.Allocator) void {
        self.iterable_path.deinit(allocator);
    }
};

/// If/elif statement: {% if condition %} or {% elif condition %}
pub const JinjaIfStatement = struct {
    condition: []const u8, // Raw condition string
    line: usize,
    column: usize,
};

/// End statement: {% endfor %}, {% endif %}, {% else %}
pub const JinjaEndStatement = struct {
    line: usize,
    column: usize,
};

/// A statement: {% for x in y %}, {% if x %}, etc.
pub const JinjaStatement = union(JinjaStatementType) {
    for_start: JinjaForStatement,
    endfor: JinjaEndStatement,
    if_start: JinjaIfStatement,
    elif: JinjaIfStatement,
    else_block: JinjaEndStatement,
    endif: JinjaEndStatement,

    pub fn deinit(self: *JinjaStatement, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .for_start => |*f| f.deinit(allocator),
            else => {},
        }
    }
};

/// Jinja template parser
pub const JinjaParser = struct {
    tokens: []const JinjaToken,
    pos: usize,

    pub fn init(tokens: []const JinjaToken) JinjaParser {
        return JinjaParser{
            .tokens = tokens,
            .pos = 0,
        };
    }

    pub fn parse(self: *JinjaParser, allocator: std.mem.Allocator) !std.ArrayList(JinjaNode) {
        var nodes = std.ArrayList(JinjaNode){};
        errdefer {
            for (nodes.items) |*node| {
                node.deinit(allocator);
            }
            nodes.deinit(allocator);
        }

        while (self.pos < self.tokens.len and self.peek().type != .eof) {
            const node = try self.parseNode(allocator);
            try nodes.append(allocator, node);
        }

        return nodes;
    }

    fn parseNode(self: *JinjaParser, allocator: std.mem.Allocator) !JinjaNode {
        const token = self.peek();

        switch (token.type) {
            .text => {
                self.advance();
                return JinjaNode{ .text = token.lexeme };
            },
            .variable_start => {
                return try self.parseVariable(allocator);
            },
            .statement_start => {
                return try self.parseStatement(allocator);
            },
            .comment_start => {
                return try self.parseComment(allocator);
            },
            else => {
                // Treat unexpected tokens as text
                self.advance();
                return JinjaNode{ .text = token.lexeme };
            },
        }
    }

    fn parseVariable(self: *JinjaParser, allocator: std.mem.Allocator) !JinjaNode {
        const start_token = self.expect(.variable_start);
        var variable = JinjaVariable.init(allocator, start_token.line, start_token.column);
        errdefer variable.deinit(allocator);

        // Parse variable path (e.g., p.name.first)
        while (self.pos < self.tokens.len) {
            const token = self.peek();

            if (token.type == .variable_end) {
                self.advance();
                break;
            }

            if (token.type == .identifier) {
                try variable.path.append(allocator, token.lexeme);
                self.advance();

                // Check for dot accessor
                if (self.pos < self.tokens.len and self.peek().type == .dot) {
                    self.advance(); // consume dot
                }
            } else if (token.type == .pipe) {
                self.advance(); // consume pipe
                // Parse filter name
                if (self.pos < self.tokens.len and self.peek().type == .identifier) {
                    const filter_token = self.peek();
                    try variable.filters.append(allocator, filter_token.lexeme);
                    self.advance();
                }
            } else {
                self.advance(); // skip unknown tokens
            }
        }

        return JinjaNode{ .variable = variable };
    }

    fn parseStatement(self: *JinjaParser, allocator: std.mem.Allocator) !JinjaNode {
        const start_token = self.expect(.statement_start);

        // Get statement type and dispatch to appropriate parser
        if (self.pos < self.tokens.len and self.peek().type == .identifier) {
            const stmt_type = self.peek().lexeme;

            if (std.mem.eql(u8, stmt_type, "for")) {
                return try self.parseForStatement(allocator, start_token);
            } else if (std.mem.eql(u8, stmt_type, "endfor")) {
                self.advance(); // consume "endfor"
                _ = self.expect(.statement_end);
                return JinjaNode{
                    .statement = JinjaStatement{
                        .endfor = JinjaEndStatement{
                            .line = start_token.line,
                            .column = start_token.column,
                        },
                    },
                };
            } else if (std.mem.eql(u8, stmt_type, "if")) {
                return try self.parseIfStatement(allocator, start_token, .if_start);
            } else if (std.mem.eql(u8, stmt_type, "elif")) {
                return try self.parseIfStatement(allocator, start_token, .elif);
            } else if (std.mem.eql(u8, stmt_type, "else")) {
                self.advance(); // consume "else"
                _ = self.expect(.statement_end);
                return JinjaNode{
                    .statement = JinjaStatement{
                        .else_block = JinjaEndStatement{
                            .line = start_token.line,
                            .column = start_token.column,
                        },
                    },
                };
            } else if (std.mem.eql(u8, stmt_type, "endif")) {
                self.advance(); // consume "endif"
                _ = self.expect(.statement_end);
                return JinjaNode{
                    .statement = JinjaStatement{
                        .endif = JinjaEndStatement{
                            .line = start_token.line,
                            .column = start_token.column,
                        },
                    },
                };
            }
        }

        // Unknown statement type - skip to end
        while (self.pos < self.tokens.len and self.peek().type != .statement_end) {
            self.advance();
        }
        if (self.pos < self.tokens.len) {
            _ = self.expect(.statement_end);
        }

        // Return an empty endif as a fallback
        return JinjaNode{
            .statement = JinjaStatement{
                .endif = JinjaEndStatement{
                    .line = start_token.line,
                    .column = start_token.column,
                },
            },
        };
    }

    fn parseForStatement(
        self: *JinjaParser,
        allocator: std.mem.Allocator,
        start_token: JinjaToken,
    ) !JinjaNode {
        // Expect: for <loop_var> in <iterable>
        _ = self.expect(.identifier); // consume "for"

        // Get loop variable
        var loop_var: []const u8 = "";
        if (self.pos < self.tokens.len and self.peek().type == .identifier) {
            loop_var = self.peek().lexeme;
            self.advance();
        }

        // Expect "in" keyword
        if (self.pos < self.tokens.len and self.peek().type == .identifier) {
            const in_token = self.peek();
            if (!std.mem.eql(u8, in_token.lexeme, "in")) {
                // Error: expected "in" - skip to end
                while (self.pos < self.tokens.len and self.peek().type != .statement_end) {
                    self.advance();
                }
                if (self.pos < self.tokens.len) {
                    _ = self.expect(.statement_end);
                }
                return JinjaNode{
                    .statement = JinjaStatement{
                        .for_start = JinjaForStatement{
                            .loop_var = loop_var,
                            .iterable = "",
                            .iterable_path = std.ArrayList([]const u8){},
                            .line = start_token.line,
                            .column = start_token.column,
                        },
                    },
                };
            }
            self.advance(); // consume "in"
        }

        // Parse iterable (could be simple identifier or path like ctx.client.provider)
        var iterable: []const u8 = "";
        var iterable_path = std.ArrayList([]const u8){};
        errdefer iterable_path.deinit(allocator);

        if (self.pos < self.tokens.len and self.peek().type == .identifier) {
            iterable = self.peek().lexeme;
            try iterable_path.append(allocator, iterable);
            self.advance();

            // Check for dot-path (e.g., ctx.client.provider)
            while (self.pos < self.tokens.len and self.peek().type == .dot) {
                self.advance(); // consume dot
                if (self.pos < self.tokens.len and self.peek().type == .identifier) {
                    const next = self.peek();
                    try iterable_path.append(allocator, next.lexeme);
                    self.advance();
                }
            }
        }

        _ = self.expect(.statement_end);

        return JinjaNode{
            .statement = JinjaStatement{
                .for_start = JinjaForStatement{
                    .loop_var = loop_var,
                    .iterable = iterable,
                    .iterable_path = iterable_path,
                    .line = start_token.line,
                    .column = start_token.column,
                },
            },
        };
    }

    fn parseIfStatement(
        self: *JinjaParser,
        allocator: std.mem.Allocator,
        start_token: JinjaToken,
        stmt_type: JinjaStatementType,
    ) !JinjaNode {
        _ = self.expect(.identifier); // consume "if" or "elif"

        // Collect condition tokens until statement_end
        const condition_start = self.pos;
        var condition_parts = std.ArrayList([]const u8){};
        defer condition_parts.deinit(allocator);

        while (self.pos < self.tokens.len and self.peek().type != .statement_end) {
            const token = self.peek();
            try condition_parts.append(allocator, token.lexeme);
            self.advance();
        }

        _ = self.expect(.statement_end);

        // Build condition string
        var condition: []const u8 = "";
        if (condition_start < self.tokens.len) {
            condition = self.tokens[condition_start].lexeme;
        }

        const if_stmt = JinjaIfStatement{
            .condition = condition,
            .line = start_token.line,
            .column = start_token.column,
        };

        return JinjaNode{
            .statement = if (stmt_type == .if_start)
                JinjaStatement{ .if_start = if_stmt }
            else
                JinjaStatement{ .elif = if_stmt },
        };
    }

    fn parseComment(self: *JinjaParser, allocator: std.mem.Allocator) !JinjaNode {
        _ = allocator;
        _ = self.expect(.comment_start);

        var content: []const u8 = "";

        // Collect comment content
        while (self.pos < self.tokens.len and self.peek().type != .comment_end) {
            const token = self.peek();
            if (token.type == .text or token.type == .identifier) {
                content = token.lexeme;
            }
            self.advance();
        }

        if (self.pos < self.tokens.len) {
            _ = self.expect(.comment_end);
        }

        return JinjaNode{ .comment = content };
    }

    fn peek(self: *JinjaParser) JinjaToken {
        if (self.pos >= self.tokens.len) {
            return JinjaToken{
                .type = .eof,
                .lexeme = "",
                .line = 0,
                .column = 0,
            };
        }
        return self.tokens[self.pos];
    }

    fn advance(self: *JinjaParser) void {
        if (self.pos < self.tokens.len) {
            self.pos += 1;
        }
    }

    fn expect(self: *JinjaParser, expected: JinjaTokenType) JinjaToken {
        const token = self.peek();
        if (token.type == expected) {
            self.advance();
            return token;
        }
        return token;
    }
};

/// Statement context for tracking nesting
pub const StatementContext = struct {
    type: enum { for_loop, if_block },
    line: usize,
    column: usize,
    loop_var: ?[]const u8, // Only for for_loop
};

/// Jinja template validator
pub const JinjaValidator = struct {
    allocator: std.mem.Allocator,
    errors: std.ArrayList(ValidationError),
    param_names: std.StringHashMap(void),
    statement_stack: std.ArrayList(StatementContext), // Track nesting
    loop_vars: std.StringHashMap(void), // Track loop variables in scope

    pub const ValidationError = struct {
        message: []const u8,
        line: usize,
        column: usize,
    };

    pub fn init(allocator: std.mem.Allocator) JinjaValidator {
        return JinjaValidator{
            .allocator = allocator,
            .errors = std.ArrayList(ValidationError){},
            .param_names = std.StringHashMap(void).init(allocator),
            .statement_stack = std.ArrayList(StatementContext){},
            .loop_vars = std.StringHashMap(void).init(allocator),
        };
    }

    pub fn deinit(self: *JinjaValidator) void {
        self.errors.deinit(self.allocator);
        self.param_names.deinit();
        self.statement_stack.deinit(self.allocator);
        self.loop_vars.deinit();
    }

    /// Add a parameter name to the list of valid variables
    pub fn addParameter(self: *JinjaValidator, name: []const u8) !void {
        try self.param_names.put(name, {});
    }

    /// Validate a Jinja template
    pub fn validate(self: *JinjaValidator, template: []const u8) !void {
        var lexer = JinjaLexer.init(template);
        var tokens = try lexer.tokenize(self.allocator);
        defer tokens.deinit(self.allocator);

        var parser = JinjaParser.init(tokens.items);
        var nodes = try parser.parse(self.allocator);
        defer {
            for (nodes.items) |*node| {
                node.deinit(self.allocator);
            }
            nodes.deinit(self.allocator);
        }

        // Validate each node
        for (nodes.items) |*node| {
            try self.validateNode(node);
        }

        // Check for unclosed blocks
        if (self.statement_stack.items.len > 0) {
            const unclosed = self.statement_stack.items[0];
            const block_type = if (unclosed.type == .for_loop) "{% for %}" else "{% if %}";
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "Unclosed {s} block",
                .{block_type},
            );
            try self.addError(msg, unclosed.line, unclosed.column);
        }
    }

    fn validateNode(self: *JinjaValidator, node: *const JinjaNode) !void {
        switch (node.*) {
            .variable => |*v| try self.validateVariable(v),
            .statement => |*s| try self.validateStatement(s),
            else => {},
        }
    }

    fn validateVariable(self: *JinjaValidator, variable: *const JinjaVariable) !void {
        if (variable.path.items.len == 0) {
            try self.addError("Empty variable reference", variable.line, variable.column);
            return;
        }

        const root = variable.path.items[0];

        // Check for BAML built-ins
        if (std.mem.eql(u8, root, "ctx") or std.mem.eql(u8, root, "_")) {
            // Built-in variables are always valid
            return;
        }

        // Check if it's a loop variable
        if (self.loop_vars.contains(root)) {
            return; // Loop variables are valid in their scope
        }

        // Check if it's a declared parameter
        if (!self.param_names.contains(root)) {
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "Undefined variable '{s}' - not found in function parameters",
                .{root},
            );
            try self.addError(msg, variable.line, variable.column);
        }
    }

    fn validateStatement(self: *JinjaValidator, statement: *const JinjaStatement) !void {
        switch (statement.*) {
            .for_start => |*for_stmt| {
                // Validate iterable exists in parameters or is built-in
                try self.validateIterableReference(for_stmt);

                // Add loop variable to scope
                try self.loop_vars.put(for_stmt.loop_var, {});

                // Push for_loop onto stack
                try self.statement_stack.append(self.allocator, StatementContext{
                    .type = .for_loop,
                    .line = for_stmt.line,
                    .column = for_stmt.column,
                    .loop_var = for_stmt.loop_var,
                });
            },
            .endfor => |*end_stmt| {
                // Pop statement stack and validate it was a for_loop
                if (self.statement_stack.items.len == 0) {
                    try self.addError("Unmatched {% endfor %}", end_stmt.line, end_stmt.column);
                    return;
                }
                const context = self.statement_stack.items[self.statement_stack.items.len - 1];
                _ = self.statement_stack.pop();
                if (context.type != .for_loop) {
                    try self.addError("{% endfor %} without matching {% for %}", end_stmt.line, end_stmt.column);
                }

                // Remove loop variable from scope
                if (context.loop_var) |loop_var| {
                    _ = self.loop_vars.remove(loop_var);
                }
            },
            .if_start => |*if_stmt| {
                // Push if_block onto stack
                try self.statement_stack.append(self.allocator, StatementContext{
                    .type = .if_block,
                    .line = if_stmt.line,
                    .column = if_stmt.column,
                    .loop_var = null,
                });
            },
            .elif => |*elif_stmt| {
                // Validate we're inside an if block
                if (self.statement_stack.items.len == 0) {
                    try self.addError("{% elif %} without {% if %}", elif_stmt.line, elif_stmt.column);
                    return;
                }
                const top = self.statement_stack.items[self.statement_stack.items.len - 1];
                if (top.type != .if_block) {
                    try self.addError("{% elif %} must be inside {% if %} block", elif_stmt.line, elif_stmt.column);
                }
            },
            .else_block => |*else_stmt| {
                // Validate we're inside a for or if block
                if (self.statement_stack.items.len == 0) {
                    try self.addError("{% else %} without opening block", else_stmt.line, else_stmt.column);
                }
            },
            .endif => |*end_stmt| {
                // Pop statement stack and validate it was an if_block
                if (self.statement_stack.items.len == 0) {
                    try self.addError("Unmatched {% endif %}", end_stmt.line, end_stmt.column);
                    return;
                }
                const context = self.statement_stack.items[self.statement_stack.items.len - 1];
                _ = self.statement_stack.pop();
                if (context.type != .if_block) {
                    try self.addError("{% endif %} without matching {% if %}", end_stmt.line, end_stmt.column);
                }
            },
        }
    }

    fn validateIterableReference(self: *JinjaValidator, for_stmt: *const JinjaForStatement) !void {
        const root = for_stmt.iterable;

        // Empty iterable
        if (root.len == 0) {
            try self.addError("Empty iterable in for loop", for_stmt.line, for_stmt.column);
            return;
        }

        // Check for BAML built-ins
        if (std.mem.eql(u8, root, "ctx") or std.mem.eql(u8, root, "_")) {
            return;
        }

        // Check if it's a declared parameter
        if (!self.param_names.contains(root)) {
            const msg = try std.fmt.allocPrint(
                self.allocator,
                "Undefined iterable '{s}' in for loop - not found in function parameters",
                .{root},
            );
            try self.addError(msg, for_stmt.line, for_stmt.column);
        }
    }

    fn addError(self: *JinjaValidator, message: []const u8, line: usize, column: usize) !void {
        try self.errors.append(self.allocator, ValidationError{
            .message = message,
            .line = line,
            .column = column,
        });
    }

    pub fn hasErrors(self: *const JinjaValidator) bool {
        return self.errors.items.len > 0;
    }

    pub fn getErrors(self: *const JinjaValidator) []const ValidationError {
        return self.errors.items;
    }
};

/// Validate a function's prompt template against its parameters
pub fn validateFunctionPrompt(
    allocator: std.mem.Allocator,
    prompt: []const u8,
    parameters: []const []const u8,
) ![]const JinjaValidator.ValidationError {
    var validator = JinjaValidator.init(allocator);
    defer validator.deinit();

    // Add all parameter names
    for (parameters) |param_name| {
        try validator.addParameter(param_name);
    }

    // Validate the template
    try validator.validate(prompt);

    // Return a copy of the errors
    const errors = try allocator.alloc(JinjaValidator.ValidationError, validator.errors.items.len);
    @memcpy(errors, validator.errors.items);
    return errors;
}

// Tests
test "JinjaLexer: tokenize simple text" {
    var lexer = JinjaLexer.init("Hello, world!");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(@as(usize, 2), tokens.items.len);
    try std.testing.expectEqual(JinjaTokenType.text, tokens.items[0].type);
    try std.testing.expectEqual(JinjaTokenType.eof, tokens.items[1].type);
}

test "JinjaLexer: tokenize variable" {
    var lexer = JinjaLexer.init("{{ name }}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(@as(usize, 3), tokens.items.len);
    try std.testing.expectEqual(JinjaTokenType.variable_start, tokens.items[0].type);
    try std.testing.expectEqual(JinjaTokenType.variable_end, tokens.items[1].type);
}

test "JinjaLexer: tokenize statement" {
    var lexer = JinjaLexer.init("{% for x in items %}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(@as(usize, 3), tokens.items.len);
    try std.testing.expectEqual(JinjaTokenType.statement_start, tokens.items[0].type);
    try std.testing.expectEqual(JinjaTokenType.statement_end, tokens.items[1].type);
}

test "JinjaLexer: tokenize comment" {
    var lexer = JinjaLexer.init("{# This is a comment #}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    try std.testing.expectEqual(@as(usize, 3), tokens.items.len);
    try std.testing.expectEqual(JinjaTokenType.comment_start, tokens.items[0].type);
    try std.testing.expectEqual(JinjaTokenType.comment_end, tokens.items[1].type);
}

test "JinjaParser: parse simple variable" {
    var lexer = JinjaLexer.init("{{ name }}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    var parser = JinjaParser.init(tokens.items);
    var nodes = try parser.parse(std.testing.allocator);
    defer {
        for (nodes.items) |*node| {
            node.deinit(std.testing.allocator);
        }
        nodes.deinit();
    }

    try std.testing.expectEqual(@as(usize, 1), nodes.items.len);
    try std.testing.expect(nodes.items[0] == .variable);
}

test "JinjaParser: parse text and variable" {
    var lexer = JinjaLexer.init("Hello {{ name }}!");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    var parser = JinjaParser.init(tokens.items);
    var nodes = try parser.parse(std.testing.allocator);
    defer {
        for (nodes.items) |*node| {
            node.deinit(std.testing.allocator);
        }
        nodes.deinit();
    }

    try std.testing.expectEqual(@as(usize, 3), nodes.items.len);
    try std.testing.expect(nodes.items[0] == .text);
    try std.testing.expect(nodes.items[1] == .variable);
    try std.testing.expect(nodes.items[2] == .text);
}

test "JinjaValidator: valid parameter reference" {
    const params = [_][]const u8{"name"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "Hello {{ name }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: undefined variable" {
    const params = [_][]const u8{"name"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "Hello {{ age }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expect(std.mem.indexOf(u8, errors[0].message, "Undefined variable") != null);
}

test "JinjaValidator: BAML built-in ctx" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{{ ctx.output_format }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: BAML built-in underscore" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{{ _.role(\"user\") }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: multiple valid parameters" {
    const params = [_][]const u8{ "text", "image" };
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "Text: {{ text }}\nImage: {{ image }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: property access on parameter" {
    const params = [_][]const u8{"person"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "Hello {{ person.name }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    // Should not error - we validate the root variable, not nested properties
    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: complex template with mixed content" {
    const params = [_][]const u8{ "p", "text" };
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        \\{{ _.role("user") }}
        \\Extract person from: {{ text }}
        \\Name: {{ p.name }}
        \\
        \\{{ ctx.output_format }}
    ,
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

// ===== Loop and Conditional Tests =====

test "JinjaParser: parse for loop" {
    var lexer = JinjaLexer.init("{% for item in items %}{{ item }}{% endfor %}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    var parser = JinjaParser.init(tokens.items);
    var nodes = try parser.parse(std.testing.allocator);
    defer {
        for (nodes.items) |*node| {
            node.deinit(std.testing.allocator);
        }
        nodes.deinit();
    }

    try std.testing.expectEqual(@as(usize, 3), nodes.items.len);
    try std.testing.expect(nodes.items[0] == .statement);
    try std.testing.expect(nodes.items[0].statement == .for_start);
    try std.testing.expect(nodes.items[1] == .variable);
    try std.testing.expect(nodes.items[2] == .statement);
    try std.testing.expect(nodes.items[2].statement == .endfor);

    const for_stmt = nodes.items[0].statement.for_start;
    try std.testing.expectEqualStrings("item", for_stmt.loop_var);
    try std.testing.expectEqualStrings("items", for_stmt.iterable);
}

test "JinjaParser: parse if statement" {
    var lexer = JinjaLexer.init("{% if condition %}Yes{% endif %}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    var parser = JinjaParser.init(tokens.items);
    var nodes = try parser.parse(std.testing.allocator);
    defer {
        for (nodes.items) |*node| {
            node.deinit(std.testing.allocator);
        }
        nodes.deinit();
    }

    try std.testing.expectEqual(@as(usize, 3), nodes.items.len);
    try std.testing.expect(nodes.items[0] == .statement);
    try std.testing.expect(nodes.items[0].statement == .if_start);
    try std.testing.expect(nodes.items[1] == .text);
    try std.testing.expect(nodes.items[2] == .statement);
    try std.testing.expect(nodes.items[2].statement == .endif);
}

test "JinjaParser: parse if-elif-else statement" {
    var lexer = JinjaLexer.init("{% if x %}A{% elif y %}B{% else %}C{% endif %}");
    var tokens = try lexer.tokenize(std.testing.allocator);
    defer tokens.deinit();

    var parser = JinjaParser.init(tokens.items);
    var nodes = try parser.parse(std.testing.allocator);
    defer {
        for (nodes.items) |*node| {
            node.deinit(std.testing.allocator);
        }
        nodes.deinit();
    }

    try std.testing.expectEqual(@as(usize, 8), nodes.items.len);
    try std.testing.expect(nodes.items[0].statement == .if_start);
    try std.testing.expect(nodes.items[2].statement == .elif);
    try std.testing.expect(nodes.items[4].statement == .else_block);
    try std.testing.expect(nodes.items[6].statement == .endif);
}

test "JinjaValidator: valid for loop with parameter" {
    const params = [_][]const u8{"messages"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% for m in messages %}{{ m }}{% endfor %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: loop variable in scope" {
    const params = [_][]const u8{"items"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% for item in items %}Name: {{ item.name }}{% endfor %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    // Loop variable 'item' should be valid inside the loop
    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: undefined iterable in for loop" {
    const params = [_][]const u8{"other"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% for item in items %}{{ item }}{% endfor %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expect(std.mem.indexOf(u8, errors[0].message, "Undefined iterable") != null);
}

test "JinjaValidator: unmatched endfor" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% endfor %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expect(std.mem.indexOf(u8, errors[0].message, "Unmatched") != null);
}

test "JinjaValidator: unclosed for loop" {
    const params = [_][]const u8{"items"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% for item in items %}{{ item }}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expect(std.mem.indexOf(u8, errors[0].message, "Unclosed") != null);
}

test "JinjaValidator: valid if block" {
    const params = [_][]const u8{"condition"};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% if condition %}Yes{% endif %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: unmatched endif" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% endif %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expect(std.mem.indexOf(u8, errors[0].message, "Unmatched") != null);
}

test "JinjaValidator: elif without if" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% elif condition %}Yes{% endif %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 2), errors.len);
    // First error: elif without if
    // Second error: unmatched endif (because the if block was never opened)
}

test "JinjaValidator: else without opening block" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% else %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 1), errors.len);
    try std.testing.expect(std.mem.indexOf(u8, errors[0].message, "{% else %}") != null);
}

test "JinjaValidator: nested for loops" {
    const params = [_][]const u8{ "outer", "inner" };
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        \\{% for o in outer %}
        \\  {% for i in inner %}
        \\    {{ o }} {{ i }}
        \\  {% endfor %}
        \\{% endfor %}
    ,
        &params,
    );
    defer std.testing.allocator.free(errors);

    // Both loop variables should be valid
    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: for loop with built-in iterable" {
    const params = [_][]const u8{};
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        "{% for m in ctx.messages %}{{ m }}{% endfor %}",
        &params,
    );
    defer std.testing.allocator.free(errors);

    // ctx is a built-in, should be valid
    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "JinjaValidator: complete example with loops and conditionals" {
    const params = [_][]const u8{ "messages", "show_role" };
    const errors = try validateFunctionPrompt(
        std.testing.allocator,
        \\{% for m in messages %}
        \\  {% if show_role %}
        \\    {{ _.role(m.role) }}
        \\  {% endif %}
        \\  {{ m.content }}
        \\{% endfor %}
        \\{{ ctx.output_format }}
    ,
        &params,
    );
    defer std.testing.allocator.free(errors);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}
