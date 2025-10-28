const std = @import("std");
const minibaml = @import("_2025_10_28_ralph_wiggum_coding_");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        return;
    }

    const command = args[1];

    // Handle flags
    if (std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        std.debug.print("minibaml version {s}\n", .{minibaml.getVersion()});
        return;
    }

    if (std.mem.eql(u8, command, "--help") or std.mem.eql(u8, command, "-h")) {
        printUsage();
        return;
    }

    // Handle commands
    if (std.mem.eql(u8, command, "fmt")) {
        if (args.len < 3) {
            try printError("fmt command requires a file argument", "minibaml fmt <file.baml>");
            return;
        }
        try formatCommand(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "generate") or std.mem.eql(u8, command, "gen")) {
        if (args.len < 3) {
            try printError("generate command requires a file argument", "minibaml generate <file.baml> [--typescript|--python|--go|--typebuilder]");
            return;
        }
        const path = args[2];
        var use_typescript = false;
        var use_go = false;
        var typebuilder_only = false;

        // Check for flags
        if (args.len >= 4) {
            if (std.mem.eql(u8, args[3], "--typescript") or std.mem.eql(u8, args[3], "-ts")) {
                use_typescript = true;
            } else if (std.mem.eql(u8, args[3], "--go")) {
                use_go = true;
            } else if (std.mem.eql(u8, args[3], "--typebuilder") or std.mem.eql(u8, args[3], "-tb")) {
                typebuilder_only = true;
            }
        }

        try generateCommand(allocator, path, use_typescript, use_go, typebuilder_only);
    } else if (std.mem.eql(u8, command, "parse")) {
        if (args.len < 3) {
            try printError("parse command requires a file argument", "minibaml parse <file.baml>");
            return;
        }
        try parseCommand(allocator, args[2]);
    } else if (std.mem.eql(u8, command, "check")) {
        if (args.len < 3) {
            try printError("check command requires a file argument", "minibaml check <file.baml>");
            return;
        }
        try checkCommand(allocator, args[2]);
    } else {
        // Default: tokenize
        try tokenizeCommand(allocator, command);
    }
}

fn printUsage() void {
    std.fs.File.stdout().writeAll(
        \\minibaml - BAML language tool
        \\
        \\Usage:
        \\  minibaml <file.baml>              Tokenize a BAML file
        \\  minibaml parse <path>             Parse and show AST (file or directory)
        \\  minibaml check <path>             Validate BAML file or directory
        \\  minibaml fmt <file.baml>          Format a BAML file
        \\  minibaml generate <path> [opts]   Generate code from BAML (default: Python)
        \\  minibaml gen <path> [opts]        Alias for generate
        \\
        \\Code Generation Options:
        \\  --python                          Generate Python code (default)
        \\  --typescript, -ts                 Generate TypeScript code
        \\  --go                              Generate Go code
        \\  --typebuilder, -tb                Generate Python TypeBuilder module only
        \\
        \\Global Options:
        \\  --help, -h                        Show this help message
        \\  --version, -v                     Show version information
        \\
        \\Examples:
        \\  minibaml test.baml                # Show tokens
        \\  minibaml parse test.baml          # Show parsed AST (single file)
        \\  minibaml parse baml_src           # Show parsed AST (directory)
        \\  minibaml check baml_src           # Validate directory
        \\  minibaml fmt test.baml            # Format and print
        \\  minibaml generate baml_src        # Generate Python code
        \\  minibaml gen baml_src --typescript # Generate TypeScript code
        \\  minibaml gen baml_src --go        # Generate Go code
        \\  minibaml gen baml_src --typebuilder > type_builder.py # Generate TypeBuilder
        \\
    ) catch {};
}

fn printError(message: []const u8, usage: []const u8) !void {
    std.debug.print("Error: {s}\n", .{message});
    std.debug.print("Usage: {s}\n", .{usage});
}

const ParseResult = struct {
    tree: minibaml.Ast,
    parser: minibaml.Parser,
    source: []const u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *ParseResult) void {
        self.tree.deinit();
        self.parser.deinit();
        self.allocator.free(self.source);
    }
};

fn isDirectory(path: []const u8) bool {
    const stat = std.fs.cwd().statFile(path) catch |err| {
        if (err == error.FileNotFound) {
            // Try as directory
            var dir = std.fs.cwd().openDir(path, .{}) catch {
                return false;
            };
            dir.close();
            return true;
        }
        return false;
    };
    return stat.kind == .directory;
}

fn parseFile(allocator: std.mem.Allocator, filename: []const u8) !ParseResult {
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        std.debug.print("Error: Cannot open file '{s}': {s}\n", .{ filename, @errorName(err) });
        return err;
    };
    defer file.close();

    const source = file.readToEndAlloc(allocator, 1024 * 1024) catch |err| {
        std.debug.print("Error: Cannot read file '{s}': {s}\n", .{ filename, @errorName(err) });
        return err;
    };
    errdefer allocator.free(source);

    var lex = minibaml.Lexer.init(source);
    var tokens = try lex.tokenize(allocator);
    defer tokens.deinit(allocator);

    var parser = minibaml.Parser.init(allocator, tokens.items);
    errdefer parser.deinit();

    var tree = minibaml.Ast.init(allocator);
    errdefer tree.deinit();

    while (!parser.isAtEnd()) {
        parser.skipTrivia();
        if (parser.isAtEnd()) break;

        const current = parser.peek() orelse break;

        const decl: minibaml.Declaration = switch (current.tag) {
            .keyword_class => .{ .class_decl = try parser.parseClassDecl() },
            .keyword_enum => .{ .enum_decl = try parser.parseEnumDecl() },
            .keyword_function => .{ .function_decl = try parser.parseFunctionDecl() },
            .keyword_client => .{ .client_decl = try parser.parseClientDecl() },
            .keyword_test => .{ .test_decl = try parser.parseTestDecl() },
            .keyword_generator => .{ .generator_decl = try parser.parseGeneratorDecl() },
            .keyword_template_string => .{ .template_string_decl = try parser.parseTemplateStringDecl() },
            else => {
                std.debug.print("Error: Unexpected token '{s}' at line {d}, col {d}\n", .{
                    @tagName(current.tag),
                    current.line,
                    current.column,
                });
                return error.UnexpectedToken;
            },
        };

        try tree.declarations.append(allocator, decl);
        parser.skipTrivia();
    }

    if (parser.errors.items.len > 0) {
        std.debug.print("Parse errors in '{s}':\n", .{filename});
        for (parser.errors.items) |err| {
            std.debug.print("  Line {d}, Col {d}: {s}\n", .{ err.line, err.column, err.message });
        }
        return error.ParseError;
    }

    return ParseResult{
        .tree = tree,
        .parser = parser,
        .source = source, // Keep source alive for AST string pointers
        .allocator = allocator,
    };
}

fn tokenizeCommand(allocator: std.mem.Allocator, filename: []const u8) !void {
    const file = std.fs.cwd().openFile(filename, .{}) catch |err| {
        std.debug.print("Error: Cannot open file '{s}': {s}\n", .{ filename, @errorName(err) });
        return err;
    };
    defer file.close();

    const source = file.readToEndAlloc(allocator, 1024 * 1024) catch |err| {
        std.debug.print("Error: Cannot read file '{s}': {s}\n", .{ filename, @errorName(err) });
        return err;
    };
    defer allocator.free(source);

    var lex = minibaml.Lexer.init(source);
    var tokens = try lex.tokenize(allocator);
    defer tokens.deinit(allocator);

    std.debug.print("Tokenized {s}: {d} tokens\n\n", .{ filename, tokens.items.len });

    for (tokens.items, 0..) |token, i| {
        std.debug.print("{d:4}: {s:20} | Line {d:3}, Col {d:3} | \"{s}\"\n", .{
            i,
            @tagName(token.tag),
            token.line,
            token.column,
            token.lexeme,
        });
    }
}

fn parseCommand(allocator: std.mem.Allocator, path: []const u8) !void {
    if (isDirectory(path)) {
        try parseDirectory(allocator, path);
    } else {
        try parseSingleFile(allocator, path);
    }
}

fn parseSingleFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    var result = try parseFile(allocator, filename);
    defer result.deinit();

    std.debug.print("Successfully parsed {s}\n\n", .{filename});
    std.debug.print("Declarations: {d}\n", .{result.tree.declarations.items.len});

    for (result.tree.declarations.items, 0..) |decl, i| {
        std.debug.print("\n{d}. ", .{i + 1});
        switch (decl) {
            .class_decl => |class| std.debug.print("class {s} ({d} properties)", .{ class.name, class.properties.items.len }),
            .enum_decl => |enum_decl| std.debug.print("enum {s} ({d} values)", .{ enum_decl.name, enum_decl.values.items.len }),
            .function_decl => |func| std.debug.print("function {s} ({d} parameters)", .{ func.name, func.parameters.items.len }),
            .client_decl => |client| std.debug.print("client<llm> {s}", .{client.name}),
            .test_decl => |test_decl| std.debug.print("test {s} ({d} functions)", .{ test_decl.name, test_decl.functions.items.len }),
            .generator_decl => |gen| std.debug.print("generator {s}", .{gen.name}),
            .template_string_decl => |template| std.debug.print("template_string {s} ({d} parameters)", .{ template.name, template.parameters.items.len }),
            .type_alias_decl => |alias| std.debug.print("type {s}", .{alias.name}),
        }
    }
    std.debug.print("\n", .{});
}

fn parseDirectory(allocator: std.mem.Allocator, dir_path: []const u8) !void {
    var project = minibaml.MultiFileProject.init(allocator);
    defer project.deinit();

    std.debug.print("Loading BAML files from '{s}'...\n\n", .{dir_path});
    try project.loadDirectory(dir_path);

    const files = project.getFiles();
    std.debug.print("Successfully parsed {d} file(s):\n\n", .{files.len});

    for (files) |file| {
        std.debug.print("  {s}\n", .{file.path});
        std.debug.print("    Declarations: {d}\n", .{file.tree.declarations.items.len});
        for (file.tree.declarations.items) |decl| {
            switch (decl) {
                .class_decl => |class| std.debug.print("      - class {s}\n", .{class.name}),
                .enum_decl => |enum_decl| std.debug.print("      - enum {s}\n", .{enum_decl.name}),
                .function_decl => |func| std.debug.print("      - function {s}\n", .{func.name}),
                .client_decl => |client| std.debug.print("      - client<llm> {s}\n", .{client.name}),
                .test_decl => |test_decl| std.debug.print("      - test {s}\n", .{test_decl.name}),
                .generator_decl => |gen| std.debug.print("      - generator {s}\n", .{gen.name}),
                .template_string_decl => |template| std.debug.print("      - template_string {s}\n", .{template.name}),
                .type_alias_decl => |alias| std.debug.print("      - type {s}\n", .{alias.name}),
            }
        }
        std.debug.print("\n", .{});
    }

    const merged_ast = project.getMergedAst();
    std.debug.print("Merged AST: {d} total declarations\n", .{merged_ast.declarations.items.len});
}

fn checkCommand(allocator: std.mem.Allocator, path: []const u8) !void {
    if (isDirectory(path)) {
        try checkDirectory(allocator, path);
    } else {
        try checkFile(allocator, path);
    }
}

fn checkFile(allocator: std.mem.Allocator, filename: []const u8) !void {
    var result = try parseFile(allocator, filename);
    defer result.deinit();

    var validator = minibaml.Validator.init(allocator);
    defer validator.deinit();

    validator.validate(&result.tree) catch |err| {
        std.debug.print("Validation failed: {s}\n", .{@errorName(err)});
    };

    if (validator.diagnostics.items.len == 0) {
        std.debug.print("✓ {s} is valid\n", .{filename});
    } else {
        std.debug.print("Validation errors in '{s}':\n\n", .{filename});
        for (validator.diagnostics.items) |diag| {
            const severity = switch (diag.severity) {
                .err => "error",
                .warning => "warning",
                .info => "info",
            };
            std.debug.print("  [{s}] Line {d}, Col {d}: {s}\n", .{
                severity,
                diag.line,
                diag.column,
                diag.message,
            });
        }
        std.debug.print("\nFound {d} error(s)\n", .{validator.diagnostics.items.len});
        std.process.exit(1);
    }
}

fn checkDirectory(allocator: std.mem.Allocator, dir_path: []const u8) !void {
    var project = minibaml.MultiFileProject.init(allocator);
    defer project.deinit();

    std.debug.print("Loading BAML files from '{s}'...\n", .{dir_path});
    project.loadDirectory(dir_path) catch |err| {
        std.debug.print("Error loading directory: {s}\n", .{@errorName(err)});
        return err;
    };

    const files = project.getFiles();
    std.debug.print("Loaded {d} file(s)\n\n", .{files.len});

    for (files) |file| {
        std.debug.print("  - {s} ({d} declarations)\n", .{ file.path, file.tree.declarations.items.len });
    }

    std.debug.print("\nValidating merged AST...\n", .{});

    var validator = minibaml.Validator.init(allocator);
    defer validator.deinit();

    const merged_ast = project.getMergedAst();
    validator.validate(merged_ast) catch |err| {
        std.debug.print("Validation failed: {s}\n", .{@errorName(err)});
    };

    if (validator.diagnostics.items.len == 0) {
        std.debug.print("✓ {s} is valid (total {d} declarations)\n", .{ dir_path, merged_ast.declarations.items.len });
    } else {
        std.debug.print("Validation errors:\n\n", .{});
        for (validator.diagnostics.items) |diag| {
            const severity = switch (diag.severity) {
                .err => "error",
                .warning => "warning",
                .info => "info",
            };
            std.debug.print("  [{s}] Line {d}, Col {d}: {s}\n", .{
                severity,
                diag.line,
                diag.column,
                diag.message,
            });
        }
        std.debug.print("\nFound {d} error(s)\n", .{validator.diagnostics.items.len});
        std.process.exit(1);
    }
}

fn formatCommand(allocator: std.mem.Allocator, filename: []const u8) !void {
    var result = try parseFile(allocator, filename);
    defer result.deinit();

    var buffer = std.ArrayList(u8){};
    defer buffer.deinit(allocator);

    var fmt = minibaml.Formatter.init(allocator, &buffer);
    try fmt.formatAst(&result.tree);

    try std.fs.File.stdout().writeAll(buffer.items);
}

fn generateCommand(allocator: std.mem.Allocator, path: []const u8, use_typescript: bool, use_go: bool, typebuilder_only: bool) !void {
    var buffer = std.ArrayList(u8){};
    defer buffer.deinit(allocator);

    if (use_typescript) {
        var gen = minibaml.TypeScriptGenerator.init(allocator, &buffer);

        if (isDirectory(path)) {
            var project = minibaml.MultiFileProject.init(allocator);
            defer project.deinit();

            try project.loadDirectory(path);
            const merged_ast = project.getMergedAst();
            try gen.generate(merged_ast);
        } else {
            var result = try parseFile(allocator, path);
            defer result.deinit();
            try gen.generate(&result.tree);
        }
    } else if (use_go) {
        var gen = minibaml.GoGenerator.init(allocator, &buffer);

        if (isDirectory(path)) {
            var project = minibaml.MultiFileProject.init(allocator);
            defer project.deinit();

            try project.loadDirectory(path);
            const merged_ast = project.getMergedAst();
            try gen.generate(merged_ast);
        } else {
            var result = try parseFile(allocator, path);
            defer result.deinit();
            try gen.generate(&result.tree);
        }
    } else {
        var gen = minibaml.PythonGenerator.init(allocator, &buffer);

        if (typebuilder_only) {
            // Generate TypeBuilder only
            if (isDirectory(path)) {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();

                try project.loadDirectory(path);
                const merged_ast = project.getMergedAst();
                try gen.generateTypeBuilder(merged_ast);
            } else {
                var result = try parseFile(allocator, path);
                defer result.deinit();
                try gen.generateTypeBuilder(&result.tree);
            }
        } else {
            // Generate normal Python code
            if (isDirectory(path)) {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();

                try project.loadDirectory(path);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            } else {
                var result = try parseFile(allocator, path);
                defer result.deinit();
                try gen.generate(&result.tree);
            }
        }
    }

    try std.fs.File.stdout().writeAll(buffer.items);
}

test "simple test" {
    const result = 2 + 2;
    try std.testing.expectEqual(4, result);
}
