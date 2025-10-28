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
            try printError("generate command requires at least one file argument", "minibaml generate <path> [path2 ...] [--typescript|--python|--go|--ruby|--rust|--elixir|--java|--csharp|--swift|--kotlin|--php|--scala|--typebuilder]");
            return;
        }

        // Collect file paths and flags
        var paths = std.ArrayList([]const u8){};
        defer paths.deinit(allocator);
        var use_typescript = false;
        var use_go = false;
        var use_ruby = false;
        var use_rust = false;
        var use_elixir = false;
        var use_java = false;
        var use_csharp = false;
        var use_swift = false;
        var use_kotlin = false;
        var use_php = false;
        var use_scala = false;
        var use_zig = false;
        var typebuilder_only = false;

        // Parse arguments to separate paths from flags
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            const arg = args[i];
            if (std.mem.startsWith(u8, arg, "--") or std.mem.startsWith(u8, arg, "-")) {
                // This is a flag
                if (std.mem.eql(u8, arg, "--typescript") or std.mem.eql(u8, arg, "-ts")) {
                    use_typescript = true;
                } else if (std.mem.eql(u8, arg, "--go")) {
                    use_go = true;
                } else if (std.mem.eql(u8, arg, "--ruby")) {
                    use_ruby = true;
                } else if (std.mem.eql(u8, arg, "--rust")) {
                    use_rust = true;
                } else if (std.mem.eql(u8, arg, "--elixir")) {
                    use_elixir = true;
                } else if (std.mem.eql(u8, arg, "--java")) {
                    use_java = true;
                } else if (std.mem.eql(u8, arg, "--csharp") or std.mem.eql(u8, arg, "-cs")) {
                    use_csharp = true;
                } else if (std.mem.eql(u8, arg, "--swift")) {
                    use_swift = true;
                } else if (std.mem.eql(u8, arg, "--kotlin") or std.mem.eql(u8, arg, "-kt")) {
                    use_kotlin = true;
                } else if (std.mem.eql(u8, arg, "--php")) {
                    use_php = true;
                } else if (std.mem.eql(u8, arg, "--scala")) {
                    use_scala = true;
                } else if (std.mem.eql(u8, arg, "--zig")) {
                    use_zig = true;
                } else if (std.mem.eql(u8, arg, "--typebuilder") or std.mem.eql(u8, arg, "-tb")) {
                    typebuilder_only = true;
                }
            } else {
                // This is a file path
                try paths.append(allocator, arg);
            }
        }

        if (paths.items.len == 0) {
            try printError("generate command requires at least one file or directory", "minibaml generate <path> [path2 ...] [--typescript|--go|...]");
            return;
        }

        try generateCommand(allocator, paths.items, use_typescript, use_go, use_ruby, use_rust, use_elixir, use_java, use_csharp, use_swift, use_kotlin, use_php, use_scala, use_zig, typebuilder_only);
    } else if (std.mem.eql(u8, command, "parse")) {
        if (args.len < 3) {
            try printError("parse command requires at least one file argument", "minibaml parse <file.baml> [file2.baml ...]");
            return;
        }
        try parseCommand(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "check")) {
        if (args.len < 3) {
            try printError("check command requires at least one file argument", "minibaml check <file.baml> [file2.baml ...]");
            return;
        }
        try checkCommand(allocator, args[2..]);
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
        \\  minibaml <file.baml>                    Tokenize a BAML file
        \\  minibaml parse <path> [path2 ...]       Parse and show AST (files or directory)
        \\  minibaml check <path> [path2 ...]       Validate BAML files or directory
        \\  minibaml fmt <file.baml>                Format a BAML file
        \\  minibaml generate <path> [path2 ...] [opts]  Generate code from BAML
        \\  minibaml gen <path> [path2 ...] [opts]       Alias for generate
        \\
        \\Code Generation Options:
        \\  --python                          Generate Python code (default)
        \\  --typescript, -ts                 Generate TypeScript code
        \\  --go                              Generate Go code
        \\  --ruby                            Generate Ruby code
        \\  --rust                            Generate Rust code
        \\  --elixir                          Generate Elixir code
        \\  --java                            Generate Java code
        \\  --csharp, -cs                     Generate C# code
        \\  --swift                           Generate Swift code
        \\  --kotlin, -kt                     Generate Kotlin code
        \\  --php                             Generate PHP code
        \\  --scala                           Generate Scala code
        \\  --zig                             Generate Zig code
        \\  --typebuilder, -tb                Generate Python TypeBuilder module only
        \\
        \\Global Options:
        \\  --help, -h                        Show this help message
        \\  --version, -v                     Show version information
        \\
        \\Examples:
        \\  minibaml test.baml                      # Show tokens
        \\  minibaml parse test.baml                # Parse single file
        \\  minibaml parse file1.baml file2.baml    # Parse multiple files
        \\  minibaml parse baml_src                 # Parse directory
        \\  minibaml check test.baml                # Validate single file
        \\  minibaml check file1.baml file2.baml    # Validate multiple files
        \\  minibaml check baml_src                 # Validate directory
        \\  minibaml fmt test.baml                  # Format and print
        \\  minibaml gen baml_src                   # Generate Python (directory)
        \\  minibaml gen file1.baml file2.baml      # Generate Python (multiple files)
        \\  minibaml gen baml_src --typescript      # Generate TypeScript
        \\  minibaml gen file1.baml file2.baml --go # Generate Go (multiple files)
        \\  minibaml gen baml_src --rust            # Generate Rust
        \\  minibaml gen baml_src --typebuilder > type_builder.py # TypeBuilder
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
            .keyword_retry_policy => .{ .retry_policy_decl = try parser.parseRetryPolicyDecl() },
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

fn parseCommand(allocator: std.mem.Allocator, paths: []const []const u8) !void {
    if (paths.len == 0) {
        try printError("parse command requires at least one file or directory", "minibaml parse <path> [path2 ...]");
        return;
    }

    if (paths.len == 1) {
        // Single path: could be file or directory
        if (isDirectory(paths[0])) {
            try parseDirectory(allocator, paths[0]);
        } else {
            try parseSingleFile(allocator, paths[0]);
        }
    } else {
        // Multiple paths: process as multiple files
        try parseMultipleFiles(allocator, paths);
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
            .retry_policy_decl => |retry_policy| std.debug.print("retry_policy {s} (max_retries: {d})", .{ retry_policy.name, retry_policy.max_retries }),
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
                .retry_policy_decl => |retry_policy| std.debug.print("      - retry_policy {s}\n", .{retry_policy.name}),
            }
        }
        std.debug.print("\n", .{});
    }

    const merged_ast = project.getMergedAst();
    std.debug.print("Merged AST: {d} total declarations\n", .{merged_ast.declarations.items.len});
}

fn parseMultipleFiles(allocator: std.mem.Allocator, file_paths: []const []const u8) !void {
    var project = minibaml.MultiFileProject.init(allocator);
    defer project.deinit();

    std.debug.print("Loading {d} BAML file(s)...\n\n", .{file_paths.len});
    try project.loadFiles(file_paths);

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
                .retry_policy_decl => |retry_policy| std.debug.print("      - retry_policy {s}\n", .{retry_policy.name}),
            }
        }
        std.debug.print("\n", .{});
    }

    const merged_ast = project.getMergedAst();
    std.debug.print("Merged AST: {d} total declarations\n", .{merged_ast.declarations.items.len});
}

fn checkCommand(allocator: std.mem.Allocator, paths: []const []const u8) !void {
    if (paths.len == 0) {
        try printError("check command requires at least one file or directory", "minibaml check <path> [path2 ...]");
        return;
    }

    if (paths.len == 1) {
        // Single path: could be file or directory
        if (isDirectory(paths[0])) {
            try checkDirectory(allocator, paths[0]);
        } else {
            try checkFile(allocator, paths[0]);
        }
    } else {
        // Multiple paths: process as multiple files
        try checkMultipleFiles(allocator, paths);
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

fn checkMultipleFiles(allocator: std.mem.Allocator, file_paths: []const []const u8) !void {
    var project = minibaml.MultiFileProject.init(allocator);
    defer project.deinit();

    std.debug.print("Loading {d} BAML file(s)...\n", .{file_paths.len});
    project.loadFiles(file_paths) catch |err| {
        std.debug.print("Error loading files: {s}\n", .{@errorName(err)});
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
        std.debug.print("✓ All files are valid (total {d} declarations)\n", .{merged_ast.declarations.items.len});
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

fn generateCommand(allocator: std.mem.Allocator, paths: []const []const u8, use_typescript: bool, use_go: bool, use_ruby: bool, use_rust: bool, use_elixir: bool, use_java: bool, use_csharp: bool, use_swift: bool, use_kotlin: bool, use_php: bool, use_scala: bool, use_zig: bool, typebuilder_only: bool) !void {
    var buffer = std.ArrayList(u8){};
    defer buffer.deinit(allocator);

    // Determine how to load the AST based on input paths
    const LoadMode = enum { single_file, directory, multiple_files };
    var load_mode: LoadMode = undefined;
    if (paths.len == 1) {
        if (isDirectory(paths[0])) {
            load_mode = .directory;
        } else {
            load_mode = .single_file;
        }
    } else {
        load_mode = .multiple_files;
    }

    if (use_typescript) {
        var gen = minibaml.TypeScriptGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_go) {
        var gen = minibaml.GoGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_ruby) {
        var gen = minibaml.RubyGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_rust) {
        var gen = minibaml.RustGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_elixir) {
        var gen = minibaml.ElixirGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_java) {
        var gen = minibaml.JavaGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_csharp) {
        var gen = minibaml.CSharpGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_swift) {
        var gen = minibaml.SwiftGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_kotlin) {
        var gen = minibaml.KotlinGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_php) {
        var gen = minibaml.PHPGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_scala) {
        var gen = minibaml.ScalaGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else if (use_zig) {
        var gen = minibaml.ZigGenerator.init(allocator, &buffer);

        switch (load_mode) {
            .directory => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadDirectory(paths[0]);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
            .single_file => {
                var result = try parseFile(allocator, paths[0]);
                defer result.deinit();
                try gen.generate(&result.tree);
            },
            .multiple_files => {
                var project = minibaml.MultiFileProject.init(allocator);
                defer project.deinit();
                try project.loadFiles(paths);
                const merged_ast = project.getMergedAst();
                try gen.generate(merged_ast);
            },
        }
    } else {
        var gen = minibaml.PythonGenerator.init(allocator, &buffer);

        if (typebuilder_only) {
            switch (load_mode) {
                .directory => {
                    var project = minibaml.MultiFileProject.init(allocator);
                    defer project.deinit();
                    try project.loadDirectory(paths[0]);
                    const merged_ast = project.getMergedAst();
                    try gen.generateTypeBuilder(merged_ast);
                },
                .single_file => {
                    var result = try parseFile(allocator, paths[0]);
                    defer result.deinit();
                    try gen.generateTypeBuilder(&result.tree);
                },
                .multiple_files => {
                    var project = minibaml.MultiFileProject.init(allocator);
                    defer project.deinit();
                    try project.loadFiles(paths);
                    const merged_ast = project.getMergedAst();
                    try gen.generateTypeBuilder(merged_ast);
                },
            }
        } else {
            switch (load_mode) {
                .directory => {
                    var project = minibaml.MultiFileProject.init(allocator);
                    defer project.deinit();
                    try project.loadDirectory(paths[0]);
                    const merged_ast = project.getMergedAst();
                    try gen.generate(merged_ast);
                },
                .single_file => {
                    var result = try parseFile(allocator, paths[0]);
                    defer result.deinit();
                    try gen.generate(&result.tree);
                },
                .multiple_files => {
                    var project = minibaml.MultiFileProject.init(allocator);
                    defer project.deinit();
                    try project.loadFiles(paths);
                    const merged_ast = project.getMergedAst();
                    try gen.generate(merged_ast);
                },
            }
        }
    }

    try std.fs.File.stdout().writeAll(buffer.items);
}

test "simple test" {
    const result = 2 + 2;
    try std.testing.expectEqual(4, result);
}
