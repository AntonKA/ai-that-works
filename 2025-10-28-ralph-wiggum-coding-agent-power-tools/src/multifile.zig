const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

/// Represents a single parsed BAML file
pub const SourceFile = struct {
    path: []const u8,
    source: []const u8, // Keep source alive since AST holds pointers to it
    tree: ast.Ast,

    pub fn deinit(self: *SourceFile, allocator: std.mem.Allocator) void {
        allocator.free(self.path);
        allocator.free(self.source);
        self.tree.deinit();
    }
};

/// Represents a multi-file BAML project
pub const MultiFileProject = struct {
    allocator: std.mem.Allocator,
    files: std.ArrayList(SourceFile),
    merged_ast: ast.Ast,

    pub fn init(allocator: std.mem.Allocator) MultiFileProject {
        return MultiFileProject{
            .allocator = allocator,
            .files = std.ArrayList(SourceFile){},
            .merged_ast = ast.Ast.init(allocator),
        };
    }

    pub fn deinit(self: *MultiFileProject) void {
        for (self.files.items) |*file| {
            file.deinit(self.allocator);
        }
        self.files.deinit(self.allocator);
        // Don't deinit merged_ast contents since they're shallow copies
        // Just free the ArrayList
        self.merged_ast.declarations.deinit(self.allocator);
    }

    /// Scan a directory recursively for .baml files and parse them
    pub fn loadDirectory(self: *MultiFileProject, dir_path: []const u8) !void {
        var dir = try std.fs.cwd().openDir(dir_path, .{ .iterate = true });
        defer dir.close();

        try self.scanDirectoryRecursive(dir, dir_path);
        try self.mergeDeclarations();
    }

    /// Load multiple individual .baml files and merge them
    pub fn loadFiles(self: *MultiFileProject, file_paths: []const []const u8) !void {
        for (file_paths) |file_path| {
            // Duplicate the path string since parseAndAddFile takes ownership
            const path_copy = try self.allocator.dupe(u8, file_path);
            errdefer self.allocator.free(path_copy);

            try self.parseAndAddFile(path_copy);
        }
        try self.mergeDeclarations();
    }

    /// Recursively scan directory for .baml files
    fn scanDirectoryRecursive(self: *MultiFileProject, dir: std.fs.Dir, base_path: []const u8) !void {
        var iter = dir.iterate();

        while (try iter.next()) |entry| {
            const full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ base_path, entry.name });
            errdefer self.allocator.free(full_path);

            if (entry.kind == .directory) {
                // Recursively scan subdirectories
                var subdir = try dir.openDir(entry.name, .{ .iterate = true });
                defer subdir.close();
                try self.scanDirectoryRecursive(subdir, full_path);
                self.allocator.free(full_path); // Free after recursion
            } else if (entry.kind == .file) {
                // Check if file has .baml extension
                if (std.mem.endsWith(u8, entry.name, ".baml")) {
                    try self.parseAndAddFile(full_path);
                    // parseAndAddFile takes ownership of full_path
                } else {
                    self.allocator.free(full_path);
                }
            } else {
                self.allocator.free(full_path);
            }
        }
    }

    /// Parse a single BAML file and add it to the project
    fn parseAndAddFile(self: *MultiFileProject, file_path: []const u8) !void {
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        const source = try file.readToEndAlloc(self.allocator, 1024 * 1024);
        errdefer self.allocator.free(source);

        var lex = lexer.Lexer.init(source);
        var tokens = try lex.tokenize(self.allocator);
        defer tokens.deinit(self.allocator);

        var p = parser.Parser.init(self.allocator, tokens.items);
        errdefer p.deinit();

        var tree = ast.Ast.init(self.allocator);
        errdefer tree.deinit();

        while (!p.isAtEnd()) {
            p.skipTrivia();
            if (p.isAtEnd()) break;

            const current = p.peek() orelse break;

            const decl: ast.Declaration = switch (current.tag) {
                .keyword_class => .{ .class_decl = try p.parseClassDecl() },
                .keyword_enum => .{ .enum_decl = try p.parseEnumDecl() },
                .keyword_function => .{ .function_decl = try p.parseFunctionDecl() },
                .keyword_client => .{ .client_decl = try p.parseClientDecl() },
                .keyword_test => .{ .test_decl = try p.parseTestDecl() },
                .keyword_generator => .{ .generator_decl = try p.parseGeneratorDecl() },
                .keyword_template_string => .{ .template_string_decl = try p.parseTemplateStringDecl() },
                .keyword_retry_policy => .{ .retry_policy_decl = try p.parseRetryPolicyDecl() },
                else => {
                    return error.UnexpectedToken;
                },
            };

            try tree.declarations.append(self.allocator, decl);
            p.skipTrivia();
        }

        if (p.errors.items.len > 0) {
            std.debug.print("Parse errors in '{s}':\n", .{file_path});
            for (p.errors.items) |err| {
                std.debug.print("  Line {d}, Col {d}: {s}\n", .{ err.line, err.column, err.message });
            }
            return error.ParseError;
        }

        const source_file = SourceFile{
            .path = file_path,
            .source = source, // Keep source alive
            .tree = tree,
        };

        try self.files.append(self.allocator, source_file);
        p.deinit();
    }

    /// Merge all declarations from all files into a single AST
    fn mergeDeclarations(self: *MultiFileProject) !void {
        for (self.files.items) |*file| {
            for (file.tree.declarations.items) |decl| {
                // Create a copy of the declaration for the merged AST
                const decl_copy = try self.copyDeclaration(decl);
                try self.merged_ast.declarations.append(self.allocator, decl_copy);
            }
        }
    }

    /// Copy a declaration (shallow copy of pointers, as original memory is managed by source files)
    fn copyDeclaration(self: *MultiFileProject, decl: ast.Declaration) !ast.Declaration {
        _ = self;
        // Note: This is a shallow copy - the actual data is still owned by the source files
        // The merged_ast just holds references to the same declarations
        return decl;
    }

    /// Get the merged AST containing all declarations from all files
    pub fn getMergedAst(self: *const MultiFileProject) *const ast.Ast {
        return &self.merged_ast;
    }

    /// Get list of all source files
    pub fn getFiles(self: *const MultiFileProject) []const SourceFile {
        return self.files.items;
    }
};

// Tests
test "MultiFileProject: Create and cleanup" {
    const allocator = std.testing.allocator;
    var project = MultiFileProject.init(allocator);
    defer project.deinit();

    try std.testing.expect(project.files.items.len == 0);
    try std.testing.expect(project.merged_ast.declarations.items.len == 0);
}
