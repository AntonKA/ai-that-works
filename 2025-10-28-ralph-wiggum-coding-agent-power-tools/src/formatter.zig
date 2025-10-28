const std = @import("std");
const ast = @import("ast.zig");

/// Formatter for BAML code
pub const Formatter = struct {
    writer: std.ArrayList(u8).Writer,
    buffer: *std.ArrayList(u8),
    indent_level: usize,
    allocator: std.mem.Allocator,

    /// Initialize a formatter
    pub fn init(allocator: std.mem.Allocator, buffer: *std.ArrayList(u8)) Formatter {
        return Formatter{
            .writer = buffer.writer(allocator),
            .buffer = buffer,
            .indent_level = 0,
            .allocator = allocator,
        };
    }

    /// Write indentation at current level
    fn writeIndent(self: *Formatter) !void {
        for (0..self.indent_level) |_| {
            try self.writer.writeAll("  ");
        }
    }

    /// Format an entire AST
    pub fn formatAst(self: *Formatter, tree: *const ast.Ast) !void {
        for (tree.declarations.items, 0..) |*decl, i| {
            if (i > 0) {
                try self.writer.writeAll("\n");
            }
            try self.formatDeclaration(decl);
            try self.writer.writeAll("\n");
        }
    }

    /// Format a declaration
    pub fn formatDeclaration(self: *Formatter, decl: *const ast.Declaration) !void {
        switch (decl.*) {
            .class_decl => |*d| try self.formatClassDecl(d),
            .enum_decl => |*d| try self.formatEnumDecl(d),
            .function_decl => |*d| try self.formatFunctionDecl(d),
            .client_decl => |*d| try self.formatClientDecl(d),
            .test_decl => |*d| try self.formatTestDecl(d),
            .generator_decl => |*d| try self.formatGeneratorDecl(d),
            .template_string_decl => |*d| try self.formatTemplateStringDecl(d),
            .type_alias_decl => |*d| try self.formatTypeAliasDecl(d),
            .retry_policy_decl => |*d| try self.formatRetryPolicyDecl(d),
        }
    }

    /// Format a type expression
    pub fn formatTypeExpr(self: *Formatter, type_expr: *const ast.TypeExpr) !void {
        switch (type_expr.*) {
            .primitive => |prim| {
                const name = switch (prim) {
                    .string => "string",
                    .int => "int",
                    .float => "float",
                    .bool => "bool",
                    .null_type => "null",
                    .image => "image",
                    .audio => "audio",
                    .video => "video",
                    .pdf => "pdf",
                };
                try self.writer.writeAll(name);
            },
            .named => |name| {
                try self.writer.writeAll(name);
            },
            .array => |inner| {
                try self.formatTypeExpr(inner);
                try self.writer.writeAll("[]");
            },
            .optional => |inner| {
                try self.formatTypeExpr(inner);
                try self.writer.writeAll("?");
            },
            .union_type => |*u| {
                for (u.types.items, 0..) |t, i| {
                    if (i > 0) {
                        try self.writer.writeAll(" | ");
                    }
                    try self.formatTypeExpr(t);
                }
            },
            .map => |*m| {
                try self.writer.writeAll("map<");
                try self.formatTypeExpr(m.key_type);
                try self.writer.writeAll(", ");
                try self.formatTypeExpr(m.value_type);
                try self.writer.writeAll(">");
            },
            .literal => |lit| {
                switch (lit) {
                    .string => |s| {
                        try self.writer.writeAll("\"");
                        try self.writer.writeAll(s);
                        try self.writer.writeAll("\"");
                    },
                    .int => |i| {
                        try self.writer.print("{d}", .{i});
                    },
                    .float => |f| {
                        try self.writer.print("{d}", .{f});
                    },
                    .bool => |b| {
                        try self.writer.writeAll(if (b) "true" else "false");
                    },
                    .null_value => {
                        try self.writer.writeAll("null");
                    },
                }
            },
        }
    }

    /// Format a value
    pub fn formatValue(self: *Formatter, value: *const ast.Value) !void {
        switch (value.*) {
            .string => |s| {
                try self.writer.writeAll("\"");
                try self.writer.writeAll(s);
                try self.writer.writeAll("\"");
            },
            .int => |i| {
                try self.writer.print("{d}", .{i});
            },
            .float => |f| {
                try self.writer.print("{d}", .{f});
            },
            .bool => |b| {
                try self.writer.writeAll(if (b) "true" else "false");
            },
            .null_value => {
                try self.writer.writeAll("null");
            },
            .array => |*arr| {
                try self.writer.writeAll("[");
                for (arr.items, 0..) |*item, i| {
                    if (i > 0) {
                        try self.writer.writeAll(", ");
                    }
                    try self.formatValue(item);
                }
                try self.writer.writeAll("]");
            },
            .object => |*obj| {
                try self.writer.writeAll("{\n");
                self.indent_level += 1;

                var it = obj.iterator();
                var first = true;
                while (it.next()) |entry| {
                    if (!first) {
                        try self.writer.writeAll("\n");
                    }
                    first = false;

                    try self.writeIndent();
                    try self.writer.writeAll(entry.key_ptr.*);
                    try self.writer.writeAll(" ");
                    try self.formatValue(entry.value_ptr);
                }

                self.indent_level -= 1;
                try self.writer.writeAll("\n");
                try self.writeIndent();
                try self.writer.writeAll("}");
            },
            .env_var => |var_name| {
                try self.writer.writeAll("env.");
                try self.writer.writeAll(var_name);
            },
        }
    }

    /// Format an attribute
    fn formatAttribute(self: *Formatter, attr: *const ast.Attribute) !void {
        if (attr.is_class_level) {
            try self.writer.writeAll("@@");
        } else {
            try self.writer.writeAll("@");
        }
        try self.writer.writeAll(attr.name);

        if (attr.args.items.len > 0) {
            try self.writer.writeAll("(");
            for (attr.args.items, 0..) |*arg, i| {
                if (i > 0) {
                    try self.writer.writeAll(", ");
                }
                try self.formatValue(arg);
            }
            try self.writer.writeAll(")");
        }
    }

    /// Format a class declaration
    fn formatClassDecl(self: *Formatter, class_decl: *const ast.ClassDecl) !void {
        if (class_decl.docstring) |doc| {
            try self.writer.writeAll("/// ");
            try self.writer.writeAll(doc);
            try self.writer.writeAll("\n");
        }

        try self.writer.writeAll("class ");
        try self.writer.writeAll(class_decl.name);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        for (class_decl.properties.items) |*prop| {
            if (prop.docstring) |doc| {
                try self.writeIndent();
                try self.writer.writeAll("/// ");
                try self.writer.writeAll(doc);
                try self.writer.writeAll("\n");
            }

            try self.writeIndent();
            try self.writer.writeAll(prop.name);
            try self.writer.writeAll(" ");
            try self.formatTypeExpr(prop.type_expr);

            for (prop.attributes.items) |*attr| {
                try self.writer.writeAll(" ");
                try self.formatAttribute(attr);
            }

            try self.writer.writeAll("\n");
        }

        for (class_decl.attributes.items) |*attr| {
            try self.writer.writeAll("\n");
            try self.writeIndent();
            try self.formatAttribute(attr);
            try self.writer.writeAll("\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format an enum declaration
    fn formatEnumDecl(self: *Formatter, enum_decl: *const ast.EnumDecl) !void {
        if (enum_decl.docstring) |doc| {
            try self.writer.writeAll("/// ");
            try self.writer.writeAll(doc);
            try self.writer.writeAll("\n");
        }

        try self.writer.writeAll("enum ");
        try self.writer.writeAll(enum_decl.name);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        for (enum_decl.values.items) |*val| {
            if (val.docstring) |doc| {
                try self.writeIndent();
                try self.writer.writeAll("/// ");
                try self.writer.writeAll(doc);
                try self.writer.writeAll("\n");
            }

            try self.writeIndent();
            try self.writer.writeAll(val.name);

            for (val.attributes.items) |*attr| {
                try self.writer.writeAll(" ");
                try self.formatAttribute(attr);
            }

            try self.writer.writeAll("\n");
        }

        for (enum_decl.attributes.items) |*attr| {
            try self.writer.writeAll("\n");
            try self.writeIndent();
            try self.formatAttribute(attr);
            try self.writer.writeAll("\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format a function declaration
    fn formatFunctionDecl(self: *Formatter, function_decl: *const ast.FunctionDecl) !void {
        if (function_decl.docstring) |doc| {
            try self.writer.writeAll("/// ");
            try self.writer.writeAll(doc);
            try self.writer.writeAll("\n");
        }

        try self.writer.writeAll("function ");
        try self.writer.writeAll(function_decl.name);
        try self.writer.writeAll("(");

        for (function_decl.parameters.items, 0..) |*param, i| {
            if (i > 0) {
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(param.name);
            try self.writer.writeAll(": ");
            try self.formatTypeExpr(param.type_expr);
        }

        try self.writer.writeAll(") -> ");
        try self.formatTypeExpr(function_decl.return_type);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        if (function_decl.client) |client| {
            try self.writeIndent();
            try self.writer.writeAll("client \"");
            try self.writer.writeAll(client);
            try self.writer.writeAll("\"\n");
        }

        if (function_decl.prompt) |prompt| {
            try self.writeIndent();
            try self.writer.writeAll("prompt ");

            // Determine if we need ## or # based on content
            const needs_double = std.mem.indexOf(u8, prompt, "#\"") != null or
                                 std.mem.indexOf(u8, prompt, "\"#") != null;

            if (needs_double) {
                try self.writer.writeAll("##\"");
                try self.writer.writeAll(prompt);
                try self.writer.writeAll("\"##\n");
            } else {
                try self.writer.writeAll("#\"");
                try self.writer.writeAll(prompt);
                try self.writer.writeAll("\"#\n");
            }
        }

        for (function_decl.attributes.items) |*attr| {
            try self.writeIndent();
            try self.formatAttribute(attr);
            try self.writer.writeAll("\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format a client declaration
    fn formatClientDecl(self: *Formatter, client_decl: *const ast.ClientDecl) !void {
        try self.writer.writeAll("client<");
        try self.writer.writeAll(client_decl.client_type);
        try self.writer.writeAll("> ");
        try self.writer.writeAll(client_decl.name);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        try self.writeIndent();
        try self.writer.writeAll("provider \"");
        try self.writer.writeAll(client_decl.provider);
        try self.writer.writeAll("\"\n");

        if (client_decl.options.count() > 0) {
            try self.writeIndent();
            try self.writer.writeAll("options {\n");
            self.indent_level += 1;

            var it = client_decl.options.iterator();
            while (it.next()) |entry| {
                try self.writeIndent();
                try self.writer.writeAll(entry.key_ptr.*);
                try self.writer.writeAll(" ");
                try self.formatValue(entry.value_ptr);
                try self.writer.writeAll("\n");
            }

            self.indent_level -= 1;
            try self.writeIndent();
            try self.writer.writeAll("}\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format a test declaration
    fn formatTestDecl(self: *Formatter, test_decl: *const ast.TestDecl) !void {
        try self.writer.writeAll("test ");
        try self.writer.writeAll(test_decl.name);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        try self.writeIndent();
        try self.writer.writeAll("functions [");
        for (test_decl.functions.items, 0..) |func, i| {
            if (i > 0) {
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(func);
        }
        try self.writer.writeAll("]\n");

        if (test_decl.args.count() > 0) {
            try self.writeIndent();
            try self.writer.writeAll("args {\n");
            self.indent_level += 1;

            var it = test_decl.args.iterator();
            while (it.next()) |entry| {
                try self.writeIndent();
                try self.writer.writeAll(entry.key_ptr.*);
                try self.writer.writeAll(" ");
                try self.formatValue(entry.value_ptr);
                try self.writer.writeAll("\n");
            }

            self.indent_level -= 1;
            try self.writeIndent();
            try self.writer.writeAll("}\n");
        }

        for (test_decl.attributes.items) |*attr| {
            try self.writeIndent();
            try self.formatAttribute(attr);
            try self.writer.writeAll("\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format a generator declaration
    fn formatGeneratorDecl(self: *Formatter, generator_decl: *const ast.GeneratorDecl) !void {
        try self.writer.writeAll("generator ");
        try self.writer.writeAll(generator_decl.name);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        var it = generator_decl.options.iterator();
        while (it.next()) |entry| {
            try self.writeIndent();
            try self.writer.writeAll(entry.key_ptr.*);
            try self.writer.writeAll(" ");
            try self.formatValue(entry.value_ptr);
            try self.writer.writeAll("\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format a retry_policy declaration
    fn formatRetryPolicyDecl(self: *Formatter, retry_policy_decl: *const ast.RetryPolicyDecl) !void {
        try self.writer.writeAll("retry_policy ");
        try self.writer.writeAll(retry_policy_decl.name);
        try self.writer.writeAll(" {\n");

        self.indent_level += 1;

        // Format max_retries
        try self.writeIndent();
        try self.writer.writeAll("max_retries ");
        try self.writer.print("{d}\n", .{retry_policy_decl.max_retries});

        // Format strategy if present
        if (retry_policy_decl.strategy) |strategy| {
            try self.writeIndent();
            try self.writer.writeAll("strategy {\n");
            self.indent_level += 1;

            switch (strategy) {
                .constant_delay => |s| {
                    try self.writeIndent();
                    try self.writer.writeAll("type constant_delay\n");
                    try self.writeIndent();
                    try self.writer.print("delay_ms {d}\n", .{s.delay_ms});
                },
                .exponential_backoff => |s| {
                    try self.writeIndent();
                    try self.writer.writeAll("type exponential_backoff\n");
                    try self.writeIndent();
                    try self.writer.print("delay_ms {d}\n", .{s.delay_ms});
                    try self.writeIndent();
                    try self.writer.print("multiplier {d}\n", .{s.multiplier});
                    try self.writeIndent();
                    try self.writer.print("max_delay_ms {d}\n", .{s.max_delay_ms});
                },
            }

            self.indent_level -= 1;
            try self.writeIndent();
            try self.writer.writeAll("}\n");
        }

        self.indent_level -= 1;
        try self.writer.writeAll("}");
    }

    /// Format a template_string declaration
    fn formatTemplateStringDecl(self: *Formatter, template_decl: *const ast.TemplateStringDecl) !void {
        try self.writer.writeAll("template_string ");
        try self.writer.writeAll(template_decl.name);
        try self.writer.writeAll("(");

        for (template_decl.parameters.items, 0..) |*param, i| {
            if (i > 0) {
                try self.writer.writeAll(", ");
            }
            try self.writer.writeAll(param.name);
            try self.writer.writeAll(": ");
            try self.formatTypeExpr(param.type_expr);
        }

        try self.writer.writeAll(") ");

        // Determine if we need ## or # based on content
        const needs_double = std.mem.indexOf(u8, template_decl.template, "#\"") != null or
                             std.mem.indexOf(u8, template_decl.template, "\"#") != null;

        if (needs_double) {
            try self.writer.writeAll("##\"");
            try self.writer.writeAll(template_decl.template);
            try self.writer.writeAll("\"##");
        } else {
            try self.writer.writeAll("#\"");
            try self.writer.writeAll(template_decl.template);
            try self.writer.writeAll("\"#");
        }
    }

    /// Format a type alias declaration
    fn formatTypeAliasDecl(self: *Formatter, type_alias: *const ast.TypeAliasDecl) !void {
        try self.writer.writeAll("type ");
        try self.writer.writeAll(type_alias.name);
        try self.writer.writeAll(" = ");
        try self.formatTypeExpr(type_alias.type_expr);
    }
};

// Tests
test "Formatter: Format primitive types" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    const string_type = ast.TypeExpr{ .primitive = .string };
    try formatter.formatTypeExpr(&string_type);
    try std.testing.expectEqualStrings("string", buffer.items);

    buffer.clearRetainingCapacity();

    const int_type = ast.TypeExpr{ .primitive = .int };
    try formatter.formatTypeExpr(&int_type);
    try std.testing.expectEqualStrings("int", buffer.items);
}

test "Formatter: Format array type" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    const inner = try allocator.create(ast.TypeExpr);
    defer allocator.destroy(inner);
    inner.* = ast.TypeExpr{ .primitive = .string };

    const array_type = ast.TypeExpr{ .array = inner };
    try formatter.formatTypeExpr(&array_type);
    try std.testing.expectEqualStrings("string[]", buffer.items);
}

test "Formatter: Format optional type" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    const inner = try allocator.create(ast.TypeExpr);
    defer allocator.destroy(inner);
    inner.* = ast.TypeExpr{ .primitive = .int };

    const optional_type = ast.TypeExpr{ .optional = inner };
    try formatter.formatTypeExpr(&optional_type);
    try std.testing.expectEqualStrings("int?", buffer.items);
}

test "Formatter: Format union type" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    var types = std.ArrayList(*ast.TypeExpr).init(allocator);
    defer {
        for (types.items) |t| {
            allocator.destroy(t);
        }
        types.deinit();
    }

    const t1 = try allocator.create(ast.TypeExpr);
    t1.* = ast.TypeExpr{ .primitive = .string };
    try types.append(t1);

    const t2 = try allocator.create(ast.TypeExpr);
    t2.* = ast.TypeExpr{ .primitive = .int };
    try types.append(t2);

    const union_type = ast.TypeExpr{ .union_type = ast.UnionType{ .types = types } };
    try formatter.formatTypeExpr(&union_type);
    try std.testing.expectEqualStrings("string | int", buffer.items);
}

test "Formatter: Format map type" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    const key = try allocator.create(ast.TypeExpr);
    defer allocator.destroy(key);
    key.* = ast.TypeExpr{ .primitive = .string };

    const val = try allocator.create(ast.TypeExpr);
    defer allocator.destroy(val);
    val.* = ast.TypeExpr{ .primitive = .int };

    const map_type = ast.TypeExpr{ .map = ast.MapType{ .key_type = key, .value_type = val } };
    try formatter.formatTypeExpr(&map_type);
    try std.testing.expectEqualStrings("map<string, int>", buffer.items);
}

test "Formatter: Format values" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    const string_val = ast.Value{ .string = "hello" };
    try formatter.formatValue(&string_val);
    try std.testing.expectEqualStrings("\"hello\"", buffer.items);

    buffer.clearRetainingCapacity();

    const int_val = ast.Value{ .int = 42 };
    try formatter.formatValue(&int_val);
    try std.testing.expectEqualStrings("42", buffer.items);

    buffer.clearRetainingCapacity();

    const bool_val = ast.Value{ .bool = true };
    try formatter.formatValue(&bool_val);
    try std.testing.expectEqualStrings("true", buffer.items);

    buffer.clearRetainingCapacity();

    const env_val = ast.Value{ .env_var = "API_KEY" };
    try formatter.formatValue(&env_val);
    try std.testing.expectEqualStrings("env.API_KEY", buffer.items);
}

test "Formatter: Format attribute" {
    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var formatter = Formatter.init(allocator, &buffer);

    var args = std.ArrayList(ast.Value).init(allocator);
    defer args.deinit();
    try args.append(ast.Value{ .string = "test" });

    const attr = ast.Attribute{
        .name = "alias",
        .is_class_level = false,
        .args = args,
        .location = .{ .line = 1, .column = 1 },
    };

    try formatter.formatAttribute(&attr);
    try std.testing.expectEqualStrings("@alias(\"test\")", buffer.items);
}
