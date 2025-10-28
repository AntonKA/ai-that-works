const std = @import("std");

// minibaml - A BAML language implementation in Zig
//
// This module provides the core functionality for parsing and processing
// BAML (Boundary AI Markup Language) files.

pub const version = "0.1.0";

// Export core modules
pub const lexer = @import("lexer.zig");
pub const ast = @import("ast.zig");
pub const parser = @import("parser.zig");
pub const validator = @import("validator.zig");
pub const formatter = @import("formatter.zig");
pub const codegen = @import("codegen.zig");
pub const multifile = @import("multifile.zig");
pub const jinja = @import("jinja.zig");

// Convenience exports for common types
pub const Token = lexer.Token;
pub const TokenTag = lexer.TokenTag;
pub const Lexer = lexer.Lexer;
pub const Parser = parser.Parser;
pub const Ast = ast.Ast;
pub const TypeExpr = ast.TypeExpr;
pub const Declaration = ast.Declaration;
pub const Validator = validator.Validator;
pub const TypeRegistry = validator.TypeRegistry;
pub const Diagnostic = validator.Diagnostic;
pub const Formatter = formatter.Formatter;
pub const PythonGenerator = codegen.PythonGenerator;
pub const TypeScriptGenerator = codegen.TypeScriptGenerator;
pub const GoGenerator = codegen.GoGenerator;
pub const MultiFileProject = multifile.MultiFileProject;
pub const JinjaLexer = jinja.JinjaLexer;
pub const JinjaParser = jinja.JinjaParser;
pub const JinjaNode = jinja.JinjaNode;

pub fn getVersion() []const u8 {
    return version;
}

test "version test" {
    const v = getVersion();
    try std.testing.expect(v.len > 0);
}
