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

// Convenience exports for common types
pub const Token = lexer.Token;
pub const TokenTag = lexer.TokenTag;
pub const Lexer = lexer.Lexer;
pub const Parser = parser.Parser;
pub const Ast = ast.Ast;
pub const TypeExpr = ast.TypeExpr;
pub const Declaration = ast.Declaration;

pub fn getVersion() []const u8 {
    return version;
}

test "version test" {
    const v = getVersion();
    try std.testing.expect(v.len > 0);
}
