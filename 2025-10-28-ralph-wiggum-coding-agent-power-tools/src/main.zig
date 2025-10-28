const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try std.fs.File.stdout().writeAll("minibaml - BAML tokenizer\n");
        try std.fs.File.stdout().writeAll("Usage: minibaml <file.baml>\n");
        try std.fs.File.stdout().writeAll("\nExample: minibaml test.baml\n");
        return;
    }

    const filename = args[1];

    // Read the file
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024); // Max 1MB
    defer allocator.free(source);

    // Tokenize
    var lex = lexer.Lexer.init(source);
    var tokens = try lex.tokenize(allocator);
    defer tokens.deinit(allocator);

    // Print results using debug print
    std.debug.print("Tokenized {s}: {d} tokens\n\n", .{ filename, tokens.items.len });

    // Print each token
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

test "simple test" {
    const result = 2 + 2;
    try std.testing.expectEqual(4, result);
}
