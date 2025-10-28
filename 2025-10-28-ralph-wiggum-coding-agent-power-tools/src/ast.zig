const std = @import("std");
const Token = @import("lexer.zig").Token;

/// Location information for AST nodes
pub const Location = struct {
    line: usize,
    column: usize,
};

/// Root AST node containing all top-level declarations
pub const Ast = struct {
    declarations: std.ArrayList(Declaration),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Ast {
        return Ast{
            .declarations = std.ArrayList(Declaration).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Ast) void {
        for (self.declarations.items) |*decl| {
            decl.deinit(self.allocator);
        }
        self.declarations.deinit();
    }
};

/// Top-level declaration types
pub const DeclarationTag = enum {
    class_decl,
    enum_decl,
    function_decl,
    client_decl,
    test_decl,
    generator_decl,
    template_string_decl,
    type_alias_decl,
};

/// A top-level declaration in BAML
pub const Declaration = union(DeclarationTag) {
    class_decl: ClassDecl,
    enum_decl: EnumDecl,
    function_decl: FunctionDecl,
    client_decl: ClientDecl,
    test_decl: TestDecl,
    generator_decl: GeneratorDecl,
    template_string_decl: TemplateStringDecl,
    type_alias_decl: TypeAliasDecl,

    pub fn deinit(self: *Declaration, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .class_decl => |*d| d.deinit(allocator),
            .enum_decl => |*d| d.deinit(allocator),
            .function_decl => |*d| d.deinit(allocator),
            .client_decl => |*d| d.deinit(allocator),
            .test_decl => |*d| d.deinit(allocator),
            .generator_decl => |*d| d.deinit(allocator),
            .template_string_decl => |*d| d.deinit(allocator),
            .type_alias_decl => |*d| d.deinit(allocator),
        }
    }
};

/// Class declaration: class Name { ... }
pub const ClassDecl = struct {
    name: []const u8,
    properties: std.ArrayList(Property),
    attributes: std.ArrayList(Attribute),
    docstring: ?[]const u8,
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, location: Location) ClassDecl {
        return ClassDecl{
            .name = name,
            .properties = std.ArrayList(Property).init(allocator),
            .attributes = std.ArrayList(Attribute).init(allocator),
            .docstring = null,
            .location = location,
        };
    }

    pub fn deinit(self: *ClassDecl, allocator: std.mem.Allocator) void {
        for (self.properties.items) |*prop| {
            prop.deinit(allocator);
        }
        self.properties.deinit();
        for (self.attributes.items) |*attr| {
            attr.deinit(allocator);
        }
        self.attributes.deinit();
    }
};

/// Enum declaration: enum Name { ... }
pub const EnumDecl = struct {
    name: []const u8,
    values: std.ArrayList(EnumValue),
    attributes: std.ArrayList(Attribute),
    docstring: ?[]const u8,
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, location: Location) EnumDecl {
        return EnumDecl{
            .name = name,
            .values = std.ArrayList(EnumValue).init(allocator),
            .attributes = std.ArrayList(Attribute).init(allocator),
            .docstring = null,
            .location = location,
        };
    }

    pub fn deinit(self: *EnumDecl, allocator: std.mem.Allocator) void {
        for (self.values.items) |*val| {
            val.deinit(allocator);
        }
        self.values.deinit();
        for (self.attributes.items) |*attr| {
            attr.deinit(allocator);
        }
        self.attributes.deinit();
    }
};

/// Function declaration: function Name(params) -> ReturnType { ... }
pub const FunctionDecl = struct {
    name: []const u8,
    parameters: std.ArrayList(Parameter),
    return_type: *TypeExpr,
    client: ?[]const u8,
    prompt: ?[]const u8,
    attributes: std.ArrayList(Attribute),
    docstring: ?[]const u8,
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, location: Location) FunctionDecl {
        return FunctionDecl{
            .name = name,
            .parameters = std.ArrayList(Parameter).init(allocator),
            .return_type = undefined, // Must be set by parser
            .client = null,
            .prompt = null,
            .attributes = std.ArrayList(Attribute).init(allocator),
            .docstring = null,
            .location = location,
        };
    }

    pub fn deinit(self: *FunctionDecl, allocator: std.mem.Allocator) void {
        for (self.parameters.items) |*param| {
            param.deinit(allocator);
        }
        self.parameters.deinit();
        self.return_type.deinit(allocator);
        allocator.destroy(self.return_type);
        for (self.attributes.items) |*attr| {
            attr.deinit(allocator);
        }
        self.attributes.deinit();
    }
};

/// Client declaration: client<llm> Name { provider ... options { ... } }
pub const ClientDecl = struct {
    name: []const u8,
    client_type: []const u8, // e.g., "llm"
    provider: []const u8,
    options: std.StringHashMap(Value),
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, client_type: []const u8, location: Location) ClientDecl {
        return ClientDecl{
            .name = name,
            .client_type = client_type,
            .provider = "",
            .options = std.StringHashMap(Value).init(allocator),
            .location = location,
        };
    }

    pub fn deinit(self: *ClientDecl, allocator: std.mem.Allocator) void {
        var it = self.options.iterator();
        while (it.next()) |entry| {
            var value = entry.value_ptr.*;
            value.deinit(allocator);
        }
        self.options.deinit();
    }
};

/// Test declaration: test Name { functions [...] args { ... } }
pub const TestDecl = struct {
    name: []const u8,
    functions: std.ArrayList([]const u8),
    args: std.StringHashMap(Value),
    attributes: std.ArrayList(Attribute),
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, location: Location) TestDecl {
        return TestDecl{
            .name = name,
            .functions = std.ArrayList([]const u8).init(allocator),
            .args = std.StringHashMap(Value).init(allocator),
            .attributes = std.ArrayList(Attribute).init(allocator),
            .location = location,
        };
    }

    pub fn deinit(self: *TestDecl, allocator: std.mem.Allocator) void {
        self.functions.deinit();
        var it = self.args.iterator();
        while (it.next()) |entry| {
            var value = entry.value_ptr.*;
            value.deinit(allocator);
        }
        self.args.deinit();
        for (self.attributes.items) |*attr| {
            attr.deinit(allocator);
        }
        self.attributes.deinit();
    }
};

/// Generator declaration: generator Name { ... }
pub const GeneratorDecl = struct {
    name: []const u8,
    options: std.StringHashMap(Value),
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, location: Location) GeneratorDecl {
        return GeneratorDecl{
            .name = name,
            .options = std.StringHashMap(Value).init(allocator),
            .location = location,
        };
    }

    pub fn deinit(self: *GeneratorDecl, allocator: std.mem.Allocator) void {
        var it = self.options.iterator();
        while (it.next()) |entry| {
            var value = entry.value_ptr.*;
            value.deinit(allocator);
        }
        self.options.deinit();
    }
};

/// Template string declaration: template_string Name(params) #"..."#
pub const TemplateStringDecl = struct {
    name: []const u8,
    parameters: std.ArrayList(Parameter),
    template: []const u8,
    location: Location,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, location: Location) TemplateStringDecl {
        return TemplateStringDecl{
            .name = name,
            .parameters = std.ArrayList(Parameter).init(allocator),
            .template = "",
            .location = location,
        };
    }

    pub fn deinit(self: *TemplateStringDecl, allocator: std.mem.Allocator) void {
        for (self.parameters.items) |*param| {
            param.deinit(allocator);
        }
        self.parameters.deinit();
    }
};

/// Type alias declaration: type Name = Type
pub const TypeAliasDecl = struct {
    name: []const u8,
    type_expr: *TypeExpr,
    location: Location,

    pub fn deinit(self: *TypeAliasDecl, allocator: std.mem.Allocator) void {
        self.type_expr.deinit(allocator);
        allocator.destroy(self.type_expr);
    }
};

/// Type expression tags
pub const TypeExprTag = enum {
    primitive,
    named,
    array,
    optional,
    union_type,
    map,
    literal,
};

/// Type expression representing BAML types
pub const TypeExpr = union(TypeExprTag) {
    primitive: PrimitiveType,
    named: []const u8,
    array: *TypeExpr,
    optional: *TypeExpr,
    union_type: UnionType,
    map: MapType,
    literal: LiteralValue,

    pub fn deinit(self: *TypeExpr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .array => |inner| {
                inner.deinit(allocator);
                allocator.destroy(inner);
            },
            .optional => |inner| {
                inner.deinit(allocator);
                allocator.destroy(inner);
            },
            .union_type => |*u| {
                for (u.types.items) |*t| {
                    t.deinit(allocator);
                    allocator.destroy(t);
                }
                u.types.deinit();
            },
            .map => |*m| {
                m.key_type.deinit(allocator);
                allocator.destroy(m.key_type);
                m.value_type.deinit(allocator);
                allocator.destroy(m.value_type);
            },
            else => {},
        }
    }
};

/// Primitive type enumeration
pub const PrimitiveType = enum {
    string,
    int,
    float,
    bool,
    null_type,
    image,
    audio,
    video,
    pdf,
};

/// Union type: Type | Type | ...
pub const UnionType = struct {
    types: std.ArrayList(*TypeExpr),
};

/// Map type: map<K, V>
pub const MapType = struct {
    key_type: *TypeExpr,
    value_type: *TypeExpr,
};

/// Literal value in types or expressions
pub const LiteralValue = union(enum) {
    string: []const u8,
    int: i64,
    float: f64,
    bool: bool,
    null_value,
};

/// Class or enum property
pub const Property = struct {
    name: []const u8,
    type_expr: *TypeExpr,
    attributes: std.ArrayList(Attribute),
    docstring: ?[]const u8,
    location: Location,

    pub fn deinit(self: *Property, allocator: std.mem.Allocator) void {
        self.type_expr.deinit(allocator);
        allocator.destroy(self.type_expr);
        for (self.attributes.items) |*attr| {
            attr.deinit(allocator);
        }
        self.attributes.deinit();
    }
};

/// Enum value
pub const EnumValue = struct {
    name: []const u8,
    attributes: std.ArrayList(Attribute),
    docstring: ?[]const u8,
    location: Location,

    pub fn deinit(self: *EnumValue, allocator: std.mem.Allocator) void {
        for (self.attributes.items) |*attr| {
            attr.deinit(allocator);
        }
        self.attributes.deinit();
    }
};

/// Function parameter
pub const Parameter = struct {
    name: []const u8,
    type_expr: *TypeExpr,
    location: Location,

    pub fn deinit(self: *Parameter, allocator: std.mem.Allocator) void {
        self.type_expr.deinit(allocator);
        allocator.destroy(self.type_expr);
    }
};

/// Attribute: @name or @@name with optional arguments
pub const Attribute = struct {
    name: []const u8,
    is_class_level: bool, // @@ vs @
    args: std.ArrayList(Value),
    location: Location,

    pub fn deinit(self: *Attribute, allocator: std.mem.Allocator) void {
        for (self.args.items) |*arg| {
            arg.deinit(allocator);
        }
        self.args.deinit();
    }
};

/// Value type for attribute arguments, options, etc.
pub const ValueTag = enum {
    string,
    int,
    float,
    bool,
    null_value,
    array,
    object,
    env_var,
};

/// Value in attribute args, test args, client options, etc.
pub const Value = union(ValueTag) {
    string: []const u8,
    int: i64,
    float: f64,
    bool: bool,
    null_value,
    array: std.ArrayList(Value),
    object: std.StringHashMap(Value),
    env_var: []const u8, // env.VAR_NAME

    pub fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .array => |*arr| {
                for (arr.items) |*item| {
                    item.deinit(allocator);
                }
                arr.deinit();
            },
            .object => |*obj| {
                var it = obj.iterator();
                while (it.next()) |entry| {
                    var value = entry.value_ptr.*;
                    value.deinit(allocator);
                }
                obj.deinit();
            },
            else => {},
        }
    }
};

// Tests
test "AST: Create and cleanup Ast" {
    const allocator = std.testing.allocator;
    var ast = Ast.init(allocator);
    defer ast.deinit();

    try std.testing.expect(ast.declarations.items.len == 0);
}

test "AST: Create ClassDecl" {
    const allocator = std.testing.allocator;
    var class_decl = ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });
    defer class_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Person", class_decl.name);
    try std.testing.expect(class_decl.properties.items.len == 0);
    try std.testing.expect(class_decl.location.line == 1);
}

test "AST: Create EnumDecl" {
    const allocator = std.testing.allocator;
    var enum_decl = EnumDecl.init(allocator, "Status", .{ .line = 1, .column = 1 });
    defer enum_decl.deinit(allocator);

    try std.testing.expectEqualStrings("Status", enum_decl.name);
    try std.testing.expect(enum_decl.values.items.len == 0);
}

test "AST: Create Value types" {
    const allocator = std.testing.allocator;

    var str_val = Value{ .string = "hello" };
    str_val.deinit(allocator);

    var int_val = Value{ .int = 42 };
    int_val.deinit(allocator);

    var bool_val = Value{ .bool = true };
    bool_val.deinit(allocator);

    var null_val = Value{ .null_value = {} };
    null_val.deinit(allocator);
}

test "AST: PrimitiveType enum" {
    const pt = PrimitiveType.string;
    try std.testing.expect(pt == .string);
}

test "AST: LiteralValue union" {
    const lit_str = LiteralValue{ .string = "test" };
    try std.testing.expectEqualStrings("test", lit_str.string);

    const lit_int = LiteralValue{ .int = 123 };
    try std.testing.expect(lit_int.int == 123);

    const lit_bool = LiteralValue{ .bool = false };
    try std.testing.expect(lit_bool.bool == false);
}
