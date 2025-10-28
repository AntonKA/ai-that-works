const std = @import("std");
const ast = @import("ast.zig");
const jinja = @import("jinja.zig");

/// Validation error types
pub const ValidationError = error{
    DuplicateDefinition,
    UndefinedType,
    CircularDependency,
    InvalidType,
    InvalidAttribute,
    OutOfMemory,
};

/// Validation diagnostic message
pub const Diagnostic = struct {
    message: []const u8,
    line: usize,
    column: usize,
    severity: Severity,

    pub const Severity = enum {
        err,
        warning,
        info,
    };

    pub fn deinit(self: *Diagnostic, allocator: std.mem.Allocator) void {
        allocator.free(self.message);
    }
};

/// Type kind in the symbol table
pub const TypeKind = enum {
    class,
    enum_type,
    primitive,
};

/// Symbol table entry for a type
pub const TypeSymbol = struct {
    name: []const u8,
    kind: TypeKind,
    location: ast.Location,
};

/// Type registry for tracking all declared types
pub const TypeRegistry = struct {
    types: std.StringHashMap(TypeSymbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeRegistry {
        var registry = TypeRegistry{
            .types = std.StringHashMap(TypeSymbol).init(allocator),
            .allocator = allocator,
        };

        // Register primitive types
        registry.registerPrimitive("string") catch {};
        registry.registerPrimitive("int") catch {};
        registry.registerPrimitive("float") catch {};
        registry.registerPrimitive("bool") catch {};
        registry.registerPrimitive("null") catch {};
        registry.registerPrimitive("image") catch {};
        registry.registerPrimitive("audio") catch {};
        registry.registerPrimitive("video") catch {};
        registry.registerPrimitive("pdf") catch {};

        return registry;
    }

    pub fn deinit(self: *TypeRegistry) void {
        self.types.deinit();
    }

    fn registerPrimitive(self: *TypeRegistry, name: []const u8) !void {
        try self.types.put(name, TypeSymbol{
            .name = name,
            .kind = .primitive,
            .location = .{ .line = 0, .column = 0 },
        });
    }

    pub fn registerClass(self: *TypeRegistry, name: []const u8, location: ast.Location) !void {
        if (self.types.contains(name)) {
            return ValidationError.DuplicateDefinition;
        }
        try self.types.put(name, TypeSymbol{
            .name = name,
            .kind = .class,
            .location = location,
        });
    }

    pub fn registerEnum(self: *TypeRegistry, name: []const u8, location: ast.Location) !void {
        if (self.types.contains(name)) {
            return ValidationError.DuplicateDefinition;
        }
        try self.types.put(name, TypeSymbol{
            .name = name,
            .kind = .enum_type,
            .location = location,
        });
    }

    pub fn isDefined(self: *const TypeRegistry, name: []const u8) bool {
        return self.types.contains(name);
    }

    pub fn getType(self: *const TypeRegistry, name: []const u8) ?TypeSymbol {
        return self.types.get(name);
    }
};

/// Function registry for tracking all declared functions
pub const FunctionRegistry = struct {
    functions: std.StringHashMap(ast.Location),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) FunctionRegistry {
        return FunctionRegistry{
            .functions = std.StringHashMap(ast.Location).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *FunctionRegistry) void {
        self.functions.deinit();
    }

    pub fn registerFunction(self: *FunctionRegistry, name: []const u8, location: ast.Location) !void {
        if (self.functions.contains(name)) {
            return ValidationError.DuplicateDefinition;
        }
        try self.functions.put(name, location);
    }

    pub fn isDefined(self: *const FunctionRegistry, name: []const u8) bool {
        return self.functions.contains(name);
    }
};

/// Retry policy registry for tracking all declared retry policies
pub const RetryPolicyRegistry = struct {
    policies: std.StringHashMap(ast.Location),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) RetryPolicyRegistry {
        return RetryPolicyRegistry{
            .policies = std.StringHashMap(ast.Location).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *RetryPolicyRegistry) void {
        self.policies.deinit();
    }

    pub fn registerRetryPolicy(self: *RetryPolicyRegistry, name: []const u8, location: ast.Location) !void {
        if (self.policies.contains(name)) {
            return ValidationError.DuplicateDefinition;
        }
        try self.policies.put(name, location);
    }

    pub fn isDefined(self: *const RetryPolicyRegistry, name: []const u8) bool {
        return self.policies.contains(name);
    }
};

/// Client registry for tracking all declared clients
pub const ClientRegistry = struct {
    clients: std.StringHashMap(ast.Location),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ClientRegistry {
        return ClientRegistry{
            .clients = std.StringHashMap(ast.Location).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ClientRegistry) void {
        self.clients.deinit();
    }

    pub fn registerClient(self: *ClientRegistry, name: []const u8, location: ast.Location) !void {
        if (self.clients.contains(name)) {
            return ValidationError.DuplicateDefinition;
        }
        try self.clients.put(name, location);
    }

    pub fn isDefined(self: *const ClientRegistry, name: []const u8) bool {
        return self.clients.contains(name);
    }
};

/// Validator for BAML AST
pub const Validator = struct {
    allocator: std.mem.Allocator,
    type_registry: TypeRegistry,
    function_registry: FunctionRegistry,
    retry_policy_registry: RetryPolicyRegistry,
    client_registry: ClientRegistry,
    diagnostics: std.ArrayList(Diagnostic),

    pub fn init(allocator: std.mem.Allocator) Validator {
        return Validator{
            .allocator = allocator,
            .type_registry = TypeRegistry.init(allocator),
            .function_registry = FunctionRegistry.init(allocator),
            .retry_policy_registry = RetryPolicyRegistry.init(allocator),
            .client_registry = ClientRegistry.init(allocator),
            .diagnostics = std.ArrayList(Diagnostic){},
        };
    }

    pub fn deinit(self: *Validator) void {
        for (self.diagnostics.items) |*diag| {
            diag.deinit(self.allocator);
        }
        self.diagnostics.deinit(self.allocator);
        self.type_registry.deinit();
        self.function_registry.deinit();
        self.retry_policy_registry.deinit();
        self.client_registry.deinit();
    }

    /// Validate an entire AST
    pub fn validate(self: *Validator, tree: *const ast.Ast) !void {
        // Phase 1: Register all types and functions
        try self.registerDeclarations(tree);

        // Phase 2: Validate type references
        try self.validateTypeReferences(tree);

        // Phase 3: Check for circular dependencies
        try self.checkCircularDependencies(tree);

        // Phase 4: Validate attribute usage
        try self.validateAttributes(tree);

        // Phase 5: Validate Jinja templates in prompts
        try self.validateTemplates(tree);
    }

    /// Register all declarations in the AST
    fn registerDeclarations(self: *Validator, tree: *const ast.Ast) !void {
        for (tree.declarations.items) |decl| {
            switch (decl) {
                .class_decl => |class| {
                    self.type_registry.registerClass(class.name, class.location) catch |err| {
                        if (err == ValidationError.DuplicateDefinition) {
                            try self.addError("Duplicate class definition: {s}", .{class.name}, class.location);
                        } else {
                            return err;
                        }
                    };
                },
                .enum_decl => |enum_decl| {
                    self.type_registry.registerEnum(enum_decl.name, enum_decl.location) catch |err| {
                        if (err == ValidationError.DuplicateDefinition) {
                            try self.addError("Duplicate enum definition: {s}", .{enum_decl.name}, enum_decl.location);
                        } else {
                            return err;
                        }
                    };
                },
                .function_decl => |func| {
                    self.function_registry.registerFunction(func.name, func.location) catch |err| {
                        if (err == ValidationError.DuplicateDefinition) {
                            try self.addError("Duplicate function definition: {s}", .{func.name}, func.location);
                        } else {
                            return err;
                        }
                    };
                },
                .retry_policy_decl => |policy| {
                    self.retry_policy_registry.registerRetryPolicy(policy.name, policy.location) catch |err| {
                        if (err == ValidationError.DuplicateDefinition) {
                            try self.addError("Duplicate retry_policy definition: {s}", .{policy.name}, policy.location);
                        } else {
                            return err;
                        }
                    };
                },
                .client_decl => |client| {
                    self.client_registry.registerClient(client.name, client.location) catch |err| {
                        if (err == ValidationError.DuplicateDefinition) {
                            try self.addError("Duplicate client definition: {s}", .{client.name}, client.location);
                        } else {
                            return err;
                        }
                    };
                },
                else => {},
            }
        }
    }

    /// Validate all type references in the AST
    fn validateTypeReferences(self: *Validator, tree: *const ast.Ast) !void {
        for (tree.declarations.items) |decl| {
            switch (decl) {
                .class_decl => |class| {
                    for (class.properties.items) |prop| {
                        try self.validateTypeExpr(prop.type_expr, prop.location);
                    }
                },
                .function_decl => |func| {
                    // Validate parameter types
                    for (func.parameters.items) |param| {
                        try self.validateTypeExpr(param.type_expr, param.location);
                    }
                    // Validate return type
                    try self.validateTypeExpr(func.return_type, func.location);
                },
                .template_string_decl => |tmpl| {
                    // Validate parameter types
                    for (tmpl.parameters.items) |param| {
                        try self.validateTypeExpr(param.type_expr, param.location);
                    }
                },
                .test_decl => |test_decl| {
                    // Validate function references in tests
                    for (test_decl.functions.items) |func_name| {
                        if (!self.function_registry.isDefined(func_name)) {
                            try self.addError("Undefined function in test: {s}", .{func_name}, test_decl.location);
                        }
                    }
                },
                .client_decl => |client| {
                    // Validate retry_policy references in clients
                    if (client.retry_policy) |policy_name| {
                        if (!self.retry_policy_registry.isDefined(policy_name)) {
                            try self.addError("Undefined retry_policy in client: {s}", .{policy_name}, client.location);
                        }
                    }

                    // Validate strategy lists in fallback/round_robin clients
                    if (std.mem.eql(u8, client.provider, "fallback") or std.mem.eql(u8, client.provider, "round_robin")) {
                        if (client.options.get("strategy")) |strategy_value| {
                            try self.validateStrategyList(strategy_value, client.location);
                        }
                    }
                },
                else => {},
            }
        }
    }

    /// Validate a type expression
    fn validateTypeExpr(self: *Validator, type_expr: *const ast.TypeExpr, location: ast.Location) ValidationError!void {
        switch (type_expr.*) {
            .primitive => {
                // Primitive types are always valid
            },
            .named => |name| {
                if (!self.type_registry.isDefined(name)) {
                    try self.addError("Undefined type: {s}", .{name}, location);
                }
            },
            .array => |inner| {
                try self.validateTypeExpr(inner, location);
            },
            .optional => |inner| {
                try self.validateTypeExpr(inner, location);
            },
            .union_type => |union_type| {
                for (union_type.types.items) |inner| {
                    try self.validateTypeExpr(inner, location);
                }
            },
            .map => |map_type| {
                try self.validateTypeExpr(map_type.key_type, location);
                try self.validateTypeExpr(map_type.value_type, location);
            },
            .literal => {
                // Literal types are always valid
            },
        }
    }

    /// Validate a strategy list in fallback/round_robin clients
    fn validateStrategyList(self: *Validator, strategy_value: ast.Value, location: ast.Location) ValidationError!void {
        switch (strategy_value) {
            .array => |arr| {
                // Validate each client name in the strategy list
                for (arr.items) |item| {
                    switch (item) {
                        .string => |client_name| {
                            if (!self.client_registry.isDefined(client_name)) {
                                try self.addError("Undefined client in strategy list: {s}", .{client_name}, location);
                            }
                        },
                        else => {
                            try self.addError("Strategy list must contain client names (strings), found {s}", .{@tagName(item)}, location);
                        },
                    }
                }
            },
            else => {
                try self.addError("Strategy field must be an array of client names", .{}, location);
            },
        }
    }

    /// Check for circular dependencies in type definitions
    fn checkCircularDependencies(self: *Validator, tree: *const ast.Ast) !void {
        var visited = std.StringHashMap(void).init(self.allocator);
        defer visited.deinit();

        var visiting = std.StringHashMap(void).init(self.allocator);
        defer visiting.deinit();

        for (tree.declarations.items) |decl| {
            switch (decl) {
                .class_decl => |class| {
                    visited.clearRetainingCapacity();
                    visiting.clearRetainingCapacity();
                    try self.checkClassCircular(tree, class.name, &visited, &visiting, class.location);
                },
                else => {},
            }
        }
    }

    /// Check if a class has circular dependencies
    fn checkClassCircular(
        self: *Validator,
        tree: *const ast.Ast,
        class_name: []const u8,
        visited: *std.StringHashMap(void),
        visiting: *std.StringHashMap(void),
        location: ast.Location,
    ) ValidationError!void {
        if (visited.contains(class_name)) {
            return;
        }

        if (visiting.contains(class_name)) {
            try self.addError("Circular dependency detected in type: {s}", .{class_name}, location);
            return;
        }

        try visiting.put(class_name, {});

        // Find the class declaration
        for (tree.declarations.items) |decl| {
            switch (decl) {
                .class_decl => |class| {
                    if (std.mem.eql(u8, class.name, class_name)) {
                        // Check all property types
                        for (class.properties.items) |prop| {
                            try self.checkTypeExprCircular(tree, prop.type_expr, visited, visiting, prop.location);
                        }
                        break;
                    }
                },
                else => {},
            }
        }

        _ = visiting.remove(class_name);
        try visited.put(class_name, {});
    }

    /// Check if a type expression leads to circular dependencies
    fn checkTypeExprCircular(
        self: *Validator,
        tree: *const ast.Ast,
        type_expr: *const ast.TypeExpr,
        visited: *std.StringHashMap(void),
        visiting: *std.StringHashMap(void),
        location: ast.Location,
    ) ValidationError!void {
        switch (type_expr.*) {
            .named => |name| {
                // Only check class types for circular dependencies
                if (self.type_registry.getType(name)) |type_symbol| {
                    if (type_symbol.kind == .class) {
                        try self.checkClassCircular(tree, name, visited, visiting, location);
                    }
                }
            },
            .array => |inner| {
                try self.checkTypeExprCircular(tree, inner, visited, visiting, location);
            },
            .optional => |inner| {
                try self.checkTypeExprCircular(tree, inner, visited, visiting, location);
            },
            .union_type => |union_type| {
                for (union_type.types.items) |inner| {
                    try self.checkTypeExprCircular(tree, inner, visited, visiting, location);
                }
            },
            .map => |map_type| {
                try self.checkTypeExprCircular(tree, map_type.key_type, visited, visiting, location);
                try self.checkTypeExprCircular(tree, map_type.value_type, visited, visiting, location);
            },
            else => {},
        }
    }

    /// Validate all attributes in the AST
    fn validateAttributes(self: *Validator, tree: *const ast.Ast) !void {
        for (tree.declarations.items) |decl| {
            switch (decl) {
                .class_decl => |class| {
                    // Validate class-level attributes
                    try self.validateClassAttributes(class.attributes.items, class.location);
                    // Validate property-level attributes
                    for (class.properties.items) |prop| {
                        try self.validatePropertyAttributes(prop.attributes.items, prop.location);
                    }
                },
                .enum_decl => |enum_decl| {
                    // Validate enum-level attributes
                    try self.validateEnumAttributes(enum_decl.attributes.items, enum_decl.location);
                    // Validate enum value attributes
                    for (enum_decl.values.items) |val| {
                        try self.validateEnumValueAttributes(val.attributes.items, val.location);
                    }
                },
                .test_decl => |test_decl| {
                    // Validate test-level attributes
                    try self.validateTestAttributes(test_decl.attributes.items, test_decl.location);
                },
                .function_decl => |func| {
                    // Validate function-level attributes
                    try self.validateFunctionAttributes(func.attributes.items, func.location);
                },
                else => {},
            }
        }
    }

    /// Validate property-level attributes
    fn validatePropertyAttributes(self: *Validator, attributes: []const ast.Attribute, _: ast.Location) !void {
        for (attributes) |attr| {
            // Check if it's a class-level attribute on a property (@@)
            if (attr.is_class_level) {
                try self.addError("Class-level attribute @@{s} cannot be used on properties", .{attr.name}, attr.location);
                continue;
            }

            // Validate specific property attributes
            if (std.mem.eql(u8, attr.name, "alias")) {
                // @alias requires exactly 1 string argument
                if (attr.args.items.len != 1) {
                    try self.addError("@alias requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@alias requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "description")) {
                // @description requires exactly 1 string argument
                if (attr.args.items.len != 1) {
                    try self.addError("@description requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@description requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "skip")) {
                // @skip should have no arguments
                if (attr.args.items.len > 0) {
                    try self.addWarning("@skip does not take arguments", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "assert")) {
                // @assert is for properties (constraint validation)
                if (attr.args.items.len == 0) {
                    try self.addError("@assert requires at least 1 argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "check")) {
                // @check is for properties (validation check)
                if (attr.args.items.len == 0) {
                    try self.addError("@check requires at least 1 argument", .{}, attr.location);
                }
            } else {
                // Unknown attribute - warning
                try self.addWarning("Unknown property attribute @{s}", .{attr.name}, attr.location);
            }
        }
    }

    /// Validate class-level attributes
    fn validateClassAttributes(self: *Validator, attributes: []const ast.Attribute, _: ast.Location) !void {
        for (attributes) |attr| {
            // Check if it's a property-level attribute on a class (@)
            if (!attr.is_class_level) {
                try self.addError("Property-level attribute @{s} cannot be used on classes (use @@{s} instead)", .{ attr.name, attr.name }, attr.location);
                continue;
            }

            // Validate specific class attributes
            if (std.mem.eql(u8, attr.name, "alias")) {
                // @@alias requires exactly 1 string argument
                if (attr.args.items.len != 1) {
                    try self.addError("@@alias requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@@alias requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "description")) {
                // @@description requires exactly 1 string argument
                if (attr.args.items.len != 1) {
                    try self.addError("@@description requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@@description requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "dynamic")) {
                // @@dynamic should have no arguments
                if (attr.args.items.len > 0) {
                    try self.addWarning("@@dynamic does not take arguments", .{}, attr.location);
                }
            } else {
                // Unknown attribute - warning
                try self.addWarning("Unknown class attribute @@{s}", .{attr.name}, attr.location);
            }
        }
    }

    /// Validate enum-level attributes
    fn validateEnumAttributes(self: *Validator, attributes: []const ast.Attribute, _: ast.Location) !void {
        for (attributes) |attr| {
            // Check if it's a property-level attribute on an enum (@)
            if (!attr.is_class_level) {
                try self.addError("Property-level attribute @{s} cannot be used on enums (use @@{s} instead)", .{ attr.name, attr.name }, attr.location);
                continue;
            }

            // Validate specific enum attributes (same as class attributes)
            if (std.mem.eql(u8, attr.name, "alias")) {
                if (attr.args.items.len != 1) {
                    try self.addError("@@alias requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@@alias requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "description")) {
                if (attr.args.items.len != 1) {
                    try self.addError("@@description requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@@description requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "dynamic")) {
                if (attr.args.items.len > 0) {
                    try self.addWarning("@@dynamic does not take arguments", .{}, attr.location);
                }
            } else {
                try self.addWarning("Unknown enum attribute @@{s}", .{attr.name}, attr.location);
            }
        }
    }

    /// Validate enum value attributes
    fn validateEnumValueAttributes(self: *Validator, attributes: []const ast.Attribute, _: ast.Location) !void {
        // Enum values use property-level attributes (@)
        for (attributes) |attr| {
            if (attr.is_class_level) {
                try self.addError("Class-level attribute @@{s} cannot be used on enum values", .{attr.name}, attr.location);
                continue;
            }

            // Validate specific enum value attributes (similar to properties)
            if (std.mem.eql(u8, attr.name, "alias")) {
                if (attr.args.items.len != 1) {
                    try self.addError("@alias requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@alias requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "description")) {
                if (attr.args.items.len != 1) {
                    try self.addError("@description requires exactly 1 argument, got {d}", .{attr.args.items.len}, attr.location);
                } else if (attr.args.items[0] != .string) {
                    try self.addError("@description requires a string argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "skip")) {
                if (attr.args.items.len > 0) {
                    try self.addWarning("@skip does not take arguments", .{}, attr.location);
                }
            } else {
                try self.addWarning("Unknown enum value attribute @{s}", .{attr.name}, attr.location);
            }
        }
    }

    /// Validate test-level attributes
    fn validateTestAttributes(self: *Validator, attributes: []const ast.Attribute, _: ast.Location) !void {
        for (attributes) |attr| {
            // Test attributes must be class-level (@@)
            if (!attr.is_class_level) {
                try self.addError("Test attribute @{s} must be class-level (use @@{s})", .{ attr.name, attr.name }, attr.location);
                continue;
            }

            // Validate specific test attributes
            if (std.mem.eql(u8, attr.name, "check")) {
                // @@check requires at least 1 argument (the expression to check)
                if (attr.args.items.len == 0) {
                    try self.addError("@@check requires at least 1 argument", .{}, attr.location);
                }
            } else if (std.mem.eql(u8, attr.name, "assert")) {
                // @@assert requires at least 1 argument (the expression to assert)
                if (attr.args.items.len == 0) {
                    try self.addError("@@assert requires at least 1 argument", .{}, attr.location);
                }
            } else {
                try self.addWarning("Unknown test attribute @@{s}", .{attr.name}, attr.location);
            }
        }
    }

    /// Validate function-level attributes
    fn validateFunctionAttributes(self: *Validator, attributes: []const ast.Attribute, _: ast.Location) !void {
        // Functions don't have many standard attributes in BAML
        for (attributes) |attr| {
            // Just warn about any attributes on functions
            if (attr.is_class_level) {
                try self.addWarning("Attribute @@{s} on function may not be supported", .{attr.name}, attr.location);
            } else {
                try self.addWarning("Attribute @{s} on function may not be supported", .{attr.name}, attr.location);
            }
        }
    }

    /// Validate Jinja templates in function prompts and template_strings
    fn validateTemplates(self: *Validator, tree: *const ast.Ast) !void {
        for (tree.declarations.items) |decl| {
            switch (decl) {
                .function_decl => |func| {
                    if (func.prompt) |prompt| {
                        try self.validateFunctionPrompt(func, prompt);
                    }
                },
                .template_string_decl => |tmpl| {
                    try self.validateTemplateString(tmpl);
                },
                else => {},
            }
        }
    }

    /// Validate a function's prompt template
    fn validateFunctionPrompt(self: *Validator, func: ast.FunctionDecl, prompt: []const u8) !void {
        // Collect parameter names
        var param_names = std.ArrayList([]const u8){};
        defer param_names.deinit(self.allocator);

        for (func.parameters.items) |param| {
            try param_names.append(self.allocator, param.name);
        }

        // Validate the prompt
        const errors = try jinja.validateFunctionPrompt(
            self.allocator,
            prompt,
            param_names.items,
        );
        defer self.allocator.free(errors);

        // Add any Jinja validation errors as diagnostics
        for (errors) |err| {
            try self.addError("{s}", .{err.message}, ast.Location{
                .line = err.line,
                .column = err.column,
            });
        }
    }

    /// Validate a template_string's template
    fn validateTemplateString(self: *Validator, tmpl: ast.TemplateStringDecl) !void {
        // Collect parameter names
        var param_names = std.ArrayList([]const u8){};
        defer param_names.deinit(self.allocator);

        for (tmpl.parameters.items) |param| {
            try param_names.append(self.allocator, param.name);
        }

        // Validate the template
        const errors = try jinja.validateFunctionPrompt(
            self.allocator,
            tmpl.template,
            param_names.items,
        );
        defer self.allocator.free(errors);

        // Add any Jinja validation errors as diagnostics
        for (errors) |err| {
            try self.addError("{s}", .{err.message}, ast.Location{
                .line = err.line,
                .column = err.column,
            });
        }
    }

    /// Add an error diagnostic
    fn addError(self: *Validator, comptime fmt: []const u8, args: anytype, location: ast.Location) !void {
        const message = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.diagnostics.append(self.allocator, Diagnostic{
            .message = message,
            .line = location.line,
            .column = location.column,
            .severity = .err,
        });
    }

    /// Add a warning diagnostic
    fn addWarning(self: *Validator, comptime fmt: []const u8, args: anytype, location: ast.Location) !void {
        const message = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.diagnostics.append(self.allocator, Diagnostic{
            .message = message,
            .line = location.line,
            .column = location.column,
            .severity = .warning,
        });
    }

    /// Check if validation found any errors
    pub fn hasErrors(self: *const Validator) bool {
        for (self.diagnostics.items) |diag| {
            if (diag.severity == .err) {
                return true;
            }
        }
        return false;
    }

    /// Get all diagnostics
    pub fn getDiagnostics(self: *const Validator) []const Diagnostic {
        return self.diagnostics.items;
    }
};

// Tests
test "Validator: Create and cleanup" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    try std.testing.expect(validator.diagnostics.items.len == 0);
}

test "Validator: TypeRegistry primitives" {
    const allocator = std.testing.allocator;
    var registry = TypeRegistry.init(allocator);
    defer registry.deinit();

    try std.testing.expect(registry.isDefined("string"));
    try std.testing.expect(registry.isDefined("int"));
    try std.testing.expect(registry.isDefined("float"));
    try std.testing.expect(registry.isDefined("bool"));
    try std.testing.expect(registry.isDefined("image"));
    try std.testing.expect(!registry.isDefined("CustomType"));
}

test "Validator: Register class" {
    const allocator = std.testing.allocator;
    var registry = TypeRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerClass("Person", .{ .line = 1, .column = 1 });
    try std.testing.expect(registry.isDefined("Person"));

    const symbol = registry.getType("Person").?;
    try std.testing.expectEqualStrings("Person", symbol.name);
    try std.testing.expect(symbol.kind == .class);
}

test "Validator: Detect duplicate class" {
    const allocator = std.testing.allocator;
    var registry = TypeRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerClass("Person", .{ .line = 1, .column = 1 });
    const result = registry.registerClass("Person", .{ .line = 10, .column = 1 });
    try std.testing.expectError(ValidationError.DuplicateDefinition, result);
}

test "Validator: Register enum" {
    const allocator = std.testing.allocator;
    var registry = TypeRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerEnum("Status", .{ .line = 1, .column = 1 });
    try std.testing.expect(registry.isDefined("Status"));

    const symbol = registry.getType("Status").?;
    try std.testing.expectEqualStrings("Status", symbol.name);
    try std.testing.expect(symbol.kind == .enum_type);
}

test "Validator: FunctionRegistry" {
    const allocator = std.testing.allocator;
    var registry = FunctionRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerFunction("greet", .{ .line = 1, .column = 1 });
    try std.testing.expect(registry.isDefined("greet"));
    try std.testing.expect(!registry.isDefined("other"));
}

test "Validator: Detect duplicate function" {
    const allocator = std.testing.allocator;
    var registry = FunctionRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerFunction("greet", .{ .line = 1, .column = 1 });
    const result = registry.registerFunction("greet", .{ .line = 10, .column = 1 });
    try std.testing.expectError(ValidationError.DuplicateDefinition, result);
}

test "Validator: Validate simple class" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a simple class with primitive types
    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    // Add property: name string
    const name_type = try allocator.create(ast.TypeExpr);
    name_type.* = ast.TypeExpr{ .primitive = .string };

    const name_prop = ast.Property{
        .name = "name",
        .type_expr = name_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };
    try class.properties.append(allocator, name_prop);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Detect undefined type" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a class with undefined type
    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    // Add property: address Address (Address not defined)
    const address_type = try allocator.create(ast.TypeExpr);
    address_type.* = ast.TypeExpr{ .named = "Address" };

    const address_prop = ast.Property{
        .name = "address",
        .type_expr = address_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };
    try class.properties.append(allocator, address_prop);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
    try std.testing.expect(validator.diagnostics.items.len > 0);
}

test "Validator: Detect undefined function in test" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a test that references undefined function
    var test_decl = ast.TestDecl.init(allocator, "TestGreet", .{ .line = 1, .column = 1 });
    try test_decl.functions.append(allocator, "UndefinedFunction");

    try tree.declarations.append(allocator, ast.Declaration{ .test_decl = test_decl });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Detect circular dependency" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create A -> B -> A circular dependency
    var class_a = ast.ClassDecl.init(allocator, "A", .{ .line = 1, .column = 1 });
    const b_type = try allocator.create(ast.TypeExpr);
    b_type.* = ast.TypeExpr{ .named = "B" };
    const b_prop = ast.Property{
        .name = "b",
        .type_expr = b_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };
    try class_a.properties.append(allocator, b_prop);

    var class_b = ast.ClassDecl.init(allocator, "B", .{ .line = 5, .column = 1 });
    const a_type = try allocator.create(ast.TypeExpr);
    a_type.* = ast.TypeExpr{ .named = "A" };
    const a_prop = ast.Property{
        .name = "a",
        .type_expr = a_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 6, .column = 3 },
    };
    try class_b.properties.append(allocator, a_prop);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class_a });
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class_b });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Complex types are valid" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Register Address class first
    const addr_class = ast.ClassDecl.init(allocator, "Address", .{ .line = 1, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = addr_class });

    // Create Person class with complex types
    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 5, .column = 1 });

    // Add property: addresses Address[]
    const inner_type = try allocator.create(ast.TypeExpr);
    inner_type.* = ast.TypeExpr{ .named = "Address" };
    const array_type = try allocator.create(ast.TypeExpr);
    array_type.* = ast.TypeExpr{ .array = inner_type };

    const addresses_prop = ast.Property{
        .name = "addresses",
        .type_expr = array_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 6, .column = 3 },
    };
    try class.properties.append(allocator, addresses_prop);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Valid @alias on property" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    // Add property with valid @alias attribute
    const name_type = try allocator.create(ast.TypeExpr);
    name_type.* = ast.TypeExpr{ .primitive = .string };

    var name_prop = ast.Property{
        .name = "name",
        .type_expr = name_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };

    // Create @alias("full_name") attribute
    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = false,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 2, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .string = "full_name" });
    try name_prop.attributes.append(allocator, alias_attr);

    try class.properties.append(allocator, name_prop);
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Invalid @alias with no arguments" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    const name_type = try allocator.create(ast.TypeExpr);
    name_type.* = ast.TypeExpr{ .primitive = .string };

    var name_prop = ast.Property{
        .name = "name",
        .type_expr = name_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };

    // Create @alias() with no arguments (invalid)
    const alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = false,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 2, .column = 10 },
    };
    try name_prop.attributes.append(allocator, alias_attr);

    try class.properties.append(allocator, name_prop);
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Invalid @alias with non-string argument" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    const name_type = try allocator.create(ast.TypeExpr);
    name_type.* = ast.TypeExpr{ .primitive = .string };

    var name_prop = ast.Property{
        .name = "name",
        .type_expr = name_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };

    // Create @alias(123) with int argument (invalid)
    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = false,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 2, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .int = 123 });
    try name_prop.attributes.append(allocator, alias_attr);

    try class.properties.append(allocator, name_prop);
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Valid @@alias on class" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    // Create @@alias("human") attribute
    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = true,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 1, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .string = "human" });
    try class.attributes.append(allocator, alias_attr);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Invalid @@alias on property (should be @)" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    const name_type = try allocator.create(ast.TypeExpr);
    name_type.* = ast.TypeExpr{ .primitive = .string };

    var name_prop = ast.Property{
        .name = "name",
        .type_expr = name_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };

    // Create @@alias on property (invalid - should be @)
    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = true,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 2, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .string = "full_name" });
    try name_prop.attributes.append(allocator, alias_attr);

    try class.properties.append(allocator, name_prop);
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Invalid @alias on class (should be @@)" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    // Create @alias on class (invalid - should be @@)
    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = false,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 1, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .string = "human" });
    try class.attributes.append(allocator, alias_attr);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Valid @@dynamic on class" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    // Create @@dynamic attribute (no arguments)
    const dynamic_attr = ast.Attribute{
        .name = "dynamic",
        .is_class_level = true,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 1, .column = 10 },
    };
    try class.attributes.append(allocator, dynamic_attr);

    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Valid @@check on test" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a test with @@check attribute
    var test_decl = ast.TestDecl.init(allocator, "TestGreet", .{ .line = 1, .column = 1 });

    // Create @@check(output, "length > 0") attribute
    var check_attr = ast.Attribute{
        .name = "check",
        .is_class_level = true,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 1, .column = 10 },
    };
    try check_attr.args.append(allocator, ast.Value{ .string = "output" });
    try check_attr.args.append(allocator, ast.Value{ .string = "length > 0" });
    try test_decl.attributes.append(allocator, check_attr);

    try tree.declarations.append(allocator, ast.Declaration{ .test_decl = test_decl });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Invalid @@check with no arguments" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var test_decl = ast.TestDecl.init(allocator, "TestGreet", .{ .line = 1, .column = 1 });

    // Create @@check() with no arguments (invalid)
    const check_attr = ast.Attribute{
        .name = "check",
        .is_class_level = true,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 1, .column = 10 },
    };
    try test_decl.attributes.append(allocator, check_attr);

    try tree.declarations.append(allocator, ast.Declaration{ .test_decl = test_decl });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());
}

test "Validator: Valid @skip on property" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var class = ast.ClassDecl.init(allocator, "Person", .{ .line = 1, .column = 1 });

    const name_type = try allocator.create(ast.TypeExpr);
    name_type.* = ast.TypeExpr{ .primitive = .string };

    var name_prop = ast.Property{
        .name = "internal_id",
        .type_expr = name_type,
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };

    // Create @skip attribute (no arguments)
    const skip_attr = ast.Attribute{
        .name = "skip",
        .is_class_level = false,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 2, .column = 10 },
    };
    try name_prop.attributes.append(allocator, skip_attr);

    try class.properties.append(allocator, name_prop);
    try tree.declarations.append(allocator, ast.Declaration{ .class_decl = class });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Valid @alias on enum value" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var enum_decl = ast.EnumDecl.init(allocator, "Status", .{ .line = 1, .column = 1 });

    // Create enum value with @alias attribute
    var enum_val = ast.EnumValue{
        .name = "Active",
        .attributes = std.ArrayList(ast.Attribute){},
        .docstring = null,
        .location = .{ .line = 2, .column = 3 },
    };

    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = false,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 2, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .string = "currently_active" });
    try enum_val.attributes.append(allocator, alias_attr);

    try enum_decl.values.append(allocator, enum_val);
    try tree.declarations.append(allocator, ast.Declaration{ .enum_decl = enum_decl });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Valid @@alias on enum" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var enum_decl = ast.EnumDecl.init(allocator, "Status", .{ .line = 1, .column = 1 });

    // Create @@alias on enum
    var alias_attr = ast.Attribute{
        .name = "alias",
        .is_class_level = true,
        .args = std.ArrayList(ast.Value){},
        .location = .{ .line = 1, .column = 10 },
    };
    try alias_attr.args.append(allocator, ast.Value{ .string = "user_status" });
    try enum_decl.attributes.append(allocator, alias_attr);

    try tree.declarations.append(allocator, ast.Declaration{ .enum_decl = enum_decl });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Jinja validation - valid parameter reference" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a function with a prompt that uses a valid parameter
    var func = ast.FunctionDecl.init(allocator, "Greet", .{ .line = 1, .column = 1 });

    // Add parameter
    const string_type = try allocator.create(ast.TypeExpr);
    string_type.* = ast.TypeExpr{ .primitive = "string" };

    const param = ast.Parameter{
        .name = "name",
        .type_expr = string_type,
        .location = .{ .line = 1, .column = 15 },
    };
    try func.parameters.append(allocator, param);

    // Set return type
    const return_type = try allocator.create(ast.TypeExpr);
    return_type.* = ast.TypeExpr{ .primitive = "string" };
    func.return_type = return_type;

    // Set prompt with valid variable reference
    func.prompt = "Hello {{ name }}!";

    try tree.declarations.append(allocator, ast.Declaration{ .function_decl = func });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Jinja validation - undefined variable" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a function with a prompt that uses an undefined variable
    var func = ast.FunctionDecl.init(allocator, "Greet", .{ .line = 1, .column = 1 });

    // Add parameter
    const string_type = try allocator.create(ast.TypeExpr);
    string_type.* = ast.TypeExpr{ .primitive = "string" };

    const param = ast.Parameter{
        .name = "name",
        .type_expr = string_type,
        .location = .{ .line = 1, .column = 15 },
    };
    try func.parameters.append(allocator, param);

    // Set return type
    const return_type = try allocator.create(ast.TypeExpr);
    return_type.* = ast.TypeExpr{ .primitive = "string" };
    func.return_type = return_type;

    // Set prompt with INVALID variable reference (age is not a parameter)
    func.prompt = "Person age: {{ age }}";

    try tree.declarations.append(allocator, ast.Declaration{ .function_decl = func });

    try validator.validate(&tree);

    // Should have an error about undefined variable
    try std.testing.expect(validator.hasErrors());
    const diagnostics = validator.getDiagnostics();
    try std.testing.expect(diagnostics.len > 0);

    // Check that the error message mentions "Undefined variable"
    var found_error = false;
    for (diagnostics) |diag| {
        if (std.mem.indexOf(u8, diag.message, "Undefined variable") != null) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "Validator: Jinja validation - BAML built-ins are valid" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a function with a prompt that uses BAML built-ins
    var func = ast.FunctionDecl.init(allocator, "Extract", .{ .line = 1, .column = 1 });

    // Add parameter
    const string_type = try allocator.create(ast.TypeExpr);
    string_type.* = ast.TypeExpr{ .primitive = "string" };

    const param = ast.Parameter{
        .name = "text",
        .type_expr = string_type,
        .location = .{ .line = 1, .column = 15 },
    };
    try func.parameters.append(allocator, param);

    // Set return type
    const return_type = try allocator.create(ast.TypeExpr);
    return_type.* = ast.TypeExpr{ .primitive = "string" };
    func.return_type = return_type;

    // Set prompt with BAML built-ins (ctx and _)
    func.prompt =
        \\{{ _.role("user") }}
        \\Extract from: {{ text }}
        \\{{ ctx.output_format }}
    ;

    try tree.declarations.append(allocator, ast.Declaration{ .function_decl = func });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: RetryPolicyRegistry" {
    const allocator = std.testing.allocator;
    var registry = RetryPolicyRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerRetryPolicy("MyRetryPolicy", .{ .line = 1, .column = 1 });
    try std.testing.expect(registry.isDefined("MyRetryPolicy"));
    try std.testing.expect(!registry.isDefined("OtherPolicy"));
}

test "Validator: Detect duplicate retry_policy" {
    const allocator = std.testing.allocator;
    var registry = RetryPolicyRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerRetryPolicy("MyRetryPolicy", .{ .line = 1, .column = 1 });
    const result = registry.registerRetryPolicy("MyRetryPolicy", .{ .line = 10, .column = 1 });
    try std.testing.expectError(ValidationError.DuplicateDefinition, result);
}

test "Validator: Valid retry_policy reference in client" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a retry_policy declaration
    const retry_policy = ast.RetryPolicyDecl.init(allocator, "MyRetryPolicy", 3, .{ .line = 1, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .retry_policy_decl = retry_policy });

    // Create a client that references the retry_policy
    var client = ast.ClientDecl.init(allocator, "MyClient", "llm", .{ .line = 5, .column = 1 });
    client.provider = "openai";
    client.retry_policy = "MyRetryPolicy";
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Undefined retry_policy in client" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a client that references an undefined retry_policy
    var client = ast.ClientDecl.init(allocator, "MyClient", "llm", .{ .line = 1, .column = 1 });
    client.provider = "openai";
    client.retry_policy = "UndefinedPolicy";
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());

    // Check that the error message mentions the undefined retry_policy
    const diagnostics = validator.getDiagnostics();
    try std.testing.expect(diagnostics.len > 0);

    var found_error = false;
    for (diagnostics) |diag| {
        if (std.mem.indexOf(u8, diag.message, "Undefined retry_policy") != null) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "Validator: ClientRegistry" {
    const allocator = std.testing.allocator;
    var registry = ClientRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerClient("MyClient", .{ .line = 1, .column = 1 });
    try std.testing.expect(registry.isDefined("MyClient"));
    try std.testing.expect(!registry.isDefined("OtherClient"));
}

test "Validator: Detect duplicate client" {
    const allocator = std.testing.allocator;
    var registry = ClientRegistry.init(allocator);
    defer registry.deinit();

    try registry.registerClient("MyClient", .{ .line = 1, .column = 1 });
    const result = registry.registerClient("MyClient", .{ .line = 10, .column = 1 });
    try std.testing.expectError(ValidationError.DuplicateDefinition, result);
}

test "Validator: Valid fallback client with strategy list" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create ClientA
    const client_a = ast.ClientDecl.init(allocator, "ClientA", "llm", .{ .line = 1, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client_a });

    // Create ClientB
    const client_b = ast.ClientDecl.init(allocator, "ClientB", "llm", .{ .line = 5, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client_b });

    // Create fallback client with valid strategy list
    var fallback_client = ast.ClientDecl.init(allocator, "FallbackClient", "llm", .{ .line = 10, .column = 1 });
    fallback_client.provider = "fallback";

    // Add strategy array to options
    var strategy_array = std.ArrayList(ast.Value){};
    try strategy_array.append(allocator, ast.Value{ .string = "ClientA" });
    try strategy_array.append(allocator, ast.Value{ .string = "ClientB" });
    try fallback_client.options.put("strategy", ast.Value{ .array = strategy_array });

    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = fallback_client });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Undefined client in fallback strategy list" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create ClientA
    const client_a = ast.ClientDecl.init(allocator, "ClientA", "llm", .{ .line = 1, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client_a });

    // Create fallback client with INVALID strategy list (ClientB doesn't exist)
    var fallback_client = ast.ClientDecl.init(allocator, "FallbackClient", "llm", .{ .line = 5, .column = 1 });
    fallback_client.provider = "fallback";

    // Add strategy array with undefined client
    var strategy_array = std.ArrayList(ast.Value){};
    try strategy_array.append(allocator, ast.Value{ .string = "ClientA" });
    try strategy_array.append(allocator, ast.Value{ .string = "UndefinedClient" });
    try fallback_client.options.put("strategy", ast.Value{ .array = strategy_array });

    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = fallback_client });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());

    // Check error message mentions undefined client
    const diagnostics = validator.getDiagnostics();
    try std.testing.expect(diagnostics.len > 0);

    var found_error = false;
    for (diagnostics) |diag| {
        if (std.mem.indexOf(u8, diag.message, "Undefined client in strategy list") != null) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "Validator: Valid round_robin client with strategy list" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create ClientA
    const client_a = ast.ClientDecl.init(allocator, "ClientA", "llm", .{ .line = 1, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client_a });

    // Create ClientB
    const client_b = ast.ClientDecl.init(allocator, "ClientB", "llm", .{ .line = 5, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client_b });

    // Create round_robin client with valid strategy list
    var rr_client = ast.ClientDecl.init(allocator, "RoundRobinClient", "llm", .{ .line = 10, .column = 1 });
    rr_client.provider = "round_robin";

    // Add strategy array to options
    var strategy_array = std.ArrayList(ast.Value){};
    try strategy_array.append(allocator, ast.Value{ .string = "ClientA" });
    try strategy_array.append(allocator, ast.Value{ .string = "ClientB" });
    try rr_client.options.put("strategy", ast.Value{ .array = strategy_array });

    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = rr_client });

    try validator.validate(&tree);
    try std.testing.expect(!validator.hasErrors());
}

test "Validator: Undefined client in round_robin strategy list" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create ClientA
    const client_a = ast.ClientDecl.init(allocator, "ClientA", "llm", .{ .line = 1, .column = 1 });
    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = client_a });

    // Create round_robin client with INVALID strategy list
    var rr_client = ast.ClientDecl.init(allocator, "RoundRobinClient", "llm", .{ .line = 5, .column = 1 });
    rr_client.provider = "round_robin";

    // Add strategy array with undefined client
    var strategy_array = std.ArrayList(ast.Value){};
    try strategy_array.append(allocator, ast.Value{ .string = "ClientA" });
    try strategy_array.append(allocator, ast.Value{ .string = "NonExistentClient" });
    try rr_client.options.put("strategy", ast.Value{ .array = strategy_array });

    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = rr_client });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());

    // Check error message
    const diagnostics = validator.getDiagnostics();
    try std.testing.expect(diagnostics.len > 0);

    var found_error = false;
    for (diagnostics) |diag| {
        if (std.mem.indexOf(u8, diag.message, "Undefined client in strategy list") != null) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "Validator: Strategy list with non-string values" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create fallback client with INVALID strategy list (contains int instead of string)
    var fallback_client = ast.ClientDecl.init(allocator, "FallbackClient", "llm", .{ .line = 1, .column = 1 });
    fallback_client.provider = "fallback";

    // Add strategy array with invalid type
    var strategy_array = std.ArrayList(ast.Value){};
    try strategy_array.append(allocator, ast.Value{ .int = 123 });
    try fallback_client.options.put("strategy", ast.Value{ .array = strategy_array });

    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = fallback_client });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());

    // Check error message
    const diagnostics = validator.getDiagnostics();
    try std.testing.expect(diagnostics.len > 0);

    var found_error = false;
    for (diagnostics) |diag| {
        if (std.mem.indexOf(u8, diag.message, "Strategy list must contain client names") != null) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}

test "Validator: Strategy field is not an array" {
    const allocator = std.testing.allocator;
    var validator = Validator.init(allocator);
    defer validator.deinit();

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create fallback client with INVALID strategy (string instead of array)
    var fallback_client = ast.ClientDecl.init(allocator, "FallbackClient", "llm", .{ .line = 1, .column = 1 });
    fallback_client.provider = "fallback";

    // Add strategy as string (invalid)
    try fallback_client.options.put("strategy", ast.Value{ .string = "ClientA" });

    try tree.declarations.append(allocator, ast.Declaration{ .client_decl = fallback_client });

    try validator.validate(&tree);
    try std.testing.expect(validator.hasErrors());

    // Check error message
    const diagnostics = validator.getDiagnostics();
    try std.testing.expect(diagnostics.len > 0);

    var found_error = false;
    for (diagnostics) |diag| {
        if (std.mem.indexOf(u8, diag.message, "Strategy field must be an array") != null) {
            found_error = true;
            break;
        }
    }
    try std.testing.expect(found_error);
}
