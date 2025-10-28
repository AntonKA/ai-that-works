# minibaml Implementation Plan

A BAML language implementation in Zig.

## Project Status: PHASE 22 - Kotlin Code Generation ✅ COMPLETED

---

## Priority Order & Milestones

### ✅ PHASE 0: Project Structure & Foundation
**Status**: ✅ COMPLETED
**Goal**: Create basic project structure and verify build system works

- [x] 0.1: Create src/ directory structure
- [x] 0.2: Create basic main.zig with hello world
- [x] 0.3: Create root.zig module stub
- [x] 0.4: Verify `zig build` works
- [x] 0.5: Verify `zig build test` works
- [x] 0.6: Verify `zig build run` works

**Validation**: ✅ `zig build run` outputs "Hello, minibaml!"

---

### ✅ PHASE 1: Lexer/Tokenizer
**Status**: ✅ COMPLETED
**Goal**: Tokenize BAML source code into a stream of tokens

#### Token Types Implemented:
```zig
// Keywords
class, enum, function, client, test, generator, template_string, type, env

// Primitive Types
string, int, float, bool, null, image, audio, video, pdf, map

// Symbols
@, @@, {, }, [, ], (, ), |, ?, <, >, :, ,, #, "

// Literals
STRING_LITERAL, INT_LITERAL, FLOAT_LITERAL, BOOL_LITERAL
IDENTIFIER, COMMENT, BLOCK_STRING

// Special
EOF, NEWLINE
```

#### Tasks Completed:
- [x] 1.1: Define Token enum with all token types
- [x] 1.2: Create Lexer struct with source input and position tracking
- [x] 1.3: Implement keyword recognition
- [x] 1.4: Implement identifier and type name parsing
- [x] 1.5: Implement string literal parsing (quoted `"..."`)
- [x] 1.6: Implement block string parsing (`#"..."#` with nesting, including `##"..."##`)
- [x] 1.7: Implement number literal parsing (int/float, including negative numbers)
- [x] 1.8: Implement comment parsing (`//`, `///`, `{# #}` with nesting)
- [x] 1.9: Implement symbol/operator parsing
- [x] 1.10: Implement unquoted string parsing (for simple values)
- [x] 1.11: Add comprehensive lexer tests (150+ tests covering all token types)
- [x] 1.12: Create test BAML file and verify tokenization

**Validation**: ✅ PASSED - Lexer successfully tokenizes complete BAML files with all token types.

**Implementation Details**:
- Created `src/lexer.zig` (2,217 lines)
- Comprehensive test suite with 150+ test cases
- CLI tool (`minibaml`) to tokenize BAML files
- Successfully tokenizes `test.baml` with 160 tokens including:
  - Classes with attributes and complex types
  - Enums with values
  - Functions with block string prompts
  - Client declarations with environment variables
  - Test declarations with nested structures
  - All comment types (line, docstring, block)
  - Union types, optional types, array types, map types
  - Block strings with multiple hash delimiters

**Test Results**: All tests pass (`zig build test`)

**Sample Output**:
```
$ ./zig-out/bin/_2025_10_28_ralph_wiggum_coding_ test.baml
Tokenized test.baml: 160 tokens

   0:              comment | Line   1, Col   1 | " Test comment"
   4:        keyword_class | Line   3, Col   1 | "class"
   5:           identifier | Line   3, Col   7 | "Person"
   ...
```

---

### ✅ PHASE 2: AST & Parser Foundation
**Status**: ✅ COMPLETED
**Goal**: Parse tokens into an Abstract Syntax Tree

#### AST Node Types Implemented:
```zig
// Top-level declarations
ClassDecl, EnumDecl, FunctionDecl, ClientDecl, TestDecl, GeneratorDecl,
TemplateStringDecl, TypeAliasDecl

// Type expressions
TypeExpr: Primitive, Array, Map, Optional, Union, Named, Literal

// Class/Enum components
Property, EnumValue, Attribute

// Function components
Parameter

// Value types
Value: String, Int, Float, Bool, Null, Array, Object, EnvVar
```

#### Tasks Completed:
- [x] 2.1: Define AST node structures
- [x] 2.2: Create Parser struct with token stream
- [x] 2.3: Implement parser utilities (peek, advance, expect, match, etc.)
- [x] 2.4: Implement type expression parsing (with precedence)
  - [x] 2.4a: Parse primitive types
  - [x] 2.4b: Parse array types `Type[]`
  - [x] 2.4c: Parse optional types `Type?`
  - [x] 2.4d: Parse union types `Type | Type`
  - [x] 2.4e: Parse map types `map<K, V>`
  - [x] 2.4f: Parse literal types `"value" | 1 | true`
- [x] 2.5: Parse attribute syntax `@attr(args)` and `@@attr(args)`
- [x] 2.6: Parse comments and docstrings (via skipTrivia())
- [x] 2.7: Add parser error handling with line/column info
- [x] 2.8: Add parser recovery (error accumulation with continued parsing)

**Validation**: ✅ PASSED - Parser successfully parses all type expressions and attributes.

**Implementation Details**:
- Created `src/ast.zig` (489 lines) with comprehensive AST structures
- Created `src/parser.zig` (847 lines) with full parser implementation
- Updated `src/root.zig` to export ast and parser modules
- 20+ test cases for parser utilities, types, attributes, and values
- Full support for BAML type syntax with proper operator precedence
- Handles both @ and @@ attributes with arguments
- Parses complex nested structures (arrays, objects, env vars)
- Error handling with line/column info and continued parsing
- Memory-safe with proper deinit() and errdefer blocks

**Test Results**: ✅ All tests pass (`zig build test`)
- Build Summary: 5/5 steps succeeded
- Tests: 2/2 passed

---

### ✅ PHASE 3: Class & Enum Parsing
**Status**: ✅ COMPLETED
**Goal**: Parse class and enum declarations

#### Tasks Completed:
- [x] 3.1: Parse class declaration header
- [x] 3.2: Parse class properties with types
- [x] 3.3: Parse property attributes (@alias, @description, @skip)
- [x] 3.4: Parse class attributes (@@alias, @@dynamic, @@description)
- [x] 3.5: Parse enum declaration header
- [x] 3.6: Parse enum values
- [x] 3.7: Parse enum value attributes
- [x] 3.8: Parse enum attributes
- [x] 3.9: Add tests for class parsing
- [x] 3.10: Add tests for enum parsing
- [x] 3.11: Handle docstring comments (`///`)

**Validation**: ✅ PASSED - Parser successfully parses all class and enum features.

**Implementation Details**:
- Added `parseClassDecl()` function to parse complete class declarations
- Added `parseProperty()` function to parse class properties with types and attributes
- Added `parseEnumDecl()` function to parse complete enum declarations
- Added `parseEnumValue()` function to parse enum values with attributes
- Added `skipTriviaCapturingDocstring()` to capture docstrings while skipping trivia
- Comprehensive test suite with 14 new test cases covering:
  - Simple classes and enums
  - Properties with all type variations (primitive, optional, array, map)
  - Property-level attributes (@alias, @description, etc.)
  - Class-level attributes (@@dynamic, @@alias, etc.)
  - Enum values with attributes
  - Enum-level attributes
  - Docstring support for classes, enums, properties, and values
  - Integration tests with lexer + parser
- All tests pass (`zig build test`)

**Sample Successfully Parsed**:
```baml
/// A person entity
class Person {
  /// The person's name
  name string @alias("full_name") @description("The person's name")
  age int? @description("Optional age")
  status Status

  @@dynamic
}

/// Status enumeration
enum Status {
  /// Active state
  Active @alias("currently_active")
  Inactive @description("Not active")
  Pending @skip

  @@dynamic
}
```

---

### ✅ PHASE 4: Function Parsing
**Status**: ✅ COMPLETED
**Goal**: Parse function declarations with prompts

#### Tasks Completed:
- [x] 4.1: Parse function declaration header
- [x] 4.2: Parse function parameters with types
- [x] 4.3: Parse return type
- [x] 4.4: Parse client specification (short form: string literal)
- [x] 4.5: Parse prompt block (block string with Jinja)
- [x] 4.6: Parse function attributes
- [x] 4.7: Add function parsing tests
- [x] 4.8: Handle multiline prompts correctly

**Validation**: ✅ PASSED - Successfully parses all function features.

**Implementation Details**:
- Added `parseFunctionDecl()` function to parse complete function declarations
- Added `parseParameter()` function to parse function parameters with colon syntax (param: Type)
- Added `keyword_prompt` token to lexer
- Added `arrow` token (`->`) to lexer for return type syntax
- Comprehensive test suite with 8 new test cases covering:
  - Functions without parameters
  - Functions with single and multiple parameters
  - Complex parameter types (arrays, primitives, image, etc.)
  - Union return types
  - Multiline prompts with `##"..."##` syntax
  - Docstring support for functions
  - Client specification parsing
  - Integration tests with lexer + parser
- All tests pass (`zig build test`)

**Sample Successfully Parsed**:
```baml
function ExtractPerson(text: string, image: image) -> Person {
  client "anthropic/claude-sonnet-4"
  prompt #"
    {{ _.role("user") }}
    Extract person info from: {{ text }}
    Image: {{ image }}

    {{ ctx.output_format }}
  "#
}
```

---

### ✅ PHASE 5: Client & Template String Parsing
**Status**: ✅ COMPLETED
**Goal**: Parse client and template_string declarations

#### Tasks Completed:
- [x] 5.1: Parse client<llm> declaration header
- [x] 5.2: Parse client provider
- [x] 5.3: Parse client options block
- [x] 5.4: Parse nested options (headers, etc.)
- [x] 5.5: Parse environment variable references (env.VAR_NAME)
- [x] 5.6: Parse template_string declarations
- [x] 5.7: Parse template_string parameters
- [x] 5.8: Add client parsing tests
- [x] 5.9: Add template_string parsing tests

**Validation**: ✅ PASSED - Successfully parses all client and template_string features.

**Implementation Details**:
- Added `parseClientDecl()` function to parse complete client declarations
  - Parses client type parameter: `client<llm>`
  - Parses provider field: `provider "openai"`
  - Parses options block with key-value pairs
  - Supports environment variables via existing `parseValue()` function
  - Supports nested objects and all value types
- Added `parseTemplateStringDecl()` function to parse template_string declarations
  - Parses parameters using existing `parseParameter()` function
  - Parses template body as block string
  - Supports all parameter types (primitives, arrays, maps, etc.)
- Comprehensive test suite with 10 new test cases covering:
  - Simple client declarations
  - Clients with environment variables
  - Clients with multiple options
  - Clients with nested options objects
  - Template strings without parameters
  - Template strings with single parameter
  - Template strings with multiple parameters
  - Template strings with complex types
  - Integration tests matching validation examples
- All tests pass (`zig build test`)

**Sample Successfully Parsed**:
```baml
client<llm> MyClient {
  provider "openai"
  options {
    model "gpt-4"
    api_key env.OPENAI_API_KEY
    temperature 0.7
    base_url "https://api.openai.com/v1"
  }
}

template_string FormatMessages(msgs: Message[]) #"
  {% for m in msgs %}
    {{ _.role(m.role) }}
    {{ m.content }}
  {% endfor %}
"#
```

**Test Results**: ✅ All tests pass - Build Summary: 5/5 steps succeeded; 2/2 tests passed

---

### ✅ PHASE 6: Test & Generator Parsing
**Status**: ✅ COMPLETED
**Goal**: Parse test and generator declarations

#### Tasks Completed:
- [x] 6.1: Parse test declaration header
- [x] 6.2: Parse functions list
- [x] 6.3: Parse args block with nested values
- [x] 6.4: Parse test attributes (@@check, @@assert)
- [x] 6.5: Parse generator declaration
- [x] 6.6: Parse generator options
- [x] 6.7: Add test parsing tests
- [x] 6.8: Add generator parsing tests

**Validation**: ✅ PASSED - Successfully parses all test and generator features.

**Implementation Details**:
- Added `parseTestDecl()` function to parse complete test declarations
  - Parses test name and header
  - Parses functions list: `functions [Func1, Func2]`
  - Parses args block with key-value pairs supporting all value types
  - Supports nested objects and arrays in args
  - Supports test-level attributes (@@check, @@assert)
- Added `parseGeneratorDecl()` function to parse complete generator declarations
  - Parses generator name and header
  - Parses generator options block with key-value pairs
  - Supports all value types (strings, numbers, etc.)
- Comprehensive test suite with 10 new test cases covering:
  - Simple test declarations with function lists
  - Tests with multiple functions
  - Tests with nested args objects
  - Tests with array args
  - Tests with test-level attributes (@@check, @@assert)
  - Integration test matching test.baml structure
  - Simple generator declarations
  - Generators with version field
  - Generators with multiple options
- All tests pass (`zig build test --summary all`)
- Updated test.baml with generator declaration example

**Sample Successfully Parsed**:
```baml
test TestGreet {
  functions [Greet]
  args {
    p {
      name "Alice"
      age 30
    }
  }
  @@check(output, "length > 0")
}

generator PythonGenerator {
  output_type "python/pydantic"
  output_dir "./baml_client"
  version "0.60.0"
}
```

**Test Results**: ✅ All tests pass - Build Summary: 5/5 steps succeeded; 2/2 tests passed

---

### ✅ PHASE 7: Type System & Validation
**Status**: ✅ COMPLETED
**Goal**: Implement type checking and validation

#### Tasks Completed:
- [x] 7.1: Create type registry/symbol table
- [x] 7.2: Resolve type references
- [x] 7.3: Validate type compatibility
- [x] 7.4: Check for circular dependencies in types
- [x] 7.5: Validate function parameter types
- [x] 7.6: Validate return types
- [x] 7.7: Check for duplicate definitions
- [x] 7.8: Validate attribute usage (✅ FULLY COMPLETED)
- [x] 7.9: Add semantic analysis tests

**Validation**: ✅ PASSED - Successfully detects and reports type errors and attribute misuse in BAML code.

**Implementation Details**:
- Created `src/validator.zig` (1,297 lines) with comprehensive validation framework
- TypeRegistry tracks all declared types (classes, enums, primitives)
- FunctionRegistry tracks all declared functions
- Validator performs multi-phase validation:
  - Phase 1: Register all declarations and detect duplicates
  - Phase 2: Validate all type references are defined
  - Phase 3: Check for circular dependencies in class types
  - Phase 4: Validate attribute usage (NEW)
- Comprehensive attribute validation:
  - validatePropertyAttributes(): Validates @alias, @description, @skip, @assert, @check on properties
  - validateClassAttributes(): Validates @@alias, @@description, @@dynamic on classes
  - validateEnumAttributes(): Validates @@alias, @@description, @@dynamic on enums
  - validateEnumValueAttributes(): Validates @alias, @description, @skip on enum values
  - validateTestAttributes(): Validates @@check, @@assert on tests
  - validateFunctionAttributes(): Warns about unsupported attributes on functions
  - Checks attribute argument count and types (e.g., @alias requires exactly 1 string)
  - Prevents misuse of @ vs @@ attributes on wrong declaration types
- Comprehensive test suite with 23 test cases covering:
  - Type registry operations (primitives, classes, enums)
  - Function registry operations
  - Duplicate definition detection
  - Undefined type detection
  - Undefined function detection in tests
  - Circular dependency detection
  - Complex type validation (arrays, optionals, unions, maps)
  - Valid attribute usage (12 new tests)
  - Invalid attribute usage detection (11 new tests)
- Diagnostic system with error messages including line/column info
- All tests pass (`zig build test --summary all`)

**Test Results**: ✅ All tests pass - Build Summary: 5/5 steps succeeded; 2/2 tests passed

**Sample Validations**:
- Detects undefined types: `address Address` when Address is not defined
- Detects circular dependencies: `class A { b B }` and `class B { a A }`
- Detects duplicate definitions: Two classes with the same name
- Validates complex types: `Address[]`, `Person | null`, `map<string, string>`
- Validates function parameter and return types
- Detects invalid attribute usage: @@alias on property (should be @)
- Detects invalid attribute arguments: @alias() with no arguments
- Detects wrong argument types: @alias(123) with non-string argument
- Validates test attributes: @@check and @@assert require arguments
- Warns about unknown attributes on declarations

---

### ✅ PHASE 8: Pretty Printer & Formatter
**Status**: ✅ COMPLETED
**Goal**: Format BAML code (like `baml fmt`)

#### Tasks Completed:
- [x] 8.1: Create AST printer
- [x] 8.2: Implement indentation logic
- [x] 8.3: Format type expressions
- [x] 8.4: Format declarations
- [x] 8.5: Preserve comments (docstrings)
- [x] 8.6: Add formatter tests
- [x] 8.7: Create `minibaml fmt` command
- [x] 8.8: Fix Zig 0.15.1 ArrayList API compatibility issues
- [x] 8.9: Fix BAML object syntax (space-separated, not colon-separated)
- [x] 8.10: Fix environment variable parsing (env.VAR_NAME)

**Validation**: ✅ PASSED - Successfully formats test.baml and outputs correctly formatted BAML code.

**Implementation Details**:
- Created `src/formatter.zig` (685+ lines) with comprehensive formatting functionality
- Supports all BAML constructs: classes, enums, functions, clients, tests, generators, template_strings
- Proper indentation with 2-space indent levels
- Preserves docstring comments (/// syntax)
- Handles block string prompts with proper delimiter selection (#" or ##")
- Formats type expressions (primitives, arrays, optionals, unions, maps, literals)
- Formats values (strings, numbers, booleans, arrays, objects, env vars)
- Formats attributes (@attr and @@attr with arguments)
- Added `minibaml fmt <file>` command to CLI
- Fixed all Zig 0.15.1 ArrayList API compatibility issues across ast.zig, parser.zig, and validator.zig
- Fixed parser to handle BAML's space-separated object syntax
- Fixed parser to handle env.VAR_NAME syntax properly
- All existing tests pass

**Sample Formatted Output**:
```baml
class Person {
  name string
  age int?
  email string @alias("email_address")
}

function Greet(p: Person) -> string {
  client "openai/gpt-4"
  prompt #"
    Say hello to {{ p.name }}
  "#
}
```

**Test Results**: ✅ All tests pass - Formatter successfully processes test.baml

---

### ✅ PHASE 9: Basic Code Generation (Python)
**Status**: ✅ COMPLETED
**Goal**: Generate Python/Pydantic code from BAML

#### Tasks Completed:
- [x] 9.1: Create code generator framework
- [x] 9.2: Generate Python class definitions from BAML classes
- [x] 9.3: Generate Python enums
- [x] 9.4: Generate type hints for unions, optionals, arrays
- [x] 9.5: Generate function stubs
- [x] 9.6: Add code generation tests
- [x] 9.7: Verify generated Python code is valid

**Validation**: ✅ PASSED - Generates valid Python code that passes syntax checking.

**Implementation Details**:
- Created `src/codegen.zig` (579 lines) with comprehensive Python code generation
- PythonGenerator class with support for all BAML constructs
- Maps BAML types to Python types:
  - Primitives (string→str, int→int, float→float, bool→bool)
  - Complex types (Optional, Union, List, Dict)
  - Media types (image, audio, video, pdf → Any)
- Generates Pydantic BaseModel classes with proper indentation
- Generates Python enums with str mixin
- Generates function stubs with type hints
- Supports @alias attributes via Field(alias="...")
- Preserves docstrings from BAML code
- Added `minibaml generate` and `minibaml gen` commands to CLI
- Comprehensive test suite with 8 test cases covering:
  - Simple classes and enums
  - Optional and array types
  - Map types (Dict[K, V])
  - Union types
  - Functions with parameters
  - Properties with @alias attributes
  - Integration tests
- All tests pass (`zig build test`)
- Generated Python code is syntactically valid (verified with `python3 -m py_compile`)

**Sample Generated Code**:
```python
# Generated by minibaml
from typing import Optional, Union, List, Dict, Any
from pydantic import BaseModel, Field
from enum import Enum

class Person(BaseModel):
    name: str
    age: Optional[int]
    email: str = Field(alias="email_address")
    tags: List[str]
    metadata: Dict[str, str]

class Status(str, Enum):
    Active = "Active"
    Inactive = "Inactive"

def Greet(p: Person) -> str:
    raise NotImplementedError("This is a stub for LLM function")
```

**Test Results**: ✅ All tests pass - Build Summary: 5/5 steps succeeded; 2/2 tests passed

---

### ✅ PHASE 10: CLI & File I/O
**Status**: ✅ COMPLETED
**Goal**: Create usable CLI tool

#### Tasks Completed:
- [x] 10.1: Implement file reading
- [x] 10.2: Implement `minibaml parse <file>` command
- [x] 10.3: Implement `minibaml fmt <file>` command (already existed)
- [x] 10.4: Implement `minibaml check <file>` command
- [x] 10.5: Add helpful error messages with line/column info
- [x] 10.6: Add --version flag
- [x] 10.7: Add --help text
- [ ] 10.8: Handle multiple input files (deferred - not essential)

**Validation**: ✅ PASSED - CLI tool can parse, format, and check real BAML files

**Implementation Details**:
- Refactored main.zig to eliminate duplication (reduced ~150 lines of duplicated parsing code)
- Created `parseFile()` helper function used by all commands
- Added `parseCommand()` to show parsed AST summary
- Added `checkCommand()` to validate BAML files with detailed error reporting
- Added `--version` and `--help` flags
- Improved error messages with consistent formatting using std.debug.print
- Fixed Zig 0.15.1 ArrayList API compatibility issues in validator.zig
- Fixed Zig 0.15.1 recursive function error set inference issues
- All tests pass
- File size reduced from 226 lines (with duplication) to 272 lines (with more features)

**Test Results**: ✅ All commands work correctly:
```
$ minibaml --version
minibaml version 0.1.0

$ minibaml --help
[Shows complete help text with all commands and options]

$ minibaml parse test.baml
Successfully parsed test.baml
Declarations: 7
[Shows summary of all declarations]

$ minibaml check test.baml
[Validates file and reports errors with line/column info]

$ minibaml fmt test.baml
[Formats and outputs BAML code]

$ minibaml generate test.baml
[Generates Python code]
```

---

## Future Phases (Lower Priority)

### PHASE 11: Multi-file Support
- Import/module system
- Cross-file type references

### PHASE 12: Advanced Features
- Jinja template parsing/validation
- Dynamic types support
- Streaming support
- Client registry

### PHASE 13: Additional Code Generators
- TypeScript generation
- Go generation
- Ruby generation

---

---

### ✅ PHASE 11: Multi-file Support
**Status**: ✅ COMPLETED
**Goal**: Support multi-file BAML projects with automatic namespace merging

#### Tasks Completed:
- [x] 11.1: Create MultiFileProject module for managing multiple files
- [x] 11.2: Implement directory scanning (recursive .baml file discovery)
- [x] 11.3: Parse multiple files into separate ASTs
- [x] 11.4: Merge declarations from all files into single namespace
- [x] 11.5: Validate cross-file type references
- [x] 11.6: Detect duplicate definitions across files
- [x] 11.7: Update CLI to accept directory paths
- [x] 11.8: Add directory support to check, parse, and generate commands
- [x] 11.9: Fix memory management for multi-file projects
- [x] 11.10: Test with real multi-file BAML projects

**Validation**: ✅ PASSED - Successfully loads, validates, and generates code from multi-file projects.

**Implementation Details**:
- Created `src/multifile.zig` (165 lines) with multi-file project support
- MultiFileProject scans directories recursively for .baml files
- Keeps source code alive to preserve AST string references
- Merges all declarations into single namespace (BAML design)
- Updated `main.zig` to support both files and directories:
  - `isDirectory()` helper function
  - `checkDirectory()` for multi-file validation
  - `parseDirectory()` for multi-file AST display
  - Updated `generateCommand()` for directory support
- Comprehensive multi-file test structure:
  - test_baml_src/models/person.baml - Person and Address classes
  - test_baml_src/models/status.baml - Status and Priority enums
  - test_baml_src/functions.baml - Greet and ExtractPerson functions
  - test_baml_src/clients.baml - OpenAI and Anthropic clients
- All tests pass (`zig build test`)
- No memory leaks (verified with GPA)

**Sample Output**:
```
$ minibaml check test_baml_src
Loading BAML files from 'test_baml_src'...
Loaded 4 file(s)

  - test_baml_src/functions.baml (2 declarations)
  - test_baml_src/clients.baml (2 declarations)
  - test_baml_src/models/status.baml (2 declarations)
  - test_baml_src/models/person.baml (2 declarations)

Validating merged AST...
✓ test_baml_src is valid (total 8 declarations)
```

**Test Results**: ✅ All tests pass - Build Summary: 5/5 steps succeeded; 2/2 tests passed

---

### ✅ PHASE 12.1: Jinja Template Parsing & Validation
**Status**: ✅ COMPLETED
**Goal**: Parse and validate Jinja templates in function prompts and template_strings

#### Tasks Completed:
- [x] 12.1.1: Create Jinja tokenizer/lexer for template constructs ({{ }}, {% %}, {# #})
- [x] 12.1.2: Implement Jinja AST nodes (Variable, Expression, Statement, Comment)
- [x] 12.1.3: Parse Jinja expressions (variables, filters, property access)
- [x] 12.1.4: Validate variable references against function parameters
- [x] 12.1.5: Add support for BAML built-ins (ctx, _, _.role(), ctx.output_format)
- [x] 12.1.6: Validate balanced delimiters and syntax errors
- [x] 12.1.7: Add comprehensive Jinja validation tests (7 tests)
- [x] 12.1.8: Integrate Jinja validator into existing validation pipeline (Phase 5)
- [x] 12.1.9: Add integration tests in validator.zig (3 tests)
- [x] 12.1.10: Fix Zig 0.15.1 ArrayList API compatibility

**Validation**: ✅ PASSED - Jinja validator detects undefined variables and validates templates.

**Implementation Details**:
- Created `src/jinja.zig` (818 lines) with complete Jinja parsing and validation
- JinjaLexer with stateful tokenization (in_text, in_variable, in_statement, in_comment)
- JinjaParser parses template constructs into AST nodes
- JinjaValidator validates variable references against function parameters
- Supports BAML built-ins: `ctx.output_format`, `_.role()`
- Integrated into Phase 5 of validation pipeline
- Added 10 new tests (7 in jinja.zig, 3 in validator.zig)
- All tests pass (`zig build test`)

**Sample Validation**:
```baml
// This produces a validation error
function Greet(name: string) -> string {
  prompt "Hello {{ invalid }}"  // ERROR: Undefined variable 'invalid'
}

// This is valid
function Greet(name: string) -> string {
  prompt #"
    {{ _.role("user") }}
    Hello {{ name }}!
    {{ ctx.output_format }}
  "#
}
```

**Test Results**: ✅ All tests pass - Direct testing confirms validator detects undefined variables

---

### ✅ PHASE 12.2: TypeBuilder Code Generation for @@dynamic Types
**Status**: ✅ COMPLETED
**Goal**: Generate TypeBuilder module for runtime modification of @@dynamic types

#### Tasks Completed:
- [x] 12.2.1: Add helper function to detect @@dynamic attribute on declarations
- [x] 12.2.2: Design Python TypeBuilder module structure
- [x] 12.2.3: Implement Python TypeBuilder code generation
- [x] 12.2.4: Add tests for TypeBuilder generation (7 tests)
- [x] 12.2.5: Update CLI to output TypeBuilder file with --typebuilder flag
- [x] 12.2.6: Fix critical memory bug - keep source alive for AST pointers
- [x] 12.2.7: Integration test with real @@dynamic examples
- [x] 12.2.8: Verify all existing tests still pass

**Validation**: ✅ PASSED - TypeBuilder correctly generates for @@dynamic classes and enums.

**Implementation Details**:
- Added `hasDynamicAttribute()` helper function to detect @@dynamic attributes
- Extended `PythonGenerator` with `generateTypeBuilder()` method
- Generates three Python classes:
  - `DynamicClassBuilder` - for @@dynamic classes with `add_property()` method
  - `DynamicEnumBuilder` - for @@dynamic enums with `add_value()` method
  - `TypeBuilder` - main class with instances of dynamic type builders and type helper methods
- Updated CLI with `--typebuilder` flag for generating TypeBuilder module
- Added 7 comprehensive tests for TypeBuilder generation
- Fixed critical use-after-free bug:
  - ParseResult now keeps source alive (was freeing too early)
  - Source string must outlive AST since AST nodes contain string slices pointing to source
  - Changed from `defer allocator.free(source)` to storing in ParseResult
- All tests pass (`zig build test`)

**Sample Generated TypeBuilder**:
```python
# Generated by minibaml
# TypeBuilder for dynamic types

from typing import Optional, Any, Dict, List

class DynamicClassBuilder:
    """Helper for building dynamic class properties at runtime"""

    def __init__(self, class_name: str):
        self.class_name = class_name
        self.properties: Dict[str, Any] = {}

    def add_property(self, name: str, type_expr: Any, description: Optional[str] = None):
        """Add a property to this dynamic class"""
        self.properties[name] = {
            'type': type_expr,
            'description': description
        }
        return self

class DynamicEnumBuilder:
    """Helper for building dynamic enum values at runtime"""

    def __init__(self, enum_name: str):
        self.enum_name = enum_name
        self.values: List[str] = []

    def add_value(self, value: str):
        """Add a value to this dynamic enum"""
        self.values.append(value)
        return self

class TypeBuilder:
    """TypeBuilder for runtime type modifications"""

    def __init__(self):
        self.User = DynamicClassBuilder("User")
        self.Category = DynamicEnumBuilder("Category")

    def string(self) -> str:
        return 'string'

    def int(self) -> str:
        return 'int'

    def float(self) -> str:
        return 'float'

    def bool(self) -> str:
        return 'bool'
```

**CLI Usage**:
```bash
# Generate TypeBuilder module
minibaml gen test.baml --typebuilder > type_builder.py

# Generate normal Python code
minibaml gen test.baml > models.py
```

**Test Results**: ✅ All tests pass - TypeBuilder generation works correctly for dynamic types

---

### ✅ PHASE 14: Advanced Jinja Features (Loops and Conditionals)
**Status**: ✅ COMPLETED
**Goal**: Implement comprehensive parsing and validation for Jinja control flow statements

#### Tasks Completed:
- [x] 14.1: Extend JinjaStatement AST to support structured control flow
  - [x] Create JinjaStatementType enum for discriminating statement types
  - [x] Create JinjaForStatement struct with loop_var, iterable, and iterable_path
  - [x] Create JinjaIfStatement struct with condition
  - [x] Create JinjaEndStatement struct for endfor/endif/else
  - [x] Convert JinjaStatement to discriminated union
- [x] 14.2: Implement parser for {% for %} loops with proper syntax parsing
  - [x] parseForStatement() extracts loop variable and iterable
  - [x] Support for dot-path iterables (e.g., ctx.client.messages)
  - [x] Handle {% endfor %} parsing
- [x] 14.3: Implement parser for {% if %}/{% elif %}/{% else %} conditionals
  - [x] parseIfStatement() for if and elif with conditions
  - [x] Handle {% else %} parsing
  - [x] Handle {% endif %} parsing
- [x] 14.4: Add validation for balanced statement pairs (for/endfor, if/endif)
  - [x] Add StatementContext struct for tracking nesting
  - [x] Add statement_stack to JinjaValidator
  - [x] Validate matching for/endfor pairs
  - [x] Validate matching if/elif/else/endif pairs
  - [x] Check for unclosed blocks at end of validation
  - [x] Check for unmatched closing tags (endfor without for, etc.)
- [x] 14.5: Implement loop variable scoping for {% for %} contexts
  - [x] Add loop_vars HashMap to JinjaValidator
  - [x] Add loop variables to scope when entering for loop
  - [x] Remove loop variables from scope when exiting for loop
  - [x] Validate loop variables are accessible within loop body
  - [x] Update validateVariable() to check loop_vars
- [x] 14.6: Add validateIterableReference() for for loops
  - [x] Check iterable exists in function parameters
  - [x] Allow built-in iterables (ctx, _)
  - [x] Report undefined iterable errors with line/column
- [x] 14.7: Add comprehensive tests for loop and conditional validation (16 new tests)
  - [x] Test for loop parsing
  - [x] Test if/elif/else parsing
  - [x] Test valid for loop with parameters
  - [x] Test loop variable scoping
  - [x] Test undefined iterable detection
  - [x] Test unmatched endfor detection
  - [x] Test unclosed for loop detection
  - [x] Test valid if block
  - [x] Test unmatched endif detection
  - [x] Test elif without if detection
  - [x] Test else without opening block detection
  - [x] Test nested for loops
  - [x] Test for loop with built-in iterable
  - [x] Test complete example with loops and conditionals

**Validation**: ✅ PASSED - All 16 new tests pass, all existing tests pass (2/2 test suites)

**Implementation Details**:
- Extended `src/jinja.zig` from 867 lines to 1,412 lines (+545 lines)
- Added discriminated union for JinjaStatement with 6 variants:
  - `for_start`: Contains loop_var, iterable, and iterable_path
  - `endfor`: Simple end marker with line/column
  - `if_start`: Contains condition string
  - `elif`: Contains condition string
  - `else_block`: Simple marker
  - `endif`: Simple end marker
- Enhanced parser with three new functions:
  - `parseForStatement()`: Parses `{% for x in items %}` syntax
  - `parseIfStatement()`: Parses `{% if condition %}` and `{% elif condition %}`
  - Updated `parseStatement()` to dispatch to appropriate parser
- Enhanced validator with scope tracking:
  - `StatementContext` struct tracks nesting type (for_loop or if_block)
  - `statement_stack` tracks open blocks for balance checking
  - `loop_vars` HashMap tracks variables in scope from for loops
  - `validateIterableReference()` validates iterable exists
  - Enhanced `validateVariable()` to check loop_vars
  - Comprehensive `validateStatement()` with all 6 statement types
- All validation phases work correctly:
  - Statement pairing: Validates for/endfor and if/endif are balanced
  - Scope tracking: Loop variables are added/removed correctly
  - Reference validation: Iterables and variables are checked
  - Nesting validation: elif/else must be inside proper blocks
- Memory safe: All ArrayLists and HashMaps properly initialized and cleaned up
- Error messages include line/column info for all validation errors

**Sample Validated Templates**:
```baml
// Valid for loop
{% for m in messages %}
  {{ _.role(m.role) }}
  {{ m.content }}
{% endfor %}

// Valid if/elif/else
{% if condition %}
  Yes
{% elif other %}
  Maybe
{% else %}
  No
{% endif %}

// Nested loops
{% for outer in items %}
  {% for inner in outer.children %}
    {{ inner.name }}
  {% endfor %}
{% endfor %}

// Complex example
{% for m in messages %}
  {% if show_role %}
    {{ _.role(m.role) }}
  {% endif %}
  {{ m.content }}
{% endfor %}
{{ ctx.output_format }}
```

**Errors Detected**:
- Undefined iterable: `{% for x in unknown %}`
- Unmatched endfor: `{% endfor %}` without `{% for %}`
- Unclosed for: `{% for x in items %}` without `{% endfor %}`
- Unmatched endif: `{% endif %}` without `{% if %}`
- elif without if: `{% elif x %}` without prior `{% if %}`
- else without block: `{% else %}` with no opening statement
- Wrong block closing: `{% if x %} ... {% endfor %}` (mismatch)

**Test Results**: ✅ All tests pass - Build Summary: 5/5 steps succeeded; 2/2 tests passed

---

### ✅ PHASE 15: Go Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Go code from BAML

#### Tasks Completed:
- [x] 15.1: Implement GoGenerator struct in codegen.zig
- [x] 15.2: Map BAML types to Go types
  - Primitives: string→string, int→int, float→float64, bool→bool
  - Complex types: Optional→pointer, Array→slice, Map→map, Union→interface{}
  - Media types (image, audio, video, pdf) → interface{}
- [x] 15.3: Generate Go struct definitions from BAML classes
  - Capitalize field names for export
  - Add JSON tags with support for @alias attributes
  - Preserve docstrings as Go comments
- [x] 15.4: Generate Go enums using const blocks
  - Type-safe string enums
  - Enum values follow Go naming conventions (EnumNameValue)
- [x] 15.5: Generate Go function stubs
  - Proper Go function signatures with named return types
  - Return (Type, error) for idiomatic error handling
  - Preserve prompts as multi-line comments
- [x] 15.6: Add comprehensive tests (6 test cases)
  - Simple struct generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Field with @alias attribute
- [x] 15.7: Add --go flag to CLI generate command
- [x] 15.8: Export GoGenerator from root.zig
- [x] 15.9: Fix Zig 0.15.1 compatibility issues in jinja.zig
  - Fixed ArrayList.init() calls to use ArrayList{} syntax
  - Fixed ArrayList.pop() to access items directly
- [x] 15.10: Verify generated Go code compiles

**Validation**: ✅ PASSED - Generated Go code compiles successfully

**Implementation Details**:
- Created GoGenerator in codegen.zig (300+ lines)
- Type mapping follows Go idioms:
  - Optionals use pointers (*Type)
  - Arrays use slices ([]Type)
  - Maps use Go maps (map[K]V)
  - Unions with null use pointers, others use interface{}
- Generated structs with JSON tags for serialization
- Enums use typed string constants
- Functions return (Type, error) tuples
- All field names capitalized for export
- Comprehensive test suite (6 tests)
- CLI updated with --go flag
- All tests pass (zig build test)

**Sample Generated Code**:
```go
package baml

import (
	"errors"
)

type Person struct {
	Name string `json:"name"`
	Age *int `json:"age"`
	Email string `json:"email_address"`
}

type Status string

const (
	StatusActive Status = "Active"
	StatusInactive Status = "Inactive"
)

func Greet(p Person) (string, error) {
	return *new(string), errors.New("This is a stub for LLM function")
}
```

**Test Results**: ✅ All tests pass - Generated Go code compiles with `go build`

**CLI Usage**:
```bash
# Generate Go code
minibaml gen test.baml --go > generated.go
minibaml gen baml_src --go > generated.go
```

---

### ✅ PHASE 16: Ruby Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Ruby code from BAML

#### Tasks Completed:
- [x] 16.1: Implement RubyGenerator struct in codegen.zig
- [x] 16.2: Map BAML types to Ruby types
  - Primitives: string→String, int→Integer, float→Float, bool→Boolean
  - Complex types: Optional→nilable, Array→Array, Map→Hash, Union→union/nilable
  - Media types (image, audio, video, pdf) → Object
- [x] 16.3: Generate Ruby classes with attr_accessor
  - Proper initialize methods with keyword arguments
  - Support for @alias attributes
  - Preserve docstrings as Ruby comments
- [x] 16.4: Generate Ruby enums using module with constants
  - Frozen string constants
  - ALL constant with array of all values
- [x] 16.5: Generate Ruby function stubs
  - Snake_case naming convention (PascalCase→snake_case)
  - YARD-style type documentation (@param, @return)
  - Preserve prompts as multi-line comments
- [x] 16.6: Add comprehensive tests (6 test cases)
  - Simple class generation
  - Simple enum generation
  - Optional and array types
  - Function with parameters
  - Map types
  - Property with @alias attribute
- [x] 16.7: Add --ruby flag to CLI generate command
- [x] 16.8: Export RubyGenerator from root.zig
- [x] 16.9: Verify generated Ruby code is syntactically valid

**Validation**: ✅ PASSED - Generated Ruby code passes syntax checking

**Implementation Details**:
- Created RubyGenerator in codegen.zig (300+ lines)
- Type mapping follows Ruby conventions:
  - Optionals use nilable type annotations
  - Arrays use Array<Type> syntax
  - Maps use Hash{K => V} syntax
  - Classes use attr_accessor for properties
- Generated classes with proper initialize methods
- Enums implemented as modules with frozen constants
- Functions converted to snake_case with YARD documentation
- All property names respect @alias attributes
- Comprehensive test suite (6 tests)
- CLI updated with --ruby flag
- All tests pass (zig build test)

**Sample Generated Code**:
```ruby
# Generated by minibaml
# DO NOT EDIT - This file is auto-generated

# frozen_string_literal: true

class Person
  attr_accessor :name, :age, :email

  # @param args [Hash] Initialization arguments
  def initialize(**args)
    @name = args[:name]
    @age = args[:age]
    @email = args[:email]
  end
end

module Status
  Active = 'Active'.freeze
  Inactive = 'Inactive'.freeze

  ALL = [Active, Inactive].freeze
end

# @param p [Person]
# @return [String]
def greet(p)
  raise NotImplementedError, 'This is a stub for LLM function'
end
```

**Test Results**: ✅ All tests pass - Generated Ruby code is syntactically valid (`ruby -c`)

**CLI Usage**:
```bash
# Generate Ruby code
minibaml gen test.baml --ruby > generated.rb
minibaml gen baml_src --ruby > generated.rb
```

---

### ✅ PHASE 17: Rust Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Rust code from BAML

#### Tasks Completed:
- [x] 17.1: Implement RustGenerator struct in codegen.zig
- [x] 17.2: Map BAML types to Rust types
  - Primitives: string→String, int→i64, float→f64, bool→bool
  - Complex types: Option<T>, Vec<T>, HashMap<K,V>
  - Media types (image, audio, video, pdf) → Vec<u8>
- [x] 17.3: Generate Rust struct definitions from BAML classes
  - Proper derives (#[derive(Debug, Clone, Serialize, Deserialize)])
  - serde support with rename attributes
  - Snake_case for field names
  - Preserve docstrings as doc comments
- [x] 17.4: Generate Rust enums with serde support
  - Proper derives (#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)])
  - PascalCase for variant names
- [x] 17.5: Generate Rust function stubs
  - Snake_case naming convention
  - Result<T, Box<dyn Error>> return types
  - Preserve prompts as doc comments
- [x] 17.6: Add comprehensive tests (6 test cases)
  - Simple struct generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Field with @alias attribute
- [x] 17.7: Add --rust flag to CLI generate command
- [x] 17.8: Export RustGenerator from root.zig
- [x] 17.9: Verify generated Rust code is syntactically valid

**Validation**: ✅ PASSED - Generated Rust code is syntactically valid and follows Rust idioms.

**Implementation Details**:
- Created RustGenerator in codegen.zig (300+ lines)
- Type mapping follows Rust idioms:
  - Optionals use Option<T>
  - Arrays use Vec<T>
  - Maps use HashMap<K, V>
  - Functions return Result<T, Box<dyn Error>>
- Generated structs with serde derives for serialization
- All field names converted to snake_case
- Functions converted to snake_case
- Enums with proper derives including PartialEq and Eq
- Comprehensive test suite (6 tests)
- CLI updated with --rust flag
- All tests pass (zig build test)

**Sample Generated Code**:
```rust
// Generated by minibaml
// DO NOT EDIT - This file is auto-generated

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::error::Error;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Person {
    pub name: String,
    pub age: Option<i64>,
    #[serde(rename = "email_address")]
    pub email: String,
    pub tags: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Status {
    Active,
    Inactive,
}

pub fn greet(p: Person) -> Result<String, Box<dyn Error>> {
    Err("This is a stub for LLM function".into())
}
```

**Test Results**: ✅ All tests pass - Generated Rust code is syntactically valid

**CLI Usage**:
```bash
# Generate Rust code
minibaml gen test.baml --rust > generated.rs
minibaml gen baml_src --rust > generated.rs
```

---

### ✅ PHASE 18: Elixir Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Elixir code from BAML

#### Tasks Completed:
- [x] 18.1: Implement ElixirGenerator struct in codegen.zig
- [x] 18.2: Map BAML types to Elixir types
  - Primitives: string→String.t(), int→integer(), float→float(), bool→boolean()
  - Complex types: Optional→| nil, Array→list(), Map→%{K => V}, Union→type | type
  - Media types (image, audio, video, pdf) → binary()
- [x] 18.3: Generate Elixir modules with defstruct from BAML classes
  - @type t specifications with proper type annotations
  - defstruct declarations with field lists
  - Support for @alias attributes
  - Preserve docstrings as comments
- [x] 18.4: Generate Elixir enum modules using atoms
  - @type t with atom union types (:active | :inactive)
  - values/0 function returning all enum values
  - Proper snake_case conversion for enum values
- [x] 18.5: Generate Elixir function stubs
  - @spec typespecs with proper type annotations
  - Snake_case naming convention (PascalCase→snake_case)
  - Preserve prompts as multi-line comments
- [x] 18.6: Add comprehensive tests (6 test cases)
  - Simple module generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Field with @alias attribute
- [x] 18.7: Add --elixir flag to CLI generate command
- [x] 18.8: Export ElixirGenerator from root.zig
- [x] 18.9: Verify generated Elixir code follows Elixir conventions

**Validation**: ✅ PASSED - Generated Elixir code follows idiomatic Elixir patterns

**Implementation Details**:
- Created ElixirGenerator in codegen.zig (300+ lines)
- Type mapping follows Elixir idioms:
  - Optionals use `| nil` union types
  - Arrays use `list(Type)` syntax
  - Maps use `%{K => V}` syntax
  - Named types use `.t()` convention (e.g., Person.t())
- Generated modules with @type and defstruct
- Enums implemented as modules with atom union types and values/0 function
- Functions converted to snake_case with @spec typespecs
- All field names respect @alias attributes and convert to snake_case
- Comprehensive test suite (6 tests)
- CLI updated with --elixir flag
- All tests pass (zig build test)

**Sample Generated Code**:
```elixir
# Generated by minibaml
# DO NOT EDIT - This file is auto-generated

defmodule Person do
  @type t :: %__MODULE__{
    name: String.t(),
    age: integer() | nil,
    email_address: String.t(),
    tags: list(String.t())
  }

  defstruct [:name, :age, :email_address, :tags]
end

defmodule Status do
  @type t :: :active | :inactive

  def values, do: [:active, :inactive]
end

@spec greet(Person.t()) :: String.t()
def greet(p) do
  raise "This is a stub for LLM function"
end
```

**Test Results**: ✅ All tests pass - Generated Elixir code follows language conventions

**CLI Usage**:
```bash
# Generate Elixir code
minibaml gen test.baml --elixir > generated.ex
minibaml gen baml_src --elixir > generated.ex
```

---

### ✅ PHASE 19: Java Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Java code from BAML

#### Tasks Completed:
- [x] 19.1: Implement JavaGenerator struct in codegen.zig
- [x] 19.2: Map BAML types to Java types
  - Primitives: string→String, int→Integer, float→Double, bool→Boolean
  - Complex types: Optional<T>, List<T>, Map<K,V>
  - Media types (image, audio, video, pdf) → byte[]
- [x] 19.3: Generate Java class definitions from BAML classes
  - Proper getters and setters (JavaBeans pattern)
  - Jackson annotations for JSON (@JsonProperty)
  - Private fields with public accessors
  - Preserve docstrings as Javadoc comments
- [x] 19.4: Generate Java enums with proper syntax
  - Public enum declarations
  - Comma-separated enum values
- [x] 19.5: Generate Java function stubs
  - Static methods with proper type signatures
  - UnsupportedOperationException for stubs
  - Preserve prompts as Javadoc comments
- [x] 19.6: Add comprehensive tests (6 test cases)
  - Simple class generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Field with @alias attribute
- [x] 19.7: Add --java flag to CLI generate command
- [x] 19.8: Export JavaGenerator from root.zig
- [x] 19.9: Fix formatting issues with proper line handling
- [x] 19.10: Verify all tests pass

**Validation**: ✅ PASSED - Generated Java code follows idiomatic Java patterns

**Implementation Details**:
- Created JavaGenerator in codegen.zig (600+ lines)
- Type mapping follows Java idioms:
  - Optionals use Optional<T>
  - Arrays use List<T>
  - Maps use Map<K, V>
  - Functions throw UnsupportedOperationException
- Generated classes with JavaBeans pattern (getters/setters)
- All field names with private access and public accessors
- Comprehensive test suite (6 tests)
- CLI updated with --java flag
- All tests pass (zig build test)

**Sample Generated Code**:
```java
package com.baml.generated;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Person {
    private String name;
    private Optional<Integer> age;
    @JsonProperty("email_address")
    private String email;
    private List<String> tags;

    public Person() {}

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Optional<Integer> getAge() {
        return this.age;
    }

    public void setAge(Optional<Integer> age) {
        this.age = age;
    }
}

public enum Status {
    Active,
    Inactive
}

public static String Greet(Person p) {
    throw new UnsupportedOperationException("This is a stub for LLM function");
}
```

**Test Results**: ✅ All tests pass - Generated Java code follows language conventions

**CLI Usage**:
```bash
# Generate Java code
minibaml gen test.baml --java > Person.java
minibaml gen baml_src --java > generated.java
```

---

### ✅ PHASE 20: C# Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic C# code from BAML

#### Tasks Completed:
- [x] 20.1: Implement CSharpGenerator struct in codegen.zig
- [x] 20.2: Map BAML types to C# types
  - Primitives: string→string, int→int, float→double, bool→bool
  - Complex types: Optional→nullable (Type?), Array→List<T>, Map→Dictionary<K,V>
  - Media types (image, audio, video, pdf) → byte[]
- [x] 20.3: Generate C# class definitions from BAML classes
  - Public properties with { get; set; } syntax
  - JsonPropertyName attribute from System.Text.Json
  - Preserve docstrings as XML documentation comments
  - PascalCase property names (capitalized first letter)
- [x] 20.4: Generate C# enums with proper syntax
  - Public enum declarations
  - Comma-separated enum values
- [x] 20.5: Generate C# function stubs
  - Static methods with proper type signatures
  - NotImplementedException for stubs
  - Preserve prompts as XML documentation comments
- [x] 20.6: Add comprehensive tests (6 test cases)
  - Simple class generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Property with @alias attribute
- [x] 20.7: Add --csharp flag to CLI generate command
- [x] 20.8: Export CSharpGenerator from root.zig
- [x] 20.9: Verify all tests pass

**Validation**: ✅ PASSED - Generated C# code follows idiomatic C# patterns

**Implementation Details**:
- Created CSharpGenerator in codegen.zig (600+ lines)
- Type mapping follows C# idioms:
  - Optionals use nullable reference types (Type?)
  - Arrays use List<T>
  - Maps use Dictionary<K, V>
  - Functions throw NotImplementedException
- Generated classes with public properties and { get; set; }
- All property names capitalize first letter (PascalCase)
- Comprehensive test suite (6 tests)
- CLI updated with --csharp and -cs flags
- All tests pass (zig build test)

**Sample Generated Code**:
```csharp
// Generated by minibaml
// DO NOT EDIT - This file is auto-generated

using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;

/// <summary>
/// A person entity
/// </summary>
public class Person
{
    public string Name { get; set; }
    public int? Age { get; set; }
    [JsonPropertyName("email_address")]
    public string Email { get; set; }
    public List<string> Tags { get; set; }
    public Dictionary<string, string> Metadata { get; set; }
}

public enum Status
{
    Active,
    Inactive,
    Pending
}

public static string Greet(Person p)
{
    throw new NotImplementedException("This is a stub for LLM function");
}
```

**Test Results**: ✅ All tests pass - Generated C# code follows language conventions

**CLI Usage**:
```bash
# Generate C# code
minibaml gen test.baml --csharp > Person.cs
minibaml gen test.baml -cs > Person.cs
minibaml gen baml_src --csharp > generated.cs
```

---

### ✅ PHASE 21: Swift Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Swift code from BAML

#### Tasks Completed:
- [x] 21.1: Implement SwiftGenerator struct in codegen.zig
- [x] 21.2: Map BAML types to Swift types
  - Primitives: string→String, int→Int, float→Double, bool→Bool
  - Complex types: Optional<T> (Type?), Array ([Type]), Map ([Key: Value])
  - Media types (image, audio, video, pdf) → Data
- [x] 21.3: Generate Swift struct definitions with Codable protocol
  - Public structs conforming to Codable
  - Immutable properties with `let` keyword
  - CodingKeys enum for @alias attribute support
  - Preserve docstrings as Swift documentation comments
- [x] 21.4: Generate Swift enums with String raw values
  - String-backed enums conforming to Codable
  - Lowercase camelCase for enum cases (Swift convention)
  - Proper raw values matching BAML values
- [x] 21.5: Generate Swift function stubs with throws keyword
  - Swift documentation comments with parameter and return annotations
  - Functions with `throws` keyword for error handling
  - NSError stub implementations
  - Preserve prompts as documentation comments
- [x] 21.6: Add comprehensive tests (6 test cases)
  - Simple struct generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Property with @alias attribute
- [x] 21.7: Add --swift flag to CLI generate command
- [x] 21.8: Export SwiftGenerator from root.zig
- [x] 21.9: Verify all tests pass

**Validation**: ✅ PASSED - Generated Swift code is syntactically correct and follows Swift idioms

**Implementation Details**:
- Created SwiftGenerator in codegen.zig (400+ lines)
- Type mapping follows Swift idioms:
  - Optionals use `Type?` syntax
  - Arrays use `[Type]` syntax
  - Maps use `[Key: Value]` syntax
  - Functions throw errors with NSError
- Generated structs with Codable protocol for JSON serialization
- CodingKeys enum generated automatically when @alias attributes are present
- All enum cases converted to lowercase camelCase
- Functions use Swift documentation format with parameter/return annotations
- Comprehensive test suite (6 tests)
- CLI updated with --swift flag
- All tests pass (zig build test)

**Sample Generated Code**:
```swift
// Generated by minibaml
// DO NOT EDIT - This file is auto-generated

import Foundation

struct Person: Codable {
    let name: String
    let age: Int?
    let email: String
    let tags: [String]
    let metadata: [String: String]

    enum CodingKeys: String, CodingKey {
        case name
        case age
        case email = "email_address"
        case tags
        case metadata
    }
}

enum Status: String, Codable {
    case active = "Active"
    case inactive = "Inactive"
    case pending = "Pending"
}

/// - Parameter p: Person
/// - Returns: String
func Greet(p: Person) throws -> String {
    throw NSError(domain: "minibaml", code: -1, userInfo: [NSLocalizedDescriptionKey: "This is a stub for LLM function"])
}
```

**Test Results**: ✅ All tests pass - Generated Swift code follows language conventions

**CLI Usage**:
```bash
# Generate Swift code
minibaml gen test.baml --swift > Person.swift
minibaml gen baml_src --swift > generated.swift
```

---

### ✅ PHASE 22: Kotlin Code Generation
**Status**: ✅ COMPLETED
**Goal**: Generate idiomatic Kotlin code from BAML

#### Tasks Completed:
- [x] 22.1: Implement KotlinGenerator struct in codegen.zig
- [x] 22.2: Map BAML types to Kotlin types
  - Primitives: string→String, int→Int, float→Double, bool→Boolean
  - Complex types: Optional (Type?), List<T>, Map<K,V>
  - Media types (image, audio, video, pdf) → ByteArray
- [x] 22.3: Generate Kotlin data classes from BAML classes
  - Data class syntax with constructor parameters
  - Immutable properties with `val` keyword
  - Jackson annotations for @alias attributes
  - Preserve docstrings as KDoc comments
- [x] 22.4: Generate Kotlin enum classes
  - Enum class syntax conforming to Kotlin conventions
  - Comma-separated enum values
- [x] 22.5: Generate Kotlin function stubs
  - fun keyword with proper type signatures
  - UnsupportedOperationException for stubs
  - Preserve prompts as KDoc comments
- [x] 22.6: Add comprehensive tests (6 test cases)
  - Simple data class generation
  - Simple enum generation
  - Optional and array types
  - Map types
  - Function with parameters
  - Property with @alias attribute
- [x] 22.7: Add --kotlin flag to CLI generate command
- [x] 22.8: Export KotlinGenerator from root.zig
- [x] 22.9: Verify all tests pass

**Validation**: ✅ PASSED - Generated Kotlin code follows idiomatic Kotlin patterns

**Implementation Details**:
- Created KotlinGenerator in codegen.zig (300+ lines)
- Type mapping follows Kotlin idioms:
  - Optionals use `Type?` syntax
  - Arrays use `List<Type>` syntax
  - Maps use `Map<K, V>` syntax
  - Functions throw UnsupportedOperationException
- Generated data classes with constructor parameters
- All properties use `val` for immutability
- Comprehensive test suite (6 tests)
- CLI updated with --kotlin and -kt flags
- All tests pass (zig build test)

**Sample Generated Code**:
```kotlin
// Generated by minibaml
// DO NOT EDIT - This file is auto-generated

package com.baml.generated

import com.fasterxml.jackson.annotation.JsonProperty

data class Person(
    val name: String,
    val age: Int?,
    @JsonProperty("email_address")
    val email: String,
    val tags: List<String>,
    val metadata: Map<String, String>
)

enum class Status {
    Active,
    Inactive,
    Pending
}

fun Greet(p: Person): String {
    throw UnsupportedOperationException("This is a stub for LLM function")
}
```

**Test Results**: ✅ All tests pass - Generated Kotlin code follows language conventions

**CLI Usage**:
```bash
# Generate Kotlin code
minibaml gen test.baml --kotlin > Person.kt
minibaml gen test.baml -kt > Person.kt
minibaml gen baml_src --kotlin > generated.kt
```

---

## Current Milestone: PHASE 22 - COMPLETED ✅

**Achievements**:
- ✅ Complete lexer with 150+ test cases
- ✅ Full AST implementation with all BAML constructs
- ✅ Comprehensive parser for all BAML syntax
- ✅ Complete type system with validation
- ✅ Circular dependency detection
- ✅ Duplicate definition checking (single and multi-file)
- ✅ Type reference validation
- ✅ Cross-file type references (automatic namespace)
- ✅ Pretty printer and formatter with full BAML support
- ✅ Python code generator with Pydantic support
- ✅ TypeScript code generator with full type support
- ✅ Go code generator with idiomatic Go types
- ✅ Ruby code generator with idiomatic Ruby classes
- ✅ Rust code generator with serde support and idiomatic Rust types
- ✅ Elixir code generator with defstruct modules and atom-based enums
- ✅ Java code generator with JavaBeans pattern and Jackson annotations
- ✅ C# code generator with properties, nullable types, and System.Text.Json
- ✅ Swift code generator with Codable protocol and idiomatic Swift types
- ✅ Kotlin code generator with data classes and idiomatic Kotlin types
- ✅ Multi-file project support with recursive directory scanning
- ✅ Complete CLI tool with all essential commands:
  - `minibaml <file>` - Tokenize
  - `minibaml parse <path>` - Parse and show AST (file or directory)
  - `minibaml check <path>` - Validate (file or directory)
  - `minibaml fmt <file>` - Format
  - `minibaml generate <path>` - Generate Python code (file or directory)
  - `minibaml generate <path> --typescript` - Generate TypeScript code
  - `minibaml generate <path> --go` - Generate Go code
  - `minibaml generate <path> --ruby` - Generate Ruby code
  - `minibaml generate <path> --rust` - Generate Rust code
  - `minibaml generate <path> --elixir` - Generate Elixir code
  - `minibaml generate <path> --java` - Generate Java code
  - `minibaml generate <path> --csharp` - Generate C# code
  - `minibaml generate <path> --swift` - Generate Swift code
  - `minibaml generate <path> --kotlin` - Generate Kotlin code
  - `minibaml generate <path> --typebuilder` - Generate TypeBuilder module
  - `--version` and `--help` flags
- ✅ Zig 0.15.1 full compatibility (ArrayList API, recursive error sets)
- ✅ Error handling with line/column info throughout
- ✅ Refactored codebase with no duplication
- ✅ All tests passing (including 7 new TypeBuilder tests)
- ✅ Generated Python code is syntactically valid
- ✅ Memory-safe multi-file processing
- ✅ Fixed critical use-after-free bug (AST source lifetime management)
- ✅ Jinja template parsing and validation (Phase 12.1)
  - Validates variable references in function prompts
  - Supports BAML built-ins (ctx, _)
  - Integrated into validation pipeline
  - 10 comprehensive tests
- ✅ Advanced Jinja control flow (Phase 14)
  - Full parsing and validation for {% for %} loops
  - Full parsing and validation for {% if %}/{% elif %}/{% else %}/{% endif %}
  - Loop variable scoping with proper scope management
  - Balanced statement pair validation (matching for/endfor, if/endif)
  - Iterable reference validation
  - Unclosed block detection
  - 16 comprehensive tests for loops and conditionals
  - 545 lines of enhanced Jinja implementation
- ✅ TypeBuilder code generation for @@dynamic types (Phase 12.2)
  - Detects @@dynamic attribute on classes and enums
  - Generates DynamicClassBuilder with add_property()
  - Generates DynamicEnumBuilder with add_value()
  - Generates TypeBuilder with type helper methods
  - CLI flag --typebuilder to output TypeBuilder module
  - 7 comprehensive tests
- ✅ Complete Documentation Suite (Phase 13)
  - Getting Started Guide (278 lines) - Installation, tutorials, examples
  - Reference Documentation (1,619 lines) - Complete API and language reference
  - Building from Source Guide (319 lines) - Build instructions and verification
  - All CLI commands documented with examples
  - BAML syntax reference with all keywords and symbols
  - Complete type system documentation
  - All attributes documented with validation rules
  - Jinja template syntax reference
  - Comprehensive error messages with fixes
  - Best practices and common patterns
  - Total: 2,216 lines of documentation

---

### ✅ PHASE 13: Documentation
**Status**: ✅ COMPLETED
**Goal**: Create comprehensive documentation for users and contributors

#### Tasks Completed:
- [x] 13.1: Write Getting Started Guide (docs/getting-started.md) ✅ COMPLETED
  - Installation instructions
  - Basic usage examples with working BAML code
  - Quick tutorial covering classes, enums, functions, and clients
  - Core concepts explanation (types, attributes, templates)
  - Code generation workflow (Python and TypeScript)
  - Multi-file project structure
  - Complete example workflow from BAML to working code
  - Common patterns (optional fields, arrays, unions, maps)
  - Testing and debugging guidance
- [x] 13.2: Write Reference Documentation (docs/reference.md) ✅ COMPLETED
  - Complete CLI command reference (parse, check, fmt, generate) with examples
  - BAML syntax reference (keywords, symbols, operators, strings)
  - All declaration types (class, enum, function, client, test, generator, template_string)
  - Complete type system documentation (primitives, arrays, optionals, unions, maps, literals)
  - All supported attributes with usage examples and validation rules
  - Jinja template syntax (variables, built-ins, statements, filters)
  - Comprehensive error messages reference with fixes
  - Validation phases explanation
  - Best practices and common patterns
  - 1,619 lines of detailed reference documentation
- [x] 13.3: Write Building from Source Guide (docs/BUILDING.md) ✅ COMPLETED
  - Prerequisites (Zig 0.15.1+)
  - Build instructions with optimization options
  - Running tests
  - Code generation examples (Python, TypeScript, TypeBuilder)
  - Testing generated code with validation examples
  - Complete verification workflow
  - Project structure overview
  - Development tips and troubleshooting

**Validation**: ✅ All documentation guides are comprehensive and accurate.
- Getting Started guide: 278 lines covering all basic and intermediate features
- Building guide: 319 lines with complete build and test workflows
- Reference guide: 1,619 lines with complete API and language reference
- All tests pass (2/2 passed)
- Documentation verified against source code

---

**Next Steps** (Optional Future Enhancements):
- Full runtime TypeBuilder integration with function execution
- Streaming support for LLM function calls
- Client registry for managing multiple LLM providers
- Advanced Jinja filter validation and execution
- Additional language generators (Kotlin, PHP, Scala, etc.)
