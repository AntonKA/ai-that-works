# minibaml

A high-performance BAML (Boundary ML) language implementation written in Zig, featuring a complete lexer, parser, type system, and code generators for 12+ programming languages.

## Overview

minibaml is a from-scratch implementation of the BAML language specification, designed for building LLM-powered applications with type-safe structured extraction. It provides a complete toolchain for parsing, validating, formatting, and generating code from BAML schemas.

### Key Features

- 🚀 **Complete BAML Implementation**: Full support for classes, enums, functions, clients, tests, and generators
- 🔍 **Advanced Type System**: Primitives, arrays, maps, optionals, unions, and literal types with circular dependency detection
- 🎯 **Multi-Language Code Generation**: Generate idiomatic code for 12+ languages
- 🌳 **Multi-File Projects**: Automatic namespace merging for complex projects
- 🧪 **Jinja Template Validation**: Parse and validate Jinja templates with loop and conditional support
- ⚡ **Fast & Reliable**: Built with Zig for maximum performance and safety
- 📝 **Pretty Formatter**: Format BAML code with consistent style
- 🔒 **Type-Safe**: Comprehensive validation with detailed error messages

### Supported Languages

Code generation for the following languages is fully implemented and tested:

| Language | Flag | Status |
|----------|------|--------|
| Python (Pydantic) | `--python` | ✅ Default |
| TypeScript | `--typescript`, `-ts` | ✅ |
| Go | `--go` | ✅ |
| Ruby | `--ruby` | ✅ |
| Rust (serde) | `--rust` | ✅ |
| Elixir | `--elixir` | ✅ |
| Java | `--java` | ✅ |
| C# | `--csharp`, `-cs` | ✅ |
| Swift (Codable) | `--swift` | ✅ |
| Kotlin | `--kotlin`, `-kt` | ✅ |
| PHP 8.1+ | `--php` | ✅ |
| Scala (circe) | `--scala` | ✅ |

## Installation

### Prerequisites

- Zig 0.15.1 or later

### Building from Source

```bash
# Clone the repository
git clone <repository-url>
cd minibaml

# Build the project
zig build

# Run tests
zig build test

# Install (optional)
zig build -Doptimize=ReleaseFast
```

The compiled binary will be available at `zig-out/bin/minibaml`.

## Quick Start

### 1. Create a BAML File

Create a file named `example.baml`:

```baml
// Define a class
class Person {
  name string
  age int?
  email string @alias("email_address")
  tags string[]
}

// Define an enum
enum Status {
  Active
  Inactive
  Pending
}

// Define an LLM function
function ExtractPerson(text: string) -> Person {
  client "openai/gpt-4"
  prompt #"
    Extract person information from: {{ text }}

    {{ ctx.output_format }}
  "#
}
```

### 2. Validate Your BAML

```bash
minibaml check example.baml
```

### 3. Generate Code

Generate Python code:
```bash
minibaml gen example.baml > models.py
```

Generate TypeScript code:
```bash
minibaml gen example.baml --typescript > models.ts
```

Generate Go code:
```bash
minibaml gen example.baml --go > models.go
```

### 4. Format Your Code

```bash
minibaml fmt example.baml
```

## Usage

### Commands

```
minibaml <file.baml>              Tokenize a BAML file
minibaml parse <path>             Parse and show AST (file or directory)
minibaml check <path>             Validate BAML file or directory
minibaml fmt <file.baml>          Format a BAML file
minibaml generate <path> [opts]   Generate code from BAML
minibaml gen <path> [opts]        Alias for generate
```

### Code Generation Options

```
--python                          Generate Python code (default)
--typescript, -ts                 Generate TypeScript code
--go                              Generate Go code
--ruby                            Generate Ruby code
--rust                            Generate Rust code
--elixir                          Generate Elixir code
--java                            Generate Java code
--csharp, -cs                     Generate C# code
--swift                           Generate Swift code
--kotlin, -kt                     Generate Kotlin code
--php                             Generate PHP code
--scala                           Generate Scala code
--typebuilder, -tb                Generate Python TypeBuilder module
```

## Examples

### Basic Types

```baml
class User {
  id string
  name string
  age int
  active bool
  score float
  metadata map<string, string>
  tags string[]
  profile Profile?
}
```

### Enums

```baml
enum Priority {
  Low
  Medium
  High
  Critical
}
```

### Functions with Jinja Templates

```baml
function Greet(person: Person) -> string {
  client "openai/gpt-4"
  prompt #"
    {{ _.role("user") }}
    Say hello to {{ person.name }}
    {% if person.age %}
      They are {{ person.age }} years old.
    {% endif %}
  "#
}
```

### Multi-File Projects

Organize your BAML files in a directory:

```
baml_src/
├── models/
│   ├── person.baml
│   └── status.baml
├── functions.baml
└── clients.baml
```

Process the entire directory:

```bash
minibaml check baml_src
minibaml gen baml_src --typescript > generated.ts
```

### TypeBuilder for Dynamic Types

For classes or enums marked with `@@dynamic`:

```baml
class User {
  name string

  @@dynamic
}
```

Generate a TypeBuilder module:

```bash
minibaml gen example.baml --typebuilder > type_builder.py
```

## Language-Specific Output

### Python (Pydantic)

```python
from typing import Optional, List, Dict
from pydantic import BaseModel, Field

class Person(BaseModel):
    name: str
    age: Optional[int]
    email: str = Field(alias="email_address")
    tags: List[str]
```

### TypeScript

```typescript
export interface Person {
  name: string;
  age: number | undefined;
  email_address: string;
  tags: string[];
}

export enum Status {
  Active = "Active",
  Inactive = "Inactive"
}
```

### Go

```go
type Person struct {
    Name string `json:"name"`
    Age *int `json:"age"`
    Email string `json:"email_address"`
    Tags []string `json:"tags"`
}

type Status string

const (
    StatusActive Status = "Active"
    StatusInactive Status = "Inactive"
)
```

### Rust

```rust
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
```

## Features

### Type System

- **Primitives**: `string`, `int`, `float`, `bool`, `null`
- **Media Types**: `image`, `audio`, `video`, `pdf`
- **Collections**: Arrays (`Type[]`), Maps (`map<K, V>`)
- **Modifiers**: Optional (`Type?`), Union (`Type1 | Type2`)
- **Literal Types**: String, integer, and boolean literals

### Attributes

- `@alias("name")` - Rename fields in generated code
- `@description("text")` - Add documentation
- `@skip` - Exclude from processing
- `@assert(condition)` - Runtime assertions
- `@check(condition)` - Validation checks
- `@@dynamic` - Mark types as runtime-modifiable
- `@@alias("name")` - Class/enum level aliases

### Validation

minibaml performs comprehensive validation:

- ✅ Type checking and inference
- ✅ Circular dependency detection
- ✅ Duplicate definition checking
- ✅ Cross-file type reference validation
- ✅ Jinja template variable validation
- ✅ Attribute usage validation
- ✅ Loop and conditional statement validation

### Error Messages

Clear, actionable error messages with line and column information:

```
Error at line 12, column 5: Undefined type 'InvalidType'
Error at line 24, column 10: Circular dependency detected: Person -> Address -> Person
Error at line 18, column 3: Undefined variable 'invalid' in template
```

## Documentation

For detailed documentation, see:

- [Getting Started Guide](docs/getting-started.md) - Comprehensive tutorial
- [Reference Documentation](docs/reference.md) - Complete API reference
- [Building from Source](docs/BUILDING.md) - Development guide
- [Implementation Plan](IMPLEMENTATION_PLAN.md) - Development roadmap

## Project Status

**Current Version**: 0.1.0

**Completed Phases**:
- ✅ Phase 0-10: Core language implementation (lexer, parser, AST, validator, formatter)
- ✅ Phase 11: Multi-file project support
- ✅ Phase 12: TypeBuilder and Jinja validation
- ✅ Phase 13: Complete documentation suite
- ✅ Phase 14: Advanced Jinja features (loops, conditionals)
- ✅ Phase 15-24: Code generators for 12+ languages

All tests passing with comprehensive coverage.

## Development

### Running Tests

```bash
# Run all tests
zig build test

# Run with detailed output
zig build test --summary all
```

### Project Structure

```
minibaml/
├── src/
│   ├── lexer.zig          # Tokenizer (2,217 lines)
│   ├── ast.zig            # AST definitions (489 lines)
│   ├── parser.zig         # Parser (847 lines)
│   ├── validator.zig      # Type checker (1,297 lines)
│   ├── jinja.zig          # Jinja template validator (1,412 lines)
│   ├── formatter.zig      # Pretty printer (685 lines)
│   ├── codegen.zig        # Code generators (4,000+ lines)
│   ├── multifile.zig      # Multi-file support (165 lines)
│   ├── main.zig           # CLI (272 lines)
│   └── root.zig           # Module exports
├── docs/
│   ├── getting-started.md
│   ├── reference.md
│   └── BUILDING.md
├── test_baml_src/         # Test fixtures
├── build.zig
└── IMPLEMENTATION_PLAN.md
```

## Contributing

Contributions are welcome! Please ensure:

1. All tests pass: `zig build test`
2. Code follows Zig conventions
3. New features include tests
4. Documentation is updated

## License

See LICENSE file for details.

## Acknowledgments

minibaml implements the BAML language specification from [Boundary ML](https://www.boundaryml.com/). This is an independent implementation written in Zig for educational and research purposes.

## See Also

- [BAML Official Documentation](https://docs.boundaryml.com/)
- [Zig Programming Language](https://ziglang.org/)

---

**Built with ❤️ in Zig**
