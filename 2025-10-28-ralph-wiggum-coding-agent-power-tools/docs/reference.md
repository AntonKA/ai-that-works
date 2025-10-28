# minibaml Reference Documentation

Complete reference for the minibaml BAML language implementation in Zig.

## Table of Contents

- [CLI Command Reference](#cli-command-reference)
- [BAML Language Syntax](#baml-language-syntax)
- [Type System](#type-system)
- [Attributes Reference](#attributes-reference)
- [Jinja Template Syntax](#jinja-template-syntax)
- [Validation & Error Messages](#validation--error-messages)

---

## CLI Command Reference

### Overview

```bash
minibaml <command> [arguments] [options]
```

### Global Options

- `--help`, `-h` - Show help message and exit
- `--version`, `-v` - Show version information and exit

### Commands

#### Tokenize (Default)

```bash
minibaml <file.baml>
```

Tokenizes a BAML file and displays all tokens with their line and column positions.

**Output:**
```
Tokenized test.baml: 160 tokens

   0:              comment | Line   1, Col   1 | " Test comment"
   4:        keyword_class | Line   3, Col   1 | "class"
   5:           identifier | Line   3, Col   7 | "Person"
   ...
```

**Use Cases:**
- Debugging lexer issues
- Understanding how BAML source is tokenized
- Learning BAML syntax

---

#### Parse

```bash
minibaml parse <path>
```

Parses BAML file(s) and displays the Abstract Syntax Tree (AST) structure.

**Arguments:**
- `<path>` - Path to a BAML file or directory

**Single File Example:**
```bash
minibaml parse test.baml
```

**Output:**
```
Successfully parsed test.baml

Declarations: 7

1. class Person (3 properties)
2. enum Status (3 values)
3. function Greet (1 parameters)
4. client<llm> MyClient
5. test TestGreet (1 functions)
6. generator PythonGenerator
7. template_string FormatMessages (1 parameters)
```

**Directory Example:**
```bash
minibaml parse baml_src
```

**Output:**
```
Loading BAML files from 'baml_src'...

Successfully parsed 4 file(s):

  baml_src/functions.baml
    Declarations: 2
      - function Greet
      - function ExtractPerson

  baml_src/clients.baml
    Declarations: 2
      - client<llm> OpenAI
      - client<llm> Anthropic

  baml_src/models/status.baml
    Declarations: 2
      - enum Status
      - enum Priority

  baml_src/models/person.baml
    Declarations: 2
      - class Person
      - class Address

Merged AST: 8 total declarations
```

**Use Cases:**
- Verifying that BAML syntax is correctly parsed
- Understanding AST structure
- Debugging parser issues
- Exploring multi-file project structure

---

#### Check

```bash
minibaml check <path>
```

Validates BAML file(s) for semantic errors including type checking, circular dependencies, and attribute usage.

**Arguments:**
- `<path>` - Path to a BAML file or directory

**Single File Example:**
```bash
minibaml check test.baml
```

**Success Output:**
```
✓ test.baml is valid
```

**Error Output:**
```
Validation errors in 'test.baml':

  [error] Line 12, Col 8: Undefined type: Address
  [error] Line 25, Col 3: Circular dependency detected in type: Person
  [warning] Line 7, Col 10: Unknown property attribute @unknown

Found 2 error(s)
```

**Directory Example:**
```bash
minibaml check baml_src
```

**Output:**
```
Loading BAML files from 'baml_src'...
Loaded 4 file(s)

  - baml_src/functions.baml (2 declarations)
  - baml_src/clients.baml (2 declarations)
  - baml_src/models/status.baml (2 declarations)
  - baml_src/models/person.baml (2 declarations)

Validating merged AST...
✓ baml_src is valid (total 8 declarations)
```

**Exit Codes:**
- `0` - Validation successful, no errors
- `1` - Validation failed with errors

**Validations Performed:**
- Phase 1: Register all declarations, detect duplicates
- Phase 2: Validate type references are defined
- Phase 3: Check for circular dependencies in types
- Phase 4: Validate attribute usage and arguments
- Phase 5: Validate Jinja templates in prompts

**Use Cases:**
- Pre-deployment validation
- CI/CD integration
- Development workflow validation
- Cross-file type reference checking

---

#### Format

```bash
minibaml fmt <file.baml>
```

Formats a BAML file and outputs the formatted code to stdout.

**Arguments:**
- `<file.baml>` - Path to a BAML file

**Example:**
```bash
minibaml fmt test.baml > test_formatted.baml
```

**Input:**
```baml
class Person{name string age int?}
```

**Formatted Output:**
```baml
class Person {
  name string
  age int?
}
```

**Formatting Rules:**
- 2-space indentation
- Consistent spacing around braces
- One property/value per line
- Preserved docstring comments (`///`)
- Block strings with appropriate delimiter selection (`#"` or `##"`)

**Use Cases:**
- Code formatting in development
- Standardizing BAML file style
- Pre-commit hooks
- Editor integration

---

#### Generate

```bash
minibaml generate <path> [options]
minibaml gen <path> [options]
```

Generates code from BAML files. Supports Python and TypeScript.

**Arguments:**
- `<path>` - Path to a BAML file or directory

**Options:**
- `--python` - Generate Python code (default)
- `--typescript`, `-ts` - Generate TypeScript code
- `--typebuilder`, `-tb` - Generate Python TypeBuilder module for `@@dynamic` types

**Python Generation Example:**
```bash
minibaml generate test.baml > models.py
minibaml gen baml_src --python > baml_client/models.py
```

**Generated Python Output:**
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
    Pending = "Pending"

def Greet(p: Person) -> str:
    raise NotImplementedError("This is a stub for LLM function")
```

**TypeScript Generation Example:**
```bash
minibaml generate test.baml --typescript > models.ts
minibaml gen baml_src -ts > baml_client/models.ts
```

**Generated TypeScript Output:**
```typescript
// Generated by minibaml

export interface Person {
  name: string;
  age?: number;
  email: string; // alias: email_address
  tags: string[];
  metadata: { [key: string]: string };
}

export enum Status {
  Active = "Active",
  Inactive = "Inactive",
  Pending = "Pending",
}

export type GreetInput = { p: Person };
export type GreetOutput = string;
```

**TypeBuilder Generation Example:**
```bash
minibaml gen test_dynamic.baml --typebuilder > type_builder.py
```

**Generated TypeBuilder Output:**
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

**Use Cases:**
- Generating Pydantic models for Python
- Generating TypeScript interfaces for web frontends
- Creating TypeBuilder for dynamic type modification
- Multi-language code generation from single source

---

## BAML Language Syntax

### Comments

BAML supports three types of comments:

#### Line Comments
```baml
// This is a line comment
```

#### Docstring Comments
```baml
/// This is a docstring comment
/// Used to document declarations
class Person {
  /// The person's name
  name string
}
```

#### Block Comments
```baml
{# This is a block comment
   Can span multiple lines
   Supports nesting: {# nested #} #}
```

---

### Keywords

- `class` - Define a data class
- `enum` - Define an enumeration
- `function` - Define an LLM function
- `client` - Define an LLM client
- `test` - Define a test case
- `generator` - Define code generator settings
- `template_string` - Define a reusable template
- `type` - Define a type alias
- `prompt` - Specify function prompt (keyword)
- `env` - Reference environment variable

---

### Primitive Types

- `string` - Text string
- `int` - Integer number
- `float` - Floating-point number
- `bool` - Boolean (true/false)
- `null` - Null value
- `image` - Image input
- `audio` - Audio input
- `video` - Video input
- `pdf` - PDF document input

---

### Symbols and Operators

- `@` - Property-level attribute prefix
- `@@` - Declaration-level attribute prefix
- `{` `}` - Block delimiters
- `[` `]` - Array type and list delimiters
- `(` `)` - Function parameters and attribute arguments
- `|` - Union type separator
- `?` - Optional type suffix
- `<` `>` - Generic type parameters
- `->` - Function return type separator
- `:` - Type annotation separator
- `,` - List separator
- `#` - Block string delimiter marker
- `"` - String literal delimiter

---

### String Literals

#### Regular Strings
```baml
"Hello, World!"
'Single quoted'
```

#### Block Strings
Block strings support multiple hash delimiter levels for nesting:

```baml
#"This is a block string"#

##"This allows #"nested"# strings"##

###"Even deeper ##"nesting"## is possible"###
```

**Use Cases:**
- Multi-line prompts
- Templates with quotes
- Nested template content

---

### Declarations

#### Class Declaration

```baml
/// Documentation for Person class
class Person {
  /// Person's full name
  name string

  /// Optional age
  age int?

  /// Email with alias
  email string @alias("email_address")

  /// List of tags
  tags string[]

  /// Key-value metadata
  metadata map<string, string>

  @@dynamic
  @@alias("PersonEntity")
}
```

**Syntax:**
```
class ClassName {
  propertyName Type [Attributes]
  ...
  [ClassAttributes]
}
```

---

#### Enum Declaration

```baml
/// Status enumeration
enum Status {
  /// Active state
  Active @alias("currently_active")

  /// Inactive state
  Inactive @description("Not active")

  /// Pending state
  Pending @skip

  @@dynamic
}
```

**Syntax:**
```
enum EnumName {
  ValueName [Attributes]
  ...
  [EnumAttributes]
}
```

---

#### Function Declaration

```baml
/// Greet a person
function Greet(p: Person) -> string {
  client "openai/gpt-4"
  prompt #"
    {{ _.role("user") }}
    Say hello to {{ p.name }}
    {{ ctx.output_format }}
  "#
}
```

**Syntax:**
```
function FunctionName(param1: Type, param2: Type, ...) -> ReturnType {
  client "provider/model"
  prompt #"
    Template content with {{ variables }}
  "#
}
```

**Parameters:**
- Format: `paramName: Type`
- Types can be any valid BAML type
- Multiple parameters separated by commas

---

#### Client Declaration

```baml
client<llm> MyClient {
  provider "openai"
  options {
    model "gpt-4"
    api_key env.OPENAI_API_KEY
    temperature 0.7
    base_url "https://api.openai.com/v1"
    headers {
      Authorization "Bearer token"
    }
  }
}
```

**Syntax:**
```
client<llm> ClientName {
  provider "providerName"
  options {
    key value
    ...
  }
}
```

**Environment Variables:**
```baml
api_key env.OPENAI_API_KEY
```

---

#### Test Declaration

```baml
test TestGreet {
  functions [Greet, ExtractPerson]
  args {
    p {
      name "Alice"
      age 30
      tags ["developer", "engineer"]
    }
    text "Sample text"
  }

  @@check(output, "length > 0")
  @@assert(output, "contains('Hello')")
}
```

**Syntax:**
```
test TestName {
  functions [FunctionName1, FunctionName2, ...]
  args {
    argName value
    ...
  }
  [TestAttributes]
}
```

---

#### Generator Declaration

```baml
generator PythonGenerator {
  output_type "python/pydantic"
  output_dir "./baml_client"
  version "0.60.0"
}
```

**Syntax:**
```
generator GeneratorName {
  key value
  ...
}
```

---

#### Template String Declaration

```baml
template_string FormatMessages(msgs: Message[]) #"
  {% for m in msgs %}
    {{ _.role(m.role) }}
    {{ m.content }}
  {% endfor %}
"#
```

**Syntax:**
```
template_string TemplateName(param1: Type, ...) #"
  Template content
"#
```

---

## Type System

### Primitive Types

| Type | Description | Example |
|------|-------------|---------|
| `string` | Text string | `"Hello"` |
| `int` | Integer | `42` |
| `float` | Floating-point | `3.14` |
| `bool` | Boolean | `true`, `false` |
| `null` | Null value | `null` |
| `image` | Image input | (runtime value) |
| `audio` | Audio input | (runtime value) |
| `video` | Video input | (runtime value) |
| `pdf` | PDF document | (runtime value) |

---

### Complex Types

#### Arrays

Represent lists of values of the same type.

**Syntax:** `Type[]`

**Examples:**
```baml
tags string[]
numbers int[]
people Person[]
```

**Generated Code:**
- Python: `List[str]`, `List[int]`, `List[Person]`
- TypeScript: `string[]`, `number[]`, `Person[]`

---

#### Optional Types

Represent values that may be null or undefined.

**Syntax:** `Type?`

**Examples:**
```baml
age int?
email string?
address Address?
```

**Generated Code:**
- Python: `Optional[int]`, `Optional[str]`, `Optional[Address]`
- TypeScript: `number | undefined`, `string | undefined`, `Address | undefined`

---

#### Union Types

Represent values that can be one of multiple types.

**Syntax:** `Type1 | Type2 | ...`

**Examples:**
```baml
result Person | null
value string | int
status Active | Inactive | Pending
```

**Generated Code:**
- Python: `Union[Person, None]`, `Union[str, int]`
- TypeScript: `Person | null`, `string | number`

---

#### Map Types

Represent key-value dictionaries.

**Syntax:** `map<KeyType, ValueType>`

**Examples:**
```baml
metadata map<string, string>
scores map<string, int>
nested map<string, Person[]>
```

**Generated Code:**
- Python: `Dict[str, str]`, `Dict[str, int]`, `Dict[str, List[Person]]`
- TypeScript: `{ [key: string]: string }`, `{ [key: string]: number }`, `{ [key: string]: Person[] }`

---

#### Literal Types

Represent specific constant values.

**Syntax:** `"value"` | `123` | `true`

**Examples:**
```baml
mode "production" | "development"
status 1 | 2 | 3
enabled true | false
```

---

#### Named Types

Reference to user-defined classes or enums.

**Examples:**
```baml
person Person
status Status
addresses Address[]
```

---

### Type Precedence

When parsing complex types, operators are applied in this order:

1. **Named types and primitives** - Base types
2. **Array suffix `[]`** - Applied left-to-right
3. **Optional suffix `?`** - Applied left-to-right
4. **Union operator `|`** - Lowest precedence

**Examples:**
```baml
string[]?          // Optional array of strings
Person | null      // Union of Person or null
string | int[]     // Union of string or array of ints
map<string, int?>  // Map with optional int values
```

---

## Attributes Reference

Attributes provide metadata and behavior modifications for declarations and properties.

### Attribute Syntax

- **Property-level attributes:** `@attributeName(arg1, arg2, ...)`
- **Declaration-level attributes:** `@@attributeName(arg1, arg2, ...)`

### Property-Level Attributes (`@`)

Used on class properties and enum values.

---

#### @alias

Specifies an alternative name for serialization/deserialization.

**Usage:**
```baml
class Person {
  email string @alias("email_address")
  full_name string @alias("fullName")
}
```

**Arguments:**
- Exactly 1 string argument (the alias name)

**Generated Code:**
- Python: `Field(alias="email_address")`
- TypeScript: `// alias: email_address` (comment)

**Validation:**
- ❌ Error if no arguments
- ❌ Error if more than 1 argument
- ❌ Error if argument is not a string

---

#### @description

Provides documentation for a property or enum value.

**Usage:**
```baml
class Person {
  age int @description("Person's age in years")
}

enum Status {
  Active @description("Currently active")
}
```

**Arguments:**
- Exactly 1 string argument (the description)

**Generated Code:**
- Python: Field docstring or comment
- TypeScript: JSDoc comment

**Validation:**
- ❌ Error if no arguments
- ❌ Error if more than 1 argument
- ❌ Error if argument is not a string

---

#### @skip

Marks a property or enum value to be skipped during code generation.

**Usage:**
```baml
class Person {
  internal_id string @skip
}

enum Status {
  Deprecated @skip
}
```

**Arguments:**
- No arguments

**Validation:**
- ⚠️ Warning if arguments are provided

---

#### @assert

Defines a validation assertion for a property value.

**Usage:**
```baml
class Person {
  age int @assert(age > 0)
  email string @assert(email.contains("@"))
}
```

**Arguments:**
- At least 1 argument (assertion expression)

**Validation:**
- ❌ Error if no arguments

---

#### @check

Defines a validation check for a property value.

**Usage:**
```baml
class Person {
  email string @check(is_valid_email(email))
}
```

**Arguments:**
- At least 1 argument (check expression)

**Validation:**
- ❌ Error if no arguments

---

### Declaration-Level Attributes (`@@`)

Used on classes, enums, and tests.

---

#### @@alias

Specifies an alternative name for the entire class or enum.

**Usage:**
```baml
class Person {
  name string

  @@alias("PersonEntity")
}

enum Status {
  Active
  Inactive

  @@alias("StatusEnum")
}
```

**Arguments:**
- Exactly 1 string argument (the alias name)

**Validation:**
- ❌ Error if used with `@` on properties
- ❌ Error if no arguments
- ❌ Error if more than 1 argument
- ❌ Error if argument is not a string

---

#### @@description

Provides documentation for a class or enum.

**Usage:**
```baml
class Person {
  name string

  @@description("Represents a person entity")
}
```

**Arguments:**
- Exactly 1 string argument (the description)

**Validation:**
- ❌ Error if no arguments
- ❌ Error if more than 1 argument
- ❌ Error if argument is not a string

---

#### @@dynamic

Marks a class or enum as dynamically modifiable at runtime using TypeBuilder.

**Usage:**
```baml
class User {
  name string

  @@dynamic
}

enum Category {
  Tech
  Science

  @@dynamic
}
```

**Arguments:**
- No arguments

**Effect:**
- Generates TypeBuilder helper classes for runtime modification
- Use `minibaml gen --typebuilder` to generate TypeBuilder module

**Generated TypeBuilder Example:**
```python
class TypeBuilder:
    def __init__(self):
        self.User = DynamicClassBuilder("User")
        self.Category = DynamicEnumBuilder("Category")
```

**Validation:**
- ⚠️ Warning if arguments are provided

---

#### @@check (Tests)

Defines a validation check for test output.

**Usage:**
```baml
test TestGreet {
  functions [Greet]
  args { p { name "Alice" } }

  @@check(output, "length > 0")
}
```

**Arguments:**
- At least 1 argument (check expression)

**Validation:**
- ❌ Error if no arguments

---

#### @@assert (Tests)

Defines a validation assertion for test output.

**Usage:**
```baml
test TestGreet {
  functions [Greet]
  args { p { name "Alice" } }

  @@assert(output, "contains('Hello')")
}
```

**Arguments:**
- At least 1 argument (assertion expression)

**Validation:**
- ❌ Error if no arguments

---

### Attribute Validation Rules

| Attribute | Level | Arguments | Type | Usage |
|-----------|-------|-----------|------|-------|
| `@alias` | Property | 1 | string | Property alias |
| `@description` | Property | 1 | string | Property docs |
| `@skip` | Property | 0 | - | Skip in codegen |
| `@assert` | Property | 1+ | any | Validation |
| `@check` | Property | 1+ | any | Validation |
| `@@alias` | Class/Enum | 1 | string | Type alias |
| `@@description` | Class/Enum | 1 | string | Type docs |
| `@@dynamic` | Class/Enum | 0 | - | Dynamic types |
| `@@check` | Test | 1+ | any | Test check |
| `@@assert` | Test | 1+ | any | Test assertion |

---

## Jinja Template Syntax

BAML uses Jinja2-style templates in function prompts and template_strings.

### Template Delimiters

| Delimiter | Purpose | Example |
|-----------|---------|---------|
| `{{ }}` | Variable interpolation | `{{ name }}` |
| `{% %}` | Statements (loops, conditionals) | `{% for x in items %}` |
| `{# #}` | Comments | `{# This is a comment #}` |

---

### Variables

Access function parameters and built-ins using `{{ }}`.

**Function Parameters:**
```baml
function Greet(name: string, age: int) -> string {
  prompt #"
    Hello {{ name }}, you are {{ age }} years old.
  "#
}
```

**Property Access:**
```baml
function Process(person: Person) -> string {
  prompt #"
    Name: {{ person.name }}
    Email: {{ person.email }}
    Age: {{ person.age }}
  "#
}
```

---

### Built-in Variables

#### ctx

Context object providing template metadata.

**`ctx.output_format`**

Inserts the expected output format specification.

```baml
function Extract(text: string) -> Person {
  prompt #"
    Extract person from: {{ text }}

    {{ ctx.output_format }}
  "#
}
```

**`ctx.client`**

Access client metadata (if available).

---

#### _ (Underscore)

Utility object for template helpers.

**`_.role(roleName)`**

Specifies the message role for LLM conversations.

```baml
function Chat(user_message: string) -> string {
  prompt #"
    {{ _.role("system") }}
    You are a helpful assistant.

    {{ _.role("user") }}
    {{ user_message }}
  "#
}
```

**Common Roles:**
- `"system"` - System message
- `"user"` - User message
- `"assistant"` - Assistant message

---

### Statements

#### For Loops

Iterate over arrays and lists.

```baml
template_string FormatMessages(messages: Message[]) #"
  {% for msg in messages %}
    {{ _.role(msg.role) }}
    {{ msg.content }}
  {% endfor %}
"#
```

---

#### Conditionals

Conditional template rendering.

```baml
function Greet(person: Person) -> string {
  prompt #"
    Hello {{ person.name }}
    {% if person.age %}
      You are {{ person.age }} years old.
    {% endif %}
  "#
}
```

---

### Comments

Template comments are not included in output.

```baml
prompt #"
  {# This comment won't appear in the prompt #}
  Hello {{ name }}
"#
```

---

### Filters

Apply transformations using the pipe `|` operator.

```baml
{{ name | upper }}
{{ items | length }}
```

---

### Template Validation

minibaml validates Jinja templates during the check phase:

1. **Variable References:** Ensures all variables are defined parameters
2. **Built-in Functions:** Validates `ctx` and `_` usage
3. **Balanced Delimiters:** Checks for matching `{{ }}`, `{% %}`, `{# #}`

**Example Validation Error:**
```baml
function Greet(name: string) -> string {
  prompt "Hello {{ invalid }}"  // ❌ ERROR: Undefined variable 'invalid'
}
```

**Valid Template:**
```baml
function Greet(name: string) -> string {
  prompt #"
    {{ _.role("user") }}
    Hello {{ name }}!
    {{ ctx.output_format }}
  "#
}
```

---

## Validation & Error Messages

minibaml performs comprehensive validation in multiple phases.

### Validation Phases

1. **Phase 1:** Register declarations, detect duplicates
2. **Phase 2:** Validate type references
3. **Phase 3:** Check circular dependencies
4. **Phase 4:** Validate attribute usage
5. **Phase 5:** Validate Jinja templates

---

### Error Types

#### Duplicate Definition

**Error:** A class, enum, or function is defined more than once.

```baml
class Person { name string }
class Person { email string }  // ❌ ERROR: Duplicate class definition: Person
```

**Multi-file Context:**
```
baml_src/models.baml:   class User { ... }
baml_src/entities.baml: class User { ... }  // ❌ ERROR: Duplicate class definition: User
```

---

#### Undefined Type

**Error:** A type reference that doesn't exist.

```baml
class Person {
  address Address  // ❌ ERROR: Undefined type: Address
}
```

**Fix:** Define the Address class first:
```baml
class Address {
  street string
  city string
}

class Person {
  address Address  // ✓ Valid
}
```

---

#### Circular Dependency

**Error:** Types that reference each other in a cycle.

```baml
class Person {
  company Company  // ❌ ERROR: Circular dependency detected in type: Person
}

class Company {
  owner Person
}
```

**Fix:** Use optional types or arrays to break the cycle:
```baml
class Person {
  company Company?  // ✓ Valid - optional breaks the cycle
}

class Company {
  owner Person
}
```

---

#### Invalid Type

**Error:** Type expression that doesn't conform to BAML syntax.

```baml
class Person {
  tags string[[]  // ❌ ERROR: Invalid type expression
}
```

---

#### Invalid Attribute

**Error:** Attribute used incorrectly or with wrong arguments.

**Wrong Level:**
```baml
class Person {
  name string @@alias("fullName")  // ❌ ERROR: Class-level attribute @@alias cannot be used on properties
}
```

**Wrong Arguments:**
```baml
class Person {
  name string @alias()  // ❌ ERROR: @alias requires exactly 1 argument, got 0
  age int @alias(123)   // ❌ ERROR: @alias requires a string argument
}
```

**Unknown Attribute:**
```baml
class Person {
  name string @unknown  // ⚠️ WARNING: Unknown property attribute @unknown
}
```

---

#### Undefined Function in Test

**Error:** Test references a function that doesn't exist.

```baml
test TestGreet {
  functions [NonExistent]  // ❌ ERROR: Undefined function in test: NonExistent
  args { }
}
```

---

#### Jinja Template Errors

**Undefined Variable:**
```baml
function Greet(name: string) -> string {
  prompt "Hello {{ invalid }}"  // ❌ ERROR: Undefined variable 'invalid' in template
}
```

**Valid Template:**
```baml
function Greet(name: string) -> string {
  prompt "Hello {{ name }}"  // ✓ Valid - 'name' is a parameter
}
```

---

### Diagnostic Severity Levels

| Level | Symbol | Description |
|-------|--------|-------------|
| **error** | ❌ | Validation failure, code generation blocked |
| **warning** | ⚠️ | Potential issue, code generation continues |
| **info** | ℹ️ | Informational message |

---

### Common Validation Patterns

#### Cross-file Type References

BAML automatically merges all declarations from a directory:

```
baml_src/
  models/person.baml    → class Person
  models/address.baml   → class Address
  functions.baml        → function Process(p: Person) -> Address
```

All types are in the same namespace, so cross-file references work automatically.

---

#### Breaking Circular Dependencies

**Option 1: Optional Types**
```baml
class Person {
  company Company?  // Optional breaks cycle
}

class Company {
  employees Person[]
}
```

**Option 2: Arrays**
```baml
class Person {
  friends Person[]  // Array allows self-reference
}
```

---

#### Attribute Best Practices

```baml
class Person {
  // ✓ Property-level attributes use @
  email string @alias("email_address")
  name string @description("Full name")

  // ✓ Class-level attributes use @@
  @@dynamic
  @@alias("PersonEntity")
}
```

---

### Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success - no errors |
| `1` | Validation failed - errors present |

---

## Best Practices

### Multi-file Projects

**Recommended Structure:**
```
baml_src/
  models/
    person.baml
    address.baml
  enums/
    status.baml
  functions/
    greet.baml
    extract.baml
  clients.baml
  generators.baml
  tests.baml
```

### Type Design

**Prefer optional over union with null:**
```baml
age int?              // ✓ Cleaner
age int | null        // Works, but verbose
```

**Use descriptive names:**
```baml
class UserProfile { }    // ✓ Clear
class UP { }             // ❌ Unclear
```

### Attributes

**Use @@dynamic for extensible types:**
```baml
class Config {
  base_setting string

  @@dynamic  // Allow runtime additions
}
```

**Use @alias for API compatibility:**
```baml
class Person {
  full_name string @alias("fullName")  // Maps to camelCase API
}
```

### Templates

**Always include ctx.output_format:**
```baml
function Extract(text: string) -> Person {
  prompt #"
    Extract person from: {{ text }}

    {{ ctx.output_format }}  // ✓ Ensures proper output format
  "#
}
```

---

## Version Information

minibaml version: 0.1.0

Built with Zig 0.15.1+

---

## See Also

- [Getting Started Guide](getting-started.md) - Learn BAML basics
- [Building from Source](BUILDING.md) - Build minibaml yourself

---

*Last updated: 2025-10-28*
