// Test comment
/// Documentation comment
class Person {
  name string
  age int?
  email string @alias("email_address")
  tags string[]
  metadata map<string, string>
}

enum Status {
  Active
  Inactive
  Pending
}

function Greet(p: Person) -> string {
  client "openai/gpt-4"
  prompt #"
    Say hello to {{ p.name }}
  "#
}

function ExtractData(text: string, img: image) -> Person | null {
  client "anthropic/claude-sonnet-4"
  prompt ##"
    Extract person from: {{ text }}
    Image: {{ img }}

    {{ ctx.output_format }}
  "##
}

client<llm> MyClient {
  provider "openai"
  options {
    model "gpt-4"
    api_key env.OPENAI_API_KEY
    temperature 0.7
  }
}

test TestGreet {
  functions [Greet]
  args {
    p {
      name "Alice"
      age 30
    }
  }
}

{# This is a
   multiline
   block comment #}
