
# 🦄 ai that works: Bash vs. MCP - Token Efficient Coding Agent Tooling

> In this episode, Dex and Vaibhav delve into the intricacies of coding agents, focusing on the debate between using MCP (Model Control Protocol) and Bash for tool integration, exploring context windows, token management, and optimization strategies.

[Video](https://www.youtube.com/watch?v=RtXpXIY4sLk) (1h27m)

[![Bash vs. MCP](https://img.youtube.com/vi/RtXpXIY4sLk/0.jpg)](https://www.youtube.com/watch?v=RtXpXIY4sLk)

Links:

## Episode Overview

This episode explores the fundamental trade-offs between using Bash and MCP (Model Control Protocol) for coding agent tool integration. The hosts demonstrate real-world examples comparing token usage, examine the impact on context windows, and share advanced techniques for optimizing coding agent performance.

### Key Topics Covered

- Token efficiency and the downsides of JSON in tool definitions
- Understanding context windows and their impact on model accuracy
- Writing your own drop-in replacements for MCP tools
- Naming conventions and their critical role in model outputs
- Dynamic context engineering techniques
- Advanced tricks like .shims for forcing uv instead of python or bun instead of npm
- Real-world applications and performance optimizations
- Best practices for using MCPs effectively

## Key Takeaways

- There is no one-size-fits-all solution in coding agents - choose tools based on your specific needs
- Understanding the underlying mechanics of models and context management is crucial for effective use
- The accuracy of results can be significantly impacted by how you manage context
- MCP tools can simplify integration for those unfamiliar with APIs, but come with token overhead
- Dynamic context engineering can enhance the performance of coding agents
- Naming conventions play a critical role in the accuracy of model outputs
- Efficient token usage is essential for maximizing context window effectiveness
- Real-world applications demonstrate the practical implications of these concepts
- Flexibility in tool usage allows for better customization and performance
- Community engagement and feedback are vital for continuous improvement

## Episode Highlights

> "Token efficiency isn't just about saving money - it's about preserving context space for what really matters."

> "Naming conventions matter more than you think. The names you give your tools directly impact how accurately the model uses them."

> "Don't just blindly use MCP for everything. Understand the trade-offs and pick the right tool for the job."

## Resources

- [Session Recording](https://www.youtube.com/watch?v=RtXpXIY4sLk)
- [Discord Community](https://boundaryml.com/discord)
- Sign up for the next session on [Luma](https://lu.ma/baml)

## Whiteboards

---

## Code Overview


#### example claude output w/ token ccounts

```
claude -p "write foo to bar.txt" \
    --allowedTools=Write,Read,Edit \
    --output-format=stream-json \
    --verbose
```

output message (trimmed, formatted)

```
{
    "input_tokens":4,
    "cache_creation_input_tokens":24841
    "cache_read_input_tokens":4802,
    "cache_creation":{
       "ephemeral_5m_input_tokens":24841,
       "ephemeral_1h_input_tokens":0
    },
    "output_tokens":129,
    "service_tier":"standard"
}
```



#### Claude w/ the token counter

```
claude -p "write foo to bar.txt" \
    --allowedTools=Write,Read,Edit \
    --output-format=stream-json \
    --verbose \
    | bun run src/inspect-logs.ts --stdin
```

Running it again with the cache

```
claude -p "write foo to bar.txt" \
    --allowedTools=Write,Read,Edit \
    --output-format=stream-json \
    --verbose \
    | bun run src/inspect-logs.ts --stdin
```

Running it again without the cache

```
claude -p "PLEASE write foo to bar.txt" \
    --allowedTools=Write,Read,Edit \
    --output-format=stream-json \
    --verbose \
    | bun run src/inspect-logs.ts --stdin
```

```
Streaming cache_creation_input_tokens:
--------------------------------------------------
Line 2: assistant (text)                         cache_creation: 12672
Line 5: assistant (tool_use)                     cache_creation: 184
Line 7: assistant (tool_use)                     cache_creation: 184
Line 9: assistant (text)                         cache_creation: 185
Line 10: result                                   cache_creation: 0
--------------------------------------------------

Total tool calls: 3
Total cache creation tokens: 13225
```



#### Adding MCP tools and inspecting context differences

use mcp-linear.json to add linear mcp tools

```
claude -p "write arg foo to bar.txt" \
    --allowedTools=Write,Read,Edit \
    --output-format=stream-json \
    --verbose \
    --mcp-config=mcp-linear.json \
    | bun run src/inspect-logs.ts --stdin
```

```
Streaming cache_creation_input_tokens:
--------------------------------------------------
Line 2: assistant (text)                         cache_creation: 18395
Line 5: assistant (tool_use)                     cache_creation: 171
Line 7: assistant (tool_use)                     cache_creation: 184
Line 9: assistant (text)                         cache_creation: 207
Line 10: result                                   cache_creation: 0
--------------------------------------------------

Total tool calls: 3
Total cache creation tokens: 18957
```

#### Linear CLI

```
export LINEAR_API_KEY=sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

bun run linear-cli/linear-cli.ts get-issue ENG-1709
bun run linear-cli/linear-cli.ts get-issue ENG-1709 --comments
```

```
cp CLAUDE_linear_cli.md CLAUDE.md
```

```
claude -p "write arg foo to bar.txt" \
    --allowedTools=Bash(bun run linear-cli/:*) \
    --output-format=stream-json \
    --verbose \
    | bun run src/inspect-logs.ts --stdin
```

now fetch the issue

```
claude -p "fetch issue ENG-XXXX" \
    --allowedTools=Bash(bun run linear-cli/:*) \
    --output-format=stream-json \
    --verbose \
    | bun run src/inspect-logs.ts --stdin
```


now fetch the issue and all comments

```
claude -p "fetch issue ENG-XXXX and all comments" \
    --allowedTools=Bash(bun run linear-cli/:*) \
    --output-format=stream-json \
    --verbose \
    | bun run src/inspect-logs.ts --stdin
```

### now fetch with mcp

```
cp CLAUDE_linear_mcp.md CLAUDE.md
```

```
claude -p "fetch issue ENG-1709" \
    --allowedTools='mcp__linear2__*' \
    --output-format=stream-json \
    --verbose \
    --mcp-config=mcp-linear.json \
    | bun run src/inspect-logs.ts --stdin
```

```
claude -p "fetch issue ENG-1709 and all comments" \
    --dangerously-skip-permissions \
    --output-format=stream-json \
    --verbose \
    --mcp-config=mcp-linear.json \
    | bun run src/inspect-logs.ts --stdin
```
