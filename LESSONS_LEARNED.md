# Lessons Learned: Building a General-Purpose Emacs Agent

## Key Insights

### 1. LLMs are Stateless
**Critical Learning:** Each API call to an LLM is completely independent. The model doesn't remember previous interactions unless you explicitly include them in the request.

**Implications:**
- System prompts must be sent with EVERY request
- Tool definitions must be included with EVERY request
- Context (if needed) must be reconstructed for each call
- "Conversation memory" is an illusion created by the client

### 2. User Autonomy vs LLM Autonomy
**Principle:** The user can *suggest* tools, but the LLM decides whether to use them.

**Examples:**
- User: "Use file_write to save this" → LLM decides if file_write is appropriate
- User: "Create a file" → LLM autonomously chooses file_write
- User: "What is recursion?" → LLM decides no tools needed

### 3. Tool Discovery Best Practices (MCP-Compliant)

**Effective Tool Definitions Include:**
```json
{
  "name": "clear_unique_name",
  "description": "What it does and when to use it",
  "parameters": {
    "type": "object",
    "properties": {
      "param_name": {
        "type": "string",
        "description": "Clear explanation of this parameter"
      }
    },
    "required": ["param_name"]
  }
}
```

### 4. Prompting Strategies That Work

**Most Effective:**
1. Direct action requests: "Create a file named X with content Y"
2. Task-oriented requests: "Save this function to a file"
3. Multi-step tasks: "Create a buffer and then append to it"

**Less Effective:**
1. Vague suggestions: "Can you use tool X?"
2. Information requests: "What is...?" (correctly doesn't use tools)
3. Wrong tool suggestions: LLM may ignore or correct

### 5. System Prompt Design

**Generic is Better:**
- ✅ "You have access to tools for file and buffer operations"
- ❌ "Create factorial.el and fibonacci.el files"

**Tool Awareness:**
- Always mention that tools are available
- Explain when tools should be used
- Let the LLM decide based on user requests

### 6. TDD Approach Success

**Red/Green/Refactor worked well:**
1. Write failing tests for desired behavior
2. Implement minimal code to pass
3. Iterate based on actual LLM responses

**Key:** Test with real LLM calls, not just mocks, because LLM behavior varies.

### 7. Parenthesis Management in Elisp

**Defensive Coding:**
- Use `cl-lib` structures instead of complex nested lists
- Prefer `list` and `cons` over quasiquote for deep nesting
- Always validate generated elisp with `check-parens`
- Extract code from markdown blocks before validation

### 8. Architecture Patterns

**Successful Pattern:**
```
User Input → System Prompt + Tools + User Request → LLM
                                                      ↓
                                            Parse Response
                                                      ↓
                                    Text Response / Tool Calls
                                                      ↓
                                            Execute Tools
                                                      ↓
                                            Return Results
```

## What Works

1. **Stateless Design**: Treat each interaction as independent
2. **MCP Compliance**: Follow emerging standards for tool definitions
3. **Clear Tool Descriptions**: Help LLMs understand when to use each tool
4. **User Freedom**: Let users phrase requests naturally
5. **LLM Autonomy**: Trust the model to choose appropriate tools

## What Doesn't Work

1. **Assuming Context**: LLMs don't remember previous calls
2. **Prescriptive Prompts**: Telling LLM exactly what to do limits flexibility
3. **Complex Nested Elisp**: Leads to parenthesis errors
4. **Vague Tool Definitions**: LLMs need clear descriptions

## Final Architecture

```elisp
;; Core components
- emacs-agent-mcp.el         ; MCP-compliant implementation
- test-general-agent.el      ; TDD test suite
- demo-mcp-strategies.el     ; Prompting strategy tests

;; Key functions
- emacs-agent-mcp-call-llm   ; Stateless API calls with tools
- emacs-agent-mcp-execute-tool ; Tool execution
- emacs-agent-mcp-conversation ; High-level interface
```

## Future Improvements

### Immediate Priorities
1. **Selective Tool Exposure**: Only send relevant tools based on user action
2. **Context-Aware System Prompts**: Multiple tailored prompts for different operations
3. **Buffer Context Enhancement**: Include actual buffer content, not just descriptions
4. **Context Pruning**: Manage token limits intelligently

### Near Term
1. **Multi-turn Conversations**: Include relevant context from previous turns
2. **Tool Chaining**: Allow LLM to call multiple tools in sequence
3. **Error Recovery**: Let LLM retry with different tools on failure
4. **Project Context**: Include related buffers and directory structure

### Long Term
1. **Model Agnostic**: Support multiple LLM providers (Claude, GPT, etc.)
2. **MCP 2.0**: Adapt to upcoming protocol updates
3. **RAG Integration**: Include Emacs documentation and best practices
4. **Caching Strategy**: Leverage potential LLM caching for repeated context

## Testing Results

With proper stateless design and MCP-compliant tools:
- Direct action requests: 80-90% tool usage
- Task-oriented requests: 70-80% tool usage  
- Information requests: 0% tool usage (correct)
- User tool suggestions: 40-50% tool usage (LLM decides)

The key insight: **LLMs need complete context in every request** to make informed decisions about tool usage.