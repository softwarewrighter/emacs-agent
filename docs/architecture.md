# AI Agent UI Architecture

## Overview
A lightweight Elisp-based AI agent interface that enables bi-directional communication between Emacs and LLM services, with tool use capabilities focused on buffer and file manipulation.

## System Components

### 1. Core Communication Layer
**Module:** `ai-agent-core.el`
- **HTTP Client**: Handles requests to local/remote LLM endpoints
- **Message Formatter**: Structures messages in API-compatible format (OpenAI/Anthropic style)
- **Response Parser**: Extracts content and tool calls from LLM responses
- **Context Manager**: Maintains conversation history

### 2. Tool Execution Engine
**Module:** `ai-agent-tools.el`
- **Tool Registry**: Defines available tools and their schemas
- **Tool Dispatcher**: Routes tool calls to appropriate Elisp functions
- **Result Handler**: Formats tool execution results for LLM consumption

### 3. User Interface
**Module:** `ai-agent-ui.el`
- **Interactive Buffer**: Main interface for user input and LLM responses
- **Context Buffer**: Editable conversation history
- **Log Buffer**: Detailed request/response logging
- **Region Handler**: Captures and sends marked regions

### 4. Buffer Management
**Module:** `ai-agent-buffers.el`
- **Buffer Operations**: Create, edit, append, save buffers
- **File Operations**: Read, write, list files and directories
- **Path Utilities**: Handle file paths and directory navigation

## Data Flow

```
User Input → Message Formatter → HTTP Client → LLM Service
                                                    ↓
                                              LLM Response
                                                    ↓
                                            Response Parser
                                                    ↓
                                    Tool Call? → Tool Execution
                                         ↓            ↓
                                    Display      Tool Result
                                         ↓            ↓
                                    UI Buffer    Back to LLM
```

## Key Design Decisions

### 1. Stateful Context Management
- Maintain full conversation history in memory
- Allow user editing of context buffer
- Persist sessions to disk for resumption

### 2. Tool Schema Definition
- Use JSON Schema-like format for tool definitions
- Align with OpenAI/Anthropic function calling conventions
- Provide clear tool descriptions for LLM understanding

### 3. Async Communication
- Non-blocking HTTP requests using `url-retrieve`
- Callback-based response handling
- Progress indicators during LLM processing

### 4. Buffer-Centric Design
- All interactions through Emacs buffers
- Leverage existing Emacs text manipulation capabilities
- Natural integration with user workflow

## Tool Categories

### Buffer Tools
- `buffer-append`: Append text to a buffer
- `buffer-edit`: Replace text in a buffer
- `buffer-create`: Create new buffer with content
- `buffer-save`: Save buffer to file
- `buffer-list`: List all open buffers
- `buffer-get-content`: Retrieve buffer contents

### File System Tools
- `file-read`: Read file contents
- `file-write`: Write content to file
- `directory-list`: List directory contents
- `current-directory`: Get working directory
- `file-exists`: Check file existence

### Navigation Tools
- `goto-line`: Navigate to specific line
- `search-forward`: Find text in buffer
- `mark-region`: Programmatically mark regions

## Configuration Schema

```elisp
(defcustom ai-agent-config
  '((endpoint . "http://localhost:11434/v1/chat/completions")
    (model . "llama3.2")
    (api-key . nil)
    (temperature . 0.7)
    (max-tokens . 2000)
    (system-prompt . "You are an AI assistant...")
    (tools . (buffer-append buffer-edit ...)))
  "Configuration for AI Agent")
```

## Error Handling

### Network Errors
- Retry logic with exponential backoff
- Fallback to alternate endpoints
- Clear error messages to user

### Tool Execution Errors
- Graceful failure with error details to LLM
- Sandboxed execution where possible
- User confirmation for destructive operations

### Context Overflow
- Token counting and truncation strategies
- Summary generation for long contexts
- User-controlled context pruning

## Security Considerations

1. **File Access Control**
   - Configurable allowed directories
   - Confirmation for writes outside project
   
2. **API Key Management**
   - Support for environment variables
   - Secure storage in auth-source
   
3. **Tool Permissions**
   - User-configurable tool whitelist
   - Confirmation prompts for sensitive operations

## Performance Optimization

1. **Lazy Loading**
   - Load modules on demand
   - Defer non-critical initialization
   
2. **Response Streaming**
   - Support for SSE/streaming responses
   - Incremental display of LLM output
   
3. **Context Caching**
   - Cache frequently used context
   - Efficient history management