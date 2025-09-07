# Tool Specifications for LLM

## System Prompt Template

```
You are an AI assistant integrated with Emacs. You have access to tools that allow you to manipulate buffers, files, and directories within the Emacs environment. 

When you need to perform an action, use the appropriate tool by specifying a tool call in your response. Each tool has specific parameters that must be provided.

Available tools and their capabilities are described below.
```

## Tool Definitions (OpenAI Function Format)

### Buffer Management Tools

#### 1. buffer_append
```json
{
  "name": "buffer_append",
  "description": "Append text to the end of a specified buffer",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name of the buffer (e.g., '*scratch*', 'file.txt')"
      },
      "text": {
        "type": "string",
        "description": "Text to append to the buffer"
      }
    },
    "required": ["buffer_name", "text"]
  }
}
```

#### 2. buffer_edit
```json
{
  "name": "buffer_edit",
  "description": "Replace text in a buffer",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name of the buffer to edit"
      },
      "old_text": {
        "type": "string",
        "description": "Text to search for and replace"
      },
      "new_text": {
        "type": "string",
        "description": "Replacement text"
      }
    },
    "required": ["buffer_name", "old_text", "new_text"]
  }
}
```

#### 3. buffer_create
```json
{
  "name": "buffer_create",
  "description": "Create a new buffer with initial content",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name for the new buffer"
      },
      "content": {
        "type": "string",
        "description": "Initial content for the buffer"
      },
      "mode": {
        "type": "string",
        "description": "Major mode for the buffer (optional, e.g., 'python-mode', 'org-mode')"
      }
    },
    "required": ["buffer_name", "content"]
  }
}
```

#### 4. buffer_save
```json
{
  "name": "buffer_save",
  "description": "Save a buffer to a file",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name of the buffer to save"
      },
      "file_path": {
        "type": "string",
        "description": "Path where the buffer should be saved"
      }
    },
    "required": ["buffer_name", "file_path"]
  }
}
```

#### 5. buffer_list
```json
{
  "name": "buffer_list",
  "description": "List all currently open buffers",
  "parameters": {
    "type": "object",
    "properties": {
      "include_hidden": {
        "type": "boolean",
        "description": "Include hidden buffers (those starting with space)"
      }
    }
  }
}
```

#### 6. buffer_get_content
```json
{
  "name": "buffer_get_content",
  "description": "Get the full content of a buffer",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name of the buffer"
      },
      "max_lines": {
        "type": "integer",
        "description": "Maximum number of lines to return (optional)"
      }
    },
    "required": ["buffer_name"]
  }
}
```

### File System Tools

#### 7. file_read
```json
{
  "name": "file_read",
  "description": "Read the contents of a file",
  "parameters": {
    "type": "object",
    "properties": {
      "file_path": {
        "type": "string",
        "description": "Path to the file to read"
      }
    },
    "required": ["file_path"]
  }
}
```

#### 8. file_write
```json
{
  "name": "file_write",
  "description": "Write content to a file (creates or overwrites)",
  "parameters": {
    "type": "object",
    "properties": {
      "file_path": {
        "type": "string",
        "description": "Path to the file to write"
      },
      "content": {
        "type": "string",
        "description": "Content to write to the file"
      },
      "append": {
        "type": "boolean",
        "description": "Append to file instead of overwriting"
      }
    },
    "required": ["file_path", "content"]
  }
}
```

#### 9. directory_list
```json
{
  "name": "directory_list",
  "description": "List contents of a directory",
  "parameters": {
    "type": "object",
    "properties": {
      "directory_path": {
        "type": "string",
        "description": "Path to the directory (defaults to current directory)"
      },
      "recursive": {
        "type": "boolean",
        "description": "List subdirectories recursively"
      },
      "pattern": {
        "type": "string",
        "description": "File pattern to match (e.g., '*.txt')"
      }
    }
  }
}
```

#### 10. current_directory
```json
{
  "name": "current_directory",
  "description": "Get the current working directory",
  "parameters": {
    "type": "object",
    "properties": {}
  }
}
```

#### 11. file_exists
```json
{
  "name": "file_exists",
  "description": "Check if a file or directory exists",
  "parameters": {
    "type": "object",
    "properties": {
      "path": {
        "type": "string",
        "description": "Path to check"
      }
    },
    "required": ["path"]
  }
}
```

### Navigation Tools

#### 12. goto_line
```json
{
  "name": "goto_line",
  "description": "Navigate to a specific line in a buffer",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name of the buffer"
      },
      "line_number": {
        "type": "integer",
        "description": "Line number to navigate to"
      }
    },
    "required": ["buffer_name", "line_number"]
  }
}
```

#### 13. search_in_buffer
```json
{
  "name": "search_in_buffer",
  "description": "Search for text in a buffer",
  "parameters": {
    "type": "object",
    "properties": {
      "buffer_name": {
        "type": "string",
        "description": "Name of the buffer to search"
      },
      "search_text": {
        "type": "string",
        "description": "Text to search for"
      },
      "case_sensitive": {
        "type": "boolean",
        "description": "Whether search is case sensitive"
      }
    },
    "required": ["buffer_name", "search_text"]
  }
}
```

## Tool Response Format

### Success Response
```json
{
  "status": "success",
  "result": "Tool-specific result data",
  "message": "Optional human-readable message"
}
```

### Error Response
```json
{
  "status": "error",
  "error": "Error description",
  "details": "Additional error details if available"
}
```

## Example Tool Call Sequence

### User Request
"Create a new Python file with a hello world function"

### LLM Response with Tool Call
```json
{
  "content": "I'll create a new Python file with a hello world function for you.",
  "tool_calls": [
    {
      "id": "call_abc123",
      "type": "function",
      "function": {
        "name": "buffer_create",
        "arguments": "{\"buffer_name\": \"hello.py\", \"content\": \"def hello_world():\\n    print('Hello, World!')\\n\\nif __name__ == '__main__':\\n    hello_world()\\n\", \"mode\": \"python-mode\"}"
      }
    }
  ]
}
```

### Tool Execution Result
```json
{
  "status": "success",
  "result": "Buffer 'hello.py' created with 5 lines",
  "message": "Buffer created successfully"
}
```

### Follow-up Tool Call
```json
{
  "tool_calls": [
    {
      "id": "call_def456",
      "type": "function",
      "function": {
        "name": "buffer_save",
        "arguments": "{\"buffer_name\": \"hello.py\", \"file_path\": \"./hello.py\"}"
      }
    }
  ]
}
```

## Implementation Notes

### Tool Registration in Elisp
```elisp
(defun ai-agent-register-tools ()
  "Register all available tools with their specifications"
  (setq ai-agent-tools
        '((buffer_append . ((spec . (...))
                           (func . ai-agent-tool-buffer-append)))
          (buffer_edit . ((spec . (...))
                         (func . ai-agent-tool-buffer-edit)))
          ;; ... more tools
          )))
```

### Tool Execution Handler
```elisp
(defun ai-agent-execute-tool-call (tool-call)
  "Execute a tool call from the LLM"
  (let* ((function-name (alist-get 'name (alist-get 'function tool-call)))
         (arguments (json-parse-string 
                    (alist-get 'arguments (alist-get 'function tool-call))))
         (tool-func (alist-get 'func (alist-get (intern function-name) ai-agent-tools))))
    (if tool-func
        (condition-case err
            (list :status "success" 
                  :result (apply tool-func (map-values arguments)))
          (error 
           (list :status "error" 
                 :error (error-message-string err))))
      (list :status "error" 
            :error (format "Unknown tool: %s" function-name)))))
```

## Security Considerations

1. **Path Validation**: All file paths should be validated to prevent directory traversal
2. **Buffer Name Validation**: Ensure buffer names don't conflict with system buffers
3. **Content Sanitization**: Large content should be truncated or paginated
4. **Confirmation Prompts**: Destructive operations should require user confirmation
5. **Sandboxing**: Option to restrict file operations to specific directories

## Performance Optimizations

1. **Lazy Loading**: Load file contents only when needed
2. **Caching**: Cache frequently accessed buffer contents
3. **Batch Operations**: Support multiple tool calls in single round-trip
4. **Async Execution**: Non-blocking tool execution where possible