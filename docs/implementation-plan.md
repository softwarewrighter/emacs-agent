# Implementation Plan: Emacs AI Agent UI

## Phase 1: Core Infrastructure (Week 1)

### 1.1 Basic HTTP Communication
**File:** `ai-agent-core.el`  
**LOC:** ~40

```elisp
;; Core functions to implement:
(defun ai-agent-make-request (messages)...)
(defun ai-agent-parse-response (response)...)
(defun ai-agent-format-message (role content)...)
```

**Tasks:**
- [ ] Implement POST request to LLM endpoint
- [ ] Handle JSON encoding/decoding
- [ ] Parse response for content and tool calls
- [ ] Error handling for network failures

### 1.2 Configuration System
**File:** `ai-agent-config.el`  
**LOC:** ~20

```elisp
(defgroup ai-agent nil...)
(defcustom ai-agent-endpoint...)
(defcustom ai-agent-model...)
(defcustom ai-agent-system-prompt...)
```

**Tasks:**
- [ ] Define customization group
- [ ] Create configuration variables
- [ ] Support for environment variables
- [ ] Validation functions

## Phase 2: Tool System (Week 1-2)

### 2.1 Tool Registry
**File:** `ai-agent-tools.el`  
**LOC:** ~50

```elisp
(defvar ai-agent-tools-registry...)
(defun ai-agent-register-tool (name spec func)...)
(defun ai-agent-execute-tool (name args)...)
```

**Tasks:**
- [ ] Define tool specification format
- [ ] Implement tool registration
- [ ] Create tool dispatcher
- [ ] Format tool results

### 2.2 Buffer Tools Implementation
**Functions to implement:**

```elisp
;; Buffer operations
(defun ai-agent-tool-buffer-append (buffer-name text)...)
(defun ai-agent-tool-buffer-edit (buffer-name old new)...)
(defun ai-agent-tool-buffer-create (buffer-name content)...)
(defun ai-agent-tool-buffer-save (buffer-name filepath)...)
(defun ai-agent-tool-buffer-list ()...)
(defun ai-agent-tool-buffer-get-content (buffer-name)...)
```

### 2.3 File System Tools
**Functions to implement:**

```elisp
;; File operations
(defun ai-agent-tool-file-read (filepath)...)
(defun ai-agent-tool-file-write (filepath content)...)
(defun ai-agent-tool-directory-list (path)...)
(defun ai-agent-tool-current-directory ()...)
```

## Phase 3: User Interface (Week 2)

### 3.1 Interactive Buffer
**File:** `ai-agent-ui.el`  
**LOC:** ~40

```elisp
(defvar ai-agent-interaction-buffer "*AI Agent*")
(defvar ai-agent-context-buffer "*AI Context*")
(defvar ai-agent-log-buffer "*AI Log*")

(defun ai-agent-setup-buffers ()...)
(defun ai-agent-display-message (role content)...)
(defun ai-agent-send-message ()...)
```

**Tasks:**
- [ ] Create buffer management functions
- [ ] Implement message display formatting
- [ ] Add input handling
- [ ] Setup keybindings

### 3.2 Context Management
**File:** `ai-agent-context.el`  
**LOC:** ~30

```elisp
(defvar ai-agent-context nil)
(defun ai-agent-add-to-context (message)...)
(defun ai-agent-get-context ()...)
(defun ai-agent-clear-context ()...)
(defun ai-agent-save-session (filename)...)
(defun ai-agent-load-session (filename)...)
```

## Phase 4: Integration (Week 2-3)

### 4.1 Main Loop
**File:** `ai-agent.el`  
**LOC:** ~30

```elisp
(defun ai-agent-process-message (user-input)
  "Main processing loop"
  ;; 1. Add user message to context
  ;; 2. Send to LLM with tools
  ;; 3. Parse response
  ;; 4. Execute tool calls if any
  ;; 5. Display response
  ;; 6. Loop if more tool calls)

(defun ai-agent-start ()...)
(defun ai-agent-stop ()...)
```

### 4.2 Interactive Commands

```elisp
(defun ai-agent-send-region (start end)
  "Send marked region to AI with context"
  (interactive "r")...)

(defun ai-agent-replace-region (start end)
  "Replace region with AI response"
  (interactive "r")...)
```

## Implementation Schedule

### Week 1: Foundation
- Day 1-2: Core HTTP communication
- Day 3-4: Tool system framework
- Day 5: Basic buffer tools

### Week 2: Tools & UI
- Day 1-2: Complete tool implementations
- Day 3-4: User interface buffers
- Day 5: Context management

### Week 3: Integration & Polish
- Day 1-2: Main processing loop
- Day 3: Interactive commands
- Day 4: Testing and debugging
- Day 5: Documentation and examples

## Testing Strategy

### Unit Tests
```elisp
;; Test HTTP communication
(ert-deftest ai-agent-test-format-message ()...)
(ert-deftest ai-agent-test-parse-response ()...)

;; Test tool execution
(ert-deftest ai-agent-test-buffer-append ()...)
(ert-deftest ai-agent-test-file-read ()...)
```

### Integration Tests
- Test full message round-trip
- Test tool call execution
- Test context persistence
- Test error recovery

### Manual Testing Scenarios
1. Basic conversation flow
2. File creation and editing
3. Buffer manipulation
4. Region sending/replacement
5. Session save/restore

## Code Organization

```
ai-agent/
├── ai-agent.el          # Main entry point, autoloads
├── ai-agent-core.el     # HTTP communication
├── ai-agent-config.el   # Configuration
├── ai-agent-tools.el    # Tool system
├── ai-agent-ui.el       # User interface
├── ai-agent-context.el  # Context management
└── ai-agent-tests.el    # Test suite
```

## Key Implementation Details

### 1. Async HTTP Requests
Use `url-retrieve` for non-blocking requests:

```elisp
(defun ai-agent-make-request-async (messages callback)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data (json-encode ...)))
    (url-retrieve ai-agent-endpoint callback)))
```

### 2. Tool Call Format
Follow OpenAI function calling format:

```json
{
  "tool_calls": [{
    "id": "call_123",
    "type": "function",
    "function": {
      "name": "buffer_append",
      "arguments": "{\"buffer\": \"*scratch*\", \"text\": \"Hello\"}"
    }
  }]
}
```

### 3. Context Truncation
Implement sliding window for large contexts:

```elisp
(defun ai-agent-truncate-context (context max-tokens)
  ;; Keep system message
  ;; Keep last N messages that fit in max-tokens
  ;; Optionally summarize older messages)
```

### 4. Error Recovery
Implement retry with exponential backoff:

```elisp
(defun ai-agent-retry-request (messages attempts)
  (condition-case err
      (ai-agent-make-request messages)
    (error
     (if (< attempts 3)
         (progn
           (sit-for (expt 2 attempts))
           (ai-agent-retry-request messages (1+ attempts)))
       (signal (car err) (cdr err))))))
```

## Minimal Viable Product (MVP)

### Core Features Only (~200 LOC)
1. Send messages to LLM
2. Maintain context
3. Execute 5 essential tools:
   - buffer-append
   - buffer-create
   - file-read
   - file-write
   - directory-list
4. Basic interaction buffer
5. Simple configuration

### Defer to Future
- Session persistence
- Advanced tools
- Streaming responses
- Multiple sessions
- Complex UI features

## Success Criteria

1. **Functionality**: All core features working
2. **Performance**: <1s response for local LLM
3. **Reliability**: No data loss, graceful failures
4. **Usability**: Intuitive commands and feedback
5. **Code Quality**: Clean, documented, <250 LOC total