# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview
This is an Emacs AI Agent UI project - a lightweight Elisp-based interface that enables bi-directional communication between Emacs and LLM services, with tool use capabilities focused on buffer and file manipulation.

## Architecture & Code Structure

### Core Components
- **ai-agent-core.el**: HTTP communication layer for LLM endpoints (OpenAI-compatible APIs)
- **ai-agent-tools.el**: Tool registry and execution engine for LLM function calls
- **ai-agent-ui.el**: User interface with interactive, context, and log buffers
- **ai-agent-buffers.el**: Buffer manipulation operations
- **ai-agent-context.el**: Conversation context management
- **ai-agent-config.el**: Configuration and customization system

### Tool System
The agent provides tools in OpenAI function format for:
- **Buffer Operations**: append, edit, create, save, list, get-content
- **File System**: read, write, directory-list, current-directory, file-exists
- **Navigation**: goto-line, search-in-buffer

Tools follow JSON Schema format and return structured success/error responses.

## Development Commands

### Testing Elisp Code
```bash
# Run Emacs with the agent loaded
emacs -Q -l ai-agent.el

# Run ERT tests if implemented
emacs -batch -l ai-agent-tests.el -f ert-run-tests-batch-and-exit
```

### Key Elisp Functions
```elisp
;; Start the agent
(ai-agent-start)

;; Send a message
(ai-agent-send-message)

;; Send marked region
(ai-agent-send-region start end)

;; Clear context
(ai-agent-clear-context)
```

## Implementation Guidelines

### Code Style
- Target ~200 lines of core code
- Minimize external dependencies
- Support Emacs 27+
- Use async operations with `url-retrieve` for non-blocking HTTP
- Follow buffer-centric design patterns

### Tool Implementation Pattern
```elisp
(defun ai-agent-tool-<name> (args)
  "Tool function following consistent pattern"
  (condition-case err
      (list :status "success" :result ...)
    (error (list :status "error" :error ...))))
```

### Security Considerations
- Validate all file paths to prevent directory traversal
- Require user confirmation for destructive operations
- Support sandboxed file operations with configurable allowed directories
- Never execute arbitrary code without confirmation

### Error Handling
- Implement retry logic with exponential backoff for network failures
- Return structured error responses from tools
- Provide clear error messages to both user and LLM

## Current Status
The project is in planning/early development phase with comprehensive documentation in place:
- Product Requirements Document (PRD) defines the vision and features
- Architecture document outlines system components and data flow
- Tool specifications define the LLM-Emacs interface
- Implementation plan provides a phased development approach