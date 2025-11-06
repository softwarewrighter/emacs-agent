# Contributing to Emacs Agent

Thank you for your interest in contributing to Emacs Agent! This document provides guidelines and information for contributors.

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR-USERNAME/emacs-agent.git`
3. Create a feature branch: `git checkout -b feature/your-feature-name`
4. Make your changes
5. Test thoroughly
6. Submit a pull request

## Development Setup

### Prerequisites

- Emacs 27.1 or later
- `request.el` package
- Claude API key for testing

### Local Testing

```elisp
;; In your test Emacs instance
(add-to-list 'load-path "/path/to/emacs-agent")
(require 'emacs-agent)
(setq emacs-agent-api-key (getenv "ANTHROPIC_API_KEY"))

;; Test the agent
(emacs-agent-chat)
```

## Code Style

### Emacs Lisp Conventions

- Use `lexical-binding: t` in all files
- Follow standard Emacs Lisp naming conventions
- Prefix all symbols with `emacs-agent-`
- Use `defcustom` for user-configurable variables
- Use `defvar` for internal state
- Document all public functions with docstrings
- Keep lines under 100 characters when possible

### Example

```elisp
(defcustom emacs-agent-example-setting t
  "Example user-configurable setting.
This is what the setting does in more detail."
  :type 'boolean
  :group 'emacs-agent)

(defun emacs-agent-example-function (arg)
  "Example function that does something with ARG.
Returns something useful."
  (interactive "sEnter value: ")
  (message "You entered: %s" arg))
```

## Adding New MCP Tools

To add a new MCP tool:

1. Implement the tool function in `emacs-agent-tools.el`:

```elisp
(defun emacs-agent-tool--your-tool (params)
  "Description of what your tool does.
PARAMS: :param1 (required), :param2 (optional)."
  (let ((param1 (plist-get params :param1)))
    (unless param1
      (error "Missing required parameter: param1"))
    ;; Your implementation here
    (list :success t
          :result "your result")))
```

2. Register the tool in `emacs-agent-tools-init`:

```elisp
(emacs-agent-register-tool
 "category.toolName"
 #'emacs-agent-tool--your-tool
 "Brief description of the tool"
 '((:param1 . (:type "string" :description "Description of param1 (required)"))
   (:param2 . (:type "integer" :description "Description of param2 (optional)"))))
```

3. Test the tool:

```elisp
(emacs-agent-call-tool "category.toolName"
                       '(:param1 "test value"))
```

## Testing

### Manual Testing

1. Test your changes with the chat interface
2. Try various tool combinations
3. Test error handling
4. Test with different buffer/window/frame configurations

### Test Scenarios

Before submitting a PR, please test:

- [ ] Basic chat functionality works
- [ ] Tool calls execute correctly
- [ ] Error handling works (invalid parameters, missing files, etc.)
- [ ] Approval system works for write operations
- [ ] UI updates correctly show tool calls and responses
- [ ] Conversation history is maintained correctly
- [ ] Package loads without errors in a clean Emacs

## Pull Request Process

1. **Update documentation**: If you add features, update README.md
2. **Add examples**: Include example usage in your PR description
3. **Test thoroughly**: Describe what you tested in the PR
4. **Keep commits clean**: Use clear, descriptive commit messages
5. **One feature per PR**: Keep PRs focused on a single feature/fix

### PR Description Template

```markdown
## Description
Brief description of what this PR does.

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring

## Testing
Describe what you tested and how.

## Screenshots (if applicable)
Add screenshots showing the feature in action.

## Checklist
- [ ] Code follows the style guidelines
- [ ] Documentation updated
- [ ] Tested manually
- [ ] No new warnings or errors
```

## Architecture Overview

### File Structure

- `emacs-agent.el` - Main entry point and package definition
- `emacs-agent-tools.el` - MCP tool implementations
- `emacs-agent-client.el` - Claude API client
- `emacs-agent-ui.el` - Chat interface and UI

### Data Flow

```
User Input (UI)
  → emacs-agent-ui.el
  → emacs-agent-client.el (sends to Claude API)
  → Claude responds with tool calls
  → emacs-agent-client.el (processes tool calls)
  → emacs-agent-tools.el (executes tools)
  → Results sent back to Claude
  → Final response shown in UI
```

## Ideas for Contributions

### Features

- [ ] LSP integration for code intelligence
- [ ] Git operations (commit, push, branch, diff)
- [ ] Project-aware context (recognize project structure)
- [ ] Multi-model support (OpenAI, local models via Ollama)
- [ ] Streaming response support
- [ ] Session persistence (save/load conversations)
- [ ] Tool usage analytics
- [ ] Contextual code completion
- [ ] Org-mode integration
- [ ] Magit integration

### Tools to Add

- [ ] `search.grep` - Advanced grep functionality
- [ ] `search.symbol` - Find symbol references
- [ ] `git.status`, `git.commit`, etc.
- [ ] `lsp.definition`, `lsp.references` - LSP integration
- [ ] `org.capture` - Org-mode capture
- [ ] `compile.run` - Run compilation
- [ ] `test.run` - Run tests
- [ ] `package.install` - Install packages

### Improvements

- [ ] Better error messages
- [ ] Improved approval UI (show diffs for writes)
- [ ] Rate limiting and retry logic
- [ ] Better conversation context management
- [ ] Tool call visualization
- [ ] Performance optimizations
- [ ] Async tool execution

## Code Review Expectations

When reviewing PRs, we look for:

- **Correctness**: Does it work as intended?
- **Safety**: Does it handle errors? Could it cause data loss?
- **Style**: Does it follow conventions?
- **Documentation**: Is it documented?
- **Testing**: Was it tested?
- **User Experience**: Is it intuitive?

## Getting Help

- **Questions**: Open a GitHub discussion
- **Bugs**: Open a GitHub issue
- **Feature Ideas**: Open a GitHub issue with the "enhancement" label

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

## Thank You!

Your contributions make Emacs Agent better for everyone. We appreciate your time and effort!
