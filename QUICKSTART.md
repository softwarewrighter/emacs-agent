# Quick Start Guide

Get up and running with Emacs Agent in 5 minutes.

## Prerequisites

1. **Emacs 27.1 or later**
   ```bash
   emacs --version
   ```

2. **Claude API Key** from Anthropic
   - Sign up at https://www.anthropic.com
   - Get your API key from the dashboard

3. **request.el package** (will be installed automatically if using package.el)

## Installation

### Option 1: Manual Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/softwarewrighter/emacs-agent.git ~/emacs-agent
   ```

2. Add to your `init.el` or `.emacs`:
   ```elisp
   (add-to-list 'load-path "~/emacs-agent")
   (require 'emacs-agent)
   ```

3. Install dependencies:
   ```elisp
   M-x package-install RET request RET
   ```

### Option 2: use-package (Recommended)

```elisp
(use-package emacs-agent
  :load-path "~/emacs-agent"
  :ensure nil
  :custom
  (emacs-agent-api-key (getenv "ANTHROPIC_API_KEY"))
  :bind
  (("C-c a c" . emacs-agent-chat)
   ("C-c a a" . emacs-agent-ask)))
```

## Configuration

### Set Your API Key

**Method 1: Environment Variable (Recommended)**

```bash
# Add to your ~/.bashrc or ~/.zshrc
export ANTHROPIC_API_KEY="sk-ant-your-api-key-here"
```

Then in your `init.el`:
```elisp
(setq emacs-agent-api-key (getenv "ANTHROPIC_API_KEY"))
```

**Method 2: Directly in init.el (Not Recommended)**

```elisp
(setq emacs-agent-api-key "sk-ant-your-api-key-here")
```

**Method 3: Secure File**

```bash
echo "sk-ant-your-api-key-here" > ~/.anthropic-api-key
chmod 600 ~/.anthropic-api-key
```

```elisp
(setq emacs-agent-api-key
      (with-temp-buffer
        (insert-file-contents "~/.anthropic-api-key")
        (string-trim (buffer-string))))
```

### Basic Configuration

Add to your `init.el`:

```elisp
;; API Key
(setq emacs-agent-api-key (getenv "ANTHROPIC_API_KEY"))

;; Safety settings
(setq emacs-agent-auto-approve-reads t)
(setq emacs-agent-require-approval-writes t)

;; Keybindings
(global-set-key (kbd "C-c a c") 'emacs-agent-chat)
(global-set-key (kbd "C-c a a") 'emacs-agent-ask)
```

## First Steps

### 1. Test the Installation

Restart Emacs or evaluate:
```elisp
M-: (emacs-agent-version)
```

You should see: `Emacs Agent v0.1.0 - AI-powered automation for Emacs`

### 2. Start a Chat

```
M-x emacs-agent-chat
```

Or press `C-c a c` (if you set the keybinding).

### 3. Try Some Commands

In the `*emacs-agent*` buffer, try:

**Simple question:**
```
What files are in the current directory?
```

**Code help:**
```
Create a function that reverses a string in Emacs Lisp
```

**Buffer manipulation:**
```
Show me a list of all open buffers
```

**File operations:**
```
Find all .el files in this directory
```

## Common Commands

### Interactive Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `emacs-agent-chat` | `C-c a c` | Open chat buffer |
| `emacs-agent-ask` | `C-c a a` | Quick question in minibuffer |
| `emacs-agent-explain-region` | `C-c a e` | Explain selected code |
| `emacs-agent-improve-region` | `C-c a i` | Improve selected code |
| `emacs-agent-refactor-function` | `C-c a r` | Refactor function at point |

### In the Chat Buffer

| Key | Action |
|-----|--------|
| `RET` or `C-c C-c` | Send message |
| `C-c C-n` | New conversation |
| `C-c C-k` | Clear buffer |

## Example Usage

### Example 1: Explain Code

1. Open a file with code
2. Select a region (mark and move point)
3. Run: `M-x emacs-agent-explain-region`
4. The agent will explain the code in the chat buffer

### Example 2: File Search

In the chat buffer:
```
Find all TODO comments in this project
```

The agent will use `file.search` to grep for TODOs.

### Example 3: Buffer Manipulation

```
In the *scratch* buffer, write a comment explaining what this buffer is for
```

The agent will:
1. Use `buffer.getText` to read the scratch buffer
2. Use `buffer.insert` to add a comment

### Example 4: Refactoring

Place cursor in a function and run:
```
M-x emacs-agent-refactor-function
```

The agent will analyze and suggest improvements.

## What the Agent Can Do

The agent has access to these **MCP tools**:

### Buffer Tools
- Read buffer contents
- Insert, replace, delete text
- Search within buffers
- Navigate to line/column
- Save buffers
- List all buffers

### Window Tools
- Split windows
- Show buffers in windows
- Close windows
- List windows

### Frame Tools
- Create frames
- Switch between frames
- List all frames

### File Tools
- Read files
- Write files
- Search in files (grep)
- List directory contents

### Editor Tools
- Execute Emacs Lisp code
- Run M-x commands
- Get editor state

## Safety Features

### Auto-Approval

By default:
- **Read operations** are auto-approved (safe)
- **Write operations** require confirmation (safer)

### Approval Prompt

When the agent wants to write or modify something, you'll see:

```
Agent wants to call buffer.insert with params:
{"text": "new content", "buffer": "test.el"}

Approve? (y/n)
```

You can customize this behavior:
```elisp
(setq emacs-agent-auto-approve-reads t)        ;; Auto-approve reads
(setq emacs-agent-require-approval-writes nil) ;; Don't ask for writes (be careful!)
```

## Troubleshooting

### "No API key configured"

Make sure you've set the API key:
```elisp
M-: emacs-agent-api-key
```

Should return your API key, not `nil`.

### "request.el not found"

Install the request package:
```
M-x package-refresh-contents
M-x package-install RET request RET
```

### API Errors

1. Check your internet connection
2. Verify API key is valid
3. Check Anthropic status: https://status.anthropic.com
4. Enable debug mode:
   ```elisp
   (setq request-log-level 'debug)
   ```

### Tool Not Working

Make sure tools are initialized:
```elisp
M-: (emacs-agent-tools-init)
```

Check available tools:
```elisp
M-: (emacs-agent-list-tools)
```

## Tips

1. **Start Simple**: Begin with read-only operations to get comfortable
2. **Use Approval**: Keep `require-approval-writes` enabled initially
3. **Be Specific**: The more specific your request, the better the result
4. **Show Context**: Mention buffer names, file paths, or describe what you're working on
5. **Iterate**: If the first attempt isn't perfect, ask the agent to refine it

## Next Steps

- Read the full [README.md](README.md) for detailed features
- Check [example-config.el](example-config.el) for advanced configuration
- See [CONTRIBUTING.md](CONTRIBUTING.md) if you want to add features
- Join discussions to share your use cases

## Example Session

```
You: List all Emacs Lisp files in the current directory

Agent: I'll search for .el files in the current directory.
[Using file.list...]
Found 4 Emacs Lisp files:
- emacs-agent.el
- emacs-agent-tools.el
- emacs-agent-client.el
- emacs-agent-ui.el

You: Read the first 20 lines of emacs-agent.el

Agent: [Using file.read...]
[Using buffer.getText...]
Here are the first 20 lines of emacs-agent.el:
[Shows content...]

You: Great! Now add a comment at the top explaining what this file does

Agent: I'll add a descriptive comment at the top of emacs-agent.el.
[Using buffer.insert...]

Approve? (y/n) y

Done! I've added a comment explaining the file's purpose.
```

## Getting Help

- **GitHub Issues**: Report bugs or request features
- **Discussions**: Ask questions, share ideas
- **Documentation**: Check README.md and code comments

Enjoy using Emacs Agent! 🚀
