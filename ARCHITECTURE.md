# Emacs-Agent Architecture

## Project Goal

Create a hybrid AI coding agent that combines:
- **OpenCode's agentic capabilities** (file operations, git workflows, LSP integration, multi-agent orchestration)
- **Emacs native workflow** (buffer integration, org-mode, natural editing)
- **Persistent chat transcripts** (solving Claude Code's context compression issue)

## System Overview

```
┌─────────────────────────────────────────────────────────┐
│                    Emacs Frontend                       │
│                    (emacs-agent.el)                     │
│                                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │ Agent Buffer │  │ Transcript   │  │ File Tree    │ │
│  │ (main UI)    │  │ Storage      │  │ Integration  │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │         HTTP/SSE Client (emacs-agent-api.el)     │  │
│  └──────────────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │ HTTP API + Server-Sent Events (SSE)
                       │
┌──────────────────────▼──────────────────────────────────┐
│              OpenCode Backend (Modified)                │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │              HTTP Server (Node.js)               │  │
│  │  • REST API endpoints                            │  │
│  │  • SSE for real-time events                      │  │
│  │  • Session management                            │  │
│  └──────────────────────────────────────────────────┘  │
│                         ↕                               │
│  ┌──────────────────────────────────────────────────┐  │
│  │             Event Bus (Core)                     │  │
│  │  • Message routing                               │  │
│  │  • State management                              │  │
│  │  • Event streaming                               │  │
│  └──────────────────────────────────────────────────┘  │
│                         ↕                               │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐       │
│  │   Agents   │  │   Tools    │  │    LSP     │       │
│  │            │  │            │  │            │       │
│  │ • Plan     │  │ • File ops │  │ • 14 lang  │       │
│  │ • Build    │  │ • Git      │  │   servers  │       │
│  │ • Custom   │  │ • Shell    │  │ • Live     │       │
│  │            │  │ • Search   │  │   diag.    │       │
│  └────────────┘  └────────────┘  └────────────┘       │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │        Transcript Logger (NEW)                   │  │
│  │  • Log all conversations to disk                 │  │
│  │  • Org-mode format option                        │  │
│  │  • Session resumption support                    │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## Key Architectural Decisions

### 1. Use OpenCode as Backend Foundation
**Why:**
- Mature codebase (32K+ stars, 300K+ users)
- Provider-agnostic (Claude, GPT, Gemini, local models)
- Built-in agentic capabilities (Plan/Build agents)
- LSP integration with 14 language servers
- Active development and community

**Modifications Needed:**
- Enhance HTTP API for Emacs consumption
- Add transcript logging system
- Expose more granular control over agent operations

### 2. Client/Server Architecture via HTTP + SSE
**Why:**
- OpenCode already has HTTP server (Node.js/TypeScript)
- SSE (Server-Sent Events) for real-time updates
- Separation of concerns: OpenCode = engine, Emacs = UX
- Easier to test and debug components independently

**Protocol Design:**
- HTTP REST API for commands
- SSE stream for real-time events (agent progress, file changes, diagnostics)
- JSON message format

### 3. Emacs as Natural Frontend
**Why:**
- Native buffer/window management
- Org-mode for transcript storage and organization
- Project/projectile integration
- Magit integration for git workflows
- User already lives in Emacs

**Benefits:**
- No need to recreate gptel/ellama's chat interface
- Leverage Emacs ecosystem (org-mode, magit, etc.)
- Better than running CLI agents in shell/term buffers

## Component Details

### OpenCode Backend Components

#### 1. Event Bus
- **Purpose**: Central message routing and state management
- **Events**: Agent messages, file changes, LSP diagnostics, tool calls
- **Streaming**: Events exposed via SSE to Emacs frontend

#### 2. Agent System
- **Plan Agent**: Code analysis, planning, no modifications
- **Build Agent**: Code changes, refactoring, actual edits
- **Custom Agents**: Extensible for specialized tasks

#### 3. Tool System
- **File Operations**: Read, write, edit, search
- **Git Integration**: Commits, branches, PRs
- **Shell Execution**: Run commands with safety controls
- **LSP Tools**: Diagnostics, symbol lookup, hover info

#### 4. LSP Integration
- **14 Built-in Servers**: TypeScript, Python, Go, Rust, etc.
- **Auto-spawning**: Language servers started on-demand
- **Real-time Diagnostics**: Immediate feedback on code changes
- **Event Bus Integration**: Diagnostics flow to agents

#### 5. Transcript Logger (NEW)
**Location**: `packages/core/src/transcript-logger.ts`

**Features:**
- Log every conversation turn to disk
- Support multiple formats:
  - JSON (structured)
  - Markdown (human-readable)
  - Org-mode (Emacs native)
- Session metadata (timestamp, model, project, files changed)
- Incremental writes (don't lose data on crash)

**Storage Structure:**
```
~/.opencode/transcripts/
  └── project-name/
      └── YYYY-MM-DD/
          ├── session-UUID.json
          ├── session-UUID.md
          └── session-UUID.org
```

**API:**
```typescript
interface TranscriptLogger {
  startSession(projectPath: string, config: SessionConfig): SessionId;
  logMessage(sessionId: SessionId, message: Message): void;
  logToolCall(sessionId: SessionId, tool: ToolCall): void;
  logEvent(sessionId: SessionId, event: Event): void;
  closeSession(sessionId: SessionId): void;
  resumeSession(sessionId: SessionId): SessionState;
}
```

### Emacs Frontend Components

#### 1. Core Package (emacs-agent.el)
**Purpose**: Main entry point and UI orchestration

**Functions:**
```elisp
(emacs-agent-start)           ; Start agent session
(emacs-agent-send-message)    ; Send message to agent
(emacs-agent-stop)            ; Stop agent session
(emacs-agent-resume-session)  ; Resume from transcript
```

**Buffer Management:**
- `*emacs-agent*` - Main interaction buffer
- `*emacs-agent-events*` - Event log (optional debug)
- Integration with existing project buffers

#### 2. HTTP/SSE Client (emacs-agent-api.el)
**Purpose**: Communication with OpenCode backend

**Functions:**
```elisp
(emacs-agent-api-request method endpoint data callback)
(emacs-agent-api-subscribe event-handler)
(emacs-agent-api-start-session config)
(emacs-agent-api-send-prompt text)
```

**Implementation:**
- Use `url-retrieve` for HTTP requests
- SSE via `url-http-create-request` with streaming
- JSON parsing via `json-parse-string`

#### 3. Transcript Manager (emacs-agent-transcript.el)
**Purpose**: Local transcript storage and org-mode integration

**Functions:**
```elisp
(emacs-agent-transcript-save session)
(emacs-agent-transcript-load session-id)
(emacs-agent-transcript-list)
(emacs-agent-transcript-export format)
```

**Org-Mode Format:**
```org
#+TITLE: Agent Session - Project Name
#+DATE: 2025-11-13
#+SESSION_ID: uuid-here
#+MODEL: claude-sonnet-4

* Conversation

** User [2025-11-13 10:30]
Implement user authentication

** Agent [2025-11-13 10:30:15]
I'll implement user authentication. Let me plan this:

*** Files to modify
- src/auth/login.ts
- src/middleware/auth.ts

*** Tool Calls
**** Read File: src/auth/login.ts
...

** Tool Result: Success
...
```

#### 4. Project Integration (emacs-agent-project.el)
**Purpose**: Integrate with projectile/project.el

**Functions:**
```elisp
(emacs-agent-for-project)     ; Start agent for current project
(emacs-agent-project-context) ; Send project context to agent
```

## Communication Protocol

### HTTP REST API Endpoints

```
POST   /sessions                  Create new session
GET    /sessions/:id              Get session info
DELETE /sessions/:id              End session

POST   /sessions/:id/prompt       Send user message
POST   /sessions/:id/interrupt    Interrupt agent
POST   /sessions/:id/approve      Approve tool call
POST   /sessions/:id/reject       Reject tool call

GET    /sessions/:id/transcript   Get full transcript
GET    /sessions/:id/events       SSE stream
```

### SSE Event Types

```javascript
// Agent message streaming
{
  "type": "message.delta",
  "content": "partial text...",
  "role": "agent"
}

// Tool call request
{
  "type": "tool.call",
  "tool": "edit_file",
  "params": {...},
  "requiresApproval": true
}

// File changed notification
{
  "type": "file.changed",
  "path": "/path/to/file.ts",
  "operation": "edit"
}

// LSP diagnostics
{
  "type": "lsp.diagnostics",
  "path": "/path/to/file.ts",
  "diagnostics": [...]
}

// Agent state change
{
  "type": "agent.state",
  "state": "thinking" | "using_tool" | "waiting_approval" | "idle"
}
```

## Implementation Phases

### Phase 1: Basic Integration (Week 1-2)
- [ ] Fork OpenCode repository
- [ ] Add basic HTTP API endpoints for Emacs
- [ ] Implement simple elisp HTTP client
- [ ] Create basic agent buffer UI
- [ ] Test simple prompt/response flow

### Phase 2: Transcript Logging (Week 2-3)
- [ ] Implement transcript logger in OpenCode
- [ ] Add JSON/Markdown/Org-mode formatters
- [ ] Test session save/restore
- [ ] Implement elisp transcript viewer
- [ ] Test session resumption

### Phase 3: Full Agent Features (Week 3-4)
- [ ] Add SSE streaming to Emacs
- [ ] Implement tool call approval UI
- [ ] Add file change notifications
- [ ] Integrate LSP diagnostics display
- [ ] Test Plan and Build agents

### Phase 4: Emacs Polish (Week 4-5)
- [ ] Org-mode transcript integration
- [ ] Project/projectile integration
- [ ] Magit integration for git workflows
- [ ] Add keybindings and commands
- [ ] Documentation and README

### Phase 5: Advanced Features (Week 5+)
- [ ] Multi-session management
- [ ] Custom agent definitions
- [ ] MCP server integration
- [ ] Collaborative features
- [ ] Performance optimization

## Technical Considerations

### Transcript Storage Strategy
**Requirements:**
- Never lose data (incremental writes)
- Searchable (grep, org-search)
- Resumable (restore full context)
- Portable (JSON for tools, org-mode for humans)

**Solution:**
- Write-ahead log (WAL) during session
- Flush to formatted files on save
- Keep both JSON (canonical) and org-mode (readable)

### State Management
**Emacs State:**
- Current session ID
- Agent state (idle, thinking, waiting)
- Pending tool calls
- Project context

**OpenCode State:**
- Session metadata
- Conversation history
- Tool call history
- File change tracking

### Performance
**Concerns:**
- Large transcripts (MB+)
- Real-time streaming
- Multiple concurrent sessions

**Solutions:**
- Lazy loading of old messages
- Streaming SSE with backpressure
- One session per project at a time

### Security
**Concerns:**
- Arbitrary file access
- Shell command execution
- API key storage

**Solutions:**
- OpenCode's existing sandboxing
- Tool call approval UI
- Secure credential storage in Emacs

## Technology Stack

### OpenCode Backend
- **Runtime**: Node.js/Bun + Go (TUI, but we won't use)
- **Language**: TypeScript
- **Build**: Turborepo monorepo
- **Testing**: Standard Node.js tools

### Emacs Frontend
- **Language**: Emacs Lisp
- **Dependencies**:
  - `url` (built-in HTTP)
  - `json` (built-in JSON parsing)
  - `org` (org-mode, built-in)
  - Optional: `projectile` or `project.el`
  - Optional: `magit`

## Comparison with Alternatives

### vs. gptel + extensions
| Feature | gptel | emacs-agent |
|---------|-------|-------------|
| Transcript saving | ✓ | ✓ |
| Agentic file ops | ✗ | ✓ |
| Git automation | ✗ | ✓ |
| LSP integration | ✗ | ✓ |
| Multi-agent | ✗ | ✓ |
| Maturity | High | New |

### vs. ellama
| Feature | ellama | emacs-agent |
|---------|--------|-------------|
| Task commands | ✓ | ✓ |
| Agentic workflow | ✗ | ✓ |
| Provider options | Local focus | All |
| Transcript saving | Basic | Advanced |

### vs. Running Claude Code in term
| Feature | Claude Code | emacs-agent |
|---------|-------------|-------------|
| Terminal UI | ✓ | ✗ (buffer UI) |
| Context loss | ✓ (problem!) | ✗ |
| Emacs integration | Poor | Native |
| Usability in Emacs | Low | High |

## Open Questions

1. **How to handle long-running operations?**
   - Show progress in modeline?
   - Separate "progress" buffer?
   - org-mode task list?

2. **Session persistence across Emacs restarts?**
   - Save session ID to project file?
   - Auto-resume on project open?

3. **Multiple projects?**
   - One agent per project?
   - Switch between active sessions?

4. **Approval UI for tool calls?**
   - Modal dialog (y-or-n-p)?
   - Separate approval buffer?
   - Inline in agent buffer?

5. **File change visualization?**
   - Diff buffer?
   - Highlight changed files in tree?
   - Integration with vc/magit?

## Next Steps

1. **Immediate**: Explore OpenCode codebase structure
   - Clone repository
   - Find HTTP server implementation
   - Understand event bus
   - Locate agent definitions

2. **Design**: Finalize protocol
   - Document all API endpoints
   - Define SSE event schema
   - Plan error handling

3. **Prototype**: Minimal viable integration
   - Basic HTTP client in elisp
   - Single prompt/response
   - Display in buffer

4. **Iterate**: Add features incrementally
   - Transcript logging
   - Tool calls
   - LSP integration
   - Polish UX
