# Emacs-Agent Implementation Roadmap

## Overview

This roadmap outlines the step-by-step implementation plan for emacs-agent, a hybrid AI coding assistant that combines OpenCode's agentic capabilities with Emacs' native workflow.

**Goal**: Production-ready Emacs package within 5-6 weeks
**Approach**: Iterative development with working prototypes at each phase

---

## Phase 1: Foundation & Basic Integration (Week 1-2)

### Objectives
- Fork and understand OpenCode codebase
- Establish communication between Emacs and OpenCode
- Build minimal working prototype (prompt → response)

### Tasks

#### 1.1 OpenCode Setup (Days 1-2)
- [ ] Fork `sst/opencode` repository
- [ ] Study codebase structure:
  - [ ] Locate HTTP server implementation (`packages/*/server.ts`)
  - [ ] Understand event bus system
  - [ ] Find agent definitions (Plan/Build)
  - [ ] Review tool system architecture
- [ ] Get OpenCode running locally
- [ ] Test existing functionality (CLI TUI)
- [ ] Document current API endpoints

**Deliverable**: Working OpenCode instance, architecture notes

#### 1.2 OpenCode HTTP API Extension (Days 3-5)
- [ ] Design API endpoints for Emacs:
  ```
  POST   /sessions
  DELETE /sessions/:id
  POST   /sessions/:id/prompt
  GET    /sessions/:id/events (SSE)
  ```
- [ ] Implement session management endpoints
- [ ] Add CORS support for local development
- [ ] Test with `curl`/Postman
- [ ] Document API with examples

**Deliverable**: HTTP API that accepts prompts, returns responses

#### 1.3 Basic Elisp Client (Days 6-8)
- [ ] Create `emacs-agent-api.el`:
  - [ ] HTTP request function using `url-retrieve`
  - [ ] JSON encoding/decoding helpers
  - [ ] `emacs-agent-api-start-session`
  - [ ] `emacs-agent-api-send-prompt`
- [ ] Create `emacs-agent.el`:
  - [ ] `emacs-agent-start` command
  - [ ] `emacs-agent-send-message` command
  - [ ] `emacs-agent-stop` command
- [ ] Test basic flow: start session → send prompt → receive response

**Deliverable**: Minimal elisp package that can send/receive messages

#### 1.4 Simple UI (Days 9-10)
- [ ] Create `emacs-agent-ui.el`:
  - [ ] Agent buffer creation
  - [ ] Display user messages
  - [ ] Display agent responses (no streaming yet)
- [ ] Add basic styling (faces for user/agent)
- [ ] Test complete user flow

**Deliverable**: Working prototype with simple UI

**Milestone 1**: ✓ Can start session, send prompt, see response in Emacs buffer

---

## Phase 2: Transcript Logging & Persistence (Week 2-3)

### Objectives
- Implement persistent transcript storage (solve Claude Code's context loss issue)
- Support session save/restore
- Org-mode format for Emacs users

### Tasks

#### 2.1 OpenCode Transcript Logger (Days 11-13)
- [ ] Create `packages/core/src/transcript-logger.ts`:
  - [ ] Session metadata structure
  - [ ] Message logging interface
  - [ ] Tool call logging
  - [ ] Event logging
- [ ] Implement incremental writes (don't lose data on crash)
- [ ] Add storage directory structure:
  ```
  ~/.opencode/transcripts/
    └── project-name/
        └── YYYY-MM-DD/
            └── session-UUID.json
  ```
- [ ] Integrate with event bus (log all events)
- [ ] Test crash recovery (kill process, verify saved data)

**Deliverable**: OpenCode saves all conversations to JSON files

#### 2.2 Multiple Format Support (Days 14-15)
- [ ] Add Markdown formatter:
  - [ ] Human-readable conversation format
  - [ ] Code blocks for tool calls
  - [ ] Metadata header
- [ ] Add Org-mode formatter:
  - [ ] Org headings for messages
  - [ ] Source blocks for code
  - [ ] Org properties for metadata
  - [ ] Timestamps
- [ ] Save all three formats (JSON, MD, Org) simultaneously
- [ ] Test format conversion and readability

**Deliverable**: Transcripts saved in JSON, Markdown, and Org-mode formats

#### 2.3 Session Resumption (Days 16-17)
- [ ] OpenCode API endpoint: `POST /sessions/resume`
  - [ ] Load transcript from disk
  - [ ] Restore conversation context
  - [ ] Resume agent state
- [ ] Test resuming mid-conversation
- [ ] Handle edge cases (partial transcripts, corrupt files)

**Deliverable**: OpenCode can resume sessions from transcript files

#### 2.4 Elisp Transcript Integration (Days 18-20)
- [ ] Create `emacs-agent-transcript.el`:
  - [ ] `emacs-agent-transcript-save`
  - [ ] `emacs-agent-transcript-load`
  - [ ] `emacs-agent-transcript-list`
  - [ ] `emacs-agent-transcript-export`
- [ ] Implement transcript viewer:
  - [ ] Browse past sessions
  - [ ] Search within transcripts
  - [ ] Jump to specific messages
- [ ] Add `emacs-agent-resume-session` command
- [ ] Test save/resume workflow

**Deliverable**: Emacs can list, view, and resume past sessions

**Milestone 2**: ✓ All conversations permanently saved, can resume any session

---

## Phase 3: Full Agent Features (Week 3-4)

### Objectives
- Enable agentic tool calls (file operations, git, shell)
- Add tool approval UI
- Implement real-time streaming (SSE)
- Integrate LSP diagnostics

### Tasks

#### 3.1 Server-Sent Events (SSE) Streaming (Days 21-23)
- [ ] OpenCode: Implement SSE endpoint `/sessions/:id/events`
  - [ ] Stream event bus events over HTTP
  - [ ] Handle client reconnection
  - [ ] Backpressure management
- [ ] Define event types:
  - [ ] `message.delta` (streaming text)
  - [ ] `tool.call` (tool execution request)
  - [ ] `file.changed` (file modification)
  - [ ] `lsp.diagnostics` (language server feedback)
  - [ ] `agent.state` (thinking, idle, etc.)
- [ ] Test SSE with `curl` or browser
- [ ] Implement keep-alive mechanism

**Deliverable**: OpenCode streams events in real-time

#### 3.2 Elisp SSE Client (Days 24-25)
- [ ] Extend `emacs-agent-api.el`:
  - [ ] SSE connection via `url-retrieve`
  - [ ] Parse SSE format (`data: {...}`)
  - [ ] Event dispatcher
  - [ ] Reconnection logic
- [ ] Update `emacs-agent-ui.el`:
  - [ ] Streaming message display (append deltas)
  - [ ] Real-time state updates in modeline
- [ ] Test streaming conversation

**Deliverable**: Emacs displays streaming agent responses

#### 3.3 Tool Call System (Days 26-28)
- [ ] OpenCode: Add tool approval endpoints:
  - [ ] `POST /sessions/:id/approve`
  - [ ] `POST /sessions/:id/reject`
- [ ] OpenCode: Pause agent execution on tool calls requiring approval
- [ ] Document which tools need approval:
  - [ ] Read-only: auto-approve (file reads, searches)
  - [ ] Writes: require approval (edits, creates, deletes)
  - [ ] Git: require approval (commits, pushes)
  - [ ] Shell: require approval (command execution)
- [ ] Test tool approval flow

**Deliverable**: Tool calls pause and wait for approval

#### 3.4 Elisp Tool Approval UI (Days 29-31)
- [ ] `emacs-agent-ui.el`: Tool approval prompt
  - [ ] Display tool name and parameters
  - [ ] Preview file diffs (for edit operations)
  - [ ] Options: Yes, No, View details
- [ ] Add commands:
  - [ ] `emacs-agent-approve-tool`
  - [ ] `emacs-agent-reject-tool`
  - [ ] `emacs-agent-view-tool-details`
- [ ] Implement auto-approve for read-only operations
- [ ] Test full tool workflow

**Deliverable**: User can approve/reject tool calls from Emacs

#### 3.5 File Change Notifications (Days 32-33)
- [ ] OpenCode: Emit `file.changed` events when tools modify files
- [ ] Emacs: Handle file change notifications:
  - [ ] Auto-revert affected buffers
  - [ ] Highlight changed files in project view
  - [ ] Show summary of changes
- [ ] Test multi-file edits

**Deliverable**: Emacs updates buffers when agent modifies files

#### 3.6 LSP Diagnostics Integration (Days 34-35)
- [ ] OpenCode: Ensure LSP diagnostics flow through event bus
- [ ] Emacs: Display diagnostics from agent:
  - [ ] Show errors/warnings in agent buffer
  - [ ] Optional: Integrate with flycheck/flymake
  - [ ] Highlight problematic code
- [ ] Test LSP feedback loop (agent makes change → sees error → fixes it)

**Deliverable**: Emacs shows LSP diagnostics during agent operations

**Milestone 3**: ✓ Full agentic workflow with tools, approvals, and live feedback

---

## Phase 4: Emacs Polish & Integration (Week 4-5)

### Objectives
- Integrate with Emacs ecosystem (project.el, magit)
- Improve UI/UX
- Add keybindings and commands
- Write documentation

### Tasks

#### 4.1 Project Integration (Days 36-37)
- [ ] Create `emacs-agent-project.el`:
  - [ ] Auto-detect project root (projectile, project.el, or git)
  - [ ] `emacs-agent-for-project` command
  - [ ] Project context in session
- [ ] Add project-specific transcript storage
- [ ] Test with various project types

**Deliverable**: Agent automatically works with current project

#### 4.2 Git/Magit Integration (Days 38-39)
- [ ] Create `emacs-agent-git.el`:
  - [ ] Intercept git tool calls
  - [ ] Show diff before committing
  - [ ] Optional: Use magit for git UI
- [ ] Add `emacs-agent-review-commit` command
- [ ] Test git workflows (commit, branch, PR)

**Deliverable**: Git operations integrate nicely with Emacs/magit

#### 4.3 UI Improvements (Days 40-42)
- [ ] Better message formatting:
  - [ ] Syntax highlighting for code blocks
  - [ ] Clickable file paths
  - [ ] Collapsible tool call details
- [ ] Add progress indicators:
  - [ ] Spinner in modeline
  - [ ] Progress messages
  - [ ] ETA for long operations
- [ ] Improve tool approval UI:
  - [ ] Inline diff view
  - [ ] Side-by-side comparison
- [ ] Add faces/themes for better visuals
- [ ] Test UI with various terminal/GUI Emacs

**Deliverable**: Polished, professional UI

#### 4.4 Keybindings & Commands (Days 43-44)
- [ ] Define sensible default keybindings:
  ```
  C-c a s  emacs-agent-start
  C-c a m  emacs-agent-send-message
  C-c a q  emacs-agent-stop
  C-c a r  emacs-agent-resume-session
  C-c a i  emacs-agent-interrupt
  C-c a y  emacs-agent-approve-tool
  C-c a n  emacs-agent-reject-tool
  C-c a t  emacs-agent-view-transcripts
  C-c a p  emacs-agent-for-project
  ```
- [ ] Add menu-bar entries
- [ ] Add `which-key` descriptions
- [ ] Document keybindings in README

**Deliverable**: Easy-to-use keyboard interface

#### 4.5 Documentation (Days 45-47)
- [ ] Write comprehensive README:
  - [ ] Installation instructions
  - [ ] Quick start guide
  - [ ] Configuration options
  - [ ] Usage examples
  - [ ] Troubleshooting
- [ ] Create user manual (Org/Info file):
  - [ ] Detailed command reference
  - [ ] Workflow tutorials
  - [ ] Advanced features
- [ ] Add docstrings to all functions
- [ ] Create demo video/GIF
- [ ] Write blog post announcing project

**Deliverable**: Complete documentation for users

#### 4.6 Configuration System (Days 48-49)
- [ ] Customization group:
  - [ ] `emacs-agent-backend-url`
  - [ ] `emacs-agent-model`
  - [ ] `emacs-agent-auto-approve-readonly`
  - [ ] `emacs-agent-transcript-directory`
  - [ ] `emacs-agent-keybindings`
- [ ] Support multiple backends (different OpenCode instances)
- [ ] Profile system (work vs. personal projects)
- [ ] Test configuration reload

**Deliverable**: Flexible configuration system

**Milestone 4**: ✓ Production-ready Emacs package with docs

---

## Phase 5: Advanced Features & Optimization (Week 5+)

### Objectives
- Advanced features for power users
- Performance optimization
- Community feedback integration

### Tasks

#### 5.1 Multi-Session Management (Days 50-52)
- [ ] Support multiple concurrent sessions
- [ ] Session switcher UI
- [ ] Session isolation (no cross-talk)
- [ ] Test with multiple projects

**Deliverable**: Work on multiple projects simultaneously

#### 5.2 Custom Agent Definitions (Days 53-55)
- [ ] Allow users to define custom agents in elisp:
  ```elisp
  (emacs-agent-define-agent 'my-reviewer
    :prompt "You are a code reviewer..."
    :tools '(read_file grep)
    :auto-approve-tools t)
  ```
- [ ] Agent template system
- [ ] Share agents as elisp packages
- [ ] Test custom agent creation

**Deliverable**: Extensible agent system

#### 5.3 MCP Server Integration (Days 56-58)
- [ ] Expose MCP server configuration to Emacs
- [ ] Add MCP servers via elisp:
  ```elisp
  (emacs-agent-add-mcp-server
   "filesystem" "npx" "-y" "@modelcontextprotocol/server-filesystem")
  ```
- [ ] Test with various MCP servers

**Deliverable**: MCP integration working from Emacs

#### 5.4 Performance Optimization (Days 59-61)
- [ ] Profile Emacs package:
  - [ ] Identify slow functions
  - [ ] Optimize JSON parsing
  - [ ] Lazy load large transcripts
- [ ] Profile OpenCode backend:
  - [ ] SSE streaming performance
  - [ ] Transcript write performance
- [ ] Benchmark improvements
- [ ] Test with large projects (100K+ files)

**Deliverable**: Fast, responsive agent even on large projects

#### 5.5 Advanced UI Features (Days 62-65)
- [ ] Inline diff view (show changes without leaving agent buffer)
- [ ] Side-by-side code comparison
- [ ] File tree showing changed files
- [ ] Org-babel integration (run code blocks via agent)
- [ ] Voice input (Whisper API)
- [ ] Test advanced features

**Deliverable**: Power user features

#### 5.6 Collaborative Features (Days 66-68)
- [ ] Share session via URL (optional cloud sync)
- [ ] Export session for review
- [ ] Multi-user sessions (pair programming)
- [ ] Test collaboration

**Deliverable**: Collaborative coding capabilities

#### 5.7 Testing & CI (Days 69-70)
- [ ] Write unit tests:
  - [ ] API client functions
  - [ ] Transcript parsing
  - [ ] Event handling
- [ ] Write integration tests:
  - [ ] Full session flow
  - [ ] Tool approval workflow
- [ ] Set up CI (GitHub Actions):
  - [ ] Run tests on push
  - [ ] Build package
  - [ ] Lint elisp code
- [ ] Achieve >80% test coverage

**Deliverable**: Comprehensive test suite

**Milestone 5**: ✓ Production-ready with advanced features and tests

---

## Release Plan

### Alpha Release (End of Phase 1, Week 2)
- Basic prompt/response functionality
- Limited distribution (GitHub, personal use)
- Goal: Gather initial feedback

### Beta Release (End of Phase 3, Week 4)
- Full agent features (tools, approvals, streaming)
- Transcript persistence
- Wider distribution (Reddit, HN, Emacs community)
- Goal: Identify bugs, UX issues

### v1.0 Release (End of Phase 4, Week 5)
- Production-ready
- Complete documentation
- MELPA submission
- Official announcement (blog, social media)
- Goal: Public adoption

### v1.1+ (Phase 5 and beyond)
- Advanced features based on feedback
- Performance improvements
- Community-driven development

---

## Success Metrics

### Technical Metrics
- [ ] 100% of conversation history preserved (no context loss)
- [ ] Response latency <2s for simple prompts
- [ ] Support projects up to 100K files
- [ ] <1% crash rate
- [ ] >80% test coverage

### User Metrics
- [ ] 100+ GitHub stars within first month
- [ ] 10+ community contributors
- [ ] 1000+ MELPA downloads in first quarter
- [ ] Positive feedback on Reddit/HN
- [ ] Featured in Emacs newsletters

### Feature Completeness
- [ ] All core features from roadmap implemented
- [ ] Comprehensive documentation
- [ ] Works with major Emacs distributions (Doom, Spacemacs, vanilla)
- [ ] Supports major OSes (Linux, macOS, Windows)

---

## Risk Mitigation

### Risk: OpenCode architecture changes
**Mitigation**: Fork OpenCode, maintain our own version if needed

### Risk: API rate limits
**Mitigation**: Support multiple providers, local models

### Risk: Emacs compatibility issues
**Mitigation**: Test on multiple Emacs versions (27.1+), distributions

### Risk: Performance issues with large projects
**Mitigation**: Lazy loading, incremental processing, profiling early

### Risk: User adoption
**Mitigation**: Excellent docs, demos, community engagement

---

## Development Setup

### Prerequisites
- Emacs 27.1+
- Node.js 18+ or Bun
- Git
- OpenAI/Anthropic API key (or local model)

### Getting Started

```bash
# 1. Fork and clone OpenCode
git fork sst/opencode
git clone https://github.com/yourusername/opencode
cd opencode
npm install

# 2. Clone emacs-agent
git clone https://github.com/yourusername/emacs-agent
cd emacs-agent

# 3. Start OpenCode backend
cd ../opencode
npm run dev  # Starts on :3420

# 4. Load Emacs package
emacs -Q
M-x load-file RET /path/to/emacs-agent/emacs-agent.el RET
M-x emacs-agent-start RET
```

---

## Next Immediate Steps

1. **Today**: Fork OpenCode, clone locally
2. **Tomorrow**: Study OpenCode codebase, identify HTTP server location
3. **This Week**: Implement basic HTTP API endpoints
4. **Next Week**: Build minimal Emacs client

Let's start with Phase 1! 🚀
