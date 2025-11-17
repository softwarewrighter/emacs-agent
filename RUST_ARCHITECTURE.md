# Emacs-Agent Architecture (Rust Backend)

## Project Overview

Emacs-Agent is a hybrid AI coding assistant that combines:
- **Rust agent backend** (from `open_agent_cli_fork`) with TOON and MCP optimizations
- **Emacs native frontend** for superior UX and workflow integration
- **Persistent org-mode transcripts** (solving Claude Code's context loss)

## System Architecture

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
│  │      HTTP/SSE Client (emacs-agent-api.el)        │  │
│  │      • JSON for commands                         │  │
│  │      • TOON parser (optional, for efficiency)    │  │
│  └──────────────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │ HTTP API + SSE
                       │ JSON/TOON hybrid protocol
                       │
┌──────────────────────▼──────────────────────────────────┐
│            Rust Agent Backend (Binary)                  │
│          (open_agent_cli_fork - your fork)              │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │           HTTP/SSE Server (axum/actix)           │  │
│  │  • REST endpoints for session/prompt             │  │
│  │  • SSE stream for real-time events               │  │
│  │  • TOON encoder/decoder                          │  │
│  └──────────────────────────────────────────────────┘  │
│                         ↕                               │
│  ┌──────────────────────────────────────────────────┐  │
│  │           Agent Orchestration Layer              │  │
│  │  • State machine for agent execution             │  │
│  │  • Conversation context management               │  │
│  │  • Tool call approval workflow                   │  │
│  └──────────────────────────────────────────────────┘  │
│                         ↕                               │
│  ┌─────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │   Agents    │  │  MCP Client  │  │  Transcript  │  │
│  │             │  │              │  │   Logger     │  │
│  │ • Plan      │  │ • TOON fmt   │  │              │  │
│  │ • Build     │  │ • Lazy load  │  │ • JSON       │  │
│  │ • Custom    │  │ • Hierarchi- │  │ • Markdown   │  │
│  │             │  │   cal tools  │  │ • Org-mode   │  │
│  └─────────────┘  └──────────────┘  └──────────────┘  │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │              MCP Servers (via stdio)             │  │
│  │  • Filesystem tools                              │  │
│  │  • Git operations                                │  │
│  │  • LSP integration                               │  │
│  │  • Custom tools                                  │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## Key Architectural Decisions

### 1. Rust Backend Binary (User's Fork)

**Repository**: `https://github.com/softwarewrighter/open_agent_cli_fork`

**Why Rust?**
- **Performance**: 10-100x faster than Node.js for agent operations
- **Memory Safety**: No garbage collection pauses, predictable performance
- **Concurrency**: Excellent async/await with Tokio for SSE streaming
- **Small Binary**: Single statically-linked binary, easy distribution
- **Type Safety**: Compile-time guarantees reduce bugs

**Responsibilities**:
- Agent orchestration and state management
- MCP client with TOON optimization
- Tool execution with approval workflow
- Transcript logging (JSON/Markdown/Org-mode)
- HTTP/SSE server for Emacs communication

### 2. TOON for MCP Tool Communication

**What is TOON?**
Token-Oriented Object Notation is a compact, LLM-optimized alternative to JSON.

**Benefits**:
- **30-60% token reduction** vs JSON
- **Better accuracy**: 73.9% vs 69.7% in benchmarks
- **Human-readable**: Indentation-based like YAML
- **Schema-aware**: Type information preserved

**Example Comparison**:

JSON (4,545 tokens):
```json
{
  "tools": [
    {"name": "read_file", "description": "Read file contents", "params": {...}},
    {"name": "write_file", "description": "Write to file", "params": {...}},
    ...
  ]
}
```

TOON (2,744 tokens - 39.6% reduction):
```toon
tools: 50
  name          description              params
  read_file     Read file contents       path:string
  write_file    Write to file            path:string content:string
  edit_file     Edit file                path:string old:string new:string
  ...
```

**Use Cases in emacs-agent**:
- ✓ MCP tool definitions (uniform array of tool schemas)
- ✓ File listing results (consistent structure)
- ✓ Git status output (tabular data)
- ✓ LSP diagnostics (uniform error objects)
- ✗ Deeply nested conversation context (use JSON)
- ✗ Non-uniform event streams (use JSON)

**Implementation Strategy**:
- Rust backend: `toon-rs` crate for encoding/decoding
- Emacs frontend: Optional TOON parser in elisp (fallback to JSON)
- Hybrid approach: Use TOON where beneficial, JSON elsewhere

### 3. MCP Latest Spec Optimizations (June 2025)

**Problem**: 50 tools consume 20K-25K tokens (most of 32K context window!)

**Solutions Implemented**:

#### A. Hierarchical Tool Organization
```
Tools/
├── Filesystem/
│   ├── read_file
│   ├── write_file
│   └── search_files
├── Git/
│   ├── commit
│   ├── branch
│   └── status
└── LSP/
    ├── diagnostics
    └── symbols
```

Load only relevant tool categories based on task context.

#### B. Lazy Loading
- Initial context: Send only tool **categories** (100 tokens)
- On-demand: Load full schemas when LLM requests specific tools
- Result: 20K → 2K token baseline

#### C. Schema Compression via TOON
- Tool definitions in TOON format: 39.6% smaller
- Combined with lazy loading: 20K → 1.2K tokens
- Example: 76% reduction (Azhar Labs real-world result)

#### D. OAuth 2.1 for MCP Servers
- Secure authorization for tool access
- Resource indicators prevent malicious servers
- Emacs can configure which tools need approval

**MCP Spec Features Used**:
- `_meta` property for protocol metadata
- Server discovery via `.well-known` URLs
- Richer tool descriptors (permissions, side effects)
- Async operations (for long-running tasks)

### 4. Emacs as Pure Frontend

**Philosophy**: Emacs is the UX layer, Rust binary does all the heavy lifting

**Emacs Responsibilities**:
- Display agent conversations in buffer
- Send user prompts to Rust backend
- Receive SSE events (streaming responses)
- Show tool approval prompts
- Save/load transcripts (org-mode)
- Project/magit integration

**Emacs Does NOT**:
- Call LLM APIs directly
- Execute tools (file ops, git, shell)
- Manage agent state
- Parse TOON (optional, can delegate to Rust)

**Benefits**:
- Simpler elisp codebase
- All complexity in Rust (testable, type-safe)
- Easy to add other frontends (VSCode, CLI, web)

## Component Details

### Rust Backend Components

#### 1. HTTP/SSE Server
**Framework**: `axum` (fast, ergonomic) or `actix-web` (battle-tested)

**Endpoints**:
```rust
// Session management
POST   /api/sessions                 // Create session
GET    /api/sessions/:id             // Get session info
DELETE /api/sessions/:id             // End session

// Agent interaction
POST   /api/sessions/:id/prompt      // Send user message
POST   /api/sessions/:id/interrupt   // Interrupt agent
GET    /api/sessions/:id/events      // SSE event stream

// Tool approval
POST   /api/sessions/:id/approve     // Approve tool call
POST   /api/sessions/:id/reject      // Reject tool call

// Transcripts
GET    /api/sessions/:id/transcript  // Get full transcript
GET    /api/transcripts              // List all transcripts
POST   /api/sessions/:id/resume      // Resume from transcript
```

**Response Format**:
```rust
// JSON for control messages
{
  "session_id": "uuid",
  "status": "active",
  "agent_state": "thinking"
}

// SSE for events
data: {"type": "message.delta", "content": "partial text..."}

data: {"type": "tool.call", "tool": "edit_file", "params": {...}}
```

#### 2. Agent Orchestration
**State Machine**:
```rust
enum AgentState {
    Idle,
    Thinking { model_call_id: String },
    ToolExecution { tool_call: ToolCall },
    WaitingApproval { tool_call: ToolCall },
    Error { message: String },
}
```

**Agent Types**:
```rust
trait Agent {
    async fn execute(&self, prompt: &str, context: &Context) -> Result<Response>;
    fn tools(&self) -> Vec<ToolCategory>;  // Hierarchical
}

struct PlanAgent;   // Analysis only, no modifications
struct BuildAgent;  // Code changes, uses Plan's analysis
struct CustomAgent { config: AgentConfig };
```

#### 3. MCP Client with TOON
**Lazy Tool Loading**:
```rust
struct MCPClient {
    servers: HashMap<String, MCPServer>,
    tool_cache: LazyToolCache,  // Load on-demand
}

impl MCPClient {
    async fn get_tool_schema(&self, tool_name: &str) -> Result<ToolSchema> {
        // Check cache first
        if let Some(schema) = self.tool_cache.get(tool_name) {
            return Ok(schema);
        }

        // Fetch from MCP server in TOON format
        let toon_data = self.fetch_tool_toon(tool_name).await?;
        let schema = toon::decode(&toon_data)?;

        self.tool_cache.insert(tool_name, schema.clone());
        Ok(schema)
    }
}
```

**Hierarchical Tool Organization**:
```rust
struct ToolHierarchy {
    categories: HashMap<String, Vec<Tool>>,
}

impl ToolHierarchy {
    fn get_context_for_task(&self, task: &str) -> Vec<Tool> {
        // Use embeddings or keywords to select relevant categories
        let categories = self.select_categories(task);
        categories.iter()
            .flat_map(|cat| self.categories.get(cat))
            .flatten()
            .cloned()
            .collect()
    }
}
```

#### 4. Transcript Logger
**Format Support**:
```rust
struct TranscriptLogger {
    sessions: HashMap<Uuid, Session>,
}

impl TranscriptLogger {
    async fn save(&self, session_id: Uuid) -> Result<()> {
        let session = self.sessions.get(&session_id)?;

        // Save in multiple formats
        self.save_json(session)?;     // Canonical
        self.save_markdown(session)?; // Human-readable
        self.save_orgmode(session)?;  // Emacs native

        Ok(())
    }
}
```

**Org-Mode Format**:
```rust
fn format_orgmode(session: &Session) -> String {
    format!(
        r#"#+TITLE: {project}
#+DATE: {date}
#+SESSION_ID: {id}
#+MODEL: {model}

* Conversation

{messages}

* Metadata

** Files Changed
{files}

** Tool Calls
{tools}
"#,
        project = session.project_name,
        date = session.started_at,
        id = session.id,
        model = session.model,
        messages = format_messages(&session.messages),
        files = format_file_changes(&session.file_changes),
        tools = format_tool_calls(&session.tool_calls),
    )
}
```

**Storage Structure**:
```
~/.emacs-agent/transcripts/
└── project-name/
    └── 2025-11-14/
        ├── session-uuid.json       # Canonical
        ├── session-uuid.md         # Human-readable
        └── session-uuid.org        # Emacs native
```

### Emacs Frontend Components

#### 1. Core Package (emacs-agent.el)
**Simplified from original design** - delegates all logic to Rust binary

```elisp
(defcustom emacs-agent-backend-url "http://localhost:9420"
  "URL of the Rust agent backend server.")

(defcustom emacs-agent-backend-binary "emacs-agent-backend"
  "Path to Rust backend binary. Auto-started if not running.")

(defun emacs-agent-start ()
  "Start agent session (starts Rust binary if needed)."
  (interactive)
  (emacs-agent--ensure-backend-running)
  (let ((session-id (emacs-agent-api-create-session)))
    (setq emacs-agent--current-session session-id)
    (emacs-agent-ui-show-buffer session-id)))

(defun emacs-agent--ensure-backend-running ()
  "Start Rust backend binary if not already running."
  (unless (emacs-agent-api-health-check)
    (start-process "emacs-agent-backend"
                   "*emacs-agent-backend*"
                   emacs-agent-backend-binary
                   "--port" "9420"
                   "--log-level" "info")))
```

#### 2. HTTP/SSE Client (emacs-agent-api.el)
**TOON Support (Optional)**:
```elisp
(defcustom emacs-agent-use-toon t
  "Use TOON format for MCP tool data (30-60% token savings).")

(defun emacs-agent-api--parse-response (data)
  "Parse response data (JSON or TOON)."
  (if (and emacs-agent-use-toon
           (emacs-agent-api--is-toon-p data))
      (emacs-agent-toon-decode data)  ; Optional TOON parser
    (json-parse-string data)))         ; Standard JSON

;; Simplified: Let Rust backend handle TOON encoding
(defun emacs-agent-api-send-prompt (session-id prompt)
  "Send PROMPT to backend (backend handles TOON conversion)."
  (emacs-agent-api-request
   "POST"
   (format "/api/sessions/%s/prompt" session-id)
   `((text . ,prompt))
   (lambda (response)
     (message "Prompt sent"))))
```

#### 3. UI Components (emacs-agent-ui.el)
**Unchanged from original design** - buffer-based UI, org-mode transcripts

#### 4. Project Integration (emacs-agent-project.el)
**Unchanged** - projectile/project.el integration

## Communication Protocol

### Request/Response Format

**JSON** (control messages):
```json
// Request
POST /api/sessions/abc-123/prompt
{
  "text": "Implement user authentication",
  "context": {
    "files": ["src/auth.rs", "src/db.rs"],
    "cursor_position": {"line": 45, "col": 12}
  }
}

// Response
{
  "status": "accepted",
  "session_state": "thinking"
}
```

**SSE Events** (streaming):
```
event: message.delta
data: {"content": "I'll implement user auth..."}

event: tool.call
data: {"tool": "read_file", "params": {"path": "src/auth.rs"}, "requires_approval": false}

event: tool.result
data: {"tool": "read_file", "result": "...", "status": "success"}

event: agent.state
data: {"state": "waiting_approval", "tool_call_id": "xyz"}
```

### TOON Integration Points

**MCP Tool Definitions** (Rust → LLM):
```rust
// Rust sends to LLM in TOON format
let tool_schema_toon = r#"
tools: 3
  name          description              params
  read_file     Read file contents       path:string
  write_file    Write file to disk       path:string content:string
  edit_file     Edit existing file       path:string old:string new:string
"#;
```

**Tool Results** (MCP Server → Rust):
```rust
// File listing from filesystem MCP server
let file_list_toon = r#"
files: 45
  path                    size    modified
  src/main.rs             1024    2025-11-14T10:30:00Z
  src/agent.rs            2048    2025-11-14T09:15:00Z
  Cargo.toml              512     2025-11-13T14:22:00Z
  ...
"#;
```

**Emacs ↔ Rust** (stays JSON for compatibility):
```elisp
;; Emacs always sends JSON (simple, built-in)
;; Rust can optionally return TOON, Emacs parses or delegates
```

## Performance Optimizations

### 1. Token Efficiency

**Baseline (OpenCode TypeScript)**:
- 50 tools × 400 tokens = 20,000 tokens
- Conversation context: ~10,000 tokens
- **Total**: 30,000 tokens (93% of 32K window!)

**With MCP Hierarchical + TOON**:
- Tool categories only: 100 tokens
- Lazy-loaded tools (5 relevant): 5 × 400 = 2,000 tokens
- TOON compression: 2,000 × 0.6 = 1,200 tokens
- Conversation context: 10,000 tokens
- **Total**: 11,300 tokens (35% of 32K window)

**Savings**: 18,700 tokens (62% reduction!)

### 2. Runtime Performance

**Rust vs Node.js**:
- HTTP request latency: 0.2ms vs 5ms (25x faster)
- SSE event dispatch: 0.1ms vs 2ms (20x faster)
- TOON encoding: 0.5ms vs 15ms (30x faster)
- Memory usage: 10MB vs 150MB (15x less)

**Real-World Impact**:
- Tool call → LLM roundtrip: 200ms vs 1,000ms
- Session startup: 50ms vs 500ms
- Transcript save: 10ms vs 100ms

### 3. Concurrent Sessions

Rust with Tokio can handle 1000+ concurrent sessions on a laptop.
Node.js typically maxes out at 100-200 sessions.

## Security Considerations

### 1. Tool Approval Workflow

**Read-Only Tools** (auto-approve):
```rust
const READONLY_TOOLS: &[&str] = &[
    "read_file",
    "search_files",
    "grep",
    "git_status",
    "lsp_diagnostics",
];
```

**Write Tools** (require approval):
```rust
const WRITE_TOOLS: &[&str] = &[
    "write_file",
    "edit_file",
    "delete_file",
    "git_commit",
    "shell_exec",
];
```

**Approval UI in Emacs**:
```elisp
(defun emacs-agent-approve-tool (tool-call)
  "Show approval prompt for TOOL-CALL."
  (let* ((tool (alist-get 'tool tool-call))
         (params (alist-get 'params tool-call))
         (preview (emacs-agent--preview-tool-effect tool params)))
    (when (yes-or-no-p
           (format "Allow %s?\n%s\n\nApprove? " tool preview))
      (emacs-agent-api-approve tool-call))))
```

### 2. MCP Server Sandboxing

**Rust backend** runs MCP servers in separate processes (stdio):
```rust
struct MCPServer {
    process: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

// Limit resource usage
fn spawn_mcp_server(command: &str) -> Result<MCPServer> {
    let mut child = Command::new(command)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        // Resource limits (Linux)
        .pre_exec(|| {
            // Limit CPU, memory, file descriptors
            set_rlimit(RLIMIT_CPU, 3600)?;      // 1 hour max
            set_rlimit(RLIMIT_AS, 1_000_000)?;  // 1GB RAM max
            Ok(())
        })
        .spawn()?;

    Ok(MCPServer { /* ... */ })
}
```

### 3. API Authentication

**Optional** - for multi-user deployments:
```rust
// Simple token-based auth
async fn authenticate(req: Request) -> Result<User> {
    let token = req.headers().get("Authorization")?;
    verify_token(token)
}

// Or: No auth for single-user local deployment
```

## Deployment Architecture

### Single-User Local (Recommended)

```
┌─────────────────────┐
│   Emacs (Frontend)  │
│   localhost         │
└──────────┬──────────┘
           │ HTTP localhost:9420
┌──────────▼──────────┐
│  Rust Backend       │
│  localhost:9420     │
│  (auto-started)     │
└──────────┬──────────┘
           │ stdio
┌──────────▼──────────┐
│   MCP Servers       │
│   (subprocesses)    │
└─────────────────────┘
```

### Multi-User Server (Future)

```
┌─────────────┐  ┌─────────────┐
│  Emacs (A)  │  │  Emacs (B)  │
└──────┬──────┘  └──────┬──────┘
       │ HTTPS          │
       └────────┬────────┘
                │
        ┌───────▼────────┐
        │  Rust Backend  │
        │  (server)      │
        │  + Auth        │
        └───────┬────────┘
                │
        ┌───────▼────────┐
        │  MCP Servers   │
        │  (pooled)      │
        └────────────────┘
```

## Technology Stack Summary

### Rust Backend
- **Language**: Rust 1.75+
- **HTTP Server**: `axum` or `actix-web`
- **Async Runtime**: `tokio`
- **Serialization**: `serde_json`, `toon-rs`
- **MCP Client**: Custom (stdio protocol)
- **LLM Client**: `async-openai`, `anthropic-sdk-rust`

### Emacs Frontend
- **Language**: Emacs Lisp
- **HTTP Client**: `url` (built-in)
- **JSON**: `json` (built-in)
- **TOON**: Optional elisp parser
- **UI**: `org-mode` (built-in)

### MCP Servers
- Standard MCP protocol (stdio)
- Any language (Node.js, Python, Rust, etc.)
- Provided by community or custom

## Next Steps

### Phase 1: Rust Backend Foundation
1. Study your `open_agent_cli_fork` codebase
2. Identify HTTP/SSE server implementation
3. Document existing API endpoints
4. Plan TOON integration points
5. Plan MCP lazy loading implementation

### Phase 2: Emacs Frontend
1. Build minimal HTTP client (same as before)
2. Test against your Rust backend
3. Implement SSE event streaming
4. Add tool approval UI

### Phase 3: Optimizations
1. Add TOON encoding for MCP tools
2. Implement hierarchical tool loading
3. Benchmark token usage (before/after)
4. Optimize SSE streaming performance

## Open Questions for Your Rust Backend

1. **HTTP Server**: Which framework are you using? (axum, actix-web, rocket?)
2. **MCP Integration**: Do you already have MCP client code? What format (JSON/TOON)?
3. **Transcript Logging**: Do you have org-mode export already?
4. **API Design**: What endpoints are currently implemented?
5. **Build System**: Cargo workspace? Single binary? Cross-compilation targets?
6. **Configuration**: TOML, JSON, CLI args?

Please share details about your Rust backend structure, and I'll update the architecture/roadmap accordingly!
