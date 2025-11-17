# Emacs-Agent Elisp Package Structure

## Package Overview

**Package Name**: `emacs-agent`
**Main File**: `emacs-agent.el`
**Dependencies**: `url`, `json`, `org` (all built-in), optional `projectile`/`magit`

## File Structure

```
emacs-agent/
├── emacs-agent.el              # Main entry point, user commands
├── emacs-agent-api.el          # HTTP/SSE client for OpenCode backend
├── emacs-agent-transcript.el   # Transcript management & org-mode
├── emacs-agent-ui.el           # Buffer UI and display
├── emacs-agent-project.el      # Project/projectile integration
├── emacs-agent-git.el          # Git/Magit integration (optional)
└── emacs-agent-util.el         # Utility functions
```

## Module Details

### 1. emacs-agent.el (Main Entry Point)

**Purpose**: Main package file, user-facing commands, minor mode

```elisp
;;; emacs-agent.el --- AI coding agent for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: [Your Name]
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, coding
;; URL: https://github.com/yourusername/emacs-agent

;;; Commentary:

;; emacs-agent provides an AI coding agent integrated with Emacs,
;; combining OpenCode's agentic capabilities with Emacs' native workflow.
;; Features include:
;; - Persistent chat transcripts (org-mode format)
;; - Agentic file operations and git workflows
;; - LSP integration for real-time diagnostics
;; - Multi-agent orchestration (Plan/Build agents)

;;; Code:

(require 'emacs-agent-api)
(require 'emacs-agent-ui)
(require 'emacs-agent-transcript)
(require 'emacs-agent-project)
(require 'emacs-agent-util)

;;; Customization

(defgroup emacs-agent nil
  "AI coding agent for Emacs."
  :group 'tools
  :prefix "emacs-agent-")

(defcustom emacs-agent-backend-url "http://localhost:3420"
  "URL of the OpenCode backend server."
  :type 'string
  :group 'emacs-agent)

(defcustom emacs-agent-model "claude-sonnet-4.5"
  "AI model to use for agent operations."
  :type '(choice (const "claude-sonnet-4.5")
                 (const "gpt-4")
                 (const "gemini-pro")
                 (string :tag "Custom model"))
  :group 'emacs-agent)

(defcustom emacs-agent-auto-approve-readonly t
  "Automatically approve read-only tool calls (file reads, searches)."
  :type 'boolean
  :group 'emacs-agent)

(defcustom emacs-agent-transcript-directory
  (expand-file-name "emacs-agent-transcripts/" user-emacs-directory)
  "Directory to store conversation transcripts."
  :type 'directory
  :group 'emacs-agent)

;;; Session State

(defvar emacs-agent--current-session nil
  "Current agent session ID.")

(defvar emacs-agent--session-state nil
  "Current session state: idle, thinking, using-tool, waiting-approval.")

(defvar emacs-agent--pending-tool-calls nil
  "List of tool calls awaiting approval.")

;;; Main Commands

;;;###autoload
(defun emacs-agent-start ()
  "Start a new agent session for the current project."
  (interactive)
  (let* ((project-root (emacs-agent-project-root))
         (session-id (emacs-agent-api-start-session
                      :project-root project-root
                      :model emacs-agent-model)))
    (setq emacs-agent--current-session session-id)
    (emacs-agent-ui-show-buffer session-id)
    (message "Agent session started: %s" session-id)))

;;;###autoload
(defun emacs-agent-send-message (prompt)
  "Send PROMPT to the current agent session."
  (interactive "sPrompt: ")
  (unless emacs-agent--current-session
    (user-error "No active agent session. Run `emacs-agent-start' first"))
  (emacs-agent-api-send-prompt emacs-agent--current-session prompt)
  (emacs-agent-ui-insert-user-message prompt))

;;;###autoload
(defun emacs-agent-stop ()
  "Stop the current agent session."
  (interactive)
  (when emacs-agent--current-session
    (emacs-agent-api-end-session emacs-agent--current-session)
    (emacs-agent-transcript-save emacs-agent--current-session)
    (setq emacs-agent--current-session nil)
    (message "Agent session ended")))

;;;###autoload
(defun emacs-agent-resume-session ()
  "Resume a previous agent session from transcript."
  (interactive)
  (let* ((transcripts (emacs-agent-transcript-list))
         (selected (completing-read "Resume session: " transcripts))
         (session-id (emacs-agent-transcript-session-id selected)))
    (emacs-agent-api-resume-session session-id)
    (setq emacs-agent--current-session session-id)
    (emacs-agent-ui-show-buffer session-id)
    (emacs-agent-transcript-load session-id)))

;;;###autoload
(defun emacs-agent-interrupt ()
  "Interrupt the current agent operation."
  (interactive)
  (when emacs-agent--current-session
    (emacs-agent-api-interrupt emacs-agent--current-session)
    (message "Agent interrupted")))

;;;###autoload
(defun emacs-agent-approve-tool ()
  "Approve the pending tool call."
  (interactive)
  (when-let ((tool-call (car emacs-agent--pending-tool-calls)))
    (emacs-agent-api-approve-tool emacs-agent--current-session tool-call)
    (pop emacs-agent--pending-tool-calls)
    (message "Tool call approved")))

;;;###autoload
(defun emacs-agent-reject-tool ()
  "Reject the pending tool call."
  (interactive)
  (when-let ((tool-call (car emacs-agent--pending-tool-calls)))
    (emacs-agent-api-reject-tool emacs-agent--current-session tool-call)
    (pop emacs-agent--pending-tool-calls)
    (message "Tool call rejected")))

;;; Minor Mode

(defvar emacs-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'emacs-agent-start)
    (define-key map (kbd "C-c a m") #'emacs-agent-send-message)
    (define-key map (kbd "C-c a q") #'emacs-agent-stop)
    (define-key map (kbd "C-c a r") #'emacs-agent-resume-session)
    (define-key map (kbd "C-c a i") #'emacs-agent-interrupt)
    (define-key map (kbd "C-c a y") #'emacs-agent-approve-tool)
    (define-key map (kbd "C-c a n") #'emacs-agent-reject-tool)
    map)
  "Keymap for `emacs-agent-mode'.")

;;;###autoload
(define-minor-mode emacs-agent-mode
  "Minor mode for emacs-agent integration."
  :lighter " Agent"
  :keymap emacs-agent-mode-map
  :global t)

(provide 'emacs-agent)
;;; emacs-agent.el ends here
```

### 2. emacs-agent-api.el (HTTP/SSE Client)

**Purpose**: Communication with OpenCode backend via HTTP and SSE

```elisp
;;; emacs-agent-api.el --- OpenCode backend API client -*- lexical-binding: t; -*-

;;; Code:

(require 'url)
(require 'json)

;;; HTTP Client

(defun emacs-agent-api-request (method endpoint data callback)
  "Make HTTP request to OpenCode backend.
METHOD is HTTP method (GET, POST, DELETE).
ENDPOINT is API path (e.g., '/sessions').
DATA is request body (will be JSON-encoded).
CALLBACK is called with response data."
  (let* ((url (concat emacs-agent-backend-url endpoint))
         (url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (when data (encode-coding-string (json-encode data) 'utf-8))))
    (url-retrieve url
                  (lambda (status)
                    (goto-char (point-min))
                    (re-search-forward "\n\n")
                    (let ((response (json-read)))
                      (funcall callback response)))
                  nil t)))

;;; Session Management

(defun emacs-agent-api-start-session (&rest config)
  "Start new session with CONFIG.
Returns session ID."
  (let ((session-id nil))
    (emacs-agent-api-request
     "POST" "/sessions"
     config
     (lambda (response)
       (setq session-id (alist-get 'sessionId response))
       (emacs-agent-api--subscribe-events session-id)))
    ;; Block until we get session ID (simplified, use proper async in real impl)
    session-id))

(defun emacs-agent-api-end-session (session-id)
  "End SESSION-ID."
  (emacs-agent-api-request
   "DELETE"
   (format "/sessions/%s" session-id)
   nil
   (lambda (_response) (message "Session ended"))))

(defun emacs-agent-api-send-prompt (session-id prompt)
  "Send PROMPT to SESSION-ID."
  (emacs-agent-api-request
   "POST"
   (format "/sessions/%s/prompt" session-id)
   `((text . ,prompt))
   (lambda (_response) nil)))

(defun emacs-agent-api-interrupt (session-id)
  "Interrupt SESSION-ID."
  (emacs-agent-api-request
   "POST"
   (format "/sessions/%s/interrupt" session-id)
   nil
   (lambda (_response) nil)))

(defun emacs-agent-api-approve-tool (session-id tool-call)
  "Approve TOOL-CALL for SESSION-ID."
  (emacs-agent-api-request
   "POST"
   (format "/sessions/%s/approve" session-id)
   tool-call
   (lambda (_response) nil)))

(defun emacs-agent-api-reject-tool (session-id tool-call)
  "Reject TOOL-CALL for SESSION-ID."
  (emacs-agent-api-request
   "POST"
   (format "/sessions/%s/reject" session-id)
   tool-call
   (lambda (_response) nil)))

;;; SSE Event Streaming

(defvar emacs-agent-api--event-buffers (make-hash-table :test 'equal)
  "Hash table of session-id -> event stream buffer.")

(defun emacs-agent-api--subscribe-events (session-id)
  "Subscribe to SSE events for SESSION-ID."
  (let* ((url (format "%s/sessions/%s/events" emacs-agent-backend-url session-id))
         (buffer (url-retrieve url
                               #'emacs-agent-api--handle-sse-data
                               (list session-id)
                               t)))
    (puthash session-id buffer emacs-agent-api--event-buffers)))

(defun emacs-agent-api--handle-sse-data (status session-id)
  "Handle SSE data for SESSION-ID.
STATUS is url-retrieve status."
  (when (buffer-live-p (current-buffer))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (while (re-search-forward "^data: \\(.*\\)$" nil t)
      (let* ((data-str (match-string 1))
             (event (json-parse-string data-str :object-type 'alist)))
        (emacs-agent-api--dispatch-event session-id event)))))

(defun emacs-agent-api--dispatch-event (session-id event)
  "Dispatch EVENT for SESSION-ID to appropriate handler."
  (let ((type (alist-get 'type event)))
    (pcase type
      ("message.delta"
       (emacs-agent-ui-insert-agent-delta (alist-get 'content event)))
      ("tool.call"
       (emacs-agent-api--handle-tool-call session-id event))
      ("file.changed"
       (emacs-agent-ui-notify-file-changed event))
      ("lsp.diagnostics"
       (emacs-agent-ui-show-diagnostics event))
      ("agent.state"
       (setq emacs-agent--session-state (alist-get 'state event))
       (emacs-agent-ui-update-state)))))

(defun emacs-agent-api--handle-tool-call (session-id event)
  "Handle tool call EVENT for SESSION-ID."
  (if (and emacs-agent-auto-approve-readonly
           (emacs-agent-api--readonly-tool-p event))
      (emacs-agent-api-approve-tool session-id event)
    (push event emacs-agent--pending-tool-calls)
    (emacs-agent-ui-show-tool-approval-prompt event)))

(defun emacs-agent-api--readonly-tool-p (tool-call)
  "Return t if TOOL-CALL is read-only."
  (member (alist-get 'tool tool-call)
          '("read_file" "search_files" "grep")))

(provide 'emacs-agent-api)
;;; emacs-agent-api.el ends here
```

### 3. emacs-agent-ui.el (Buffer UI)

**Purpose**: Manage agent buffer display and interaction

```elisp
;;; emacs-agent-ui.el --- UI for emacs-agent -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-agent-ui-buffer-name "*emacs-agent*"
  "Name of the agent interaction buffer.")

(defvar emacs-agent-ui--current-message-start nil
  "Marker for start of current agent message.")

;;; Buffer Management

(defun emacs-agent-ui-show-buffer (session-id)
  "Show agent buffer for SESSION-ID."
  (let ((buffer (get-buffer-create emacs-agent-ui-buffer-name)))
    (with-current-buffer buffer
      (emacs-agent-ui-mode)
      (setq-local emacs-agent--current-session session-id)
      (erase-buffer)
      (emacs-agent-ui--insert-header session-id))
    (display-buffer buffer)))

(defun emacs-agent-ui--insert-header (session-id)
  "Insert header for SESSION-ID."
  (insert (propertize "Emacs Agent Session\n" 'face 'bold))
  (insert (format "Session: %s\n" session-id))
  (insert (format "Model: %s\n" emacs-agent-model))
  (insert (format "Project: %s\n\n" (emacs-agent-project-root)))
  (insert (propertize (make-string 70 ?─) 'face 'shadow) "\n\n"))

;;; Message Display

(defun emacs-agent-ui-insert-user-message (text)
  "Insert user message TEXT."
  (with-current-buffer emacs-agent-ui-buffer-name
    (goto-char (point-max))
    (insert (propertize "You: " 'face 'success))
    (insert text "\n\n")
    (setq emacs-agent-ui--current-message-start (point-marker))))

(defun emacs-agent-ui-insert-agent-delta (delta)
  "Insert agent message DELTA (streaming)."
  (with-current-buffer emacs-agent-ui-buffer-name
    (save-excursion
      (goto-char (point-max))
      (unless (get-text-property (1- (point)) 'agent-message)
        (insert (propertize "Agent: " 'face 'warning)))
      (let ((start (point)))
        (insert delta)
        (put-text-property start (point) 'agent-message t)))))

;;; Tool Call UI

(defun emacs-agent-ui-show-tool-approval-prompt (tool-call)
  "Show approval prompt for TOOL-CALL."
  (let* ((tool (alist-get 'tool tool-call))
         (params (alist-get 'params tool-call))
         (message (format "Approve tool call: %s\nParams: %S\n(y)es, (n)o, (v)iew"
                          tool params)))
    (with-current-buffer emacs-agent-ui-buffer-name
      (goto-char (point-max))
      (insert (propertize message 'face 'highlight) "\n"))))

;;; File Change Notifications

(defun emacs-agent-ui-notify-file-changed (event)
  "Notify user about file change EVENT."
  (let ((path (alist-get 'path event))
        (operation (alist-get 'operation event)))
    (message "Agent %s: %s" operation path)
    (when (get-file-buffer path)
      (with-current-buffer (get-file-buffer path)
        (revert-buffer t t t)))))

;;; Diagnostics Display

(defun emacs-agent-ui-show-diagnostics (event)
  "Show LSP diagnostics from EVENT."
  (let ((path (alist-get 'path event))
        (diagnostics (alist-get 'diagnostics event)))
    (message "Diagnostics for %s: %d issues" path (length diagnostics))))

;;; State Display

(defun emacs-agent-ui-update-state ()
  "Update UI to reflect current agent state."
  (force-mode-line-update t))

;;; Major Mode

(defvar emacs-agent-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emacs-agent-ui-send-input)
    (define-key map (kbd "C-c C-c") #'emacs-agent-send-message)
    (define-key map (kbd "C-c C-i") #'emacs-agent-interrupt)
    map)
  "Keymap for `emacs-agent-ui-mode'.")

(define-derived-mode emacs-agent-ui-mode fundamental-mode "Agent"
  "Major mode for emacs-agent interaction."
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun emacs-agent-ui-send-input ()
  "Send input from current position."
  (interactive)
  (let ((input (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (emacs-agent-send-message input)))

(provide 'emacs-agent-ui)
;;; emacs-agent-ui.el ends here
```

### 4. emacs-agent-transcript.el (Transcript Management)

**Purpose**: Save, load, and manage conversation transcripts

```elisp
;;; emacs-agent-transcript.el --- Transcript management -*- lexical-binding: t; -*-

;;; Code:

(require 'org)

;;; Transcript Storage

(defun emacs-agent-transcript-save (session-id)
  "Save transcript for SESSION-ID."
  (let* ((transcript (emacs-agent-api-get-transcript session-id))
         (project-name (emacs-agent-project-name))
         (date-dir (format-time-string "%Y-%m-%d"))
         (dir (expand-file-name
               (format "%s/%s/" project-name date-dir)
               emacs-agent-transcript-directory))
         (org-file (expand-file-name (format "%s.org" session-id) dir)))
    (make-directory dir t)
    (emacs-agent-transcript--write-org transcript org-file)
    (message "Transcript saved: %s" org-file)))

(defun emacs-agent-transcript-load (session-id)
  "Load transcript for SESSION-ID into current buffer."
  (let ((transcript (emacs-agent-api-get-transcript session-id)))
    (with-current-buffer emacs-agent-ui-buffer-name
      (erase-buffer)
      (emacs-agent-transcript--render-transcript transcript))))

(defun emacs-agent-transcript-list ()
  "List all available transcripts."
  (let ((files (directory-files-recursively
                emacs-agent-transcript-directory
                "\\.org$")))
    (mapcar #'file-name-base files)))

;;; Org-Mode Formatting

(defun emacs-agent-transcript--write-org (transcript file)
  "Write TRANSCRIPT to FILE in org-mode format."
  (with-temp-file file
    (insert "#+TITLE: " (alist-get 'projectName transcript) "\n")
    (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M") "\n")
    (insert "#+SESSION_ID: " (alist-get 'sessionId transcript) "\n")
    (insert "#+MODEL: " (alist-get 'model transcript) "\n\n")
    (insert "* Conversation\n\n")
    (dolist (message (alist-get 'messages transcript))
      (emacs-agent-transcript--write-message message))))

(defun emacs-agent-transcript--write-message (message)
  "Write MESSAGE to current buffer."
  (let ((role (alist-get 'role message))
        (content (alist-get 'content message))
        (timestamp (alist-get 'timestamp message)))
    (insert (format "** %s [%s]\n"
                    (capitalize (symbol-name role))
                    timestamp))
    (insert content "\n\n")
    (when-let ((tool-calls (alist-get 'toolCalls message)))
      (insert "*** Tool Calls\n")
      (dolist (tool-call tool-calls)
        (insert (format "**** %s\n" (alist-get 'tool tool-call)))
        (insert "#+BEGIN_SRC json\n")
        (insert (json-encode (alist-get 'params tool-call)))
        (insert "\n#+END_SRC\n\n")))))

(provide 'emacs-agent-transcript)
;;; emacs-agent-transcript.el ends here
```

### 5. emacs-agent-project.el (Project Integration)

**Purpose**: Integrate with Emacs project management

```elisp
;;; emacs-agent-project.el --- Project integration -*- lexical-binding: t; -*-

;;; Code:

(require 'project nil t)

(defun emacs-agent-project-root ()
  "Get current project root."
  (or (when (fboundp 'projectile-project-root)
        (projectile-project-root))
      (when (fboundp 'project-root)
        (project-root (project-current)))
      default-directory))

(defun emacs-agent-project-name ()
  "Get current project name."
  (file-name-nondirectory
   (directory-file-name (emacs-agent-project-root))))

(defun emacs-agent-project-files ()
  "Get list of files in current project."
  (or (when (fboundp 'projectile-project-files)
        (projectile-project-files (emacs-agent-project-root)))
      (when (fboundp 'project-files)
        (project-files (project-current)))
      (directory-files-recursively (emacs-agent-project-root) ".*")))

(provide 'emacs-agent-project)
;;; emacs-agent-project.el ends here
```

## Installation Instructions (Future)

### For Users

```elisp
;; Add to init.el:
(use-package emacs-agent
  :ensure t
  :config
  (setq emacs-agent-backend-url "http://localhost:3420")
  (setq emacs-agent-model "claude-sonnet-4.5")
  (emacs-agent-mode 1)
  :bind-keymap
  ("C-c a" . emacs-agent-mode-map))
```

### Development Setup

```bash
# Clone repository
git clone https://github.com/yourusername/emacs-agent
cd emacs-agent

# Start OpenCode backend
npm install
npm run dev  # Starts server on :3420

# Load elisp package in Emacs
M-x load-file RET emacs-agent.el RET
M-x emacs-agent-mode RET
```

## Testing Strategy

### Unit Tests
- API client functions (HTTP requests)
- Transcript parsing/formatting
- Project detection

### Integration Tests
- Full session flow (start → prompt → tool → end)
- SSE event streaming
- Transcript save/restore

### Manual Tests
- UI responsiveness
- File change notifications
- Tool approval workflow
- Multi-file edits

## Future Enhancements

1. **Custom Agents**: Define agents in elisp
2. **Inline Diff View**: Show file changes in-place
3. **Org-Babel Integration**: Execute code blocks via agent
4. **TRAMP Support**: Remote project support
5. **Multi-Session**: Multiple concurrent agents
6. **Voice Input**: Whisper API integration
7. **Collaborative**: Share sessions via URL

## Dependencies Summary

### Required
- Emacs 27.1+
- `url` (built-in)
- `json` (built-in)
- `org` (built-in)

### Optional
- `projectile` or `project.el` (project management)
- `magit` (git integration)
- `markdown-mode` (for markdown transcript viewing)

### External
- OpenCode backend (Node.js/Bun server)
