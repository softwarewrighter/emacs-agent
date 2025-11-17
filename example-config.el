;;; example-config.el --- Example configuration for Emacs Agent -*- lexical-binding: t; -*-

;; This file shows example configurations for Emacs Agent.
;; Copy relevant sections to your init.el or use-package configuration.

;;; Basic Setup

;; Add to load path (adjust path as needed)
(add-to-list 'load-path "~/emacs-agent")

;; Load the package
(require 'emacs-agent)

;; Set API key (choose one method)

;; Method 1: Set directly (not recommended - use environment variable instead)
;; (setq emacs-agent-api-key "sk-ant-your-key-here")

;; Method 2: Use environment variable (recommended)
(setq emacs-agent-api-key (getenv "ANTHROPIC_API_KEY"))

;; Method 3: Read from file (make sure file is secure!)
;; (setq emacs-agent-api-key
;;       (with-temp-buffer
;;         (insert-file-contents "~/.anthropic-api-key")
;;         (string-trim (buffer-string))))

;;; Model Configuration

;; Choose Claude model (default: claude-3-5-sonnet-20241022)
(setq emacs-agent-model "claude-3-5-sonnet-20241022")

;; Other options:
;; (setq emacs-agent-model "claude-3-opus-20240229")
;; (setq emacs-agent-model "claude-3-haiku-20240307")

;; Set maximum tokens for responses
(setq emacs-agent-max-tokens 4096)

;;; Safety and Approval Settings

;; Auto-approve read-only operations (buffer.getText, file.read, etc.)
(setq emacs-agent-auto-approve-reads t)

;; Require approval for write operations (recommended for safety)
(setq emacs-agent-require-approval-writes t)

;;; UI Settings

;; Show tool calls in chat buffer
(setq emacs-agent-show-tool-calls t)

;;; Keybindings

;; Global keybindings (adjust prefix as desired)
(global-set-key (kbd "C-c a c") 'emacs-agent-chat)
(global-set-key (kbd "C-c a a") 'emacs-agent-ask)
(global-set-key (kbd "C-c a e") 'emacs-agent-explain-region)
(global-set-key (kbd "C-c a i") 'emacs-agent-improve-region)
(global-set-key (kbd "C-c a r") 'emacs-agent-refactor-function)

;;; use-package Configuration (alternative)

;; If you use use-package, here's an example configuration:

;; (use-package emacs-agent
;;   :load-path "~/emacs-agent"
;;   :init
;;   (setq emacs-agent-api-key (getenv "ANTHROPIC_API_KEY"))
;;   :custom
;;   (emacs-agent-model "claude-3-5-sonnet-20241022")
;;   (emacs-agent-max-tokens 4096)
;;   (emacs-agent-auto-approve-reads t)
;;   (emacs-agent-require-approval-writes t)
;;   (emacs-agent-show-tool-calls t)
;;   :bind
;;   (("C-c a c" . emacs-agent-chat)
;;    ("C-c a a" . emacs-agent-ask)
;;    ("C-c a e" . emacs-agent-explain-region)
;;    ("C-c a i" . emacs-agent-improve-region)
;;    ("C-c a r" . emacs-agent-refactor-function)))

;;; Advanced: Custom System Prompt

;; You can customize the system prompt to change the agent's behavior
;; (setq emacs-agent-system-prompt
;;       "You are an AI assistant specialized in Emacs Lisp and functional programming.
;; When helping users, always prefer functional, idiomatic Elisp code.
;; Use the MCP tools available to help users with their Emacs environment.")

;;; Advanced: Hook into tool execution

;; You can add hooks to run before/after tool execution
;; (add-hook 'emacs-agent-before-tool-hook
;;           (lambda (tool-name params)
;;             (message "About to call %s" tool-name)))

;;; Tips

;; 1. Keep your API key secure - never commit it to version control
;; 2. Use environment variables: export ANTHROPIC_API_KEY="your-key"
;; 3. Start with auto-approve-reads=t and require-approval-writes=t for safety
;; 4. Try the quick commands (emacs-agent-ask, emacs-agent-explain-region) first
;; 5. The agent can see and modify your buffers, files, and Emacs state - be mindful!

;;; Troubleshooting

;; If you get API errors:
;; - Check your API key is set correctly: (message emacs-agent-api-key)
;; - Verify internet connection
;; - Check Anthropic API status: https://status.anthropic.com

;; If tool calls aren't working:
;; - Make sure tools are initialized: (emacs-agent-tools-init)
;; - Check tool list: (emacs-agent-list-tools)

;; Enable debug mode:
;; (setq request-log-level 'debug)
;; (setq request-message-level 'debug)

;;; example-config.el ends here
