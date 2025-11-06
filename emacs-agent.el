;;; emacs-agent.el --- AI agent for Emacs using Claude API -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Emacs Agent Contributors
;; Homepage: https://github.com/softwarewrighter/emacs-agent
;; Keywords: tools, ai, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Emacs Agent is a Playwright-inspired AI agent that integrates Claude (or other LLMs)
;; directly into Emacs using the Model Context Protocol (MCP).
;;
;; Instead of working in a terminal like Claude Code or Cursor, Emacs Agent gives the AI
;; direct control over Emacs through a clean, hierarchical API:
;;
;; - buffer.* - Buffer manipulation (like page in Playwright)
;; - window.* - Window management (like viewport)
;; - frame.* - Frame operations (like browser contexts)
;; - file.* - Filesystem operations
;; - editor.* - Editor-level commands
;;
;; Quick Start:
;;
;; 1. Set your API key:
;;    (setq emacs-agent-api-key "your-api-key")
;;    ;; or use environment variable ANTHROPIC_API_KEY
;;
;; 2. Start chatting:
;;    M-x emacs-agent-chat
;;
;; 3. Ask the agent to do things:
;;    "Refactor the parse-config function in utils.el"
;;    "Find all TODOs in the project"
;;    "Add error handling to the current function"
;;
;; Useful commands:
;;
;;   M-x emacs-agent-chat - Open chat buffer
;;   M-x emacs-agent-ask - Quick question (minibuffer)
;;   M-x emacs-agent-explain-region - Explain selected code
;;   M-x emacs-agent-improve-region - Improve selected code
;;   M-x emacs-agent-refactor-function - Refactor function at point
;;
;; Configuration:
;;
;;   (setq emacs-agent-api-key "sk-ant-...")
;;   (setq emacs-agent-model "claude-3-5-sonnet-20241022")
;;   (setq emacs-agent-max-tokens 4096)
;;   (setq emacs-agent-auto-approve-reads t)
;;   (setq emacs-agent-require-approval-writes t)

;;; Code:

(require 'emacs-agent-tools)
(require 'emacs-agent-client)
(require 'emacs-agent-ui)

;;; Initialization

;;;###autoload
(defun emacs-agent-setup ()
  "Initialize Emacs Agent.
Registers all MCP tools and prepares the agent for use."
  (interactive)
  (emacs-agent-tools-init)
  (message "Emacs Agent initialized. Use M-x emacs-agent-chat to start."))

;; Auto-initialize on load
(emacs-agent-tools-init)

;;; Version Information

(defun emacs-agent-version ()
  "Display Emacs Agent version information."
  (interactive)
  (message "Emacs Agent v0.1.0 - AI-powered automation for Emacs"))

;;; API Key Configuration Helper

;;;###autoload
(defun emacs-agent-set-api-key (key)
  "Set the Claude API KEY interactively."
  (interactive "sClaude API Key: ")
  (setq emacs-agent-api-key key)
  (message "API key set. Use M-x emacs-agent-chat to start."))

;;; Autoloads

;;;###autoload
(autoload 'emacs-agent-chat "emacs-agent-ui" "Open Emacs Agent chat buffer." t)

;;;###autoload
(autoload 'emacs-agent-ask "emacs-agent-ui" "Ask the agent a quick question." t)

;;;###autoload
(autoload 'emacs-agent-explain-region "emacs-agent-ui" "Explain code in region." t)

;;;###autoload
(autoload 'emacs-agent-improve-region "emacs-agent-ui" "Improve code in region." t)

;;;###autoload
(autoload 'emacs-agent-refactor-function "emacs-agent-ui" "Refactor function at point." t)

(provide 'emacs-agent)
;;; emacs-agent.el ends here
