;;; emacs-agent-ui.el --- Chat interface for Emacs Agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Emacs Agent Contributors
;; Keywords: ui, chat, ai
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This file implements the chat interface for Emacs Agent.
;; Provides a dedicated buffer for conversing with the AI agent.

;;; Code:

(require 'emacs-agent-client)

;;; Buffer and Mode

(defvar emacs-agent-buffer-name "*emacs-agent*"
  "Name of the Emacs Agent chat buffer.")

(defvar emacs-agent-input-marker nil
  "Marker for the start of user input in the chat buffer.")

(defvar emacs-agent-pending-request nil
  "Non-nil when waiting for agent response.")

(defface emacs-agent-user-face
  '((t :foreground "#4CAF50" :weight bold))
  "Face for user messages."
  :group 'emacs-agent)

(defface emacs-agent-assistant-face
  '((t :foreground "#2196F3" :weight bold))
  "Face for assistant messages."
  :group 'emacs-agent)

(defface emacs-agent-system-face
  '((t :foreground "#FF9800" :style italic))
  "Face for system messages."
  :group 'emacs-agent)

(defface emacs-agent-tool-face
  '((t :foreground "#9C27B0" :style italic))
  "Face for tool call messages."
  :group 'emacs-agent)

;;; Mode Definition

(defvar emacs-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emacs-agent-send-input)
    (define-key map (kbd "C-c C-c") 'emacs-agent-send-input)
    (define-key map (kbd "C-c C-n") 'emacs-agent-new-conversation)
    (define-key map (kbd "C-c C-k") 'emacs-agent-clear-buffer)
    map)
  "Keymap for Emacs Agent mode.")

(define-derived-mode emacs-agent-mode fundamental-mode "Agent"
  "Major mode for chatting with Emacs Agent.

\\{emacs-agent-mode-map}"
  (setq-local emacs-agent-input-marker (point-marker))
  (setq-local emacs-agent-pending-request nil))

;;; Buffer Management

(defun emacs-agent-get-buffer ()
  "Get or create the Emacs Agent chat buffer."
  (let ((buffer (get-buffer emacs-agent-buffer-name)))
    (unless buffer
      (setq buffer (generate-new-buffer emacs-agent-buffer-name))
      (with-current-buffer buffer
        (emacs-agent-mode)
        (emacs-agent--insert-welcome)))
    buffer))

(defun emacs-agent--insert-welcome ()
  "Insert welcome message in chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "Emacs Agent - AI Assistant for Emacs\n"
                       'face 'emacs-agent-system-face))
    (insert (propertize "Type your message and press RET or C-c C-c to send.\n"
                       'face 'emacs-agent-system-face))
    (insert (propertize "C-c C-n: New conversation | C-c C-k: Clear buffer\n\n"
                       'face 'emacs-agent-system-face))
    (insert (propertize "You: " 'face 'emacs-agent-user-face))
    (setq emacs-agent-input-marker (point-marker))))

(defun emacs-agent--get-input ()
  "Get user input from the chat buffer."
  (buffer-substring-no-properties emacs-agent-input-marker (point-max)))

(defun emacs-agent--clear-input ()
  "Clear user input from the chat buffer."
  (delete-region emacs-agent-input-marker (point-max)))

(defun emacs-agent--insert-user-message (message)
  "Insert user MESSAGE into chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert message "\n\n")))

(defun emacs-agent--insert-assistant-message (message)
  "Insert assistant MESSAGE into chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "Agent: " 'face 'emacs-agent-assistant-face))
    (insert message "\n\n")
    (insert (propertize "You: " 'face 'emacs-agent-user-face))
    (setq emacs-agent-input-marker (point-marker))))

(defun emacs-agent--insert-system-message (message)
  "Insert system MESSAGE into chat buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize message 'face 'emacs-agent-system-face))
    (insert "\n")))

(defun emacs-agent--insert-tool-message (tool-name)
  "Insert tool call message for TOOL-NAME."
  (when emacs-agent-show-tool-calls
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "[Using %s...]\n" tool-name)
                         'face 'emacs-agent-tool-face)))))

;;; User Commands

;;;###autoload
(defun emacs-agent-chat ()
  "Open the Emacs Agent chat buffer."
  (interactive)
  (let ((buffer (emacs-agent-get-buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

(defun emacs-agent-send-input ()
  "Send user input to the agent."
  (interactive)
  (when emacs-agent-pending-request
    (message "Please wait for the current request to complete")
    (return))

  (let ((input (string-trim (emacs-agent--get-input))))
    (when (string-empty-p input)
      (message "Please enter a message")
      (return))

    ;; Insert user message
    (emacs-agent--insert-user-message input)
    (emacs-agent--clear-input)

    ;; Add to conversation history
    (emacs-agent-add-user-message input)

    ;; Show "thinking" message
    (emacs-agent--insert-system-message "[Agent is thinking...]")
    (setq emacs-agent-pending-request t)

    ;; Send to API
    (emacs-agent-send-message
     input
     (lambda (response tool-calls-p)
       (with-current-buffer (emacs-agent-get-buffer)
         (let ((inhibit-read-only t))
           ;; Remove "thinking" message
           (goto-char (point-max))
           (forward-line -1)
           (delete-region (point) (point-max))

           ;; Insert response
           (emacs-agent--insert-assistant-message response)
           (setq emacs-agent-pending-request nil)

           ;; Scroll to bottom
           (goto-char (point-max))
           (recenter -1)))))))

(defun emacs-agent-new-conversation ()
  "Start a new conversation, clearing history."
  (interactive)
  (when (yes-or-no-p "Start a new conversation? This will clear the history. ")
    (emacs-agent-start-conversation)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (emacs-agent--insert-welcome))
    (message "Started new conversation")))

(defun emacs-agent-clear-buffer ()
  "Clear the chat buffer but keep conversation history."
  (interactive)
  (when (yes-or-no-p "Clear the buffer? (History will be preserved) ")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Buffer cleared. Conversation history preserved.\n\n"
                         'face 'emacs-agent-system-face))
      (insert (propertize "You: " 'face 'emacs-agent-user-face))
      (setq emacs-agent-input-marker (point-marker)))
    (message "Buffer cleared")))

;;;###autoload
(defun emacs-agent-ask (prompt)
  "Ask the agent a question with PROMPT and show response in minibuffer.
For quick questions that don't need the full chat interface."
  (interactive "sAsk agent: ")
  (message "Agent is thinking...")
  (emacs-agent-send-message
   prompt
   (lambda (response tool-calls-p)
     (message "Agent: %s" response))))

;;;###autoload
(defun emacs-agent-explain-region (start end)
  "Ask the agent to explain the code in the region from START to END."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (mode (symbol-name major-mode))
         (prompt (format "Explain this %s code:\n\n%s" mode code)))
    (emacs-agent-chat)
    (with-current-buffer (emacs-agent-get-buffer)
      (goto-char (point-max))
      (insert prompt)
      (emacs-agent-send-input))))

;;;###autoload
(defun emacs-agent-improve-region (start end)
  "Ask the agent to improve the code in the region from START to END."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (mode (symbol-name major-mode))
         (prompt (format "Improve this %s code and explain your changes:\n\n%s" mode code)))
    (emacs-agent-chat)
    (with-current-buffer (emacs-agent-get-buffer)
      (goto-char (point-max))
      (insert prompt)
      (emacs-agent-send-input))))

;;;###autoload
(defun emacs-agent-refactor-function ()
  "Ask the agent to refactor the function at point."
  (interactive)
  (let* ((func-name (which-function))
         (prompt (if func-name
                    (format "Refactor the function '%s' in the current buffer to improve its code quality, readability, and maintainability." func-name)
                  "Refactor the function at point to improve its code quality.")))
    (emacs-agent-chat)
    (with-current-buffer (emacs-agent-get-buffer)
      (goto-char (point-max))
      (insert prompt)
      (emacs-agent-send-input))))

(provide 'emacs-agent-ui)
;;; emacs-agent-ui.el ends here
