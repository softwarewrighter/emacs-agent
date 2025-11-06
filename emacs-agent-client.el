;;; emacs-agent-client.el --- Claude API client for Emacs Agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Emacs Agent Contributors
;; Keywords: ai, llm, claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))

;;; Commentary:

;; This file implements the Claude API client for Emacs Agent.
;; It handles:
;; - Sending messages to Claude API
;; - Processing tool calls from the LLM
;; - Managing conversation state
;; - Streaming responses

;;; Code:

(require 'request)
(require 'json)
(require 'emacs-agent-tools)

;;; Configuration

(defgroup emacs-agent nil
  "AI agent for Emacs using Claude API."
  :group 'tools
  :prefix "emacs-agent-")

(defcustom emacs-agent-api-key nil
  "Claude API key from Anthropic.
Can also be set via ANTHROPIC_API_KEY environment variable."
  :type 'string
  :group 'emacs-agent)

(defcustom emacs-agent-model "claude-3-5-sonnet-20241022"
  "Claude model to use."
  :type 'string
  :group 'emacs-agent)

(defcustom emacs-agent-max-tokens 4096
  "Maximum tokens for Claude response."
  :type 'integer
  :group 'emacs-agent)

(defcustom emacs-agent-api-url "https://api.anthropic.com/v1/messages"
  "Claude API endpoint URL."
  :type 'string
  :group 'emacs-agent)

(defcustom emacs-agent-show-tool-calls t
  "Whether to show tool calls in the chat buffer."
  :type 'boolean
  :group 'emacs-agent)

(defcustom emacs-agent-auto-approve-reads t
  "Auto-approve read-only operations (buffer.getText, file.read, etc.)."
  :type 'boolean
  :group 'emacs-agent)

(defcustom emacs-agent-require-approval-writes t
  "Require user approval for write operations."
  :type 'boolean
  :group 'emacs-agent)

;;; Conversation State

(defvar emacs-agent-conversation-history nil
  "Current conversation history with the agent.")

(defvar emacs-agent-system-prompt
  "You are an AI assistant integrated into Emacs. You can help users with editing, coding, and managing their Emacs environment.

You have access to MCP tools that let you interact with Emacs in a Playwright-style API:
- buffer.* tools for reading and modifying buffers
- window.* tools for managing window layout
- frame.* tools for frame operations
- file.* tools for filesystem operations
- editor.* tools for executing commands and evaluating elisp

When the user asks you to do something, use these tools to accomplish the task. Always explain what you're doing and show the results.

Be concise and helpful. Focus on solving the user's problem efficiently."
  "System prompt for the agent.")

;;; Helper Functions

(defun emacs-agent--get-api-key ()
  "Get API key from config or environment."
  (or emacs-agent-api-key
      (getenv "ANTHROPIC_API_KEY")
      (error "No API key configured. Set emacs-agent-api-key or ANTHROPIC_API_KEY")))

(defun emacs-agent--format-tools-for-api ()
  "Format registered tools for Claude API."
  (let ((tools '()))
    (maphash
     (lambda (name spec)
       (let* ((params (plist-get spec :parameters))
              (properties (make-hash-table :test 'equal))
              (required '()))
         ;; Convert params to API format
         (dolist (param params)
           (let ((param-name (symbol-name (car param)))
                 (param-spec (cdr param)))
             (puthash param-name param-spec properties)
             (when (plist-get param-spec :required)
               (push param-name required))))
         (push `((name . ,name)
                (description . ,(plist-get spec :description))
                (input_schema . ((type . "object")
                               (properties . ,properties)
                               ,@(when required
                                   `((required . ,(vconcat required)))))))
               tools)))
     emacs-agent-tools)
    (vconcat (nreverse tools))))

(defun emacs-agent--should-approve-tool (tool-name)
  "Determine if tool TOOL-NAME requires user approval."
  (let ((read-only-tools '("buffer.getText" "buffer.search" "buffer.list"
                          "file.read" "file.list" "file.search"
                          "window.list" "frame.list"
                          "editor.getState")))
    (if (member tool-name read-only-tools)
        (not emacs-agent-auto-approve-reads)
      emacs-agent-require-approval-writes)))

(defun emacs-agent--request-approval (tool-name params)
  "Request user approval for calling TOOL-NAME with PARAMS.
Returns t if approved, nil otherwise."
  (let ((message (format "Agent wants to call %s with params:\n%s\n\nApprove? (y/n) "
                        tool-name
                        (json-encode params))))
    (y-or-n-p message)))

;;; API Communication

(defun emacs-agent-send-message (user-message callback)
  "Send USER-MESSAGE to Claude API and call CALLBACK with response.
CALLBACK is called with (response-text tool-calls-needed-p)."
  (let* ((api-key (emacs-agent--get-api-key))
         (tools (emacs-agent--format-tools-for-api))
         (messages (append emacs-agent-conversation-history
                          `(((role . "user")
                            (content . ,user-message)))))
         (request-data `((model . ,emacs-agent-model)
                        (max_tokens . ,emacs-agent-max-tokens)
                        (system . ,emacs-agent-system-prompt)
                        (messages . ,(vconcat messages))
                        (tools . ,tools))))

    (request emacs-agent-api-url
      :type "POST"
      :headers `(("x-api-key" . ,api-key)
                ("anthropic-version" . "2023-06-01")
                ("content-type" . "application/json"))
      :data (json-encode request-data)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (emacs-agent--process-response data callback)))
      :error (cl-function
              (lambda (&key error-thrown data &allow-other-keys)
                (message "API Error: %S" error-thrown)
                (when data
                  (message "Error data: %S" data))
                (funcall callback
                        (format "Error: %s" error-thrown)
                        nil))))))

(defun emacs-agent--process-response (response callback)
  "Process API RESPONSE and call CALLBACK.
Handles both text responses and tool use requests."
  (let* ((content (cdr (assoc 'content response)))
         (stop-reason (cdr (assoc 'stop_reason response)))
         (text-parts '())
         (tool-uses '()))

    ;; Parse content blocks
    (dotimes (i (length content))
      (let* ((block (aref content i))
             (type (cdr (assoc 'type block))))
        (cond
         ((string= type "text")
          (push (cdr (assoc 'text block)) text-parts))
         ((string= type "tool_use")
          (push block tool-uses)))))

    ;; If there are tool uses, execute them
    (if tool-uses
        (emacs-agent--execute-tool-uses
         (nreverse tool-uses)
         (lambda (tool-results)
           ;; Send tool results back to API
           (emacs-agent--send-tool-results
            response
            tool-results
            callback)))
      ;; No tool uses, just return the text
      (let ((response-text (string-join (nreverse text-parts) "\n")))
        ;; Update conversation history
        (setq emacs-agent-conversation-history
              (append emacs-agent-conversation-history
                     `(((role . "assistant")
                       (content . ,response-text)))))
        (funcall callback response-text nil)))))

(defun emacs-agent--execute-tool-uses (tool-uses callback)
  "Execute TOOL-USES and call CALLBACK with results."
  (let ((results '())
        (remaining tool-uses))
    (cl-labels ((process-next ()
                  (if (null remaining)
                      (funcall callback (nreverse results))
                    (let* ((tool-use (car remaining))
                           (tool-id (cdr (assoc 'id tool-use)))
                           (tool-name (cdr (assoc 'name tool-use)))
                           (tool-input (cdr (assoc 'input tool-use))))
                      (setq remaining (cdr remaining))

                      ;; Show tool call if configured
                      (when emacs-agent-show-tool-calls
                        (message "[Agent using %s...]" tool-name))

                      ;; Check if approval needed
                      (if (and (emacs-agent--should-approve-tool tool-name)
                              (not (emacs-agent--request-approval tool-name tool-input)))
                          ;; User denied
                          (progn
                            (push `((type . "tool_result")
                                   (tool_use_id . ,tool-id)
                                   (is_error . t)
                                   (content . "User denied tool execution"))
                                  results)
                            (process-next))
                        ;; Execute tool
                        (let* ((params (emacs-agent--convert-json-to-plist tool-input))
                               (result (emacs-agent-call-tool tool-name params)))
                          (if (plist-get result :success)
                              (push `((type . "tool_result")
                                     (tool_use_id . ,tool-id)
                                     (content . ,(json-encode (plist-get result :result))))
                                    results)
                            (push `((type . "tool_result")
                                   (tool_use_id . ,tool-id)
                                   (is_error . t)
                                   (content . ,(plist-get result :error)))
                                  results))
                          (process-next)))))))
      (process-next))))

(defun emacs-agent--send-tool-results (original-response tool-results callback)
  "Send TOOL-RESULTS back to API after ORIGINAL-RESPONSE and call CALLBACK."
  (let* ((api-key (emacs-agent--get-api-key))
         (tools (emacs-agent--format-tools-for-api))
         ;; Add original assistant message to history
         (assistant-content (cdr (assoc 'content original-response)))
         ;; Create new user message with tool results
         (messages (append emacs-agent-conversation-history
                          `(((role . "assistant")
                            (content . ,assistant-content))
                           ((role . "user")
                            (content . ,(vconcat tool-results))))))
         (request-data `((model . ,emacs-agent-model)
                        (max_tokens . ,emacs-agent-max-tokens)
                        (system . ,emacs-agent-system-prompt)
                        (messages . ,(vconcat messages))
                        (tools . ,tools))))

    (request emacs-agent-api-url
      :type "POST"
      :headers `(("x-api-key" . ,api-key)
                ("anthropic-version" . "2023-06-01")
                ("content-type" . "application/json"))
      :data (json-encode request-data)
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (emacs-agent--process-response data callback)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (funcall callback
                        (format "Error: %s" error-thrown)
                        nil))))))

(defun emacs-agent--convert-json-to-plist (json-obj)
  "Convert JSON-OBJ (alist) to plist for tool parameters."
  (let ((plist '()))
    (dolist (pair json-obj)
      (let ((key (intern (concat ":" (symbol-name (car pair)))))
            (value (cdr pair)))
        (setq plist (append plist (list key value)))))
    plist))

;;; Conversation Management

(defun emacs-agent-start-conversation ()
  "Start a new conversation, clearing history."
  (setq emacs-agent-conversation-history nil))

(defun emacs-agent-add-user-message (message)
  "Add user MESSAGE to conversation history."
  (setq emacs-agent-conversation-history
        (append emacs-agent-conversation-history
               `(((role . "user")
                 (content . ,message))))))

(defun emacs-agent-add-assistant-message (message)
  "Add assistant MESSAGE to conversation history."
  (setq emacs-agent-conversation-history
        (append emacs-agent-conversation-history
               `(((role . "assistant")
                 (content . ,message))))))

(provide 'emacs-agent-client)
;;; emacs-agent-client.el ends here
