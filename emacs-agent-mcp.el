;;; emacs-agent-mcp.el --- MCP-compliant Emacs agent -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs agent following Model Context Protocol best practices

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;; Agent structure
(cl-defstruct emacs-agent-mcp
  context
  tools
  output-dir)

;; MCP-compliant system prompt
(defun emacs-agent-mcp-system-prompt ()
  "Return MCP-style system prompt."
  "You are an AI assistant with access to Emacs tools. Available tools will be provided in the function declarations. Use tools when they would help accomplish the user's request. You can:
- Manipulate buffers and files
- Navigate directories
- Read and write content
- Execute Emacs operations

Analyze each user request and decide whether tools would be helpful. If a user mentions a specific tool, consider whether it's the right tool for the task.")

;; MCP-compliant tool definitions with proper schemas
(defun emacs-agent-mcp-tool-definitions ()
  "Return MCP-compliant tool definitions."
  (vector
   ;; file_write tool
   (list (cons 'name "file_write")
         (cons 'description "Write content to a file. Creates the file if it doesn't exist, overwrites if it does.")
         (cons 'parameters 
               (list (cons 'type "object")
                     (cons 'properties 
                           (list (cons 'filename 
                                       (list (cons 'type "string")
                                             (cons 'description "Path to the file to write")))
                                 (cons 'content
                                       (list (cons 'type "string")
                                             (cons 'description "Content to write to the file")))))
                     (cons 'required (vector "filename" "content")))))
   
   ;; file_read tool
   (list (cons 'name "file_read")
         (cons 'description "Read the contents of a file")
         (cons 'parameters
               (list (cons 'type "object")
                     (cons 'properties
                           (list (cons 'filename
                                       (list (cons 'type "string")
                                             (cons 'description "Path to the file to read")))))
                     (cons 'required (vector "filename")))))
   
   ;; buffer_create tool
   (list (cons 'name "buffer_create")
         (cons 'description "Create a new Emacs buffer with specified content")
         (cons 'parameters
               (list (cons 'type "object")
                     (cons 'properties
                           (list (cons 'buffer_name
                                       (list (cons 'type "string")
                                             (cons 'description "Name for the new buffer (e.g., *test*)")))
                                 (cons 'content
                                       (list (cons 'type "string")
                                             (cons 'description "Initial content for the buffer")))))
                     (cons 'required (vector "buffer_name" "content")))))
   
   ;; buffer_append tool
   (list (cons 'name "buffer_append")
         (cons 'description "Append text to an existing buffer")
         (cons 'parameters
               (list (cons 'type "object")
                     (cons 'properties
                           (list (cons 'buffer_name
                                       (list (cons 'type "string")
                                             (cons 'description "Name of the buffer to append to")))
                                 (cons 'text
                                       (list (cons 'type "string")
                                             (cons 'description "Text to append")))))
                     (cons 'required (vector "buffer_name" "text")))))
   
   ;; list_files tool
   (list (cons 'name "list_files")
         (cons 'description "List files in a directory")
         (cons 'parameters
               (list (cons 'type "object")
                     (cons 'properties
                           (list (cons 'directory
                                       (list (cons 'type "string")
                                             (cons 'description "Directory path (defaults to current)")))))
                     (cons 'required (vector)))))))

;; Tool execution functions
(defun emacs-agent-mcp-execute-tool (name args output-dir)
  "Execute tool NAME with ARGS, using OUTPUT-DIR for files."
  (cond
   ;; file_write
   ((string= name "file_write")
    (let* ((filename (alist-get 'filename args))
           (content (alist-get 'content args))
           (full-path (if output-dir
                          (expand-file-name filename output-dir)
                        filename)))
      (when (file-name-directory full-path)
        (make-directory (file-name-directory full-path) t))
      (with-temp-file full-path
        (insert content))
      (list :status "success" 
            :message (format "File written: %s" full-path))))
   
   ;; file_read
   ((string= name "file_read")
    (let ((filename (alist-get 'filename args)))
      (if (file-exists-p filename)
          (with-temp-buffer
            (insert-file-contents filename)
            (list :status "success"
                  :content (buffer-string)))
        (list :status "error"
              :message "File not found"))))
   
   ;; buffer_create
   ((string= name "buffer_create")
    (let ((buffer-name (alist-get 'buffer_name args))
          (content (alist-get 'content args)))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert content))
      (list :status "success"
            :message (format "Buffer %s created" buffer-name))))
   
   ;; buffer_append
   ((string= name "buffer_append")
    (let ((buffer-name (alist-get 'buffer_name args))
          (text (alist-get 'text args)))
      (if (get-buffer buffer-name)
          (progn
            (with-current-buffer buffer-name
              (goto-char (point-max))
              (insert text))
            (list :status "success"
                  :message (format "Appended to %s" buffer-name)))
        (list :status "error"
              :message (format "Buffer %s not found" buffer-name)))))
   
   ;; list_files
   ((string= name "list_files")
    (let* ((directory (or (alist-get 'directory args) "."))
           (files (directory-files directory nil "^[^.]")))
      (list :status "success"
            :files files)))
   
   ;; Unknown tool
   (t
    (list :status "error"
          :message (format "Unknown tool: %s" name)))))

;; Send request to Gemini with MCP-style tools
(defun emacs-agent-mcp-call-llm (prompt context output-dir)
  "Send PROMPT to LLM with CONTEXT and MCP tools."
  (let* ((api-key (getenv "GEMINI_API_KEY"))
         (url (format "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent?key=%s" api-key))
         
         ;; ALWAYS include system prompt with tools info in EVERY request
         ;; LLMs are stateless - they don't remember previous context
         (system-message (format "%s\n\nYou have access to tools that you can use to help fulfill requests. When appropriate, use the available tools." 
                                 (emacs-agent-mcp-system-prompt)))
         (full-prompt (format "%s\n\nUser: %s" system-message prompt))
         
         ;; Single message with complete context
         (messages (vector (list (cons 'role "user")
                                 (cons 'parts (vector (list (cons 'text full-prompt)))))))
         
         ;; MCP-style tool definitions
         (tools (list (cons 'functionDeclarations 
                            (emacs-agent-mcp-tool-definitions))))
         
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           (list (cons 'contents messages)
                 (cons 'tools (vector tools))
                 (cons 'generationConfig 
                       (list (cons 'temperature 0.7)
                             (cons 'maxOutputTokens 2048))))))
         
         (response-buffer (url-retrieve-synchronously url t t)))
    
    (when response-buffer
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "^$" nil t)
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (response (json-read)))
          (kill-buffer)
          response)))))

;; Parse response
(defun emacs-agent-mcp-parse-response (response)
  "Parse RESPONSE extracting text and tool calls."
  (let ((text nil)
        (tool-calls nil))
    
    (when-let ((candidates (alist-get 'candidates response)))
      (cl-loop for candidate across candidates do
               (when-let ((content (alist-get 'content candidate)))
                 (when-let ((parts (alist-get 'parts content)))
                   (cl-loop for part across parts do
                            (cond
                             ((alist-get 'text part)
                              (setq text (concat text (alist-get 'text part))))
                             ((alist-get 'functionCall part)
                              (push (alist-get 'functionCall part) tool-calls))))))))
    
    (list :text text :tool-calls (nreverse tool-calls))))

;; Main conversation function
(defun emacs-agent-mcp-conversation (prompt &optional context output-dir)
  "Process PROMPT with optional CONTEXT and OUTPUT-DIR."
  ;; Note: context is for our tracking, but each LLM call is stateless
  (let* ((response (emacs-agent-mcp-call-llm prompt context output-dir))
         (parsed (emacs-agent-mcp-parse-response response)))
    
    ;; Return parsed response with any tool execution results
    (when (plist-get parsed :tool-calls)
      (let ((tool-results nil))
        (dolist (tool-call (plist-get parsed :tool-calls))
          (let* ((name (alist-get 'name tool-call))
                 (args (alist-get 'args tool-call))
                 (result (emacs-agent-mcp-execute-tool name args output-dir)))
            (push (list :tool name :result result) tool-results)))
        (plist-put parsed :tool-results (nreverse tool-results))))
    
    parsed))

(provide 'emacs-agent-mcp)
;;; emacs-agent-mcp.el ends here