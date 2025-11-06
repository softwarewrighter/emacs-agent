;;; emacs-agent-tools.el --- MCP tools for Emacs Agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Emacs Agent Contributors
;; Keywords: tools, ai, mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This file implements MCP (Model Context Protocol) tools for Emacs Agent.
;; The tools are organized in a Playwright-style hierarchy:
;;
;; - buffer.* - Buffer manipulation (like page in Playwright)
;; - window.* - Window management (like viewport)
;; - frame.* - Frame operations (like browser contexts)
;; - file.* - Filesystem operations
;; - editor.* - Editor-level commands

;;; Code:

(require 'json)

;;; Tool Registry

(defvar emacs-agent-tools (make-hash-table :test 'equal)
  "Hash table of registered MCP tools.
Keys are tool names (strings), values are plists with :handler, :description, :parameters.")

(defun emacs-agent-register-tool (name handler description parameters)
  "Register an MCP tool.
NAME is the tool name (e.g., \"buffer.getText\").
HANDLER is a function that implements the tool.
DESCRIPTION is a string describing what the tool does.
PARAMETERS is a list of parameter specifications."
  (puthash name
           (list :handler handler
                 :description description
                 :parameters parameters)
           emacs-agent-tools))

(defun emacs-agent-get-tool (name)
  "Get tool definition for NAME."
  (gethash name emacs-agent-tools))

(defun emacs-agent-list-tools ()
  "Return list of all registered tools in MCP format."
  (let (tools)
    (maphash
     (lambda (name spec)
       (push (list :name name
                   :description (plist-get spec :description)
                   :inputSchema (list :type "object"
                                     :properties (plist-get spec :parameters)
                                     :required (mapcar #'car (plist-get spec :parameters))))
             tools))
     emacs-agent-tools)
    tools))

(defun emacs-agent-call-tool (name params)
  "Call tool NAME with PARAMS.
Returns a plist with :success and :result or :error."
  (let ((tool (emacs-agent-get-tool name)))
    (if (not tool)
        (list :success nil
              :error (format "Unknown tool: %s" name))
      (condition-case err
          (let ((result (funcall (plist-get tool :handler) params)))
            (list :success t
                  :result result))
        (error
         (list :success nil
               :error (format "Tool error: %s" (error-message-string err))))))))

;;; Buffer Tools (Playwright page equivalent)

(defun emacs-agent-tool--buffer-get-text (params)
  "Get text from buffer.
PARAMS may include :buffer (buffer name, defaults to current),
:start and :end (positions, defaults to whole buffer)."
  (let* ((buffer-name (or (plist-get params :buffer)
                          (buffer-name)))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buffer
      (let ((start (or (plist-get params :start) (point-min)))
            (end (or (plist-get params :end) (point-max))))
        (list :content (buffer-substring-no-properties start end)
              :buffer buffer-name
              :length (- end start))))))

(defun emacs-agent-tool--buffer-insert (params)
  "Insert text into buffer.
PARAMS: :text (required), :buffer (optional), :position (optional)."
  (let* ((text (plist-get params :text))
         (buffer-name (or (plist-get params :buffer) (buffer-name)))
         (buffer (get-buffer buffer-name))
         (position (plist-get params :position)))
    (unless text
      (error "Missing required parameter: text"))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buffer
      (save-excursion
        (when position
          (goto-char position))
        (insert text))
      (list :success t
            :buffer buffer-name
            :inserted (length text)))))

(defun emacs-agent-tool--buffer-replace (params)
  "Replace text in buffer.
PARAMS: :old (required), :new (required), :buffer (optional), :count (optional)."
  (let* ((old-text (plist-get params :old))
         (new-text (plist-get params :new))
         (buffer-name (or (plist-get params :buffer) (buffer-name)))
         (buffer (get-buffer buffer-name))
         (count (or (plist-get params :count) nil)))
    (unless (and old-text new-text)
      (error "Missing required parameters: old and new"))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((replacements 0))
          (while (and (search-forward old-text nil t)
                      (or (not count) (< replacements count)))
            (replace-match new-text nil t)
            (setq replacements (1+ replacements)))
          (list :success t
                :buffer buffer-name
                :replacements replacements))))))

(defun emacs-agent-tool--buffer-search (params)
  "Search for pattern in buffer.
PARAMS: :pattern (required), :buffer (optional), :regexp (optional)."
  (let* ((pattern (plist-get params :pattern))
         (buffer-name (or (plist-get params :buffer) (buffer-name)))
         (buffer (get-buffer buffer-name))
         (use-regexp (plist-get params :regexp)))
    (unless pattern
      (error "Missing required parameter: pattern"))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((matches '())
              (search-fn (if use-regexp #'re-search-forward #'search-forward)))
          (while (funcall search-fn pattern nil t)
            (push (list :line (line-number-at-pos)
                       :column (current-column)
                       :position (point)
                       :match (match-string 0))
                  matches))
          (list :matches (nreverse matches)
                :count (length matches)))))))

(defun emacs-agent-tool--buffer-goto (params)
  "Navigate to line/column in buffer.
PARAMS: :line (required), :column (optional), :buffer (optional)."
  (let* ((line (plist-get params :line))
         (column (plist-get params :column))
         (buffer-name (or (plist-get params :buffer) (buffer-name)))
         (buffer (get-buffer buffer-name)))
    (unless line
      (error "Missing required parameter: line"))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buffer
      (goto-char (point-min))
      (forward-line (1- line))
      (when column
        (move-to-column column))
      (list :success t
            :line line
            :column (current-column)
            :position (point)))))

(defun emacs-agent-tool--buffer-save (params)
  "Save buffer to file.
PARAMS: :buffer (optional)."
  (let* ((buffer-name (or (plist-get params :buffer) (buffer-name)))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buffer
      (save-buffer)
      (list :success t
            :buffer buffer-name
            :file (buffer-file-name)))))

(defun emacs-agent-tool--buffer-list (params)
  "List all buffers.
PARAMS: none (reserved for future filtering)."
  (let ((buffers '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (push (list :name (buffer-name)
                   :file (buffer-file-name)
                   :modified (buffer-modified-p)
                   :size (buffer-size)
                   :mode (symbol-name major-mode))
              buffers)))
    (list :buffers (nreverse buffers)
          :count (length buffers))))

;;; Window Tools

(defun emacs-agent-tool--window-split (params)
  "Split window.
PARAMS: :direction (\"horizontal\" or \"vertical\"), :size (optional)."
  (let* ((direction (plist-get params :direction))
         (size (plist-get params :size)))
    (unless direction
      (error "Missing required parameter: direction"))
    (let ((new-window
           (cond
            ((string= direction "horizontal")
             (split-window-horizontally size))
            ((string= direction "vertical")
             (split-window-vertically size))
            (t (error "Invalid direction: %s (use 'horizontal' or 'vertical')" direction)))))
      (list :success t
            :window (window-parameter new-window 'window-id)))))

(defun emacs-agent-tool--window-show-buffer (params)
  "Display buffer in window.
PARAMS: :buffer (required), :window (optional)."
  (let* ((buffer-name (plist-get params :buffer))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (error "Buffer not found: %s" buffer-name))
    (switch-to-buffer buffer)
    (list :success t
          :buffer buffer-name)))

(defun emacs-agent-tool--window-close (params)
  "Close current or specified window.
PARAMS: :window (optional)."
  (delete-window)
  (list :success t))

(defun emacs-agent-tool--window-list (params)
  "List all windows.
PARAMS: none."
  (let ((windows '())
        (idx 0))
    (dolist (win (window-list))
      (with-selected-window win
        (push (list :index idx
                   :buffer (buffer-name)
                   :width (window-width)
                   :height (window-height)
                   :dedicated (window-dedicated-p))
              windows)
        (setq idx (1+ idx))))
    (list :windows (nreverse windows)
          :count (length windows))))

;;; Frame Tools

(defun emacs-agent-tool--frame-create (params)
  "Create new frame.
PARAMS: :name (optional)."
  (let* ((name (plist-get params :name))
         (frame (make-frame)))
    (when name
      (set-frame-parameter frame 'name name))
    (list :success t
          :frame (frame-parameter frame 'name))))

(defun emacs-agent-tool--frame-list (params)
  "List all frames.
PARAMS: none."
  (let ((frames '()))
    (dolist (frame (frame-list))
      (push (list :name (frame-parameter frame 'name)
                 :width (frame-width frame)
                 :height (frame-height frame)
                 :visible (frame-visible-p frame))
            frames))
    (list :frames (nreverse frames)
          :count (length frames))))

(defun emacs-agent-tool--frame-switch (params)
  "Switch to named frame.
PARAMS: :name (required)."
  (let* ((name (plist-get params :name))
         (frame (seq-find (lambda (f)
                           (string= (frame-parameter f 'name) name))
                         (frame-list))))
    (unless frame
      (error "Frame not found: %s" name))
    (select-frame-set-input-focus frame)
    (list :success t
          :frame name)))

;;; File Tools

(defun emacs-agent-tool--file-read (params)
  "Read file contents.
PARAMS: :path (required)."
  (let ((path (plist-get params :path)))
    (unless path
      (error "Missing required parameter: path"))
    (unless (file-exists-p path)
      (error "File not found: %s" path))
    (list :content (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))
          :path path
          :size (nth 7 (file-attributes path)))))

(defun emacs-agent-tool--file-write (params)
  "Write content to file.
PARAMS: :path (required), :content (required)."
  (let ((path (plist-get params :path))
        (content (plist-get params :content)))
    (unless (and path content)
      (error "Missing required parameters: path and content"))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) path))
    (list :success t
          :path path
          :bytes (length content))))

(defun emacs-agent-tool--file-search (params)
  "Search in files (grep).
PARAMS: :pattern (required), :path (optional, defaults to current directory), :recursive (optional)."
  (let* ((pattern (plist-get params :pattern))
         (path (or (plist-get params :path) default-directory))
         (recursive (plist-get params :recursive))
         (results '()))
    (unless pattern
      (error "Missing required parameter: pattern"))
    ;; Use rgrep for recursive, grep-find for non-recursive
    (let ((grep-results
           (shell-command-to-string
            (if recursive
                (format "grep -r -n '%s' %s 2>/dev/null || true" pattern path)
              (format "grep -n '%s' %s/* 2>/dev/null || true" pattern path)))))
      (dolist (line (split-string grep-results "\n" t))
        (when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.+\\)$" line)
          (push (list :file (match-string 1 line)
                     :line (string-to-number (match-string 2 line))
                     :content (match-string 3 line))
                results)))
      (list :matches (nreverse results)
            :count (length results)))))

(defun emacs-agent-tool--file-list (params)
  "List directory contents.
PARAMS: :path (optional, defaults to current directory)."
  (let* ((path (or (plist-get params :path) default-directory))
         (files '()))
    (unless (file-directory-p path)
      (error "Not a directory: %s" path))
    (dolist (file (directory-files path t))
      (unless (member (file-name-nondirectory file) '("." ".."))
        (let ((attrs (file-attributes file)))
          (push (list :name (file-name-nondirectory file)
                     :path file
                     :type (if (file-directory-p file) "directory" "file")
                     :size (nth 7 attrs)
                     :modified (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 attrs)))
                files))))
    (list :files (nreverse files)
          :count (length files))))

;;; Editor Tools

(defun emacs-agent-tool--editor-eval (params)
  "Evaluate Emacs Lisp code.
PARAMS: :code (required)."
  (let ((code (plist-get params :code)))
    (unless code
      (error "Missing required parameter: code"))
    (let ((result (eval (read code))))
      (list :success t
            :result (format "%S" result)))))

(defun emacs-agent-tool--editor-command (params)
  "Execute M-x command.
PARAMS: :name (required), :args (optional)."
  (let ((name (plist-get params :name)))
    (unless name
      (error "Missing required parameter: name"))
    (call-interactively (intern name))
    (list :success t
          :command name)))

(defun emacs-agent-tool--editor-get-state (params)
  "Get current editor state.
PARAMS: none."
  (list :buffer (buffer-name)
        :file (buffer-file-name)
        :point (point)
        :line (line-number-at-pos)
        :column (current-column)
        :mode (symbol-name major-mode)
        :directory default-directory
        :windows (length (window-list))
        :frames (length (frame-list))))

;;; Tool Registration

(defun emacs-agent-tools-init ()
  "Initialize and register all MCP tools."
  ;; Buffer tools
  (emacs-agent-register-tool
   "buffer.getText"
   #'emacs-agent-tool--buffer-get-text
   "Get text content from a buffer"
   '((:buffer . (:type "string" :description "Buffer name (optional, defaults to current)"))
     (:start . (:type "integer" :description "Start position (optional)"))
     (:end . (:type "integer" :description "End position (optional)"))))

  (emacs-agent-register-tool
   "buffer.insert"
   #'emacs-agent-tool--buffer-insert
   "Insert text into a buffer at a specific position"
   '((:text . (:type "string" :description "Text to insert (required)"))
     (:buffer . (:type "string" :description "Buffer name (optional)"))
     (:position . (:type "integer" :description "Position to insert at (optional)"))))

  (emacs-agent-register-tool
   "buffer.replace"
   #'emacs-agent-tool--buffer-replace
   "Replace text in a buffer"
   '((:old . (:type "string" :description "Text to replace (required)"))
     (:new . (:type "string" :description "Replacement text (required)"))
     (:buffer . (:type "string" :description "Buffer name (optional)"))
     (:count . (:type "integer" :description "Max replacements (optional)"))))

  (emacs-agent-register-tool
   "buffer.search"
   #'emacs-agent-tool--buffer-search
   "Search for text pattern in a buffer"
   '((:pattern . (:type "string" :description "Search pattern (required)"))
     (:buffer . (:type "string" :description "Buffer name (optional)"))
     (:regexp . (:type "boolean" :description "Use regexp (optional)"))))

  (emacs-agent-register-tool
   "buffer.goto"
   #'emacs-agent-tool--buffer-goto
   "Navigate to a specific line/column in a buffer"
   '((:line . (:type "integer" :description "Line number (required)"))
     (:column . (:type "integer" :description "Column number (optional)"))
     (:buffer . (:type "string" :description "Buffer name (optional)"))))

  (emacs-agent-register-tool
   "buffer.save"
   #'emacs-agent-tool--buffer-save
   "Save buffer to its file"
   '((:buffer . (:type "string" :description "Buffer name (optional)"))))

  (emacs-agent-register-tool
   "buffer.list"
   #'emacs-agent-tool--buffer-list
   "List all open buffers"
   '())

  ;; Window tools
  (emacs-agent-register-tool
   "window.split"
   #'emacs-agent-tool--window-split
   "Split window horizontally or vertically"
   '((:direction . (:type "string" :description "Direction: 'horizontal' or 'vertical' (required)"))
     (:size . (:type "integer" :description "Window size (optional)"))))

  (emacs-agent-register-tool
   "window.showBuffer"
   #'emacs-agent-tool--window-show-buffer
   "Display a buffer in the current window"
   '((:buffer . (:type "string" :description "Buffer name (required)"))))

  (emacs-agent-register-tool
   "window.close"
   #'emacs-agent-tool--window-close
   "Close the current window"
   '())

  (emacs-agent-register-tool
   "window.list"
   #'emacs-agent-tool--window-list
   "List all windows"
   '())

  ;; Frame tools
  (emacs-agent-register-tool
   "frame.create"
   #'emacs-agent-tool--frame-create
   "Create a new frame"
   '((:name . (:type "string" :description "Frame name (optional)"))))

  (emacs-agent-register-tool
   "frame.list"
   #'emacs-agent-tool--frame-list
   "List all frames"
   '())

  (emacs-agent-register-tool
   "frame.switch"
   #'emacs-agent-tool--frame-switch
   "Switch to a named frame"
   '((:name . (:type "string" :description "Frame name (required)"))))

  ;; File tools
  (emacs-agent-register-tool
   "file.read"
   #'emacs-agent-tool--file-read
   "Read file contents from disk"
   '((:path . (:type "string" :description "File path (required)"))))

  (emacs-agent-register-tool
   "file.write"
   #'emacs-agent-tool--file-write
   "Write content to a file"
   '((:path . (:type "string" :description "File path (required)"))
     (:content . (:type "string" :description "File content (required)"))))

  (emacs-agent-register-tool
   "file.search"
   #'emacs-agent-tool--file-search
   "Search for pattern in files (grep)"
   '((:pattern . (:type "string" :description "Search pattern (required)"))
     (:path . (:type "string" :description "Directory path (optional)"))
     (:recursive . (:type "boolean" :description "Recursive search (optional)"))))

  (emacs-agent-register-tool
   "file.list"
   #'emacs-agent-tool--file-list
   "List directory contents"
   '((:path . (:type "string" :description "Directory path (optional)"))))

  ;; Editor tools
  (emacs-agent-register-tool
   "editor.eval"
   #'emacs-agent-tool--editor-eval
   "Evaluate Emacs Lisp code"
   '((:code . (:type "string" :description "Elisp code (required)"))))

  (emacs-agent-register-tool
   "editor.command"
   #'emacs-agent-tool--editor-command
   "Execute an Emacs command (M-x)"
   '((:name . (:type "string" :description "Command name (required)"))))

  (emacs-agent-register-tool
   "editor.getState"
   #'emacs-agent-tool--editor-get-state
   "Get current editor state (buffer, position, mode, etc.)"
   '()))

(provide 'emacs-agent-tools)
;;; emacs-agent-tools.el ends here
