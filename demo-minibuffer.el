;;; demo-minibuffer.el --- Test if LLM infers interactive commands from mini-buffer hints

(require 'emacs-agent-mcp)

;; Check for API key from environment
(unless (getenv "GEMINI_API_KEY")
  (error "GEMINI_API_KEY environment variable not set. Please set it before running the demo."))

(defun run-minibuffer-demo ()
  "Demo testing if mentioning mini-buffer/commands triggers interactive functions."
  
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                     (format "test_output/minibuffer_%s" timestamp)
                     default-directory))
         (log-file (expand-file-name "analysis.log" output-dir)))
    
    ;; Create output directory
    (make-directory output-dir t)
    
    ;; Initialize log
    (with-temp-file log-file
      (insert "=== MINI-BUFFER HINTS DEMO ===\n")
      (insert (format "Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "Testing if LLM infers (interactive) from mini-buffer/command hints\n\n"))
    
    ;; Intercept and analyze
    (cl-letf* ((original-execute (symbol-function 'emacs-agent-mcp-execute-tool))
               ((symbol-function 'emacs-agent-mcp-execute-tool)
                (lambda (name args dir)
                  (when (string= name "file_write")
                    (let* ((filename (cdr (assoc 'filename args)))
                           (content (cdr (assoc 'content args)))
                           (full-path (expand-file-name filename output-dir)))
                      
                      (append-to-file 
                       (format "\n[%s] Created: %s\n" 
                              (format-time-string "%H:%M:%S") filename)
                       nil log-file)
                      
                      ;; Detailed analysis
                      (let ((interactive-count 0)
                            (interactive-lines nil)
                            (docstring-count 0)
                            (defun-count 0)
                            (lines (split-string content "\n")))
                        
                        (dolist (line lines)
                          ;; Count defuns
                          (when (string-match "^(defun " line)
                            (setq defun-count (1+ defun-count)))
                          
                          ;; Find interactive declarations
                          (when (string-match "(interactive" line)
                            (setq interactive-count (1+ interactive-count))
                            (push line interactive-lines))
                          
                          ;; Count docstrings (indented strings after defun)
                          (when (string-match "^  \"" line)
                            (setq docstring-count (1+ docstring-count))))
                        
                        (append-to-file "Analysis:\n" nil log-file)
                        (append-to-file (format "  Functions defined: %d\n" defun-count) nil log-file)
                        (append-to-file (format "  Interactive declarations: %d\n" interactive-count) nil log-file)
                        (append-to-file (format "  Docstrings: %d\n" docstring-count) nil log-file)
                        
                        (when interactive-lines
                          (append-to-file "  Interactive specs found:\n" nil log-file)
                          (dolist (line (reverse interactive-lines))
                            (append-to-file (format "    %s\n" (string-trim line)) nil log-file)))
                        
                        (append-to-file (format "  Result: %s\n" 
                                               (if (> interactive-count 0)
                                                   "✅ Has (interactive) - callable via M-x!"
                                                 "❌ No (interactive) - not M-x callable"))
                                       nil log-file))
                      
                      (with-temp-file full-path
                        (insert content))
                      (list :status "success" :message filename)))
                  
                  (unless (string= name "file_write")
                    (funcall original-execute name args dir)))))
      
      ;; Test 1: Direct mini-buffer hint
      (append-to-file "\n==== TEST 1: Mini-buffer Hint ====\n" nil log-file)
      (let ((prompt "Create a math library for Emacs with factorial and fibonacci functions that can be called from the mini-buffer"))
        (append-to-file (format "Prompt: \"%s\"\n" prompt) nil log-file)
        (emacs-agent-mcp-conversation prompt))
      
      ;; Test 2: Command hint
      (append-to-file "\n==== TEST 2: Command Hint ====\n" nil log-file)
      (let ((prompt "Build Emacs commands for calculating factorials and fibonacci numbers that users can run"))
        (append-to-file (format "Prompt: \"%s\"\n" prompt) nil log-file)
        (emacs-agent-mcp-conversation prompt))
      
      ;; Test 3: M-x hint
      (append-to-file "\n==== TEST 3: M-x Hint ====\n" nil log-file)
      (let ((prompt "Set up math functions that work with M-x in Emacs"))
        (append-to-file (format "Prompt: \"%s\"\n" prompt) nil log-file)
        (emacs-agent-mcp-conversation prompt))
      
      ;; Test 4: Input prompt hint
      (append-to-file "\n==== TEST 4: Prompt for Input ====\n" nil log-file)
      (let ((prompt "Make factorial and fibonacci functions for Emacs that prompt the user for numbers"))
        (append-to-file (format "Prompt: \"%s\"\n" prompt) nil log-file)
        (emacs-agent-mcp-conversation prompt))
      
      ;; Test 5: Combined hints
      (append-to-file "\n==== TEST 5: Combined Requirements ====\n" nil log-file)
      (let ((prompt "Create a well-documented Emacs math library with factorial and fibonacci that other users can call as commands"))
        (append-to-file (format "Prompt: \"%s\"\n" prompt) nil log-file)
        (emacs-agent-mcp-conversation prompt))
      
      ;; Summary
      (append-to-file "\n\n=== SUMMARY ===\n" nil log-file)
      (let ((el-files (directory-files output-dir nil "\\.el$"))
            (total-interactive 0)
            (total-functions 0))
        
        (dolist (file el-files)
          (with-temp-buffer
            (insert-file-contents (expand-file-name file output-dir))
            (let ((func-count (count-matches "^(defun "))
                  (int-count (count-matches "(interactive")))
              (setq total-functions (+ total-functions func-count))
              (setq total-interactive (+ total-interactive int-count)))))
        
        (append-to-file (format "Files created: %d\n" (length el-files)) nil log-file)
        (append-to-file (format "Total functions: %d\n" total-functions) nil log-file)
        (append-to-file (format "Interactive functions: %d\n" total-interactive) nil log-file)
        (append-to-file (format "Success rate: %.0f%%\n" 
                               (if (> total-functions 0)
                                   (* 100.0 (/ (float total-interactive) total-functions))
                                 0))
                       nil log-file))
      
      ;; Console output
      (princ "\n=== MINI-BUFFER DEMO COMPLETE ===\n\n")
      (princ "Testing phrases that should trigger (interactive):\n")
      (princ "──────────────────────────────────────────────\n")
      (princ "✓ \"called from the mini-buffer\"\n")
      (princ "✓ \"Emacs commands\"\n")
      (princ "✓ \"work with M-x\"\n")
      (princ "✓ \"prompt the user\"\n")
      (princ "✓ \"call as commands\"\n")
      
      (princ "\nResults:\n")
      (let ((el-files (directory-files output-dir nil "\\.el$")))
        (dolist (file el-files)
          (let ((content (with-temp-buffer
                          (insert-file-contents 
                           (expand-file-name file output-dir))
                          (buffer-string))))
            (princ (format "\n📄 %s\n" file))
            (if (string-match "(interactive" content)
                (progn
                  (princ "   ✅ Has (interactive) - M-x callable!\n")
                  ;; Show the interactive spec
                  (when (string-match "(interactive\\([^)]*\\))" content)
                    (let ((spec (match-string 1 content)))
                      (unless (string= spec "")
                        (princ (format "   → Interactive spec: %s\n" spec))))))
              (princ "   ❌ No (interactive) - not M-x callable\n")))))
      
      (princ (format "\n📊 Full analysis: %s\n" log-file))
      
      ;; Return paths
      (list :output-dir output-dir :log-file log-file))))

;; Run if batch mode
(when noninteractive
  (run-minibuffer-demo))