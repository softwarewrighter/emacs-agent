;;; demo-autonomous.el --- Test LLM autonomous decision making with minimal guidance

(require 'emacs-agent-mcp)

;; Check for API key from environment
(unless (getenv "GEMINI_API_KEY")
  (error "GEMINI_API_KEY environment variable not set. Please set it before running the demo."))

(defun run-autonomous-demo ()
  "Demo that lets the LLM make autonomous decisions with minimal guidance."
  
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                     (format "test_output/autonomous_%s" timestamp)
                     default-directory))
         (log-file (expand-file-name "decisions.log" output-dir)))
    
    ;; Create output directory
    (make-directory output-dir t)
    
    ;; Initialize log
    (with-temp-file log-file
      (insert "=== LLM AUTONOMOUS DECISION MAKING DEMO ===\n")
      (insert (format "Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "Goal: Test what the LLM decides to do with minimal guidance\n")
      (insert "We provide high-level requests and observe the LLM's choices\n\n"))
    
    ;; Intercept tool execution to log decisions
    (cl-letf* ((original-execute (symbol-function 'emacs-agent-mcp-execute-tool))
               ((symbol-function 'emacs-agent-mcp-execute-tool)
                (lambda (name args dir)
                  ;; Log what the LLM decided to do
                  (append-to-file 
                   (format "[%s] LLM DECISION: Use '%s' tool\n" 
                          (format-time-string "%H:%M:%S") name)
                   nil log-file)
                  
                  ;; Log the specific choices made
                  (cond
                   ((string= name "file_write")
                    (let ((filename (cdr (assoc 'filename args)))
                          (content (cdr (assoc 'content args))))
                      (append-to-file 
                       (format "  → Chose filename: %s\n" filename)
                       nil log-file)
                      (append-to-file 
                       (format "  → Content length: %d chars\n" (length content))
                       nil log-file)
                      ;; Save to output directory
                      (let ((full-path (expand-file-name filename output-dir)))
                        (with-temp-file full-path
                          (insert content))
                        (list :status "success" 
                              :message (format "Created: %s" filename)))))
                   
                   ((string= name "buffer_create")
                    (let ((buffer-name (cdr (assoc 'buffer_name args))))
                      (append-to-file 
                       (format "  → Chose buffer name: %s\n" buffer-name)
                       nil log-file))
                    (funcall original-execute name args dir))
                   
                   (t
                    (append-to-file 
                     (format "  → Parameters: %S\n" args)
                     nil log-file)
                    (funcall original-execute name args dir))))))
      
      ;; Test 1: Very high-level request - let LLM decide everything
      (append-to-file "\n==== TEST 1: High-Level Request ====\n" nil log-file)
      (let ((prompt "Create an Emacs Lisp math library with two functions: factorial and fibonacci"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Will LLM create 0, 1, or 2 files? What names?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (append-to-file 
           (format "Result: %s\n" 
                  (cond
                   ((plist-get response :tool-calls)
                    (format "Used %d tool call(s)" 
                           (length (plist-get response :tool-calls))))
                   ((plist-get response :text)
                    "Responded with text only (no tools)")
                   (t "No response")))
           nil log-file)))
      
      ;; Test 2: Vague request about organization
      (append-to-file "\n==== TEST 2: Vague Organization Request ====\n" nil log-file)
      (let ((prompt "I need utilities for mathematical calculations"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: How does LLM interpret 'utilities'?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (append-to-file 
           (format "Result: %s\n" 
                  (if (plist-get response :tool-calls)
                      "Created files/buffers"
                    "Text response only"))
           nil log-file)))
      
      ;; Test 3: Ambiguous scope
      (append-to-file "\n==== TEST 3: Ambiguous Scope ====\n" nil log-file)
      (let ((prompt "Set up a project for numerical computation"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: What does LLM consider necessary for a 'project'?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (append-to-file 
           (format "Result: %s\n" 
                  (if (plist-get response :tool-calls)
                      (format "Created %d artifacts" 
                             (length (plist-get response :tool-calls)))
                    "Explained without creating"))
           nil log-file)))
      
      ;; Test 4: Implicit requirements
      (append-to-file "\n==== TEST 4: Implicit Requirements ====\n" nil log-file)
      (let ((prompt "Build something to calculate factorials"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does 'build' imply saving? What format?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (append-to-file 
           (format "Result: %s\n" 
                  (if (plist-get response :tool-calls)
                      "Decided to save/create"
                    "Generated code without saving"))
           nil log-file)))
      
      ;; Test 5: No clear action word
      (append-to-file "\n==== TEST 5: No Action Word ====\n" nil log-file)
      (let ((prompt "Fibonacci sequence generator"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does LLM infer user wants creation?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (append-to-file 
           (format "Result: %s\n" 
                  (if (plist-get response :tool-calls)
                      "Proactively created"
                    "Explained/described only"))
           nil log-file)))
      
      ;; Summary of LLM's autonomous decisions
      (append-to-file "\n\n=== SUMMARY OF LLM'S AUTONOMOUS DECISIONS ===\n" nil log-file)
      
      ;; Count what was created
      (let ((el-files (directory-files output-dir nil "\\.el$"))
            (txt-files (directory-files output-dir nil "\\.txt$"))
            (other-files (directory-files output-dir nil "^[^.].*[^log]$")))
        
        (append-to-file "\nFiles LLM chose to create:\n" nil log-file)
        (dolist (file (append el-files txt-files other-files))
          (unless (string= file "decisions.log")
            (append-to-file (format "  • %s" file) nil log-file)
            (let ((content (with-temp-buffer
                            (insert-file-contents 
                             (expand-file-name file output-dir))
                            (buffer-string))))
              (append-to-file 
               (format " (%d bytes)\n" (length content))
               nil log-file)
              ;; Log first line to see naming/structure decisions
              (when (> (length content) 0)
                (append-to-file 
                 (format "    First line: %s\n" 
                        (car (split-string content "\n")))
                 nil log-file)))))
        
        (append-to-file "\nKey Observations:\n" nil log-file)
        (append-to-file (format "• Total .el files created: %d\n" (length el-files)) nil log-file)
        (append-to-file (format "• File naming pattern: %s\n" 
                               (if el-files
                                   (if (> (length el-files) 1)
                                       "Multiple separate files"
                                     "Single library file")
                                 "No elisp files"))
                       nil log-file)
        (append-to-file "• LLM's approach: " nil log-file)
        (append-to-file (cond
                        ((= (length el-files) 0) "Did not create files\n")
                        ((= (length el-files) 1) "Combined into single library\n")
                        ((= (length el-files) 2) "Separated functions into individual files\n")
                        (t "Created multiple supporting files\n"))
                       nil log-file))
      
      ;; Print summary to console
      (princ "\n=== AUTONOMOUS DEMO COMPLETE ===\n\n")
      (princ "The LLM made these autonomous decisions:\n")
      (princ "─────────────────────────────────────\n")
      
      (let ((files (directory-files output-dir nil "^[^.].*[^log]$")))
        (if files
            (dolist (file files)
              (unless (string= file "decisions.log")
                (princ (format "  ✓ Created: %s\n" file))))
          (princ "  (No files created - LLM chose text responses only)\n")))
      
      (princ "\n📊 Review the decision log to see:\n")
      (princ "   - What tools the LLM chose to use\n")
      (princ "   - What filenames it invented\n")
      (princ "   - How it organized the code\n")
      (princ (format "\n📄 Log: %s\n" log-file))
      
      ;; Return paths
      (list :output-dir output-dir :log-file log-file))))

;; Run if batch mode
(when noninteractive
  (run-autonomous-demo))