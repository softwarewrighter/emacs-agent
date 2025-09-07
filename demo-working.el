;;; demo-working.el --- Working demo with proper API key handling

(require 'emacs-agent-mcp)

;; Check for API key from environment
(unless (getenv "GEMINI_API_KEY")
  (error "GEMINI_API_KEY environment variable not set. Please set it before running the demo."))

(defun run-working-demo ()
  "Run a working demo that shows Gemini using tools autonomously."
  
  ;; Setup output directory
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                     (format "test_output/working_demo_%s" timestamp)
                     default-directory))
         (log-file (expand-file-name "comprehensive.log" output-dir)))
    
    ;; Create output directory
    (make-directory output-dir t)
    
    ;; Open log file
    (with-temp-file log-file
      (insert "=== GEMINI TOOL USE DEMO - COMPREHENSIVE LOG ===\n")
      (insert (format "Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Output Directory: %s\n" output-dir))
      (insert (format "API Key Set: %s\n\n" (if (getenv "GEMINI_API_KEY") "YES" "NO"))))
    
    ;; Override file_write to save to our output directory
    (cl-letf* ((original-execute (symbol-function 'emacs-agent-mcp-execute-tool))
               ((symbol-function 'emacs-agent-mcp-execute-tool)
                (lambda (name args dir)
                  ;; Log the tool call
                  (append-to-file 
                   (format "\n[%s] TOOL CALL: %s\n  Args: %S\n" 
                          (format-time-string "%H:%M:%S")
                          name args)
                   nil log-file)
                  
                  ;; For file_write, save to our output directory
                  (if (string= name "file_write")
                      (let* ((filename (cdr (assoc 'filename args)))
                             (content (cdr (assoc 'content args)))
                             (full-path (expand-file-name filename output-dir)))
                        (with-temp-file full-path
                          (insert content))
                        (append-to-file 
                         (format "  Created file: %s\n" full-path)
                         nil log-file)
                        (list :status "success" 
                              :message (format "File written: %s" full-path)))
                    ;; For other tools, use original
                    (funcall original-execute name args dir)))))
      
      ;; Test 1: Direct action request
      (append-to-file "\n=== TEST 1: Direct Action Request ===\n" nil log-file)
      (append-to-file "User: \"Create a file named hello.txt containing 'Hello, World!'\"\n" nil log-file)
      (append-to-file "Expected: Should use file_write tool\n" nil log-file)
      
      (let ((response (emacs-agent-mcp-conversation 
                      "Create a file named hello.txt containing 'Hello, World!'")))
        (append-to-file 
         (format "Result: %s\n" 
                (if (plist-get response :tool-calls) 
                    "✅ USED TOOLS" 
                    "❌ NO TOOLS"))
         nil log-file))
      
      ;; Test 2: Task-oriented request  
      (append-to-file "\n=== TEST 2: Task-Oriented Request ===\n" nil log-file)
      (append-to-file "User: \"Generate a factorial function and save it to factorial.el\"\n" nil log-file)
      (append-to-file "Expected: Should generate code AND use file_write\n" nil log-file)
      
      (let ((response (emacs-agent-mcp-conversation 
                      "Generate a factorial function and save it to factorial.el")))
        (append-to-file 
         (format "Result: %s\n" 
                (if (plist-get response :tool-calls) 
                    "✅ USED TOOLS" 
                    "❌ NO TOOLS"))
         nil log-file))
      
      ;; Test 3: Multi-function library
      (append-to-file "\n=== TEST 3: Complete Math Library ===\n" nil log-file)
      (append-to-file "User: \"Create a math library with factorial and fibonacci functions in math-utils.el\"\n" nil log-file)
      (append-to-file "Expected: Should create comprehensive library file\n" nil log-file)
      
      (let ((response (emacs-agent-mcp-conversation 
                      "Create a math library with factorial and fibonacci functions in math-utils.el")))
        (append-to-file 
         (format "Result: %s\n" 
                (if (plist-get response :tool-calls) 
                    "✅ USED TOOLS" 
                    "❌ NO TOOLS"))
         nil log-file))
      
      ;; Test 4: Information request (should NOT use tools)
      (append-to-file "\n=== TEST 4: Information Request ===\n" nil log-file)
      (append-to-file "User: \"What is the difference between a buffer and a file?\"\n" nil log-file)
      (append-to-file "Expected: Should NOT use tools\n" nil log-file)
      
      (let ((response (emacs-agent-mcp-conversation 
                      "What is the difference between a buffer and a file?")))
        (append-to-file 
         (format "Result: %s\n" 
                (if (plist-get response :tool-calls) 
                    "❌ USED TOOLS (unexpected)" 
                    "✅ NO TOOLS (correct)"))
         nil log-file))
      
      ;; Summary
      (append-to-file "\n\n=== SUMMARY ===\n" nil log-file)
      (append-to-file (format "Output directory: %s\n" output-dir) nil log-file)
      (append-to-file "\nFiles created by Gemini:\n" nil log-file)
      
      (dolist (file (directory-files output-dir nil "\\.el$\\|hello\\.txt$"))
        (append-to-file (format "  - %s\n" file) nil log-file)
        (let ((size (nth 7 (file-attributes (expand-file-name file output-dir)))))
          (append-to-file (format "    Size: %d bytes\n" size) nil log-file)))
      
      ;; Print to console
      (princ "\n=== DEMO COMPLETE ===\n")
      (princ (format "📁 Output directory: %s\n" output-dir))
      (princ (format "📄 Log file: %s\n\n" log-file))
      (princ "Files created by Gemini:\n")
      (dolist (file (directory-files output-dir nil "\\.el$\\|hello\\.txt$"))
        (princ (format "  ✓ %s\n" file)))
      (princ (format "\nReview the log: cat %s\n" log-file))
      
      ;; Return paths for reference
      (list :output-dir output-dir :log-file log-file))))

;; Run if in batch mode
(when noninteractive
  (run-working-demo))