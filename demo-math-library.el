;;; demo-math-library.el --- Demonstrate autonomous tool use for math library creation

;; This demo shows how Gemini autonomously decides to use tools
;; to create a math library when asked naturally

(require 'emacs-agent-mcp)

(defun demo-math-library ()
  "Run math library creation demo with comprehensive logging."
  (interactive)
  
  ;; Setup output directory
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                     (format "test_output/math_demo_%s" timestamp)
                     default-directory)))
    
    ;; Create output directory
    (make-directory output-dir t)
    
    ;; Create log buffer
    (let ((log-buffer (get-buffer-create "*Math Library Demo Log*")))
      (with-current-buffer log-buffer
        (erase-buffer)
        (insert "=== MATH LIBRARY CREATION DEMO ===\n")
        (insert (format "Timestamp: %s\n" timestamp))
        (insert (format "Output Directory: %s\n\n" output-dir)))
      
      ;; Override file_write to save to our output directory
      (cl-letf* ((original-file-write (symbol-function 'emacs-agent-mcp-tool-file-write))
                 ((symbol-function 'emacs-agent-mcp-tool-file-write)
                  (lambda (params)
                    (let* ((filename (cdr (assoc 'filename params)))
                           (content (cdr (assoc 'content params)))
                           (full-path (expand-file-name filename output-dir)))
                      ;; Log the tool use
                      (with-current-buffer log-buffer
                        (goto-char (point-max))
                        (insert (format "\n[%s] TOOL USE: file_write\n" 
                                       (format-time-string "%H:%M:%S")))
                        (insert (format "  File: %s\n" filename))
                        (insert (format "  Content length: %d chars\n" (length content)))
                        (insert "  First line: ")
                        (insert (car (split-string content "\n")))
                        (insert "\n"))
                      ;; Save to output directory
                      (with-temp-file full-path
                        (insert content))
                      (format "File created: %s" full-path)))))
        
        ;; Test 1: Natural request - let Gemini decide
        (with-current-buffer log-buffer
          (goto-char (point-max))
          (insert "\n=== TEST 1: Natural Request ===\n")
          (insert "Prompt: 'I need a factorial function in elisp'\n")
          (insert "Expected: Gemini might generate code but may not save it\n\n"))
        
        (let ((response (emacs-agent-mcp-conversation 
                        "I need a factorial function in elisp")))
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert "Response: ")
            (insert (format "%S\n" response))))
        
        ;; Test 2: Task-oriented - mentions saving
        (with-current-buffer log-buffer
          (goto-char (point-max))
          (insert "\n=== TEST 2: Task-Oriented Request ===\n")
          (insert "Prompt: 'Create a fibonacci function and save it to fibonacci.el'\n")
          (insert "Expected: Gemini should use file_write tool\n\n"))
        
        (let ((response (emacs-agent-mcp-conversation 
                        "Create a fibonacci function and save it to fibonacci.el")))
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert "Response: ")
            (insert (format "%S\n" response))))
        
        ;; Test 3: Complete library request
        (with-current-buffer log-buffer
          (goto-char (point-max))
          (insert "\n=== TEST 3: Complete Library ===\n")
          (insert "Prompt: 'Create a math utilities library with factorial and fibonacci functions'\n")
          (insert "Expected: Gemini should create comprehensive library file\n\n"))
        
        (let ((response (emacs-agent-mcp-conversation 
                        "Create a math utilities library with factorial and fibonacci functions")))
          (with-current-buffer log-buffer
            (goto-char (point-max))
            (insert "Response: ")
            (insert (format "%S\n" response))))
        
        ;; Summary
        (with-current-buffer log-buffer
          (goto-char (point-max))
          (insert "\n\n=== SUMMARY ===\n")
          (insert (format "Output directory: %s\n" output-dir))
          (insert "Files created:\n")
          (dolist (file (directory-files output-dir nil "\\.el$"))
            (insert (format "  - %s\n" file)))
          
          ;; Save log
          (write-file (expand-file-name "demo.log" output-dir)))
        
        ;; Display results
        (display-buffer log-buffer)
        (message "Demo complete! Check %s for created files" output-dir)
        
        ;; Return output directory for inspection
        output-dir))))

;; Run if called from batch mode
(when noninteractive
  (let ((output-dir (demo-math-library)))
    (princ "\n=== MATH LIBRARY DEMO COMPLETE ===\n")
    (princ (format "Output directory: %s\n" output-dir))
    (princ "\nFiles created by Gemini:\n")
    (dolist (file (directory-files output-dir nil "\\.el$"))
      (unless (string= file "demo.log")
        (princ (format "  - %s\n" file))
        (let ((content (with-temp-buffer
                        (insert-file-contents 
                         (expand-file-name file output-dir))
                        (buffer-string))))
          (when (< (length content) 200)
            (princ (format "    Content: %s\n" content))))))
    (princ "\nReview the log file for details of tool usage.\n")))

(provide 'demo-math-library)