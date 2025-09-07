;;; demo-interactive.el --- Test LLM creating interactive Emacs functions with minimal guidance

(require 'emacs-agent-mcp)

;; Check for API key from environment
(unless (getenv "GEMINI_API_KEY")
  (error "GEMINI_API_KEY environment variable not set. Please set it before running the demo."))

(defun run-interactive-demo ()
  "Demo that tests if LLM infers the need for interactive, documented functions."
  
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                     (format "test_output/interactive_%s" timestamp)
                     default-directory))
         (log-file (expand-file-name "decisions.log" output-dir)))
    
    ;; Create output directory
    (make-directory output-dir t)
    
    ;; Initialize log
    (with-temp-file log-file
      (insert "=== LLM INTERACTIVE FUNCTION CREATION DEMO ===\n")
      (insert (format "Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "Goal: See if LLM infers need for interactive, documented functions\n")
      (insert "We state requirements that should lead to proper Emacs commands\n\n"))
    
    ;; Intercept tool execution to analyze LLM's decisions
    (cl-letf* ((original-execute (symbol-function 'emacs-agent-mcp-execute-tool))
               ((symbol-function 'emacs-agent-mcp-execute-tool)
                (lambda (name args dir)
                  (append-to-file 
                   (format "[%s] LLM DECISION: Use '%s' tool\n" 
                          (format-time-string "%H:%M:%S") name)
                   nil log-file)
                  
                  (when (string= name "file_write")
                    (let* ((filename (cdr (assoc 'filename args)))
                           (content (cdr (assoc 'content args)))
                           (full-path (expand-file-name filename output-dir)))
                      
                      ;; Log filename choice
                      (append-to-file 
                       (format "  → Filename: %s\n" filename)
                       nil log-file)
                      
                      ;; Analyze the content for key features
                      (append-to-file "  → Analysis of generated code:\n" nil log-file)
                      
                      ;; Check for interactive declarations
                      (when (string-match "(interactive" content)
                        (append-to-file "    ✓ Contains (interactive) declarations\n" nil log-file))
                      
                      ;; Check for docstrings
                      (when (string-match "\".*\"" content)
                        (append-to-file "    ✓ Contains docstrings\n" nil log-file))
                      
                      ;; Check for argument handling
                      (when (string-match "(interactive \"[^\"]*\")" content)
                        (append-to-file "    ✓ Has interactive argument specs\n" nil log-file))
                      
                      ;; Check for provide statement
                      (when (string-match "(provide " content)
                        (append-to-file "    ✓ Has (provide ...) for loading\n" nil log-file))
                      
                      ;; Check for file header
                      (when (string-match "^;;;" content)
                        (append-to-file "    ✓ Has proper file header\n" nil log-file))
                      
                      ;; Save the file
                      (with-temp-file full-path
                        (insert content))
                      (list :status "success" :message (format "Created: %s" filename))))
                  
                  ;; For other tools, use original
                  (unless (string= name "file_write")
                    (funcall original-execute name args dir)))))
      
      ;; Test 1: Requirements that imply interactive use
      (append-to-file "\n==== TEST 1: Implying Interactive Use ====\n" nil log-file)
      (let ((prompt "Create an Emacs math library that I can use while editing. I need factorial and fibonacci calculations."))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does 'use while editing' lead to interactive functions?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (if (plist-get response :tool-calls)
              (append-to-file "Result: Created file(s)\n" nil log-file)
            (append-to-file "Result: Text only\n" nil log-file))))
      
      ;; Test 2: Mentioning the minibuffer indirectly
      (append-to-file "\n==== TEST 2: Implying Command Access ====\n" nil log-file)
      (let ((prompt "Build utilities for quick mathematical calculations that I can invoke in Emacs"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does 'invoke' suggest M-x commands?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (if (plist-get response :tool-calls)
              (append-to-file "Result: Created file(s)\n" nil log-file)
            (append-to-file "Result: Text only\n" nil log-file))))
      
      ;; Test 3: Requesting help/documentation indirectly
      (append-to-file "\n==== TEST 3: Implying Documentation Need ====\n" nil log-file)
      (let ((prompt "Make a math library for Emacs that other users can understand and use. Include factorial and fibonacci."))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does 'other users can understand' lead to docstrings?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (if (plist-get response :tool-calls)
              (append-to-file "Result: Created file(s)\n" nil log-file)
            (append-to-file "Result: Text only\n" nil log-file))))
      
      ;; Test 4: Mentioning input without being explicit
      (append-to-file "\n==== TEST 4: Implying User Input ====\n" nil log-file)
      (let ((prompt "Set up Emacs functions to calculate factorial and fibonacci for any number I choose"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does 'number I choose' lead to argument handling?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (if (plist-get response :tool-calls)
              (append-to-file "Result: Created file(s)\n" nil log-file)
            (append-to-file "Result: Text only\n" nil log-file))))
      
      ;; Test 5: Professional/shareable context
      (append-to-file "\n==== TEST 5: Implying Professional Quality ====\n" nil log-file)
      (let ((prompt "Develop a reusable Emacs math module with factorial and fibonacci functionality"))
        (append-to-file (format "User: \"%s\"\n" prompt) nil log-file)
        (append-to-file "Observing: Does 'reusable module' lead to proper structure?\n\n" nil log-file)
        
        (let ((response (emacs-agent-mcp-conversation prompt)))
          (if (plist-get response :tool-calls)
              (append-to-file "Result: Created file(s)\n" nil log-file)
            (append-to-file "Result: Text only\n" nil log-file))))
      
      ;; Analyze what was created
      (append-to-file "\n\n=== ANALYSIS OF LLM'S OUTPUT ===\n" nil log-file)
      
      (let ((el-files (directory-files output-dir nil "\\.el$")))
        (dolist (file el-files)
          (append-to-file (format "\nFile: %s\n" file) nil log-file)
          (append-to-file "─────────────────\n" nil log-file)
          
          (let* ((full-path (expand-file-name file output-dir))
                 (content (with-temp-buffer
                           (insert-file-contents full-path)
                           (buffer-string)))
                 (lines (split-string content "\n")))
            
            ;; Check for key features
            (let ((has-interactive nil)
                  (has-docstring nil)
                  (has-args nil)
                  (has-provide nil)
                  (interactive-count 0)
                  (docstring-count 0))
              
              ;; Count features
              (dolist (line lines)
                (when (string-match "(interactive" line)
                  (setq has-interactive t)
                  (setq interactive-count (1+ interactive-count)))
                (when (and (string-match "^  \"" line)
                           (not (string-match ";;" line)))
                  (setq has-docstring t)
                  (setq docstring-count (1+ docstring-count)))
                (when (string-match "(defun [^ ]+ (\\([^)]+\\))" line)
                  (unless (string= (match-string 1 line) "")
                    (setq has-args t)))
                (when (string-match "(provide " line)
                  (setq has-provide t)))
              
              ;; Report findings
              (append-to-file "Features found:\n" nil log-file)
              (append-to-file (format "  • Interactive functions: %s (%d found)\n" 
                                     (if has-interactive "YES" "NO")
                                     interactive-count)
                             nil log-file)
              (append-to-file (format "  • Docstrings: %s (%d found)\n"
                                     (if has-docstring "YES" "NO")
                                     docstring-count)
                             nil log-file)
              (append-to-file (format "  • Takes arguments: %s\n"
                                     (if has-args "YES" "NO"))
                             nil log-file)
              (append-to-file (format "  • Module structure (provide): %s\n"
                                     (if has-provide "YES" "NO"))
                             nil log-file)
              
              ;; Show a sample function if present
              (when (string-match "(defun \\([^ ]+\\)[^\n]*\n\\([^\n]*\n\\)?\\([^\n]*\n\\)?" content)
                (append-to-file "\nSample function structure:\n" nil log-file)
                (append-to-file (match-string 0 content) nil log-file)
                (append-to-file "...\n" nil log-file))))))
      
      ;; Summary
      (append-to-file "\n=== SUMMARY ===\n" nil log-file)
      (append-to-file "The LLM's interpretation of requirements:\n" nil log-file)
      (append-to-file "• 'use while editing' → ?\n" nil log-file)
      (append-to-file "• 'invoke in Emacs' → ?\n" nil log-file)
      (append-to-file "• 'other users can understand' → ?\n" nil log-file)
      (append-to-file "• 'number I choose' → ?\n" nil log-file)
      (append-to-file "• 'reusable module' → ?\n" nil log-file)
      
      ;; Console output
      (princ "\n=== INTERACTIVE DEMO COMPLETE ===\n\n")
      (princ "Analyzed how LLM interprets requirements:\n")
      (princ "────────────────────────────────────────\n")
      
      (let ((el-files (directory-files output-dir nil "\\.el$")))
        (if el-files
            (progn
              (dolist (file el-files)
                (princ (format "📄 Created: %s\n" file))
                
                ;; Quick check for interactive
                (let ((content (with-temp-buffer
                                (insert-file-contents 
                                 (expand-file-name file output-dir))
                                (buffer-string))))
                  (when (string-match "(interactive" content)
                    (princ "   ✓ Has interactive functions\n"))
                  (when (string-match "\".*\"" content)
                    (princ "   ✓ Has documentation\n")))))
          (princ "  (No files created)\n")))
      
      (princ (format "\n📊 Full analysis in: %s\n" log-file))
      (princ "\nKey question: Did the LLM infer the need for:\n")
      (princ "  • (interactive) declarations?\n")
      (princ "  • Docstrings?\n")
      (princ "  • Argument handling?\n")
      (princ "  • M-x callable commands?\n")
      
      ;; Return paths
      (list :output-dir output-dir :log-file log-file))))

;; Run if batch mode
(when noninteractive
  (run-interactive-demo))