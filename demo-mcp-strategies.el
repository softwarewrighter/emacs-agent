;;; demo-mcp-strategies.el --- Test prompting strategies for tool use -*- lexical-binding: t -*-

;;; Commentary:
;; Demonstrate different prompting strategies to encourage LLM tool use

;;; Code:

(require 'emacs-agent-mcp)

(defun demo-test-prompts ()
  "Test various prompting strategies."
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                      (format "test_output/mcp_demo_%s" timestamp)
                      default-directory))
         (test-cases
          '(;; Strategy 1: Direct action request
            ("Create a file named hello.txt containing 'Hello, World!'"
             . "Should use file_write tool")
            
            ;; Strategy 2: Explicit need for tool
            ("I need to save some text to a file. Please write 'Test content' to test.txt"
             . "Should use file_write tool")
            
            ;; Strategy 3: Task-oriented request
            ("Generate a factorial function and save it to factorial.el"
             . "Should generate code and use file_write")
            
            ;; Strategy 4: User suggests tool (LLM decides)
            ("Can you use file_write to create a README file?"
             . "LLM decides whether to use suggested tool")
            
            ;; Strategy 5: Information request (no tool needed)
            ("What is the difference between a buffer and a file in Emacs?"
             . "Should respond without tools")
            
            ;; Strategy 6: Ambiguous request
            ("Show me the contents of test.txt"
             . "Might use file_read if it interprets as reading file")
            
            ;; Strategy 7: Multi-step task
            ("Create a new buffer called *scratch-test* with 'Hello' and then append ' World' to it"
             . "Should use buffer_create then buffer_append")
            
            ;; Strategy 8: Wrong tool suggestion
            ("Use buffer_append to create a new file"
             . "LLM should choose correct tool or explain")
            
            ;; Strategy 9: Exploration request
            ("What files are in the current directory?"
             . "Should use list_files tool")
            
            ;; Strategy 10: Complex task
            ("Read the file config.txt if it exists, otherwise create it with default settings"
             . "Conditional tool use based on file existence"))))
    
    (make-directory output-dir t)
    
    (princ "\n=== MCP PROMPTING STRATEGIES TEST ===\n")
    (princ (format "Output directory: %s\n\n" output-dir))
    
    ;; Test each strategy
    (let ((results nil))
      (cl-loop for (prompt . expectation) in test-cases
               for i from 1
               do
               (princ (format "Strategy %d: %s\n" i expectation))
               (princ (format "Prompt: %s\n" prompt))
               
               (let* ((response (emacs-agent-mcp-conversation prompt nil output-dir))
                      (text (plist-get response :text))
                      (tool-calls (plist-get response :tool-calls))
                      (tool-results (plist-get response :tool-results)))
                 
                 ;; Display response
                 (when text
                   (princ (format "Response: %s\n" 
                                  (substring text 0 (min 100 (length text))))))
                 
                 ;; Display tool usage
                 (if tool-calls
                     (progn
                       (princ "✅ TOOLS USED:\n")
                       (dolist (tc tool-calls)
                         (princ (format "   - %s(%S)\n" 
                                        (alist-get 'name tc)
                                        (alist-get 'args tc))))
                       (when tool-results
                         (dolist (tr tool-results)
                           (princ (format "   Result: %S\n" (plist-get tr :result))))))
                   (princ "❌ NO TOOLS USED\n"))
                 
                 ;; Record result
                 (push (list :strategy i
                             :prompt prompt
                             :used-tools (if tool-calls t nil)
                             :tools (mapcar (lambda (tc) (alist-get 'name tc)) tool-calls))
                       results)
                 
                 (princ "---\n\n")))
      
      ;; Summary
      (princ "\n=== SUMMARY ===\n")
      (let ((total (length results))
            (with-tools (cl-count-if (lambda (r) (plist-get r :used-tools)) results)))
        (princ (format "Total prompts: %d\n" total))
        (princ (format "Used tools: %d (%.1f%%)\n" 
                       with-tools 
                       (* 100.0 (/ (float with-tools) total))))
        
        ;; Show which strategies worked
        (princ "\nStrategies that triggered tool use:\n")
        (dolist (r (reverse results))
          (when (plist-get r :used-tools)
            (princ (format "  Strategy %d: %s\n" 
                           (plist-get r :strategy)
                           (mapconcat 'identity (plist-get r :tools) ", ")))))
        
        ;; Save results
        (let ((results-file (expand-file-name "results.json" output-dir)))
          (with-temp-file results-file
            (insert (json-encode (reverse results))))
          (princ (format "\nResults saved to: %s\n" results-file)))))))

(defun demo-interactive-mcp ()
  "Interactive demo with MCP-compliant agent."
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (output-dir (expand-file-name 
                      (format "test_output/mcp_interactive_%s" timestamp)
                      default-directory))
         (context (vector (list (cons 'role "system")
                                (cons 'parts (vector (list (cons 'text (emacs-agent-mcp-system-prompt))))))))
         (conversation-log nil))
    
    (make-directory output-dir t)
    
    (princ "\n=== MCP-COMPLIANT EMACS AGENT ===\n")
    (princ "Type 'exit' to quit\n")
    (princ "The agent will autonomously decide when to use tools.\n\n")
    
    (let ((continue t))
      (while continue
        (let ((user-input (read-string "You: ")))
          (if (member user-input '("exit" "quit" "bye"))
              (setq continue nil)
            
            ;; Process with MCP agent
            (let* ((response (emacs-agent-mcp-conversation user-input context output-dir))
                   (text (plist-get response :text))
                   (tool-calls (plist-get response :tool-calls)))
              
              ;; Log interaction
              (push (format "[%s] User: %s" (current-time-string) user-input) 
                    conversation-log)
              
              ;; Display response
              (when text
                (princ (format "\nAssistant: %s\n" text))
                (push (format "[%s] Assistant: %s" (current-time-string) text)
                      conversation-log))
              
              ;; Display tool usage
              (when tool-calls
                (princ "\n🔧 Tools used:\n")
                (dolist (tc tool-calls)
                  (princ (format "  - %s\n" (alist-get 'name tc)))
                  (push (format "[%s] Tool: %s" (current-time-string) (alist-get 'name tc))
                        conversation-log)))
              
              ;; Update context for next turn
              (setq context 
                    (vconcat context
                             (vector (list (cons 'role "user")
                                           (cons 'parts (vector (list (cons 'text user-input))))))
                             (when text
                               (vector (list (cons 'role "assistant")
                                             (cons 'parts (vector (list (cons 'text text)))))))))
              
              (princ "\n---\n"))))))
    
    ;; Save conversation
    (let ((log-file (expand-file-name "conversation.log" output-dir)))
      (with-temp-file log-file
        (insert "MCP Agent Conversation\n")
        (insert "======================\n\n")
        (dolist (entry (reverse conversation-log))
          (insert (format "%s\n" entry))))
      (princ (format "\nConversation saved to: %s\n" log-file)))))

;; Main entry
(defun demo-mcp ()
  "Run MCP demos."
  (if noninteractive
      (demo-test-prompts)
    (demo-interactive-mcp)))

(when noninteractive
  (demo-mcp))

(provide 'demo-mcp-strategies)
;;; demo-mcp-strategies.el ends here