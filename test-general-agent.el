;;; test-general-agent.el --- Tests for general-purpose Emacs agent -*- lexical-binding: t -*-

;;; Commentary:
;; TDD tests for a general-purpose agent

;;; Code:

(require 'ert)

;; Test 1: System prompt
(ert-deftest test-system-prompt ()
  "System prompt should be generic."
  (should (fboundp 'emacs-agent-get-system-prompt)))

;; Test 2: Tool registry  
(ert-deftest test-tool-registry ()
  "Agent should have tools."
  (should (fboundp 'emacs-agent-get-tools)))

;; Test 3: Build prompt
(ert-deftest test-build-prompt ()
  "Can build prompt from user input."
  (should (fboundp 'emacs-agent-build-prompt)))

;; Test 4: Parse response
(ert-deftest test-parse-response ()
  "Can parse LLM response."
  (should (fboundp 'emacs-agent-parse-response)))

;; Test 5: Create agent
(ert-deftest test-create-agent ()
  "Can create agent."
  (should (fboundp 'emacs-agent-create)))

;; Test 6: Add interaction
(ert-deftest test-add-interaction ()
  "Can add interaction."
  (should (fboundp 'emacs-agent-add-interaction)))

;; Test 7: Get context
(ert-deftest test-get-context ()
  "Can get context."
  (should (fboundp 'emacs-agent-get-context)))

;; Test 8: Process input
(ert-deftest test-process-input ()
  "Can process user input."
  (should (fboundp 'emacs-agent-process)))

;; Test 9: Format tools
(ert-deftest test-format-tools ()
  "Can format tools for API."
  (should (fboundp 'emacs-agent-format-tools-for-api)))

;; Test 10: Agent predicate
(ert-deftest test-agent-p ()
  "Can check if object is agent."
  (should (fboundp 'emacs-agent-p)))

(provide 'test-general-agent)
;;; test-general-agent.el ends here