#!/usr/bin/env bash
# Validate all elisp files for balanced parens

set -euo pipefail

echo "Validating all .el files for balanced parentheses..."

for file in *.el; do
  echo -n "Checking $file... "
  if emacs --batch --eval "(progn
      (condition-case err
          (with-temp-buffer
            (insert-file-contents \"$file\")
            (emacs-lisp-mode)
            (check-parens)
            (goto-char (point-min))
            (while (not (eobp))
              (forward-sexp))
            (princ \"OK\"))
        (error
          (princ (format \"ERROR: %s\" (error-message-string err)))
          (kill-emacs 1))))" 2>/dev/null; then
    echo ""
  else
    echo "FAILED"
    exit 1
  fi
done

echo ""
echo "All files validated successfully!"

# Run tests
echo ""
echo "Running all tests..."
# Check for API key
if [ -z "$GEMINI_API_KEY" ]; then
    echo "Error: GEMINI_API_KEY environment variable not set"
    echo "Please set: export GEMINI_API_KEY='your-api-key'"
    exit 1
fi

for test_file in test-*.el; do
  echo ""
  echo "Running $test_file..."
  emacs --batch -l emacs-agent-mcp.el -l "$test_file" -f ert-run-tests-batch-and-exit 2>&1 | tail -5
done

echo ""
echo "✅ All validation and tests complete!"