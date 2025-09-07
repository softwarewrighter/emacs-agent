#!/usr/bin/env bash
# Simple demo focusing on math library creation with autonomous tool use

set -euo pipefail

echo "====================================="
echo "  Math Library Creation Demo"
echo "  Autonomous Tool Use with Gemini"
echo "====================================="
echo ""
echo "This demo shows Gemini autonomously deciding"
echo "when to use the file_write tool based on"
echo "natural language requests."
echo ""

# Set API key
# Set your Gemini API key as environment variable:
# export GEMINI_API_KEY="your-api-key-here"

if [ -z "$GEMINI_API_KEY" ]; then
    echo "Error: GEMINI_API_KEY environment variable not set"
    echo "Please set: export GEMINI_API_KEY='your-api-key'"
    exit 1
fi

# Run the demo
echo "Running demo..."
echo "--------------------------------"

emacs --batch \
      --load emacs-agent-mcp.el \
      --load demo-math-library.el \
      --eval "(demo-math-library)" 2>&1

echo ""
echo "====================================="
echo "To review what happened:"
echo ""
echo "1. Check test_output/ for the latest math_demo_* directory"
echo "2. Review demo.log to see when tools were used"
echo "3. Examine .el files created by Gemini"
echo ""
echo "Key insight: Gemini autonomously decides"
echo "whether to use file_write based on the request!"
echo "====================================="