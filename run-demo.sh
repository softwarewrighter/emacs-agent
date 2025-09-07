#!/usr/bin/env bash
# Run the AI agent demo and capture outputs

set -euo pipefail

# Ensure test_output directory exists
mkdir -p test_output

# Check for API key
if [ -z "$GEMINI_API_KEY" ]; then
    echo "Error: GEMINI_API_KEY environment variable not set"
    echo "Please set: export GEMINI_API_KEY='your-api-key'"
    exit 1
fi

echo "Starting AI Agent Demo..."
echo "========================="
echo ""

# Run the demo
emacs --batch -l ai-agent.el -l run-demo.el 2>&1

echo ""
echo "Demo complete!"
echo ""

# Show what was created
if [ -d "test_output" ]; then
    echo "Output directory contents:"
    latest_session=$(ls -t test_output | head -1)
    if [ -n "$latest_session" ]; then
        echo "Latest session: test_output/$latest_session"
        echo ""
        echo "Files created:"
        ls -la "test_output/$latest_session"
        echo ""
        
        # Show conversation snippet
        if [ -f "test_output/$latest_session/conversation.el" ]; then
            echo "Conversation log preview:"
            head -20 "test_output/$latest_session/conversation.el"
            echo "..."
        fi
        
        # Show summary
        if [ -f "test_output/$latest_session/summary.txt" ]; then
            echo ""
            echo "Session summary:"
            cat "test_output/$latest_session/summary.txt"
        fi
    fi
fi