#!/usr/bin/env bash
# Clean up test output directories

if [ -d "test_output" ]; then
    echo "Test output directory contents:"
    echo "=============================="
    
    # Count sessions
    session_count=$(find test_output -maxdepth 1 -type d -name "*_*" | wc -l)
    file_count=$(find test_output -name "*.el" | wc -l)
    
    echo "Sessions: $session_count"
    echo "Total .el files: $file_count"
    echo ""
    
    # Show disk usage
    echo "Disk usage:"
    du -sh test_output
    echo ""
    
    # List recent sessions
    echo "Recent sessions:"
    ls -lt test_output | head -6
    echo ""
    
    read -p "Remove all test output? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        rm -rf test_output
        echo "Test output cleaned."
    else
        echo "Test output preserved."
    fi
else
    echo "No test_output directory found."
fi