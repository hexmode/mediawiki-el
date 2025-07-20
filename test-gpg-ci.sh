#!/bin/bash

# CI-friendly GPG integration test script
# Returns appropriate exit codes for automated testing

set -e

echo "=== MediaWiki.el GPG Integration Tests ==="
echo "Environment: $(uname -s) $(uname -r)"
echo "Emacs: $(emacs --version | head -1)"

# Check if GPG is available
if command -v gpg >/dev/null 2>&1; then
    echo "GPG: $(gpg --version | head -1)"

    # Check if gpg-agent is available
    if command -v gpg-connect-agent >/dev/null 2>&1; then
        if gpg-connect-agent /bye >/dev/null 2>&1; then
            echo "GPG Agent: Running"
        else
            echo "GPG Agent: Not running"
        fi
    else
        echo "GPG Agent: Not available"
    fi
else
    echo "GPG: Not available"
fi

echo ""

# Run the automated test suite
echo "Running GPG integration tests..."
if make test-gpg-integration; then
    echo "✓ GPG integration tests passed"
    exit 0
else
    echo "✗ GPG integration tests failed"
    exit 1
fi
