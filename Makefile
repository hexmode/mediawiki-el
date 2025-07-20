# Makefile for MediaWiki.el tests

EMACS = emacs
BATCH = $(EMACS) --batch

# Core files that tests depend on
CORE_FILES = mediawiki-core.el mediawiki-auth.el

# Test files
TEST_FILES = $(wildcard test-*.el)

.PHONY: test clean

# Run all tests
test:
	@echo "Running all MediaWiki.el tests..."
	$(BATCH) -l run-tests.el

# Run individual test files
test-minimal:
	@echo "Running minimal auth tests..."
	$(BATCH) -l test-auth-minimal.el

test-mock:
	@echo "Running mock auth-source tests..."
	$(BATCH) -l test-auth-source-mock.el

test-integration:
	@echo "Running integration demo tests..."
	$(BATCH) -l test-auth-integration-demo.el

# GPG and session persistence tests
test-session-persistence:
	@echo "Running session persistence tests..."
	$(BATCH) -l mediawiki-core.el -l mediawiki-session.el -l test-session-persistence.el

test-gpg-agent:
	@echo "Running GPG agent support tests..."
	$(BATCH) -l mediawiki-core.el -l mediawiki-session.el -l test-gpg-agent-support.el

test-gpg-integration:
	@echo "Running automated GPG integration tests..."
	$(BATCH) -l mediawiki-core.el -l mediawiki-session.el -l test-gpg-integration.el

test-gpg: test-session-persistence test-gpg-agent test-gpg-integration
	@echo "All GPG tests completed!"

# Verbose GPG tests (for debugging)
test-gpg-verbose:
	@echo "Running GPG tests with verbose output..."
	VERBOSE=1 $(BATCH) -l mediawiki-core.el -l mediawiki-session.el -l test-gpg-integration.el

# List available tests
list-tests:
	@echo "Available test files:"
	@ls test-*.el

clean:
	@echo "Cleaning up test artifacts..."
	@rm -f *.elc

help:
	@echo "Available targets:"
	@echo "  test           - Run all tests"
	@echo "  test-minimal   - Run minimal auth tests"
	@echo "  test-mock      - Run mock auth-source tests"
	@echo "  test-integration - Run integration demo tests"
	@echo "  test-session-persistence - Run session persistence tests"
	@echo "  test-gpg-agent - Run GPG agent support tests"
	@echo "  test-gpg-integration - Run automated GPG integration tests"
	@echo "  test-gpg       - Run all GPG-related tests"
	@echo "  test-gpg-verbose - Run GPG tests with verbose output"
	@echo "  list-tests     - List all test files"
	@echo "  clean          - Clean up compiled files"
	@echo "  help           - Show this help"