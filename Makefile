# Makefile for MediaWiki.el tests

EMACS ?= emacs
BATCH = $(EMACS) --batch

# Test files
TEST_FILES = $(wildcard test-*.el)

# Autoloader file
AUTOLOADS = mediawiki-autoloads.el

# Set to something other than 1 if you want interactive tests
export NO_INTERACTION ?= 1

ERT_TESTS := $(shell grep -l ert-deftest tests/test-*.el)

.PHONY: test clean

# Run all tests
test: $(patsubst tests/%.el,%,$(ERT_TESTS))

# Pattern rule to run each test file
define TEST_RULES
$(1): $(AUTOLOADS) $(2)
	@echo "Running $(1)"
	@$(BATCH) -L $(PWD) -l $(AUTOLOADS) -l tests/$(1).el \
		-f ert-run-tests-batch-and-exit;
endef

# Generate rules for each test file
$(foreach test,$(ERT_TESTS),$(eval $(call TEST_RULES,$(notdir $(patsubst %.el,%,$(test))),$(test))))

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

test-gpg-agent:
	@echo "Running GPG agent support tests..."
	$(BATCH) -l mediawiki-core.el -l mediawiki-session.el -l test-gpg-agent-support.el

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
	@rm -f *.elc $(AUTOLOADS)

# Generate autoloader file and byte-compile
$(AUTOLOADS):
	@echo "Generating autoloader file..."
	@$(BATCH) --eval "(progn \
		(require 'autoload) \
		(setq generated-autoload-file (expand-file-name \"$(AUTOLOADS)\" default-directory)) \
		(setq backup-inhibited t) \
		(update-directory-autoloads default-directory) \
		(message \"Autoloader file created: %s\" generated-autoload-file))"
	@echo "Byte-compiling all *.el files..."
	@$(BATCH) -L $(PWD) -l $(AUTOLOADS) --eval "(progn \
		(setq byte-compile-warnings '(not obsolete)) \
		(byte-recompile-directory default-directory 0 t))"

editorconfig:
	git ls-files -z | xargs -0 grep -PzZlv "\x0a$$" | xargs -0 -I{} -n 1 sh -c 'echo >> {}'
	git ls-files -z | xargs -0 grep -PZl '[[:space:]]$$' | xargs -0 -I{} sed -i 's,[[:space:]]*$$,,' {}

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
