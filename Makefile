# Makefile for MediaWiki.el tests

EMACS ?= emacs
BATCH = $(EMACS) --batch

.DEFAULT_GOAL := help

# Test files
TEST_FILES := $(wildcard tests/test-*.el)

# Autoloader file
AUTOLOADS = mediawiki-autoloads.el

# Set to something other than 1 if you want interactive tests
export NO_INTERACTION ?= 1

.PHONY: test clean autoloads

# Add test target if there are any
ifneq ($(TEST_FILES),)
ERT_TESTS := $(shell grep -l ert-deftest $(TEST_FILES))

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
endif

clean:
	@echo "Cleaning up artifacts..."
	@rm -f *.elc $(AUTOLOADS)

# Generate autoloader file and byte-compile
autoloads: $(AUTOLOADS)
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
ifneq ($(TEST_FILES),)
	@echo "  test           - Run all tests"
endif
	@echo "  autoloads      - Build autoloader"
	@echo "  editorconfig   - Clean up whitespace"
	@echo "  clean          - Remove compiled files"
	@echo "  help           - Show this help"
