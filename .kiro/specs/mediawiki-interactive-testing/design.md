# Design Document

## Overview

This design outlines a comprehensive ERT testing framework for MediaWiki interactive commands, focusing on `mediawiki-site` and `mediawiki-open`. The framework will provide thorough test coverage including unit tests, integration tests, and end-to-end workflow validation.

The design emphasizes testability, maintainability, and reliability while ensuring tests can run in various Emacs environments without external dependencies.

## Architecture

### Test Organization Structure

```
tests/
├── test-mediawiki-interactive.el          # Main test suite
├── test-mediawiki-site-command.el         # Tests for mediawiki-site
├── test-mediawiki-open-command.el         # Tests for mediawiki-open
├── test-mediawiki-supporting-functions.el # Tests for helper functions
├── test-mediawiki-error-handling.el       # Error condition tests
├── test-mediawiki-ui-integration.el       # UI enhancement tests
├── test-mediawiki-workflow.el             # End-to-end workflow tests
└── fixtures/
    ├── mock-sites.el                      # Mock site configurations
    ├── mock-responses.el                  # Mock API responses
    └── test-data.el                       # Test data and constants
```

### Test Framework Components

#### 1. Mock Infrastructure

**Mock Site Management**
- `mediawiki-test-create-mock-site`: Create test site configurations
- `mediawiki-test-setup-site-alist`: Set up test site alist
- `mediawiki-test-cleanup-sites`: Clean up test sites

**Mock Authentication**
- `mediawiki-test-mock-login-success`: Mock successful login
- `mediawiki-test-mock-login-failure`: Mock login failures
- `mediawiki-test-mock-session-state`: Mock session states

**Mock API Responses**
- `mediawiki-test-mock-api-call`: Mock API calls with predefined responses
- `mediawiki-test-mock-page-content`: Mock page content retrieval
- `mediawiki-test-mock-network-error`: Mock network failures

#### 2. Test Utilities

**Input Simulation**
- `mediawiki-test-simulate-user-input`: Simulate interactive user input
- `mediawiki-test-with-completing-read`: Mock completing-read interactions
- `mediawiki-test-with-read-string`: Mock read-string interactions

**State Management**
- `mediawiki-test-save-state`: Save current MediaWiki state
- `mediawiki-test-restore-state`: Restore saved state
- `mediawiki-test-clean-state`: Clean up test state

**Assertion Helpers**
- `mediawiki-test-assert-site-selected`: Assert correct site selection
- `mediawiki-test-assert-page-opened`: Assert page opened correctly
- `mediawiki-test-assert-buffer-state`: Assert buffer state is correct

#### 3. Test Data Management

**Site Configurations**
```elisp
(defconst mediawiki-test-sites
  '(("TestWiki" . (:url "https://test.wiki.example.com"
                   :auth-method basic
                   :first-page "Main Page"))
    ("DevWiki" . (:url "https://dev.wiki.example.com"
                  :auth-method oauth
                  :first-page "Development"))
    ("LocalWiki" . (:url "http://localhost:8080"
                    :auth-method basic
                    :first-page "Home"))))
```

**Mock Responses**
```elisp
(defconst mediawiki-test-mock-responses
  '((:login-success . ((:result . "Success") (:username . "testuser")))
    (:login-failure . ((:result . "Failed") (:reason . "Invalid credentials")))
    (:page-content . "This is test page content")
    (:page-not-found . nil)))
```

## Components and Interfaces

### 1. Core Test Suite (`test-mediawiki-interactive.el`)

**Main Test Runner**
```elisp
(ert-deftest mediawiki-interactive-test-suite ()
  "Run all interactive command tests."
  (mediawiki-test-with-clean-environment
    (mediawiki-test-run-site-command-tests)
    (mediawiki-test-run-open-command-tests)
    (mediawiki-test-run-supporting-function-tests)
    (mediawiki-test-run-error-handling-tests)
    (mediawiki-test-run-ui-integration-tests)
    (mediawiki-test-run-workflow-tests)))
```

**Environment Setup**
```elisp
(defmacro mediawiki-test-with-clean-environment (&rest body)
  "Execute BODY with a clean MediaWiki test environment."
  `(let ((mediawiki-site nil)
         (mediawiki-site-alist nil)
         (mediawiki-sessions (make-hash-table :test 'equal))
         (mediawiki-page-history '()))
     (unwind-protect
         (progn
           (mediawiki-test-setup-mock-sites)
           ,@body)
       (mediawiki-test-cleanup-environment))))
```

### 2. Site Command Tests (`test-mediawiki-site-command.el`)

**Interactive Site Selection Tests**
```elisp
(ert-deftest mediawiki-site-interactive-prompt ()
  "Test mediawiki-site prompts for site when called interactively."
  (mediawiki-test-with-mock-sites
    (mediawiki-test-with-completing-read "TestWiki"
      (call-interactively #'mediawiki-site)
      (should (string= mediawiki-site "TestWiki")))))

(ert-deftest mediawiki-site-with-argument ()
  "Test mediawiki-site accepts site as argument."
  (mediawiki-test-with-mock-sites
    (mediawiki-test-mock-login-success "TestWiki"
      (mediawiki-site "TestWiki")
      (should (string= mediawiki-site "TestWiki")))))
```

**Site Validation Tests**
```elisp
(ert-deftest mediawiki-site-invalid-site ()
  "Test mediawiki-site handles invalid site names."
  (mediawiki-test-with-mock-sites
    (should-error (mediawiki-site "NonexistentWiki")
                  :type 'error)))

(ert-deftest mediawiki-site-empty-site-list ()
  "Test mediawiki-site handles empty site list."
  (let ((mediawiki-site-alist nil))
    (should-error (call-interactively #'mediawiki-site)
                  :type 'error)))
```

### 3. Open Command Tests (`test-mediawiki-open-command.el`)

**Page Opening Tests**
```elisp
(ert-deftest mediawiki-open-interactive-prompt ()
  "Test mediawiki-open prompts for page name."
  (mediawiki-test-with-mock-site-and-ui
    (mediawiki-test-with-completing-read "Test Page"
      (call-interactively #'mediawiki-open)
      (should (get-buffer "TestWiki: Test Page")))))

(ert-deftest mediawiki-open-with-argument ()
  "Test mediawiki-open accepts page name as argument."
  (mediawiki-test-with-mock-site
    (mediawiki-open "Test Page")
    (should (get-buffer "TestWiki: Test Page"))))
```

**Error Handling Tests**
```elisp
(ert-deftest mediawiki-open-empty-name ()
  "Test mediawiki-open handles empty page name."
  (should-error (mediawiki-open "")
                :type 'error))

(ert-deftest mediawiki-open-no-site ()
  "Test mediawiki-open prompts for site when none selected."
  (let ((mediawiki-site nil))
    (mediawiki-test-with-mock-sites
      (mediawiki-test-with-completing-read "TestWiki"
        (mediawiki-test-with-read-string "Test Page"
          (call-interactively #'mediawiki-open)
          (should (string= mediawiki-site "TestWiki")))))))
```

### 4. Supporting Function Tests (`test-mediawiki-supporting-functions.el`)

**Site Prompting Tests**
```elisp
(ert-deftest mediawiki-prompt-for-site-completion ()
  "Test mediawiki-prompt-for-site provides site completion."
  (mediawiki-test-with-mock-sites
    (mediawiki-test-with-completing-read "TestWiki"
      (let ((result (mediawiki-prompt-for-site)))
        (should (string= result "TestWiki"))))))

(ert-deftest mediawiki-prompt-for-site-default ()
  "Test mediawiki-prompt-for-site shows current site as default."
  (mediawiki-test-with-mock-sites
    (let ((mediawiki-site "TestWiki"))
      (mediawiki-test-with-completing-read ""
        (let ((result (mediawiki-prompt-for-site)))
          (should (string= result "TestWiki")))))))
```

**Login Function Tests**
```elisp
(ert-deftest mediawiki-do-login-success ()
  "Test mediawiki-do-login handles successful authentication."
  (mediawiki-test-with-mock-sites
    (mediawiki-test-mock-login-success "TestWiki"
      (let ((result (mediawiki-do-login "TestWiki")))
        (should (string= result "TestWiki"))
        (should (string= mediawiki-site "TestWiki"))))))

(ert-deftest mediawiki-do-login-failure ()
  "Test mediawiki-do-login handles authentication failure."
  (mediawiki-test-with-mock-sites
    (mediawiki-test-mock-login-failure "TestWiki"
      (let ((result (mediawiki-do-login "TestWiki")))
        (should (null result))
        (should (null mediawiki-site))))))
```

### 5. Error Handling Tests (`test-mediawiki-error-handling.el`)

**Network Error Tests**
```elisp
(ert-deftest mediawiki-site-network-error ()
  "Test mediawiki-site handles network connectivity issues."
  (mediawiki-test-with-mock-sites
    (mediawiki-test-mock-network-error
      (should-error (mediawiki-site "TestWiki")
                    :type 'error))))

(ert-deftest mediawiki-open-network-error ()
  "Test mediawiki-open handles network errors gracefully."
  (mediawiki-test-with-mock-site
    (mediawiki-test-mock-network-error
      (should-error (mediawiki-open "Test Page")
                    :type 'error))))
```

**Configuration Error Tests**
```elisp
(ert-deftest mediawiki-site-malformed-config ()
  "Test mediawiki-site handles malformed site configurations."
  (let ((mediawiki-site-alist '(("BadSite" . "invalid-config"))))
    (should-error (mediawiki-site "BadSite")
                  :type 'error)))
```

### 6. UI Integration Tests (`test-mediawiki-ui-integration.el`)

**Enhanced Completion Tests**
```elisp
(ert-deftest mediawiki-open-with-ui-enhancements ()
  "Test mediawiki-open uses UI enhancements when available."
  (mediawiki-test-with-mock-site-and-ui
    (let ((mediawiki-ui-recent-pages (make-hash-table :test 'equal)))
      (puthash "TestWiki" '("Recent Page") mediawiki-ui-recent-pages)
      (mediawiki-test-with-completing-read "Recent Page"
        (call-interactively #'mediawiki-open)
        (should (get-buffer "TestWiki: Recent Page"))))))

(ert-deftest mediawiki-open-fallback-without-ui ()
  "Test mediawiki-open falls back to basic input without UI."
  (mediawiki-test-with-mock-site
    (let ((features (delq 'mediawiki-ui features)))
      (mediawiki-test-with-read-string "Test Page"
        (call-interactively #'mediawiki-open)
        (should (get-buffer "TestWiki: Test Page"))))))
```

### 7. Workflow Tests (`test-mediawiki-workflow.el`)

**End-to-End Workflow Tests**
```elisp
(ert-deftest mediawiki-complete-workflow ()
  "Test complete workflow from site selection to page editing."
  (mediawiki-test-with-clean-environment
    ;; Step 1: Select site
    (mediawiki-test-with-completing-read "TestWiki"
      (call-interactively #'mediawiki-site))

    ;; Step 2: Open page
    (mediawiki-test-with-completing-read "Test Page"
      (call-interactively #'mediawiki-open))

    ;; Step 3: Verify state
    (should (string= mediawiki-site "TestWiki"))
    (should (get-buffer "TestWiki: Test Page"))
    (should (eq major-mode 'mediawiki-mode))))
```

## Data Models

### Test Site Configuration
```elisp
(cl-defstruct mediawiki-test-site
  name                    ; Site name for testing
  url                     ; Mock URL
  auth-method             ; Authentication method to test
  first-page              ; Default page for testing
  mock-responses          ; Predefined responses for this site
  should-fail-login       ; Whether login should fail
  network-available)      ; Whether network should be available
```

### Test Session State
```elisp
(cl-defstruct mediawiki-test-session
  site-name               ; Associated test site
  authenticated           ; Whether session is authenticated
  mock-tokens             ; Mock tokens for testing
  user-info               ; Mock user information
  created-time)           ; When test session was created
```

### Test Assertion Results
```elisp
(cl-defstruct mediawiki-test-result
  test-name               ; Name of the test
  passed                  ; Whether test passed
  error-message           ; Error message if failed
  execution-time          ; Time taken to execute
  setup-time              ; Time taken for setup
  cleanup-time)           ; Time taken for cleanup
```

## Error Handling

### Test Framework Error Handling

**Setup Failures**
- Mock site creation failures
- Authentication mock setup failures
- UI component availability issues

**Execution Failures**
- Unexpected exceptions during test execution
- Timeout handling for long-running operations
- Resource cleanup failures

**Assertion Failures**
- Clear error messages for failed assertions
- Context information for debugging
- State information at time of failure

### Application Error Testing

**Network Errors**
- Connection timeouts
- DNS resolution failures
- HTTP error responses

**Authentication Errors**
- Invalid credentials
- Expired sessions
- Permission denied

**Configuration Errors**
- Malformed site configurations
- Missing required parameters
- Invalid URLs or endpoints

## Testing Strategy

### Test Categories

**Unit Tests (70% of tests)**
- Individual function testing
- Input validation
- Error condition handling
- State management

**Integration Tests (20% of tests)**
- Component interaction testing
- UI integration validation
- Authentication flow testing
- API interaction testing

**End-to-End Tests (10% of tests)**
- Complete workflow validation
- User scenario testing
- Cross-component functionality
- Performance validation

### Test Execution Strategy

**Continuous Integration**
- All tests run on every commit
- Fast feedback for developers
- Automated test result reporting

**Local Development**
- Quick test subset for rapid iteration
- Full test suite before commits
- Interactive debugging capabilities

**Release Testing**
- Comprehensive test suite execution
- Performance regression testing
- Compatibility testing across Emacs versions

### Mock Strategy

**Minimal External Dependencies**
- No network calls during testing
- No file system dependencies
- No external process dependencies

**Realistic Mock Behavior**
- Mock responses based on real API behavior
- Realistic timing and delays
- Error conditions that match real scenarios

**Deterministic Testing**
- Consistent test results across runs
- Predictable mock responses
- Controlled test environment
