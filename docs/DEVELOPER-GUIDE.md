# MediaWiki.el Developer Guide

This guide provides technical documentation for developers working on MediaWiki.el or extending its functionality.

## Table of Contents

- [Architecture Overview](#architecture-overview)
- [Module Structure](#module-structure)
- [Development Setup](#development-setup)
- [Testing Framework](#testing-framework)
- [API Reference](#api-reference)
- [Extension Points](#extension-points)
- [Contributing Guidelines](#contributing-guidelines)

## Architecture Overview

MediaWiki.el 2.5+ uses a modular architecture with clear separation of concerns:

```
mediawiki.el (Main UI)
├── mediawiki-core.el (Data structures, utilities)
├── mediawiki-http.el (HTTP communication layer)
├── mediawiki-api.el (MediaWiki API client)
├── mediawiki-auth.el (Authentication handling)
├── mediawiki-oauth.el (OAuth implementation)
├── mediawiki-session.el (Session management)
├── mediawiki-page.el (Page operations)
├── mediawiki-errors.el (Error handling)
├── mediawiki-ui.el (Modern UI components)
└── mediawiki-compat.el (Backward compatibility)
```

### Key Design Principles

1. **Async-First**: All operations support asynchronous execution
2. **Error Recovery**: Comprehensive error handling with retry logic
3. **Session Persistence**: State survives Emacs restarts
4. **Security**: Auth-source integration, encrypted sessions
5. **Testability**: Extensive test coverage with mocking support

## Module Structure

### Core Data Structures (`mediawiki-core.el`)

#### Site Configuration
```elisp
(cl-defstruct mediawiki-site-config
  name url username auth-method auth-config api-url timeout)
```

#### Session State
```elisp
(cl-defstruct mediawiki-session
  site-name login-token edit-token cookies last-activity status)
```

#### API Response
```elisp
(cl-defstruct mediawiki-api-response
  success data errors warnings continuation)
```

### HTTP Layer (`mediawiki-http.el`)

Provides robust HTTP communication with:
- Retry logic with exponential backoff
- Progress callbacks
- Request/response logging
- Custom header support for OAuth

Key functions:
```elisp
(mediawiki-http-request-async url method params callback error-callback)
(mediawiki-http-request-sync url method params)
```

### API Client (`mediawiki-api.el`)

MediaWiki API abstraction layer:
- JSON-based API communication
- Automatic token management
- Error classification
- Rate limiting support

Key functions:
```elisp
(mediawiki-api-call-async sitename action params success-callback error-callback)
(mediawiki-api-call-sync sitename action params)
```

### Authentication (`mediawiki-auth.el`, `mediawiki-oauth.el`)

Multi-method authentication system:
- Basic authentication with auth-source
- OAuth 1.0a implementation
- Domain authentication for enterprise
- Automatic token refresh

### Session Management (`mediawiki-session.el`)

Persistent session handling:
- Cross-restart persistence
- Automatic cleanup
- Token lifecycle management
- State validation

### Page Operations (`mediawiki-page.el`)

High-level page editing operations:
- Conflict detection and resolution
- Draft management
- Edit validation
- Save failure recovery

## Development Setup

### Prerequisites

- Emacs 28.1+
- Git
- Make (for running tests)
- MediaWiki test instance (optional, for integration tests)

### Development Environment

1. **Clone the repository**:
```bash
git clone https://github.com/hexmode/mediawiki-el.git
cd mediawiki-el
```

2. **Set up development configuration**:
```elisp
;; In your .emacs or init.el
(add-to-list 'load-path "/path/to/mediawiki-el")
(require 'mediawiki)

;; Enable debug mode
(setq mediawiki-debug t)
(setq mediawiki-log-level 'debug)
```

3. **Configure test environment**:
```elisp
;; Test site configuration
(setq mediawiki-site-alist
      '(("TestWiki" .
         (make-mediawiki-site-config
          :name "TestWiki"
          :url "http://localhost:8080/mediawiki/"
          :username "testuser"
          :auth-method 'basic))))
```

### Development Commands

```bash
# Run all tests
make test

# Run specific test categories
make test-minimal        # Basic functionality
make test-mock          # Mock tests
make test-integration   # Integration tests
make test-gpg          # GPG/encryption tests

# Development utilities
make clean             # Clean compiled files
make list-tests        # List available tests
```

## Testing Framework

### Test Structure

Tests are organized by functionality:
- `test-*.el` - Core functionality tests
- `test-auth-*.el` - Authentication tests
- `test-gpg-*.el` - GPG and encryption tests
- `test-backward-compatibility.el` - Compatibility tests
- `test-migration-tools-basic.el` - Migration tests

### Writing Tests

#### Basic Test Structure
```elisp
(require 'ert)
(require 'mediawiki-core)

(ert-deftest test-my-function ()
  "Test description."
  (should (equal (my-function "input") "expected-output")))
```

#### Mock Framework
```elisp
(defun test-with-mock-http (test-function)
  "Run TEST-FUNCTION with mocked HTTP layer."
  (advice-add 'mediawiki-http-request-async :override #'mock-http-request)
  (unwind-protect
      (funcall test-function)
    (advice-remove 'mediawiki-http-request-async #'mock-http-request)))

(defun mock-http-request (url method params callback error-callback)
  "Mock HTTP request for testing."
  (funcall callback '((status . success) (data . "mock response"))))
```

#### Async Testing
```elisp
(ert-deftest test-async-operation ()
  "Test asynchronous operations."
  (let ((result nil)
        (error nil))
    (mediawiki-api-call-async "test-site" "query" '()
                              (lambda (response) (setq result response))
                              (lambda (err) (setq error err)))

    ;; Wait for async operation
    (while (and (not result) (not error))
      (sit-for 0.1))

    (should result)
    (should-not error)))
```

### Test Categories

#### Unit Tests
Test individual functions in isolation:
```elisp
(ert-deftest test-site-config-creation ()
  "Test site configuration creation."
  (let ((config (make-mediawiki-site-config
                 :name "test"
                 :url "https://example.com/"
                 :username "user"
                 :auth-method 'basic)))
    (should (mediawiki-site-config-p config))
    (should (string= (mediawiki-site-config-name config) "test"))))
```

#### Integration Tests
Test complete workflows:
```elisp
(ert-deftest test-full-edit-workflow ()
  "Test complete page editing workflow."
  (test-with-clean-state
   (lambda ()
     ;; Login
     (should (mediawiki-do-login "test-site"))

     ;; Open page
     (should (mediawiki-open "Test Page"))

     ;; Edit content
     (insert "New content")

     ;; Save
     (should (mediawiki-save "Test edit")))))
```

#### Mock Tests
Test with simulated external dependencies:
```elisp
(ert-deftest test-auth-source-integration ()
  "Test auth-source integration with mocked data."
  (test-with-mock-auth-source
   '(("example.com" "testuser" "testpass"))
   (lambda ()
     (let ((creds (mediawiki-auth-get-credentials "example.com" "testuser")))
       (should (string= (plist-get creds :password) "testpass"))))))
```

## API Reference

### Core Functions

#### Site Management
```elisp
(mediawiki-add-site site-config)
(mediawiki-get-site sitename)
(mediawiki-remove-site sitename)
(mediawiki-list-sites)
```

#### API Operations
```elisp
(mediawiki-api-call-sync sitename action params)
(mediawiki-api-call-async sitename action params success-callback error-callback)
```

#### Authentication
```elisp
(mediawiki-auth-login site-config)
(mediawiki-auth-logout sitename)
(mediawiki-auth-get-credentials site username)
```

#### Session Management
```elisp
(mediawiki-session-get sitename)
(mediawiki-session-create sitename)
(mediawiki-session-update sitename updates)
(mediawiki-session-cleanup)
```

### Data Structure Accessors

#### Site Configuration
```elisp
(mediawiki-site-config-name config)
(mediawiki-site-config-url config)
(mediawiki-site-config-username config)
(mediawiki-site-config-auth-method config)
(mediawiki-site-config-auth-config config)
```

#### API Response
```elisp
(mediawiki-api-response-success response)
(mediawiki-api-response-data response)
(mediawiki-api-response-errors response)
(mediawiki-api-response-warnings response)
```

#### Session
```elisp
(mediawiki-session-site-name session)
(mediawiki-session-login-token session)
(mediawiki-session-edit-token session)
(mediawiki-session-cookies session)
```

### Error Handling

#### Error Types
```elisp
(define-error 'mediawiki-error "MediaWiki error")
(define-error 'mediawiki-auth-error "MediaWiki authentication error" 'mediawiki-error)
(define-error 'mediawiki-api-error "MediaWiki API error" 'mediawiki-error)
(define-error 'mediawiki-network-error "MediaWiki network error" 'mediawiki-error)
```

#### Error Handling Functions
```elisp
(mediawiki-error-classify error-data)
(mediawiki-error-recoverable-p error)
(mediawiki-error-retry-delay error)
```

## Extension Points

### Custom Authentication Methods

Implement custom authentication by extending the auth system:

```elisp
(defun my-custom-auth-method (site-config)
  "Custom authentication implementation."
  (let ((credentials (my-get-credentials site-config)))
    (mediawiki-api-call-sync
     (mediawiki-site-config-name site-config)
     "login"
     `(("lgname" . ,(plist-get credentials :username))
       ("lgpassword" . ,(plist-get credentials :password))
       ("lgtoken" . ,(mediawiki-get-login-token site-config))))))

;; Register custom method
(setf (alist-get 'my-custom mediawiki-auth-methods) 'my-custom-auth-method)
```

### Custom UI Components

Extend the UI system:

```elisp
(defun my-custom-ui-component ()
  "Custom UI component."
  (interactive)
  (let ((buffer (get-buffer-create "*My MediaWiki UI*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Custom MediaWiki interface\n")
      (insert "========================\n")
      ;; Add your custom UI code here
      (special-mode))
    (display-buffer buffer)))

;; Add to quick menu
(add-to-list 'mediawiki-ui-quick-menu-items
             '("My Custom Component" . my-custom-ui-component))
```

### Custom API Endpoints

Add support for custom API endpoints:

```elisp
(defun my-custom-api-call (sitename endpoint params)
  "Call custom API endpoint."
  (let ((url (concat (mediawiki-site-config-url (mediawiki-get-site sitename))
                     "/api/" endpoint)))
    (mediawiki-http-request-sync url "POST" params)))

;; Usage
(my-custom-api-call "mysite" "myextension" '(("action" . "customaction")))
```

### Hooks and Callbacks

MediaWiki.el provides various hooks for customization:

```elisp
;; Page editing hooks
(add-hook 'mediawiki-mode-hook 'my-wiki-setup)
(add-hook 'mediawiki-before-save-hook 'my-pre-save-validation)
(add-hook 'mediawiki-after-save-hook 'my-post-save-actions)

;; Authentication hooks
(add-hook 'mediawiki-login-hook 'my-login-handler)
(add-hook 'mediawiki-logout-hook 'my-logout-handler)

;; Session hooks
(add-hook 'mediawiki-session-created-hook 'my-session-setup)
(add-hook 'mediawiki-session-expired-hook 'my-session-recovery)
```

## Contributing Guidelines

### Code Style

Follow Emacs Lisp conventions:
- Use `mediawiki-` prefix for public functions
- Use `mediawiki--` prefix for private functions
- Docstrings for all public functions
- Type hints where appropriate

### Documentation

All contributions should include:
- Function docstrings
- Usage examples
- Test cases
- Updated README if needed

### Testing Requirements

All code changes must include:
- Unit tests for new functions
- Integration tests for workflows
- Mock tests for external dependencies
- Documentation updates

### Submission Process

1. **Fork the repository**
2. **Create feature branch**: `git checkout -b feature-name`
3. **Make changes** with tests and documentation
4. **Run test suite**: `make test`
5. **Submit pull request** with description of changes

### Performance Considerations

When contributing:
- Prefer async operations for network calls
- Use appropriate data structures
- Profile performance-critical code
- Consider memory usage for large datasets

### Security Guidelines

Security considerations:
- Never log passwords or tokens
- Use auth-source for credential storage
- Validate all user inputs
- Escape data in API calls

---

This developer guide provides the foundation for working with MediaWiki.el internals. For user-facing documentation, see the [User Guide](USER-GUIDE.md) and [Configuration Examples](CONFIGURATION-EXAMPLES.md).
