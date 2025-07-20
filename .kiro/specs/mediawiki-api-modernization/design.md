# Design Document

## Overview

The MediaWiki API modernization involves updating the mediawiki.el package to use current MediaWiki API standards, modern authentication methods, and improved Emacs integration. The design focuses on replacing the existing XML-based API communication with JSON-based calls, implementing robust error handling, and adding support for modern authentication methods including OAuth.

## Architecture

### Current Architecture Issues
- Uses synchronous HTTP calls that block Emacs
- Relies on XML parsing which is slower and more error-prone
- Uses deprecated login API methods
- Limited error handling and debugging capabilities
- No support for modern authentication methods

### New Architecture
The modernized architecture will implement a layered approach:

1. **HTTP Layer**: Asynchronous HTTP communication using modern Emacs HTTP libraries
2. **API Layer**: JSON-based MediaWiki API communication with proper error handling
3. **Authentication Layer**: Support for multiple authentication methods including OAuth
4. **Session Management**: Robust token and session handling with automatic refresh
5. **User Interface Layer**: Enhanced user experience with progress feedback and better error messages

## Components and Interfaces

### 1. HTTP Communication Module (`mediawiki-http.el`)

**Purpose**: Handle all HTTP communication with MediaWiki servers

**Key Functions**:
- `mediawiki-http-request-async`: Asynchronous HTTP requests with callbacks
- `mediawiki-http-request-sync`: Synchronous HTTP requests for compatibility
- `mediawiki-http-handle-response`: Process HTTP responses and extract data
- `mediawiki-http-handle-errors`: Centralized HTTP error handling

**Interface**:
```elisp
(mediawiki-http-request-async url method data callback &optional error-callback headers)
(mediawiki-http-request-sync url method data &optional timeout headers)
```

**OAuth Header Support**:
The HTTP layer needs enhancement to support custom Authorization headers for OAuth 1.0a authentication. This includes:
- Custom header parameter support in HTTP request functions
- OAuth Authorization header generation and validation
- Proper header encoding and transmission

### 2. API Communication Module (`mediawiki-api.el`)

**Purpose**: Provide high-level interface to MediaWiki API

**Key Functions**:
- `mediawiki-api-call-async`: Asynchronous API calls with JSON handling
- `mediawiki-api-call-sync`: Synchronous API calls for backward compatibility
- `mediawiki-api-parse-response`: Parse JSON responses and handle API errors
- `mediawiki-api-build-params`: Build API parameter strings

**Interface**:
```elisp
(mediawiki-api-call-async sitename action params callback &optional error-callback)
(mediawiki-api-call-sync sitename action params)
```

### 3. Authentication Module (`mediawiki-auth.el`)

**Purpose**: Handle various authentication methods

**Key Functions**:
- `mediawiki-auth-login`: Primary login function with method detection
- `mediawiki-auth-oauth-login`: OAuth authentication flow
- `mediawiki-auth-basic-login`: Traditional username/password login
- `mediawiki-auth-refresh-tokens`: Automatic token refresh
- `mediawiki-auth-logout`: Proper logout with session cleanup

**Interface**:
```elisp
(mediawiki-auth-login sitename &optional force-method)
(mediawiki-auth-check-status sitename)
(mediawiki-auth-logout sitename)
```

### 4. Session Management Module (`mediawiki-session.el`)

**Purpose**: Manage authentication tokens and session state

**Key Functions**:
- `mediawiki-session-get-token`: Retrieve and cache authentication tokens
- `mediawiki-session-refresh-token`: Refresh expired tokens
- `mediawiki-session-store-credentials`: Secure credential storage
- `mediawiki-session-cleanup`: Session cleanup on logout

**Data Structures**:
- Session cache with token expiration tracking
- Secure credential storage integration with auth-source

### 5. Error Handling Module (`mediawiki-errors.el`)

**Purpose**: Centralized error handling and user feedback

**Key Functions**:
- `mediawiki-error-handle`: Main error handling dispatcher
- `mediawiki-error-display`: User-friendly error display
- `mediawiki-error-log`: Debug logging functionality
- `mediawiki-error-suggest-fix`: Provide suggested solutions

**Error Categories**:
- Network errors (connection, timeout, DNS)
- Authentication errors (invalid credentials, expired tokens)
- API errors (permission denied, edit conflicts, rate limiting)
- Data errors (parsing failures, invalid responses)

## Data Models

### Site Configuration
```elisp
(defstruct mediawiki-site-config
  name                    ; Site display name
  url                     ; Base URL
  api-url                 ; API endpoint URL
  username                ; Username (optional, can use auth-source)
  auth-method             ; Authentication method (basic, oauth, etc.)
  auth-config             ; Method-specific configuration
  capabilities            ; Cached site capabilities
  session-info)           ; Current session information
```

### Session Information
```elisp
(defstruct mediawiki-session
  site-name               ; Associated site name
  tokens                  ; Hash table of token types and values
  token-expiry            ; Token expiration times
  user-info               ; Current user information
  login-time              ; When login occurred
  last-activity)          ; Last API activity timestamp
```

### API Response
```elisp
(defstruct mediawiki-api-response
  success                 ; Boolean success flag
  data                    ; Parsed response data
  warnings                ; API warnings
  errors                  ; API errors
  raw-response            ; Original response for debugging
  request-info)           ; Original request information
```

## Error Handling

### Error Classification
1. **Recoverable Errors**: Temporary issues that can be retried
   - Network timeouts
   - Rate limiting
   - Temporary server errors

2. **Authentication Errors**: Issues requiring user intervention
   - Invalid credentials
   - Expired tokens
   - Permission denied

3. **Data Errors**: Issues with request or response data
   - Invalid page titles
   - Edit conflicts
   - Malformed responses

4. **Configuration Errors**: Setup and configuration issues
   - Invalid site URLs
   - Missing required parameters
   - Incompatible API versions

### Error Handling Strategy
- **Automatic Retry**: For network and temporary errors with exponential backoff
- **User Prompts**: For authentication and permission issues
- **Graceful Degradation**: Fallback to read-only mode when edit permissions unavailable
- **Detailed Logging**: Comprehensive debug information when debugging enabled

## Testing Strategy

### Unit Testing
- Test individual functions with mock HTTP responses
- Test error handling with various error conditions
- Test authentication flows with different methods
- Test session management and token refresh

### Integration Testing
- Test against real MediaWiki installations
- Test with different MediaWiki versions
- Test various authentication configurations
- Test error recovery scenarios

### Performance Testing
- Measure response times for API calls
- Test with large pages and bulk operations
- Verify memory usage with long-running sessions
- Test concurrent operation handling

### Compatibility Testing
- Test with different Emacs versions (28.1+)
- Test with various MediaWiki configurations
- Test integration with other Emacs packages
- Test on different operating systems

## Implementation Phases

### Phase 1: Core Infrastructure
- Implement new HTTP communication layer
- Create basic API communication framework
- Set up error handling infrastructure
- Implement JSON response parsing

### Phase 2: Authentication Modernization
- Implement modern login API usage
- Add OAuth support framework
- Integrate with auth-source library
- Implement token management

### Phase 3: API Operations
- Update page retrieval operations
- Modernize page saving functionality
- Implement proper edit conflict handling
- Add progress feedback for operations

### Phase 4: Enhanced Features
- Add asynchronous operation support
- Implement advanced error recovery
- Add debugging and logging enhancements
- Optimize performance and caching

### Phase 5: Testing and Polish
- Comprehensive testing suite
- Documentation updates
- Backward compatibility verification
- Performance optimization

## Migration Strategy

### Backward Compatibility
- Maintain existing function signatures where possible
- Provide compatibility shims for deprecated functions
- Gradual migration path for existing configurations
- Clear migration documentation

### Configuration Migration
- Automatic detection of old configuration format
- Migration wizard for complex configurations
- Validation of migrated configurations
- Rollback capability for failed migrations

### Testing Migration
- Side-by-side testing with old implementation
- Gradual rollout with feature flags
- User feedback collection mechanism
- Quick rollback procedure if issues arise