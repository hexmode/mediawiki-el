# Future TODOs for MediaWiki API Modernization

This document tracks features and improvements that are planned for future implementation but are not part of the current modernization effort.

## Authentication Enhancements

### Two-Factor Authentication Support (from Requirement 2.4)

**Original Requirement:** IF a site requires two-factor authentication THEN the system SHALL provide appropriate prompts and handling

**Status:** Deferred - not planned for immediate implementation

**Details:**
- The current implementation has a framework for 2FA handling in the `mediawiki-auth-handle-ui-requirement` function
- Placeholder functions exist for `mediawiki-auth-handle-2fa` and `mediawiki-auth-handle-captcha`
- Full implementation would require:
  - Interactive prompting for OTP codes
  - Support for various 2FA methods (TOTP, SMS, etc.)
  - Proper continuation of login flow after 2FA verification
  - Testing with wikis that have 2FA enabled

**Implementation Notes:**
- The foundation is in place with the modern login response handling
- Would likely require `action=clientlogin` instead of `action=login` for full 2FA support
- Could be implemented when OAuth support is added (as both are "advanced" authentication features)

### OAuth Authentication Support

**Status:** Planned for future implementation

**Details:**
- OAuth would be the truly "modern" authentication method
- Current implementation uses reliable `action=login` with bot passwords
- OAuth implementation would require:
  - OAuth 1.0a or OAuth 2.0 flow implementation
  - Token management and refresh
  - Integration with MediaWiki's OAuth extension
  - User authorization flow handling

## API Enhancements

### Advanced Error Recovery

**Status:** Future enhancement

**Details:**
- More sophisticated retry logic for transient failures
- Automatic token refresh on expiration
- Better handling of rate limiting with exponential backoff

### Performance Optimizations

**Status:** Future enhancement

**Details:**
- Connection pooling for multiple API calls
- Batch API request support
- Caching of frequently accessed data

## Testing and Quality

### Comprehensive Test Suite

**Status:** Ongoing improvement

**Details:**
- More real-world wiki testing scenarios
- Integration tests with different MediaWiki versions
- Performance benchmarking

---

## Notes

- Items in this document are not blocking the current modernization effort
- They can be implemented incrementally as needed
- Priority should be given to core functionality and reliability first