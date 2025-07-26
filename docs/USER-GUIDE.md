# MediaWiki.el User Guide

A comprehensive guide to using MediaWiki.el 2.5+ for editing MediaWiki sites from Emacs.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [Basic Usage](#basic-usage)
- [Advanced Features](#advanced-features)
- [Authentication](#authentication)
- [Troubleshooting](#troubleshooting)

## Installation

### From MELPA (Recommended)

1. Add MELPA to your package sources if not already done:
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

2. Install the package:
```
M-x package-install RET mediawiki RET
```

### Manual Installation

1. Download the source files
2. Place them in your `load-path` (usually `~/.emacs.d/`)
3. Add to your configuration:
```elisp
(require 'mediawiki)
```

## Quick Start

### Basic Setup

1. Configure your first site:
```
M-x customize-group RET mediawiki RET
```

2. Set up a site (example for Wikipedia):
   - **Site Name**: `Wikipedia`
   - **URL**: `https://en.wikipedia.org/`
   - **Username**: `your-username`
   - **Auth Method**: `basic` (or `oauth` for better security)

3. Start editing:
```
M-x mediawiki-site RET Wikipedia RET
M-x mediawiki-open RET Article_Name RET
```

### Your First Edit

1. Open an article: `M-x mediawiki-open RET Sandbox RET`
2. Edit the content in the buffer
3. Save your changes: `C-x C-s` or `M-x mediawiki-save`
4. Provide an edit summary when prompted

## Configuration

### Site Configuration

MediaWiki.el supports multiple site configurations using the modern `mediawiki-site-alist` format:

```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" .
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "YourUsername"
          :auth-method 'basic))

        ("MyWiki" .
         (make-mediawiki-site-config
          :name "MyWiki"
          :url "https://wiki.example.com/"
          :username "admin"
          :auth-method 'oauth
          :auth-config '(:consumer-key "your-key"
                        :consumer-secret "your-secret")))))
```

### Authentication Methods

#### Basic Authentication
```elisp
(make-mediawiki-site-config
 :name "MySite"
 :url "https://wiki.example.com/"
 :username "myuser"
 :auth-method 'basic)
```
- Password stored securely via auth-source
- Supports domain authentication for enterprise wikis

#### OAuth Authentication
```elisp
(make-mediawiki-site-config
 :name "Wikipedia"
 :url "https://en.wikipedia.org/"
 :username "myuser"
 :auth-method 'oauth
 :auth-config '(:consumer-key "your-consumer-key"
               :consumer-secret "your-consumer-secret"))
```
- More secure than basic auth
- Requires registering an OAuth consumer on the wiki
- Tokens automatically refreshed

### Customization Options

```elisp
;; Enable non-blocking operations for better performance
(setq mediawiki-use-non-blocking t)

;; Set default edit summary
(setq mediawiki-edit-summary-default "Minor edit via Emacs")

;; Enable debug mode for troubleshooting
(setq mediawiki-debug t)

;; Configure session persistence
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
```

## Basic Usage

### Opening Pages

```
M-x mediawiki-open RET PageName RET
```

The page opens in a buffer with `mediawiki-mode` enabled, providing:
- Syntax highlighting for MediaWiki markup
- Convenient editing commands
- Automatic mode detection for `.wiki`, `.mediawiki`, `.mw` files

### Saving Pages

- **Simple save**: `C-x C-s` or `M-x mediawiki-save`
- **Save with different name**: `C-x C-w` or `M-x mediawiki-save-as`
- **Save and close buffer**: `M-x mediawiki-save-and-bury`

All save operations prompt for an edit summary, which is recommended for tracking changes.

### Site Management

- **Switch sites**: `M-x mediawiki-site RET SiteName RET`
- **Login to site**: `M-x mediawiki-do-login`
- **Logout**: `M-x mediawiki-do-logout`
- **View in browser**: `M-x mediawiki-browse`

## Advanced Features

### Modern UI Enhancements

#### Quick Menu
```
M-x mediawiki-ui-quick-menu
```
Provides fast access to common operations:
- Open/Save/Browse pages
- Site switching
- Authentication management
- Statistics and debugging

#### Enhanced Save Dialog
```
M-x mediawiki-ui-save-with-options
```
Advanced save interface with:
- Edit summary suggestions
- Minor edit checkbox
- Watch page option
- Conflict detection and resolution

#### Site Statistics
```
M-x mediawiki-ui-show-site-statistics
```
Displays comprehensive information about:
- Site configuration and status
- Authentication state
- Session information
- Recent operations

### Asynchronous Operations

MediaWiki.el 2.5+ supports non-blocking operations:

```elisp
;; Enable non-blocking mode
(setq mediawiki-use-non-blocking t)

;; View async operations
M-x mediawiki-async-list-operations

;; Show async statistics
M-x mediawiki-async-show-statistics
```

### Session Management

- **View session status**: `M-x mediawiki-session-status`
- **Clear all sessions**: `M-x mediawiki-session-clear-all-states`
- **Session report**: `M-x mediawiki-session-report-status`

Sessions persist across Emacs restarts and include:
- Authentication tokens
- Edit tokens
- Site preferences
- Recent page history

### Draft Mode

Create and edit drafts before publishing:

```
M-x mediawiki-draft
```

This opens a temporary buffer where you can:
- Compose content offline
- Use all mediawiki-mode features
- Save as draft or publish to wiki when ready

### Authentication Management

```
M-x mediawiki-ui-manage-authentication
```

Comprehensive authentication interface for:
- Testing login credentials
- Managing OAuth tokens
- Viewing authentication status
- Refreshing expired sessions

## Authentication

### Setting up Auth-Source

MediaWiki.el integrates with Emacs' auth-source library for secure password storage.

#### Using GPG (Recommended)

1. Create `~/.authinfo.gpg`:
```
machine en.wikipedia.org login yourusername password yourpassword
machine wiki.example.com login admin password adminpass
```

2. GPG will encrypt the file automatically

#### Using Plain Text (Less Secure)

Create `~/.authinfo`:
```
machine en.wikipedia.org login yourusername password yourpassword
```

#### Auth-Source Configuration

```elisp
;; Use specific auth-source files
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Enable GPG agent for password caching
(setq epa-pinentry-mode 'loopback)
```

### OAuth Setup

For sites supporting OAuth (like Wikipedia):

1. **Register OAuth Consumer**:
   - Go to `Special:OAuthConsumerRegistration` on your wiki
   - Request a consumer key/secret pair
   - Note the callback URL requirements

2. **Configure MediaWiki.el**:
```elisp
(make-mediawiki-site-config
 :name "Wikipedia"
 :url "https://en.wikipedia.org/"
 :username "yourusername"
 :auth-method 'oauth
 :auth-config '(:consumer-key "your-consumer-key"
               :consumer-secret "your-consumer-secret"))
```

3. **Complete OAuth Flow**:
   - First login attempt will open browser for authorization
   - Grant permissions to your application
   - Return to Emacs to complete setup

## Troubleshooting

### Common Issues

#### "Cannot connect to site"
1. Check internet connection
2. Verify site URL is correct
3. Test site accessibility in browser
4. Check for proxy/firewall issues

#### "Authentication failed"
1. Verify username/password in auth-source
2. Check if account exists and is active
3. For OAuth, ensure consumer keys are valid
4. Try logging in via web browser first

#### "Edit conflict detected"
1. Someone else edited the page simultaneously
2. MediaWiki.el will show conflict resolution options
3. Choose to merge changes or overwrite
4. Review changes before saving

#### "Session expired"
1. Long editing sessions may timeout
2. MediaWiki.el automatically refreshes sessions
3. If problems persist, logout and login again
4. Check `M-x mediawiki-session-status`

### Debug Mode

Enable debugging for detailed troubleshooting:

```elisp
(setq mediawiki-debug t)
```

Then use:
- `M-x mediawiki-debug-view` - View debug buffer
- `M-x mediawiki-session-report-status` - Detailed session info
- `M-x mediawiki-ui-show-site-statistics` - Site diagnostics

### Performance Issues

For large wikis or slow connections:

```elisp
;; Enable non-blocking operations
(setq mediawiki-use-non-blocking t)

;; Increase timeout values
(setq mediawiki-http-timeout 30)

;; Reduce concurrent operations
(setq mediawiki-max-concurrent-operations 3)
```

### Getting Help

1. **Check the debug buffer**: `M-x mediawiki-debug-view`
2. **Review session status**: `M-x mediawiki-session-status`
3. **Test site connectivity**: Use built-in site statistics
4. **Check configuration**: Verify site settings in customize
5. **Report bugs**: Visit the project repository for issue reporting

### Migration from Older Versions

If upgrading from MediaWiki.el < 2.5, use the migration tools:

```
M-x mediawiki-compat-migration-wizard
```

This will:
- Detect legacy configuration
- Convert to modern format
- Validate migrated settings
- Provide detailed migration report

For more details, see the [Migration Guide](MIGRATION-GUIDE.md).

## Tips and Best Practices

### Editing Workflow
1. Always provide meaningful edit summaries
2. Use draft mode for complex edits
3. Preview changes when possible (via browser)
4. Save frequently to avoid conflicts

### Site Management
1. Configure multiple sites for easy switching
2. Use OAuth when available for better security
3. Keep auth-source files encrypted
4. Regular session cleanup for privacy

### Performance
1. Enable non-blocking mode for better responsiveness
2. Use site statistics to monitor performance
3. Clear old sessions periodically
4. Monitor async operations for bottlenecks

---

For more advanced usage and development information, see the [Developer Documentation](DEVELOPER-GUIDE.md) and [Migration Guide](MIGRATION-GUIDE.md).
