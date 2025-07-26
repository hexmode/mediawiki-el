# MediaWiki.el Migration Guide

This guide helps users migrate from older versions of MediaWiki.el (< 2.5.0) to the modernized version with new architecture, enhanced features, and improved security.

## Table of Contents

- [Overview](#overview)
- [What's New in 2.5+](#whats-new-in-25)
- [Before You Start](#before-you-start)
- [Automatic Migration](#automatic-migration)
- [Manual Migration](#manual-migration)
- [Configuration Changes](#configuration-changes)
- [API Changes](#api-changes)
- [Troubleshooting](#troubleshooting)
- [Post-Migration Tasks](#post-migration-tasks)

## Overview

MediaWiki.el 2.5+ represents a complete modernization of the package with:

- **New Architecture**: Modular design with clear separation of concerns
- **Enhanced Security**: OAuth support, auth-source integration, encrypted sessions
- **Better Performance**: Asynchronous operations, session persistence, retry logic
- **Modern UI**: Enhanced interfaces, progress feedback, conflict resolution
- **Comprehensive Testing**: Extensive test suite ensuring reliability

The migration process preserves your existing configuration while upgrading to modern patterns.

## What's New in 2.5+

### Major Changes

#### Authentication System
- **Old**: Plain text passwords in configuration
- **New**: Secure auth-source integration with GPG encryption
- **Added**: OAuth 1.0a support for better security
- **Added**: Domain authentication for enterprise wikis

#### Site Configuration
```elisp
;; OLD FORMAT (Pre-2.5)
(setq mediawiki-site-alist
      '(("Wikipedia" . ("https://en.wikipedia.org/" "username" "password"))))

;; NEW FORMAT (2.5+)
(setq mediawiki-site-alist
      '(("Wikipedia" . 
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "username"
          :auth-method 'basic))))
```

#### API Layer
- **Old**: XML-based API calls with limited error handling
- **New**: JSON-based API with comprehensive error handling and retry logic
- **Added**: Asynchronous operations for better performance
- **Added**: Progress feedback and cancellation support

#### Session Management
- **Old**: No session persistence, manual token management
- **New**: Automatic session persistence, token refresh, state recovery
- **Added**: Session encryption and secure storage

#### User Interface
- **Added**: Modern UI enhancements (quick menu, enhanced save dialog)
- **Added**: Site statistics and debugging interfaces
- **Added**: Authentication management UI

### Backward Compatibility

MediaWiki.el 2.5+ includes a comprehensive compatibility layer that:
- Automatically detects legacy configuration
- Provides migration wizard for complex setups
- Maintains support for legacy function calls (with deprecation warnings)
- Validates migrated configuration

## Before You Start

### Backup Your Configuration

1. **Backup your Emacs configuration**:
```bash
cp ~/.emacs.d/init.el ~/.emacs.d/init.el.backup
cp ~/.emacs ~/.emacs.backup  # if using ~/.emacs
```

2. **Backup auth files** (if they exist):
```bash
cp ~/.authinfo ~/.authinfo.backup
cp ~/.authinfo.gpg ~/.authinfo.gpg.backup
```

3. **Document your current setup**:
```elisp
;; Run this in Emacs to see your current configuration
M-x eval-expression RET mediawiki-site-alist RET
```

### Check Current Version

Determine your current MediaWiki.el version:
```elisp
M-x eval-expression RET mediawiki-version RET
```

### Prerequisites

Ensure you have:
- **Emacs 28.1+** (required for MediaWiki.el 2.5+)
- **Network access** to your MediaWiki sites
- **Backup of current configuration** (see above)

## Automatic Migration

### Using the Migration Wizard

The easiest way to migrate is using the built-in migration wizard:

1. **Install MediaWiki.el 2.5+**:
```
M-x package-install RET mediawiki RET
```

2. **Start the migration wizard**:
```
M-x mediawiki-compat-migration-wizard
```

3. **Follow the wizard prompts**:
   - **Option 1**: Automatic migration (recommended for most users)
   - **Option 2**: Interactive migration (for complex setups)
   - **Option 3**: Preview changes (to see what will happen)
   - **Option 4**: Cancel

4. **Review migration results**:
```
M-x mediawiki-compat-show-migration-report
```

### What the Wizard Does

The migration wizard automatically:

1. **Detects Legacy Configuration**:
   - Old `mediawiki-site-alist` format
   - Individual legacy variables (`mediawiki-site-url`, etc.)
   - Deprecated API usage patterns

2. **Converts Configuration**:
   - Transforms to modern struct format
   - Preserves all site settings
   - Maintains authentication information

3. **Validates Results**:
   - Checks configuration integrity
   - Identifies potential issues
   - Provides detailed report

### Example Migration Session

```
M-x mediawiki-compat-migration-wizard

MediaWiki Configuration Migration Wizard
==========================================

Found 3 legacy configuration items:

• Legacy site: Wikipedia
  URL: https://en.wikipedia.org/
  Username: myusername
  Has stored password

• Legacy site URL: https://wiki.example.com/
• Legacy username: admin

Migration Options:
==================
[1] Automatic migration (recommended)
[2] Interactive migration  
[3] Show migration preview
[4] Cancel migration

Choose option (1-4): 1

Starting automatic migration...
Migration completed successfully!
```

## Manual Migration

### Step-by-Step Manual Process

If you prefer manual control or have complex configurations:

#### 1. Convert Site Configuration

**Before (Legacy Format)**:
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" . ("https://en.wikipedia.org/" "myuser" "mypass"))
        ("LocalWiki" . ("http://localhost/wiki/" "admin" "secret" "DOMAIN"))))
```

**After (Modern Format)**:
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" . 
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "myuser"
          :auth-method 'basic))
        
        ("LocalWiki" . 
         (make-mediawiki-site-config
          :name "LocalWiki"
          :url "http://localhost/wiki/"
          :username "admin"
          :auth-method 'basic
          :auth-config '(:domain "DOMAIN")))))
```

#### 2. Set up Auth-Source

Move passwords from configuration to secure storage:

**Create `~/.authinfo.gpg`**:
```
machine en.wikipedia.org login myuser password mypass
machine localhost login admin password secret
```

#### 3. Update Individual Variables

**Legacy Variables**:
```elisp
(setq mediawiki-site-url "https://en.wikipedia.org/")
(setq mediawiki-username "myuser")
(setq mediawiki-password "mypass")
```

**Modern Equivalent**:
```elisp
(setq mediawiki-site-alist
      '(("default" . 
         (make-mediawiki-site-config
          :name "default"
          :url "https://en.wikipedia.org/"
          :username "myuser"
          :auth-method 'basic))))

(setq mediawiki-site-default "default")
```

#### 4. Update Function Calls

**Legacy API Calls**:
```elisp
;; Old way
(mediawiki-api-call "Wikipedia" "query" '(("titles" . "Main Page")))
```

**Modern API Calls**:
```elisp
;; New way
(mediawiki-api-call-sync "Wikipedia" "query" '(("titles" . "Main Page")))
;; Or asynchronous
(mediawiki-api-call-async "Wikipedia" "query" '(("titles" . "Main Page"))
                          'success-callback 'error-callback)
```

## Configuration Changes

### Site Configuration Migration

#### Basic Sites

**Legacy**:
```elisp
(setq mediawiki-site-alist
      '(("Site1" . ("https://example.com/" "user" "pass"))))
```

**Modern**:
```elisp
(setq mediawiki-site-alist
      '(("Site1" . 
         (make-mediawiki-site-config
          :name "Site1"
          :url "https://example.com/"
          :username "user"
          :auth-method 'basic))))
```

#### Sites with Domain Authentication

**Legacy**:
```elisp
(setq mediawiki-site-alist
      '(("Enterprise" . ("https://wiki.company.com/" "user" "pass" "COMPANY"))))
```

**Modern**:
```elisp
(setq mediawiki-site-alist
      '(("Enterprise" . 
         (make-mediawiki-site-config
          :name "Enterprise"
          :url "https://wiki.company.com/"
          :username "user"
          :auth-method 'basic
          :auth-config '(:domain "COMPANY")))))
```

#### OAuth Sites

**New in 2.5+**:
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia-OAuth" . 
         (make-mediawiki-site-config
          :name "Wikipedia-OAuth"
          :url "https://en.wikipedia.org/"
          :username "user"
          :auth-method 'oauth
          :auth-config '(:consumer-key "your-key"
                        :consumer-secret "your-secret")))))
```

### Variable Migrations

| Legacy Variable | Modern Equivalent | Notes |
|----------------|-------------------|-------|
| `mediawiki-site-url` | `:url` in site config | Use structured configuration |
| `mediawiki-username` | `:username` in site config | Per-site usernames |
| `mediawiki-password` | auth-source | Secure password storage |
| `mediawiki-site-default` | `mediawiki-site-default` | Unchanged |

### New Configuration Options

Take advantage of new features:

```elisp
;; Performance settings
(setq mediawiki-use-non-blocking t)
(setq mediawiki-max-concurrent-operations 5)

;; Session settings
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
(setq mediawiki-session-auto-save t)

;; UI settings
(setq mediawiki-use-modern-ui t)
(setq mediawiki-prompt-for-summary t)

;; Debug settings (for troubleshooting)
(setq mediawiki-debug nil)
```

## API Changes

### Function Deprecations

| Deprecated | Replacement | Notes |
|------------|-------------|-------|
| `mediawiki-api-call` | `mediawiki-api-call-sync` | Use specific sync/async versions |
| `mediawiki-login` | `mediawiki-do-login` | Alias provided for compatibility |
| `mediawiki-logout` | `mediawiki-do-logout` | Alias provided for compatibility |

### New Functions

Take advantage of new functionality:

```elisp
;; Modern UI functions
M-x mediawiki-ui-quick-menu
M-x mediawiki-ui-save-with-options
M-x mediawiki-ui-manage-authentication

;; Session management
M-x mediawiki-session-status
M-x mediawiki-session-report-status

;; Debugging and diagnostics
M-x mediawiki-debug-view
M-x mediawiki-ui-show-site-statistics
```

### Asynchronous Operations

New async support for better performance:

```elisp
;; Async page loading with callback
(mediawiki-api-call-async "Wikipedia" "query" 
                          '(("titles" . "Main Page"))
                          (lambda (response)
                            (message "Page loaded: %s" response))
                          (lambda (error)
                            (message "Error: %s" error)))

;; Enable non-blocking mode
(setq mediawiki-use-non-blocking t)
```

## Troubleshooting

### Common Migration Issues

#### Issue: "Site configuration not found"

**Cause**: Legacy site configuration not properly converted

**Solution**:
1. Check migration log: `M-x mediawiki-compat-show-migration-report`
2. Validate configuration: `M-x mediawiki-compat-validate-migrated-config`
3. Manually fix configuration if needed

#### Issue: "Authentication failed after migration"

**Cause**: Passwords not properly moved to auth-source

**Solution**:
1. Check auth-source configuration:
```elisp
M-x eval-expression RET auth-sources RET
```

2. Create/update `~/.authinfo.gpg`:
```
machine your.wiki.site login username password yourpassword
```

3. Test authentication: `M-x mediawiki-ui-manage-authentication`

#### Issue: "Function not found" errors

**Cause**: Using deprecated function names

**Solution**:
1. Enable compatibility shims:
```elisp
(setq mediawiki-compat-enable-shims t)
```

2. Update function calls to modern equivalents
3. Check deprecation warnings in `*Messages*` buffer

#### Issue: "Session expired" frequently

**Cause**: Session persistence not configured

**Solution**:
```elisp
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
(setq mediawiki-session-auto-save t)
```

### Validation and Testing

#### Validate Your Migration

```elisp
;; Check configuration validity
M-x mediawiki-compat-validate-migrated-config

;; View validation report
M-x mediawiki-compat-show-validation-report

;; Test site connectivity
M-x mediawiki-ui-show-site-statistics
```

#### Test Authentication

```elisp
;; Test login to each site
M-x mediawiki-ui-manage-authentication

;; Check session status
M-x mediawiki-session-status

;; View session details
M-x mediawiki-session-report-status
```

### Getting Help

If migration fails or you encounter issues:

1. **Enable debug mode**:
```elisp
(setq mediawiki-debug t)
```

2. **Check debug output**:
```
M-x mediawiki-debug-view
```

3. **Review migration log**:
```
M-x mediawiki-compat-show-migration-report
```

4. **Validate configuration**:
```
M-x mediawiki-compat-validate-migrated-config
```

## Post-Migration Tasks

### 1. Test Basic Operations

After migration, verify that basic operations work:

```elisp
;; Test site switching
M-x mediawiki-site RET YourSite RET

;; Test page opening
M-x mediawiki-open RET TestPage RET

;; Test authentication
M-x mediawiki-do-login

;; Test saving (make a minor edit first)
C-x C-s
```

### 2. Configure New Features

Take advantage of 2.5+ features:

```elisp
;; Enable async operations
(setq mediawiki-use-non-blocking t)

;; Set up modern UI
(setq mediawiki-use-modern-ui t)

;; Configure session persistence
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
```

### 3. Update Key Bindings

Consider updating your key bindings to use new functions:

```elisp
;; Replace old bindings
(global-set-key (kbd "C-c w m") 'mediawiki-ui-quick-menu)
(global-set-key (kbd "C-c w s") 'mediawiki-ui-save-with-options)
(global-set-key (kbd "C-c w a") 'mediawiki-ui-manage-authentication)
```

### 4. Clean Up Legacy Configuration

Once migration is complete and tested:

1. **Remove legacy variables** (if any remain):
```elisp
;; Remove these from your config after migration
;; (setq mediawiki-site-url ...)
;; (setq mediawiki-username ...)
;; (setq mediawiki-password ...)
```

2. **Disable compatibility warnings** (optional):
```elisp
(setq mediawiki-compat-warn-deprecated nil)
```

3. **Remove backup files** (after confirming everything works):
```bash
rm ~/.emacs.d/init.el.backup
rm ~/.authinfo.backup
```

### 5. Explore New Features

Discover new capabilities:

```elisp
;; Try the quick menu
M-x mediawiki-ui-quick-menu

;; Check site statistics
M-x mediawiki-ui-show-site-statistics

;; Use enhanced save dialog
M-x mediawiki-ui-save-with-options

;; Manage authentication
M-x mediawiki-ui-manage-authentication
```

## Migration Checklist

Use this checklist to ensure complete migration:

### Pre-Migration
- [ ] Backup configuration files
- [ ] Document current setup
- [ ] Verify Emacs version (28.1+)
- [ ] Test current MediaWiki.el functionality

### Migration Process
- [ ] Install MediaWiki.el 2.5+
- [ ] Run migration wizard OR migrate manually
- [ ] Review migration report
- [ ] Validate migrated configuration
- [ ] Set up auth-source (if needed)

### Post-Migration Testing
- [ ] Test site switching
- [ ] Test page opening and editing
- [ ] Test authentication (login/logout)
- [ ] Test save operations
- [ ] Verify session persistence
- [ ] Check new UI features

### Configuration Updates
- [ ] Enable async operations
- [ ] Configure session persistence
- [ ] Set up modern UI features
- [ ] Update key bindings
- [ ] Clean up legacy configuration

### Final Steps
- [ ] Remove backup files (after testing)
- [ ] Disable compatibility warnings (optional)
- [ ] Document new configuration
- [ ] Explore new features

---

Congratulations! You've successfully migrated to MediaWiki.el 2.5+. Enjoy the enhanced security, performance, and features of the modernized package.

For ongoing support, refer to the [User Guide](USER-GUIDE.md) and [Configuration Examples](CONFIGURATION-EXAMPLES.md).