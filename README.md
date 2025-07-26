# MediaWiki.el

A comprehensive Emacs package for editing MediaWiki sites directly from Emacs.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/mediawiki-badge.svg)](https://melpa.org/#/mediawiki)
[![Build Status](https://github.com/hexmode/mediawiki-el/workflows/CI/badge.svg)](https://github.com/hexmode/mediawiki-el/actions)

## Features

✨ **Modern Architecture**: Modular design with async operations and robust error handling  
🔐 **Secure Authentication**: OAuth support, auth-source integration, encrypted sessions  
🚀 **Enhanced Performance**: Non-blocking operations, session persistence, retry logic  
🎨 **Modern UI**: Quick menu, enhanced save dialog, site statistics, authentication management  
📝 **Rich Editing**: Syntax highlighting, conflict resolution, draft mode, auto-completion  
🔄 **Easy Migration**: Automatic migration from older versions with comprehensive compatibility layer  
🧪 **Extensively Tested**: Comprehensive test suite ensuring reliability and stability  

## Quick Start

### Installation

#### From MELPA (Recommended)
```elisp
M-x package-install RET mediawiki RET
```

#### Manual Installation
```bash
git clone https://github.com/hexmode/mediawiki-el.git
```
Then add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/mediawiki-el")
(require 'mediawiki)
```

### Basic Setup

1. **Configure your first site**:
```
M-x customize-group RET mediawiki RET
```

2. **Or configure programmatically**:
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" . 
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "YourUsername"
          :auth-method 'basic))))
```

3. **Start editing**:
```
M-x mediawiki-site RET Wikipedia RET
M-x mediawiki-open RET PageName RET
```

## Key Commands

| Command | Key Binding | Description |
|---------|-------------|-------------|
| `mediawiki-open` | `C-c w o` | Open a wiki page |
| `mediawiki-save` | `C-x C-s` | Save current page |
| `mediawiki-save-as` | `C-x C-w` | Save with different name |
| `mediawiki-browse` | `C-c w b` | Open current page in browser |
| `mediawiki-ui-quick-menu` | `C-c w m` | Show quick access menu |
| `mediawiki-do-login` | `C-c w l` | Login to current site |
| `mediawiki-do-logout` | `C-c w q` | Logout from current site |

## What's New in 2.5+

### 🔐 Enhanced Security
- **OAuth Authentication**: Secure authentication for public wikis
- **Auth-Source Integration**: Encrypted password storage via GPG
- **Session Encryption**: Secure session persistence

### 🚀 Better Performance  
- **Asynchronous Operations**: Non-blocking network requests
- **Session Persistence**: State survives Emacs restarts
- **Smart Retry Logic**: Automatic recovery from transient failures

### 🎨 Modern User Interface
- **Quick Menu**: Fast access to common operations
- **Enhanced Save Dialog**: Rich save options with conflict detection
- **Site Statistics**: Comprehensive site and operation monitoring
- **Authentication Manager**: Centralized credential management

### 🔄 Seamless Migration
- **Automatic Detection**: Finds legacy configuration automatically
- **Migration Wizard**: Interactive upgrade process
- **Validation System**: Ensures migrated configuration works correctly
- **Backward Compatibility**: Legacy functions continue to work with warnings

## Documentation

- 📖 **[User Guide](docs/USER-GUIDE.md)** - Comprehensive usage documentation
- ⚙️ **[Configuration Examples](docs/CONFIGURATION-EXAMPLES.md)** - Practical configuration examples
- 🔄 **[Migration Guide](docs/MIGRATION-GUIDE.md)** - Upgrading from older versions
- 🛠️ **[Developer Guide](docs/DEVELOPER-GUIDE.md)** - Technical documentation for developers

## Configuration Examples

### Basic Wikipedia Setup
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" . 
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "YourUsername"
          :auth-method 'basic))))
```

### OAuth Authentication
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia-OAuth" . 
         (make-mediawiki-site-config
          :name "Wikipedia-OAuth"
          :url "https://en.wikipedia.org/"
          :username "YourUsername"
          :auth-method 'oauth
          :auth-config '(:consumer-key "your-key"
                        :consumer-secret "your-secret")))))
```

### Multi-Site Setup
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" . 
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "PublicUser"
          :auth-method 'oauth
          :auth-config '(:consumer-key "wiki-key"
                        :consumer-secret "wiki-secret")))
        
        ("CompanyWiki" . 
         (make-mediawiki-site-config
          :name "CompanyWiki"
          :url "https://wiki.company.com/"
          :username "employee.name"
          :auth-method 'basic
          :auth-config '(:domain "COMPANY")))))
```

### Performance Optimization
```elisp
;; Enable async operations
(setq mediawiki-use-non-blocking t)

;; Configure session persistence
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
(setq mediawiki-session-auto-save t)

;; Enable modern UI
(setq mediawiki-use-modern-ui t)
```

## Migration from Older Versions

If you're upgrading from MediaWiki.el < 2.5, use the migration wizard:

```
M-x mediawiki-compat-migration-wizard
```

The wizard will:
- Automatically detect your legacy configuration
- Convert it to the modern format
- Validate the migrated setup
- Provide a detailed migration report

For manual migration or complex setups, see the [Migration Guide](docs/MIGRATION-GUIDE.md).

## Authentication Setup

### Using Auth-Source (Recommended)

1. **Create encrypted auth file** (`~/.authinfo.gpg`):
```
machine en.wikipedia.org login yourusername password yourpassword
machine wiki.company.com login employee.name password companypass
```

2. **Configure Emacs**:
```elisp
(setq auth-sources '("~/.authinfo.gpg"))
```

3. **MediaWiki.el will automatically use stored credentials**

### OAuth Setup

For sites supporting OAuth (like Wikipedia):

1. **Register OAuth application** at `Special:OAuthConsumerRegistration`
2. **Get consumer key and secret**
3. **Configure MediaWiki.el**:
```elisp
(make-mediawiki-site-config
 :name "Wikipedia"
 :url "https://en.wikipedia.org/"
 :username "yourusername"
 :auth-method 'oauth
 :auth-config '(:consumer-key "your-consumer-key"
               :consumer-secret "your-consumer-secret"))
```

## Troubleshooting

### Common Issues

**Authentication Problems**:
```
M-x mediawiki-ui-manage-authentication
```

**Site Connection Issues**:
```
M-x mediawiki-ui-show-site-statistics
```

**Debug Mode**:
```elisp
(setq mediawiki-debug t)
M-x mediawiki-debug-view
```

**Session Problems**:
```
M-x mediawiki-session-status
M-x mediawiki-session-clear-all-states
```

For detailed troubleshooting, see the [User Guide](docs/USER-GUIDE.md#troubleshooting).

## Development

### Running Tests
```bash
make test                # All tests
make test-minimal        # Basic tests
make test-integration    # Integration tests
make test-gpg           # GPG/encryption tests
```

### Development Setup
```elisp
;; Enable debug mode
(setq mediawiki-debug t)

;; Test site configuration
(setq mediawiki-site-alist
      '(("TestWiki" . 
         (make-mediawiki-site-config
          :name "TestWiki"
          :url "http://localhost:8080/mediawiki/"
          :username "testuser"
          :auth-method 'basic))))
```

See the [Developer Guide](docs/DEVELOPER-GUIDE.md) for detailed development information.

## Architecture

MediaWiki.el uses a modular architecture:

```
├── mediawiki.el          # Main UI and user commands
├── mediawiki-core.el     # Core data structures and utilities
├── mediawiki-http.el     # HTTP communication layer
├── mediawiki-api.el      # MediaWiki API client
├── mediawiki-auth.el     # Authentication handling
├── mediawiki-oauth.el    # OAuth implementation
├── mediawiki-session.el  # Session management
├── mediawiki-page.el     # Page operations
├── mediawiki-errors.el   # Error handling
├── mediawiki-ui.el       # Modern UI components
└── mediawiki-compat.el   # Backward compatibility
```

Key features:
- **Async-first design** for better performance
- **Comprehensive error handling** with automatic retry
- **Session persistence** across Emacs restarts
- **Secure credential management** via auth-source
- **Extensive test coverage** ensuring reliability

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md).

### Quick Contribution Steps
1. Fork the repository
2. Create a feature branch: `git checkout -b my-feature`
3. Make changes with tests and documentation
4. Run tests: `make test`
5. Submit a pull request

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Support

- 📚 **Documentation**: See the [docs/](docs/) directory
- 🐛 **Bug Reports**: [GitHub Issues](https://github.com/hexmode/mediawiki-el/issues)
- 💬 **Discussions**: [GitHub Discussions](https://github.com/hexmode/mediawiki-el/discussions)
- 📧 **Email**: [maintainer email]

## Acknowledgments

MediaWiki.el builds upon the work of many contributors:
- Original authors: Jerry, Chong Yidong, Uwe Brauer
- Current maintainer: Mark A. Hershberger
- All the contributors who have improved the package over the years

---

**Happy editing! 🎉**