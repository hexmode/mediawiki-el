# MediaWiki.el Tutorial

A step-by-step tutorial to get you started with MediaWiki.el quickly.

## Tutorial 1: First Wiki Edit

### Goal
Edit your first Wikipedia article using MediaWiki.el.

### Prerequisites
- Emacs 28.1+
- Internet connection
- Wikipedia account (create one at https://en.wikipedia.org)

### Steps

#### 1. Install MediaWiki.el
```
M-x package-install RET mediawiki RET
```

#### 2. Configure Your Wikipedia Account
```
M-x customize-group RET mediawiki RET
```

Set:
- **Mediawiki Site Alist**: Add new entry
- **Site Name**: `Wikipedia`
- **URL**: `https://en.wikipedia.org/`
- **Username**: `your-wikipedia-username`
- **Auth Method**: `basic`

#### 3. Set up Secure Password Storage
Create `~/.authinfo.gpg`:
```
machine en.wikipedia.org login your-wikipedia-username password your-password
```

#### 4. Connect to Wikipedia
```
M-x mediawiki-site RET Wikipedia RET
```

#### 5. Open Your User Sandbox
```
M-x mediawiki-open RET User:YourUsername/sandbox RET
```

#### 6. Make Your First Edit
1. Add some text: `Hello from Emacs!`
2. Save the page: `C-x C-s`
3. Enter edit summary: `Testing MediaWiki.el`

#### 7. View Your Edit
```
M-x mediawiki-browse
```

**Congratulations!** You've made your first edit with MediaWiki.el.

## Tutorial 2: Setting Up Multiple Sites

### Goal
Configure MediaWiki.el for both Wikipedia and a private wiki.

### Steps

#### 1. Configure Multiple Sites
Add to your Emacs configuration:
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia" . 
         (make-mediawiki-site-config
          :name "Wikipedia"
          :url "https://en.wikipedia.org/"
          :username "wiki-username"
          :auth-method 'basic))
        
        ("CompanyWiki" . 
         (make-mediawiki-site-config
          :name "CompanyWiki"
          :url "https://wiki.company.com/"
          :username "employee.name"
          :auth-method 'basic
          :auth-config '(:domain "COMPANY")))))
```

#### 2. Set up Passwords
Add to `~/.authinfo.gpg`:
```
machine en.wikipedia.org login wiki-username password wiki-password
machine wiki.company.com login employee.name password company-password
```

#### 3. Switch Between Sites
```
M-x mediawiki-site RET CompanyWiki RET
M-x mediawiki-open RET Internal Documentation RET
```

#### 4. Use Quick Menu for Fast Switching
```
M-x mediawiki-ui-quick-menu
```
Select "Switch Site" and choose your target site.

## Tutorial 3: Advanced Editing Workflow

### Goal
Learn advanced editing features and workflows.

### Steps

#### 1. Enable Modern Features
```elisp
;; Add to your Emacs configuration
(setq mediawiki-use-non-blocking t)
(setq mediawiki-use-modern-ui t)
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
```

#### 2. Use Draft Mode for Complex Edits
```
M-x mediawiki-draft
```
- Write your content offline
- Use all Emacs editing features
- Save as draft or publish when ready

#### 3. Enhanced Save Dialog
```
M-x mediawiki-ui-save-with-options
```
This provides:
- Edit summary suggestions
- Minor edit checkbox
- Watch page option
- Conflict detection

#### 4. Handle Edit Conflicts
When conflicts occur:
1. MediaWiki.el detects the conflict
2. Shows both versions side by side
3. Choose to merge or overwrite
4. Review changes before saving

#### 5. Use Preview Before Saving
```
M-x mediawiki-browse
```
Opens current buffer content in browser for preview.

## Tutorial 4: OAuth Authentication

### Goal
Set up secure OAuth authentication for Wikipedia.

### Steps

#### 1. Register OAuth Application
1. Go to https://en.wikipedia.org/wiki/Special:OAuthConsumerRegistration
2. Click "Request a token for a new consumer"
3. Fill out the form:
   - **Application name**: `My MediaWiki.el`
   - **Application description**: `Personal Emacs MediaWiki client`
   - **OAuth "callback" URL**: `urn:ietf:wg:oauth:2.0:oob`
4. Select required permissions
5. Submit and get your consumer key/secret

#### 2. Configure OAuth in MediaWiki.el
```elisp
(setq mediawiki-site-alist
      '(("Wikipedia-OAuth" . 
         (make-mediawiki-site-config
          :name "Wikipedia-OAuth"
          :url "https://en.wikipedia.org/"
          :username "your-username"
          :auth-method 'oauth
          :auth-config '(:consumer-key "your-consumer-key"
                        :consumer-secret "your-consumer-secret")))))
```

#### 3. Complete OAuth Flow
1. First login attempt opens browser
2. Authorize your application
3. Return to Emacs - you're logged in!

#### 4. Verify OAuth Setup
```
M-x mediawiki-ui-manage-authentication
```
Should show "OAuth authenticated" status.

## Tutorial 5: Migration from Old Version

### Goal
Migrate from MediaWiki.el < 2.5 to the modern version.

### Steps

#### 1. Backup Current Configuration
```bash
cp ~/.emacs.d/init.el ~/.emacs.d/init.el.backup
```

#### 2. Install New Version
```
M-x package-install RET mediawiki RET
```

#### 3. Run Migration Wizard
```
M-x mediawiki-compat-migration-wizard
```

#### 4. Choose Migration Option
- **Option 1**: Automatic (recommended)
- **Option 2**: Interactive (for complex setups)
- **Option 3**: Preview (see what changes)

#### 5. Review Migration Results
```
M-x mediawiki-compat-show-migration-report
```

#### 6. Validate Configuration
```
M-x mediawiki-compat-validate-migrated-config
```

#### 7. Test Migrated Setup
1. Try logging in: `M-x mediawiki-do-login`
2. Open a page: `M-x mediawiki-open RET TestPage RET`
3. Make a test edit and save

## Tutorial 6: Troubleshooting Common Issues

### Issue 1: "Cannot connect to site"

#### Diagnosis
```
M-x mediawiki-ui-show-site-statistics
```

#### Solutions
1. Check internet connection
2. Verify site URL in configuration
3. Test site accessibility in browser
4. Check for proxy/firewall issues

### Issue 2: "Authentication failed"

#### Diagnosis
```
M-x mediawiki-ui-manage-authentication
```

#### Solutions
1. Verify credentials in auth-source
2. Check username/password are correct
3. For OAuth, verify consumer keys
4. Try logging in via web browser first

### Issue 3: "Edit conflicts"

#### Handling
1. MediaWiki.el shows conflict notification
2. Use conflict resolution interface
3. Review both versions
4. Choose merge strategy
5. Save resolved version

### Issue 4: "Session expired"

#### Solutions
```
M-x mediawiki-session-status        # Check status
M-x mediawiki-do-logout            # Logout
M-x mediawiki-do-login             # Login again
```

### Debug Mode for Complex Issues
```elisp
(setq mediawiki-debug t)
M-x mediawiki-debug-view           # View debug buffer
```

## Tutorial 7: Power User Tips

### Custom Key Bindings
```elisp
;; Add to your Emacs configuration
(global-set-key (kbd "C-c w o") 'mediawiki-open)
(global-set-key (kbd "C-c w s") 'mediawiki-save)
(global-set-key (kbd "C-c w m") 'mediawiki-ui-quick-menu)
(global-set-key (kbd "C-c w a") 'mediawiki-ui-manage-authentication)
```

### Custom Functions
```elisp
(defun my-wiki-new-article (title)
  "Create new article with standard template."
  (interactive "sArticle title: ")
  (mediawiki-open title)
  (insert (format "'''%s''' is...\n\n== Overview ==\n\n== References ==\n" title))
  (goto-char (point-min))
  (forward-line 1))
```

### Integration with Other Packages
```elisp
;; Enable flyspell for wiki editing
(add-hook 'mediawiki-mode-hook 'flyspell-mode)

;; Enable company-mode for auto-completion
(add-hook 'mediawiki-mode-hook 'company-mode)

;; Enable yasnippet for templates
(add-hook 'mediawiki-mode-hook 'yas-minor-mode)
```

### Performance Optimization
```elisp
;; Enable async operations
(setq mediawiki-use-non-blocking t)

;; Increase concurrent operations for fast editing
(setq mediawiki-max-concurrent-operations 5)

;; Enable session persistence
(setq mediawiki-session-save-file "~/.emacs.d/mediawiki-sessions")
(setq mediawiki-session-auto-save t)
```

## Next Steps

After completing these tutorials:

1. **Read the [User Guide](USER-GUIDE.md)** for comprehensive documentation
2. **Explore [Configuration Examples](CONFIGURATION-EXAMPLES.md)** for advanced setups
3. **Join the community** and share your experience
4. **Contribute** improvements and bug fixes

## Quick Reference

### Essential Commands
| Command | Description |
|---------|-------------|
| `M-x mediawiki-ui-quick-menu` | Access all features quickly |
| `M-x mediawiki-open` | Open a wiki page |
| `C-x C-s` | Save current page |
| `M-x mediawiki-browse` | Preview in browser |
| `M-x mediawiki-ui-save-with-options` | Enhanced save dialog |

### Troubleshooting Commands
| Command | Description |
|---------|-------------|
| `M-x mediawiki-ui-show-site-statistics` | Site diagnostics |
| `M-x mediawiki-ui-manage-authentication` | Auth management |
| `M-x mediawiki-session-status` | Session information |
| `M-x mediawiki-debug-view` | Debug output |

### Migration Commands
| Command | Description |
|---------|-------------|
| `M-x mediawiki-compat-migration-wizard` | Upgrade wizard |
| `M-x mediawiki-compat-show-migration-report` | Migration results |
| `M-x mediawiki-compat-validate-migrated-config` | Validate setup |

---

**Happy wiki editing with MediaWiki.el! 🎉**