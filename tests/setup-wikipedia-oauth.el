;;; setup-wikipedia-oauth.el --- Setup OAuth for Wikipedia -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper script to set up OAuth credentials for Wikipedia

;;; Code:

(require 'mediawiki-core)
(require 'mediawiki-oauth)

(defun mediawiki-oauth-setup-with-tokens (sitename consumer-key consumer-secret access-token access-secret)
  "Set up OAuth configuration for SITENAME with both consumer and access tokens.
This is for when you already have all OAuth credentials from the OAuth provider."
  (interactive "sSite name: \nsConsumer key: \nsConsumer secret: \nsAccess token: \nsAccess secret: ")

  (let ((site (mediawiki-get-site sitename)))
    (unless site
      (error "Site %s not found. Add it first with mediawiki-add-site" sitename))

    ;; Set OAuth configuration with all tokens
    (setf (mediawiki-site-config-auth-method site) 'oauth)
    (setf (mediawiki-site-config-auth-config site)
          (list :consumer-key consumer-key
                :consumer-secret consumer-secret
                :access-token access-token
                :access-secret access-secret))

    (message "OAuth configuration set for %s with access tokens. Ready for authentication!" sitename)))

(defun setup-wikipedia-oauth-example ()
  "Example function showing how to set up Wikipedia OAuth.
Replace the placeholder values with your actual OAuth credentials."

  ;; First, make sure Wikipedia is added as a site
  (unless (mediawiki-get-site "Wikipedia")
    (let ((wikipedia-site (make-mediawiki-site-config
                          :name "Wikipedia"
                          :url "https://en.wikipedia.org/"
                          :api-url "https://en.wikipedia.org/w/api.php"
                          :username nil
                          :auth-method 'basic
                          :auth-config nil
                          :capabilities nil
                          :session-info nil)))
      (mediawiki-add-site wikipedia-site)
      (message "Added Wikipedia site")))

  ;; Now set up OAuth with your credentials
  ;; REPLACE THESE WITH YOUR ACTUAL OAUTH CREDENTIALS:
  (let ((consumer-key "your-consumer-key-here")
        (consumer-secret "your-consumer-secret-here")
        (access-token "your-access-token-here")
        (access-secret "your-access-secret-here"))

    (mediawiki-oauth-setup-with-tokens "Wikipedia"
                                      consumer-key
                                      consumer-secret
                                      access-token
                                      access-secret)

    (message "Wikipedia OAuth setup complete!")))

;; Instructions for the user
(defun show-oauth-setup-instructions ()
  "Show instructions for setting up OAuth with Wikipedia."
  (message "
=== How to Set Up Your Wikipedia OAuth Credentials ===

You have 4 pieces of information from your Wikipedia OAuth consumer:

1. Consumer Key (also called Client ID)
2. Consumer Secret (also called Client Secret)
3. Access Token
4. Access Secret

To set them up:

METHOD 1 - Interactive Setup:
M-x mediawiki-oauth-setup-with-tokens
Then enter:
- Site name: Wikipedia
- Consumer key: [your consumer key]
- Consumer secret: [your consumer secret]
- Access token: [your access token]
- Access secret: [your access secret]

METHOD 2 - Edit setup-wikipedia-oauth-example():
1. Edit the setup-wikipedia-oauth-example function above
2. Replace the placeholder values with your actual credentials
3. Run: M-x setup-wikipedia-oauth-example

METHOD 3 - Direct Elisp:
(mediawiki-oauth-setup-with-tokens \"Wikipedia\"
                                  \"your-consumer-key\"
                                  \"your-consumer-secret\"
                                  \"your-access-token\"
                                  \"your-access-secret\")

After setup, you can try to authenticate with:
M-x mediawiki-oauth-login

Note: Full OAuth functionality requires HTTP module enhancement for
Authorization headers, but the setup and configuration will work.
"))

;; Show instructions when this file is loaded
(show-oauth-setup-instructions)

;;; setup-wikipedia-oauth.el ends here
