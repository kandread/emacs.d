;; -*- lexical-binding: t; -*-

;; Use mu4e and mbsync to get emails
(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-compose-new)
  :init
  (provide 'html2text)
  :bind ("C-x m" . mu4e)
  :config
  (require 'org-mu4e)
  (dolist (mm '(mu4e-headers-mode-map mu4e-view-mode-map))
    (bind-keys :map (symbol-value mm) ("c" . org-mu4e-store-and-capture)))
  (bind-keys :map 'mu4e-view-mode-map ("l" . ace-link-mu4e))
  (set-face-attribute 'mu4e-replied-face nil :inherit 'mu4e-header-face)
  (setq mu4e-maildir (expand-file-name "~/Mail/umass")
    mu4e-attachment-dir "~/Downloads"
    mu4e-get-mail-command "mbsync -a"
    mu4e-update-interval nil
    mu4e-headers-include-related nil
    mu4e-index-cleanup nil ; don't do a full cleanup check
    mu4e-index-lazy-check t ; don't consider up-to-date dirs
    message-kill-buffer-on-exit t
    mu4e-headers-skip-duplicates t
    mu4e-compose-format-flowed 
    mu4e-confirm-quit nil ; quit without asking
    mu4e-compose-dont-reply-to-self t)
  (setq mu4e-bookmarks
    '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
       ("date:today..now AND maildir:/inbox" "Today's messages" ?t)
       ("date:7d..now AND maildir:/inbox" "Last 7 days" ?w)))
  (add-to-list 'mu4e-view-actions
    '("View in browser" . mu4e-action-view-in-browser) t)
  (add-hook 'mu4e-compose-mode-hook #'flyspell-mode) ; enable spell checking when composing emails
  (setq mu4e-user-mail-address-list '("kandread@umass.edu")
    user-mail-address "kandread@umass.edu"
    user-full-name "Kostas Andreadis"
    message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'ssl
    smtpmail-default-smtp-server "mail-auth.oit.umass.edu"
    smtpmail-smtp-server "mail-auth.oit.umass.edu"
    smtpmail-smtp-service 465))

;; Show how many messages there are in each mailbox
(use-package mu4e-maildirs-extension
  :after mu4e
  :demand t
  :config
  (mu4e-maildirs-extension-load)
  (setq mu4e-maildirs-extension-title nil
    mu4e-maildirs-extension-action-text "\t[u] Update mail and index\n"
    mu4e-maildirs-extension-maildir-expanded-prefix "-"
    mu4e-maildirs-extension-maildir-default-prefix "|"))

