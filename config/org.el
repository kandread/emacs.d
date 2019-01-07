;; -*- lexical-binding: t; -*-

;; Org-mode
(use-package org
  :demand t
  :ensure org-plus-contrib
  :bind ("C-x c" . org-capture)
  :config
  ;; set up interface
  (setq-default
    org-startup-folded t
    org-startup-indented t
    org-startup-with-inline-images nil
    org-tags-column 0
    org-fontify-done-headline t
    org-fontify-quote-and-verse-blocks t
    org-fontify-whole-heading-line t
    org-footnote-auto-label 'plain
    org-hidden-keywords nil
    org-hide-emphasis-markers nil
    org-hide-leading-stars t
    org-hide-leading-stars-before-indent-mode t
    org-image-actual-width nil
    org-indent-indentation-per-level 2
    org-indent-mode-turns-on-hiding-stars t
    org-list-description-max-indent 4)
  ;; configure programming language support
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)))
  ;; set org file directory
  (setq org-files-directory "~/Documents/Org/")
  ;; set agenda files
  (setq org-agenda-files (list org-files-directory))
  ;; set default notes file
  (setq org-default-notes-file (expand-file-name "notes.org" org-files-directory))
  ;; set capture templates
  (setq org-capture-templates
    `(("r" "respond" entry (file ,(expand-file-name "email.org" org-files-directory))
        "* TODO %a %? \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))")
       ("t" "todo" entry (file ,(expand-file-name "inbox.org" org-files-directory))
         "* TODO %?\n%a\n")
       ("n" "note" entry (file ,(expand-file-name "notes.org" org-files-directory))
         "* %? :note:\n%U\n%a\n")
       ("e" "event" entry (file ,(expand-file-name "meetings.org" org-files-directory))
         "* %? \n%^T\n%a\n")))
  ;; set task states
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
       (sequence "WAITING(w@/!)" "|" "SOMEDAY(o)" "CANCELLED(c@/!)")))
  ;; trigger task states
  (setq org-todo-state-tags-triggers
    '(("CANCELLED" ("CANCELLED" . t))
       ("WAITING" ("WAITING" . t))
       (done ("WAITING"))
       ("TODO" ("WAITING") ("CANCELLED"))
       ("NEXT" ("WAITING") ("CANCELLED"))
       ("DONE" ("WAITING") ("CANCELLED"))))
  ;; exclude PROJECT tag from being inherited
  (setq org-tags-exclude-from-inheritance '("project"))
  ;; show inherited tags in agenda view
  (setq org-agenda-show-inherited-tags t)
  ;; set archive tag
  (setq org-archive-tag "archive")
  ;; set archive file
  (setq org-archive-location (concat org-files-directory "archive.org::* From %s"))
  ;; refiling targets include any file contributing to the agenda - up to 2 levels deep
  (setq org-refile-targets '((nil :maxlevel . 2)
                              (org-agenda-files :maxlevel . 2)))
  ;; show refile targets simultaneously
  (setq org-outline-path-complete-in-steps nil)
  ;; use full outline paths for refile targets
  (setq org-refile-use-outline-path 'file)
  ;; allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; exclude done tasks from refile targets
  (setq org-refile-target-verify-function #'+org/verify-refile-target))

;; Export libraries
(use-package htmlize)

;; Functions
(defun +org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
