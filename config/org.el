;; -*- lexical-binding: t; -*-

;; Org-mode
(use-package org
  :demand t
  :ensure org-plus-contrib
  :config
  ;; activate easy template expansion
  ;; (require 'org-tempo)
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
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	     (sequence "WAITING(w@/!)" "|" "SOMEDAY(o)" "CANCELLED(c@/!)"))))
  ;; trigger task states
  (setq org-todo-state-tags-triggers
    (quote (("CANCELLED" ("CANCELLED" . t))
             ("WAITING" ("WAITING" . t))
             (done ("WAITING"))
             ("TODO" ("WAITING") ("CANCELLED"))
             ("NEXT" ("WAITING") ("CANCELLED"))
	     ("DONE" ("WAITING") ("CANCELLED"))))))

;; Export libraries
(use-package htmlize)
