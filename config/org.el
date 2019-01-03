;; -*- lexical-binding: t; -*-

;; Org-mode
(use-package org
  :demand t
  :ensure org-plus-contrib
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
  ;; set org file directory
  (setq org-files-directory "~/Documents/Org/")
  ;; set default notes file
  (setq org-default-notes-file (expand-file-name "notes.org" org-files-directory)))

;; Export libraries
(use-package htmlize)
