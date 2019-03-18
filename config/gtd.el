;; -*- lexical-binding: t; -*-

;; Org-mode
(use-package org
  :ensure org-plus-contrib
  :init
  ;; set up keybindings
  (bind-keys
    ("C-x c" . org-capture)
    ("C-x C-a" . org-agenda))
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
  ;; ignore headings during export using tag
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  ;; configure programming language support
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
       (jupyter . t)
       (ipython . t)
       (python . t)))
  ;; Use HTML5 when exporting
  (setq org-html-html5-fancy t
    org-html-doctype "html5")
  ;; do not ask for confirmation when evaluating code
  (setq org-confirm-babel-evaluate nil)
  ;; use return to open links
  (setq org-return-follows-link t)
  ;; load org-mu4e to handle email links
  (require 'org-mu4e)
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
  (setq org-refile-target-verify-function #'+gtd/verify-refile-target)
  ;; include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  ;; show agenda as the only window
  (setq org-agenda-window-setup 'only-window)
  ;; define stuck projects
  (setq org-stuck-projects '("+project-done/-TODO" ("NEXT" "WAITING")))
  ;; exclude archived tasks from agenda view
  (setq org-agenda-tag-filter-preset '("-archive"))
  ;; disable compact block agenda view
  (setq org-agenda-compact-blocks nil)
  ;; block tasks that have unfinished subtasks
  (setq org-enforce-todo-dependencies t)
  ;; dim blocked tasks in agenda
  (setq org-agenda-dim-blocked-tasks t)
  ;; inhibit startup when preparing agenda buffer
  (setq org-agenda-inhibit-startup nil)
  ;; limit number of days before showing a future deadline
  (setq org-deadline-warning-days 7)
  ;; retain ignore options in tags-todo search
  (setq org-agenda-tags-todo-honor-ignore-options t)
  ;; hide certain tags from agenda view
  (setq org-agenda-hide-tags-regexp "project\\|started")
  ;; remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)
  ;; remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)
  ;; remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done t)
  ;; perform actions before finalizing agenda view
  (add-hook 'org-agenda-finalize-hook #'+gtd/cleanup-replied-emails)
  ;; resume clocking when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state #'+gtd/clock-in-to-next)
  ;; separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running)))

;; Pomodoro technique for org mode
(use-package org-pomodoro
  :commands org-pomodoro
  :bind ("C-c C-x m" . org-pomodoro))

;; Sync calendar with org mode
(use-package org-gcal
  :commands (org-gcal-fetch)
  :config
  (setq org-gcal-client-id "387490352247-km265vil3bvcvaf6g69j24arla3tr1to.apps.googleusercontent.com"
    org-gcal-client-secret "MWRWx2qELRugHIcYwxHXtAro"
    org-gcal-file-alist '(("kmandreadis@gmail.com" . "~/Documents/Org/meetings.org"))))

;; Manage bibliography with org mode
(use-package org-ref
  :after org
  :demand t
  :init
  (setq reftex-default-bibliography '("~/Documents/library.bib")
    org-ref-bibliography-notes "~/Documents/Org/notes.org"
    org-ref-default-bibliography "~/Documents/library.bib"
    org-ref-pdf-directory "~/Documents/Papers/"
    org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
    org-ref-get-pdf-filename-function #'org-ref-get-pdf-filename-helm-bibtex
    bibtex-completion-pdf-field "file"
    org-ref-note-title-format
    "* %t
     :PROPERTIES:
     :Custom_ID: %k
     :AUTHOR: %9a
     :JOURNAL: %j
     :YEAR: %y
     :DOI: %D
     :URL: %U
     :END:

      "))

;; Export libraries
(use-package htmlize)

(use-package ox-pandoc
  :init
  (with-eval-after-load 'org (require 'ox-pandoc))
  :config
  (push 'pandoc org-export-backends))

(use-package ox-reveal
  :init
  (with-eval-after-load 'org (require 'ox-reveal))
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3/"
    ;; org-reveal-root "file:///Users/kandread/.emacs.d/reveal.js"
    org-reveal-note-key-char nil
    org-reveal-mathjax t))

(use-package ox-ipynb
  :quelpa (ox-ipynb :fetcher github :repo "jkitchin/ox-ipynb")
  :init
  (with-eval-after-load 'org (require 'ox-ipynb)))

;; Functions
(defun +gtd/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun +gtd/delete-all-done-entries ()
  "Archive all entries marked DONE."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (outline-previous-heading)
      (when (org-entry-is-done-p)
	(org-cut-subtree)))))

(defun +gtd/cleanup-replied-emails ()
  "Clean up email tasks that have been replied (i.e. marked done)."
  (interactive)
  (org-map-entries #'+gtd/delete-all-done-entries nil '("~/Documents/Org/email.org"))
  (save-some-buffers 'no-confirm (equal buffer-file-name "email.org")))

(defun +gtd/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in."
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (if (member (org-get-todo-state) (list "TODO")) "NEXT")))
