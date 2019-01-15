;; -*- lexical-binding: t; -*-

;; Highlight matching delimiters
(setq show-paren-delay 0.1
  show-paren-highlight-openparen t
  show-paren-when-point-inside-paren t)
(show-paren-mode 1)

;; Jump to things in Emacs tree-style
(use-package avy
  :commands (avy-goto-char-2 avy-goto-line avy-goto-word-1)
  :bind (("M-g 2" . avy-goto-char-2)
          ("M-g l" . avy-goto-line)
          ("M-g w" . avy-goto-word-1)))

;; Expand region semantically
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Fix Lisp indent offset
(setq lisp-indent-offset 2)

;; Save place in file between sessions
(save-place-mode 1)

;; Undo/redo functionality
(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t
    undo-tree-enable-undo-in-region nil
    undo-tree-history-directory-alist '(("." . "~/.emacs.d/cache/undo")))
  (global-undo-tree-mode +1))

;; turn on line numbers for programming modes
(add-hook 'prog-mode-hook #'display-line-numbers--turn-on)
