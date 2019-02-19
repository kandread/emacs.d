;; -*- lexical-binding: t; -*-

(use-package julia-mode)

(use-package julia-repl
  :init
  (add-hook 'julia-mode-hook #'julia-repl-mode))
