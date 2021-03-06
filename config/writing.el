;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive
    (pdf-tools-install))
  (setq-default pdf-view-display-size 'fit-page))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-parse-self t ; parse on load
    TeX-auto-save t ; parse on save
    ;; use hidden dirs for auctex files
    TeX-auto-local ".auctex-auto"
    TeX-style-local ".auctex-style"
    TeX-source-correlate-mode t
    TeX-source-correlate-method 'synctex
    ;; don't start the emacs server when correlating sources
    TeX-source-correlate-start-server nil
    ;; automatically insert braces after sub/superscript in math mode
    TeX-electric-sub-and-superscript t)
  (setq-local ispell-parser 'tex))

(use-package auctex-latexmk
  :after tex
  :init
  ;; pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; set LatexMk as the default
  (setq TeX-command-default "LatexMk")
  :config
  ;; add latexmk as a TeX target
  (auctex-latexmk-setup))

(use-package ivy-bibtex
  :commands ivy-bibtex
  :bind (:map bibtex-mode-map
	  ("C-c v" . ivy-bibtex))
  :config
  (setq bibtex-completion-bibliography '("~/Documents/library.bib")
    bibtex-completion-pdf-field "file"))
