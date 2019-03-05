;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (unless noninteractive
    (pdf-tools-install))
  (setq-default pdf-view-display-size 'fit-page))
