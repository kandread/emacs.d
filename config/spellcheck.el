;; -*- lexical-binding: t; -*-

;; Define dictionary and configure spelling options
(setq ispell-dictionary "english"
      ispell-list-command "--list"
      ispell-extr-args '("--dont-tex-check-comments"))

;; Use aspell instead of ispell
(setq-default ispell-program-name "aspell")
(with-eval-after-load 'ispell
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

;; Distraction-free words correction
(use-package flyspell-correct
  :ensure flyspell-correct-ivy
  :commands (flyspell-correct-word-generic
	      flyspell-correct-previous-word-generic)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy))

