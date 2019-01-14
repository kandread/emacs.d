;; -*- lexical-binding: t; -*-

(use-package flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-indication-mode 'right-fringe))

(use-package flycheck-posframe
  :hook flycheck
  :config
  (flycheck-posframe-configure-pretty-defaults))
