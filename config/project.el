;; -*- lexical-binding: t; -*-

;; Project management
(use-package projectile
  :demand t
  :init
  (add-hook 'after-init-hook #'projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-x p") #'projectile-command-map)
  (setq projectile-project-search-path '("~/Documents/" "~/Work/")
    projectile-completion-system 'ivy))

;; Ivy UI for Projectile
(use-package counsel-projectile
  :after projectile)
    
