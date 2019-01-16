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
  :commands (counsel-projectile-find-file counsel-projectile-find-dir counsel-projectile-switch-to-buffer
	      counsel-projectile-grep counsel-projectile-ag counsel-projectile-switch-project)
  :init
  (progn
    (define-key projectile-mode-map [remap projectile-find-file] #'counsel-projectile-find-file)
    (define-key projectile-mode-map [remap projectile-find-dir] #'counsel-projectile-find-dir)
    (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer)
    (define-key projectile-mode-map [remap projectile-grep] #'counsel-projectile-grep)
    (define-key projectile-mode-map [remap projectile-ag] #'counsel-projectile-ag)
    (define-key projectile-mode-map [remap projectile-switch-project] #'counsel-projectile-switch-project))
  :config
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))
    
