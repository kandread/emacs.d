;; -*- lexical-binding: t; -*-

;; Ivy
(use-package ivy
  :hook (after-init . ivy-mode)
  :diminish ivy-mode
  :config
  (setq ivy-extra-directories ()
	ivy-initial-inputs-alist nil
	ivy-count-format "(%d/%d) "
	ivy-wrap t
	ivy-height 15))

;; Counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :diminish counsel-mode
  :bind ("A-s" . swiper)
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;; Smarter M-x
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-completion-method 'ivy
	smex-save-file "~/.emacs.d/cache/smex-items")
  (smex-initialize))

;; Recent files
(use-package recentf
  :bind (("C-x j" . counsel-recentf))
  :config
  (setq recentf-save-file "~/.emacs.d/cache/recentf"
        recentf-max-saved-items 300
        recentf-max-menu-items 0
        recentf-filename-handlers '(file-truename)
        recentf-auto-cleanup 'never)
  (recentf-mode t)
  (add-to-list 'recentf-exclude "Mail/umass")
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "/var/folders"))

;; Modular in-buffer completion framework
(use-package company
  :defer 1
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 3)
  (add-hook 'prog-mode-hook #'company-mode))

;; Snippets
(use-package yasnippet
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet
             yas-lookup-snippet yas-insert-snippet yas-new-snippet
	      yas-visit-snippet-file snippet-mode)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode-on)
  (add-hook 'text-mode-hook #'yas-minor-mode-on)
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (setq yas-verbosity 1
    yas-wrap-around-region t))

(use-package yasnippet-snippets)

