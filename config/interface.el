;; -*- lexical-binding: t; -*-

;; Disable certain byte compiler warnings to cut down on the noise
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Be quiet at startup and don't load or display anything unnecessary (including the scrollbar and toolbar).
(unless noninteractive
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message user-login-name
        inhibit-default-init t
        initial-major-mode 'fundamental-mode
        initial-scratch-message nil)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

;; No beeping or blinking either please!
(setq ring-bell-function #'ignore
      visible-cursor nil
      visible-bell nil)
(blink-cursor-mode 0)

;; Make it easier to answer yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Wrap lines visually for all buffers
(global-visual-line-mode 1)

;; Display available keybindings in popup
(use-package which-key
  :hook (after-init . which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-min-display-lines 5
        which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom))

;; Load themes
(use-package doom-themes :demand t)

;; Select a theme
(load-theme 'doom-sourcerer t)

;; Use a better modeline
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-major-mode-icon nil))

;; Let's kill buffer instead of just burying when quitting a window
(defadvice quit-window (before kill-buffer-and-window)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)
