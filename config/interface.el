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
