;; Emacs Configuration

;; Let's set some personal information
(setq user-full-name "Kostas Andreadis"
      user-mail-address "kandread@umass.edu")

;; We're going to increase the `gc-cons-threshold` to a very high number to decrease the load and compile time. We'll lower this value significantly after initialization has completed. We don't want to keep this value too high or it will result in long GC pauses during normal usage.
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; Unset file-name-handler-alist temporarily since Emacs will run through this list to check for a proper handler for any file loaded (something not needed during startup).
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Move custom settings into its own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t t)

;; History & backup settings (save nothing, that's what git is for)
(setq auto-save-default nil
      create-lockfiles nil
      history-length 500
      make-backup-files nil)

;; Set package repositories and local directories, since we want to avoid `load-path` lookups
(setq package-user-dir "~/.emacs.d/elpa"
      quelpa-user-dir "~/.emacs.d/quelpa"
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; Configure package local paths and disable at startup
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        quelpa-user-dir "~/.emacs.d/quelpa"
        package--init-file-ensured t
        package-enable-at-startup nil)
  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))
  (unless (file-directory-p quelpa-user-dir)
    (make-directory quelpa-user-dir t)))

;; Always defer loading packages
(setq use-package-always-defer t)

;; Configure quelpa settings
(setq quelpa-update-melpa-p nil
      quelpa-checkout-melpa-p nil
      quelpa-self-upgrade-p nil
      quelpa-melpa-recipe-stores nil)

;; Manually set load path
(eval-and-compile
  (setq load-path (append load-path
                          (directory-files package-user-dir t "^[^.]" t)
                          (directory-files quelpa-user-dir t "^[^.]" t))))

;; Initialize package management
(eval-when-compile
  (require 'package)
  (package-initialize)
  (unless (package-installed-p 'quelpa)
    (package-refresh-contents)
    (package-install 'quelpa))
  (require 'quelpa)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
  (require 'quelpa-use-package)
  (setq use-package-always-ensure t))

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

;; Some core packages
(require 'bind-key) ; needed to load compiled init.el
(use-package diminish :demand t)

;; Load modular configurations
(defvar emacs-config-directory (expand-file-name "config" user-emacs-directory)
  "Path to modular configuration files.")

(defvar emacs-modules-to-load
  '(macos
    editor
    completion
    interface
    version-control
    org
    python
    email
    ))

(dolist (p emacs-modules-to-load)
  (load (concat emacs-config-directory (format "/%s" p))))

;; Let's lower our GC thresholds back down to a sane level
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist doom--file-name-handler-alist)
