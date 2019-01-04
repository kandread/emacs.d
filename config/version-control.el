;; -*- lexical-binding: t; -*-

;; Magit
(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
    magit-diff-refine-hunk t ; show word-granularity on selected hunk
    magit-completing-read-function #'ivy-completing-read)
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+version-control/magit-quit))

(defun +version-control/magit-quit ()
  "Clean up all Magit buffers after quitting."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;; Highlight uncommitted changes in fringe
(defun +version-control/enable-git-gutter ()
  "Enable `git-gutter-mode'."
  (setq-local git-gutter:init-function      #'git-gutter-fr:init)
  (setq-local git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos)
  (setq-local git-gutter:clear-function     #'git-gutter-fr:clear)
  (setq-local git-gutter:window-width -1)
  (git-gutter-mode +1))

(use-package git-gutter
  :ensure git-gutter-fringe
  :init
  (require 'git-gutter-fringe)
  (add-hook 'prog-mode-hook #'+version-control/enable-git-gutter)
  :config
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))
