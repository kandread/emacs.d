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

;; Kill Magit buffer
(defun +version-control/magit-kill-buffer (buf)
  "Kill Magit buffer."
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+version-control/magit-kill-buffer buf)
            (kill-process process)
	    (kill-buffer buf)))))))

;; Clean up Magit buffers
(defun +version-control/magit-quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'."
  (interactive)
  (quit-window)
  (delete-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
			     (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (mapc #'+version-control/magit-kill-buffer (magit-mode-get-buffers))))

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
