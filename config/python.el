;; -*- lexical-binding: t; -*-

(defun +python/kill-inferior-on-exit ()
  "Kill buffer and delete window of inferior process."
  (let* ((buff (current-buffer))
          (proc (get-buffer-process buff)))
    (lexical-let ((buff buff))
      (set-process-sentinel proc (lambda (process event)
                                   (if (string= event "finished\n")
                                     (kill-buffer-and-window)))))))

(use-package anaconda-mode
  :hook python-mode
  :init
  (setq python-indent-offset 4
    anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  (add-hook 'inferior-python-mode-hook #'+python/kill-inferior-on-exit))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))
