;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell)

(setq mac-command-modifier 'meta
      mac-option-modifier 'alt
      ns-use-native-fullscreen nil)

;; A known problem with GUI Emacs on MacOS: it runs in an isolated
;; environment, so envvars will be wrong. That includes the PATH
;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
;; and benefits greatly from compilation.
(setq exec-path
      (or (eval-when-compile
            (when (require 'exec-path-from-shell nil t)
              (setq exec-path-from-shell-check-startup-files nil
                    exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
              (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
              (exec-path-from-shell-initialize)
              exec-path))
          exec-path))
