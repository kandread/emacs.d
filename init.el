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


;; Let's lower our GC thresholds back down to a sane level
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist doom--file-name-handler-alist)
