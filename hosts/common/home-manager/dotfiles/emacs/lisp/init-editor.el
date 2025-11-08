;; disable because there is an issue when using completions from copilot
(electric-pair-mode 1) ;; Did not use smartparens because it does not auto-indent on newline
(electric-indent-mode t) ;; It behaves weird in some cases that make a very large indentation when pressing '.' chacracter in a method call.

;; Improve performance when handling long lines files
;; https://emacsdocs.org/docs/emacs/Long-Lines
(global-so-long-mode t)

(global-auto-revert-mode t)

(editorconfig-mode t)

(recentf-mode t)

(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(load custom-file 'noerror)

(setq make-backup-files nil)

(when (not (display-graphic-p))
  (xterm-mouse-mode t))

;; (when (string= (getenv "XDG_SESSION_TYPE") "wayland")
;;   (setq wl-copy-process nil)
;;   (defun wl-copy (text)
;;     (setq wl-copy-process
;;           (make-process
;;            :name "wl-copy"
;;            :buffer nil
;;            :command '("wl-copy" "-f" "-n")
;;            :connection-type 'pipe))
;;     (process-send-string wl-copy-process text)
;;     (process-send-eof wl-copy-process))
;;   (defun wl-paste ()
;;     (if (and wl-copy-process (process-live-p wl-copy-process))
;;         nil ; Return nil if we own the clipboard (to avoid loops)
;;       (string-trim (shell-command-to-string "wl-paste -n | tr -d '\r'"))))
;;   (setq interprogram-cut-function #'wl-copy)
;;   (setq interprogram--function #'wl-paste))

(provide 'init-editor)
