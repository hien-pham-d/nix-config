;; disable because there is an issue when using completions from copilot
;; (electric-pair-mode 1) ;; Did not use smartparens because it does not auto-indent on newline

;; Improve performance when handling long lines files
;; https://emacsdocs.org/docs/emacs/Long-Lines
(global-so-long-mode t)

(editorconfig-mode t)

(recentf-mode t)

(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(load custom-file 'noerror)

(setq make-backup-files nil)

(provide 'init-editor)
