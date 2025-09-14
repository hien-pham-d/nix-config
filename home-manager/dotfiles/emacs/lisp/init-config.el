(with-eval-after-load 'evil
  (add-hook 'edebug-mode-hook #'evil-emacs-state))

(add-hook 'prog-mode-hook #'hs-minor-mode)

(require 'project)
(require 'magit)

(defun my/mode-line-repo-relative-path ()
  "Return the file path relative to the current project root."
  (if-let* ((proj (project-current))
            (root (project-root proj))
            (file buffer-file-name))
      (file-relative-name file root)
    (buffer-name)))

(defun my/mode-line-git-branch ()
  "Return current Git branch from Magit, or empty string."
  (or (magit-get-current-branch) ""))

(setq-default mode-line-format
              '(" "
                (:eval (evil-mode-line-format))
                (:eval (let ((proj (project-current)))
                         (if proj
                             (propertize (format "%s | " (project-mode-line-format)) 'face '(:foreground "#90cc93" :weight bold))
                           "")))
                (:eval (propertize (my/mode-line-repo-relative-path) 'face (if (buffer-modified-p) '(:foreground "#e3a07d") nil)))
                (:eval (propertize " [%*] " 'face (if (buffer-modified-p) '(:foreground "#e3a07d") nil)))

                mode-line-format-right-align ;; this is the separator

                "C:%c "
                "| %P "
                (:eval (let ((branch (my/mode-line-git-branch)))
                         (if (not (string-empty-p branch))
                             (format "| %s" (propertize (format "⎇ %s " branch) 'face '(:foreground "#90cc93" :weight bold)))
                           "")))
                "| " (:eval (format-time-string "%a %d - %H:%M"))
                "  "
                ))
(display-time-mode t)


(provide 'init-config)
