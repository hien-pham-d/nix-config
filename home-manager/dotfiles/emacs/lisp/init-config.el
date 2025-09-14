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
                (:eval (project-mode-line-format))
                " > "
                (:eval (my/mode-line-repo-relative-path))
                " [%*] "
                ;; TODO: right side
                " [git:"
                (:eval (my/mode-line-git-branch))
                "] "
                " C:%c %P"
                ))

(provide 'init-config)
