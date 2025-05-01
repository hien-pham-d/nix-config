(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-gc)
(require 'init-editor)
(require 'init-ui)
(require 'init-packages)
(require 'init-keymaps)

;; session restore
(when (file-exists-p (expand-file-name ".persp/last" user-emacs-directory))
  (persp-state-load (expand-file-name ".persp/last" user-emacs-directory)))
