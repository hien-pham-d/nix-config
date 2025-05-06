(with-eval-after-load 'evil
  (add-hook 'edebug-mode-hook #'evil-emacs-state))

(add-hook 'prog-mode-hook #'hs-minor-mode)

(provide 'init-config)
