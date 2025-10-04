;; Adjust garbage collection threshold for early startup.
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook (lambda()
                             (setq gc-cons-threshold (* 32 1024 1024))))

(provide 'init-gc)
