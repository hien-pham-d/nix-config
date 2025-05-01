;; background color
(set-background-color "#1e1e2e")
(set-face-attribute 'line-number nil :background "#1e1e2e")

;; fontsize
;; (set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140)

(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :background "#2a2b3c")
  )

;; hide some UI components
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq tab-bar-show 1) ;; show if the number of tab bars execced 1

;; show both line and column for the current cursor position
(setq column-number-mode t)

;; relative line number at global
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; highlight the current line at the cursor position
(global-hl-line-mode)

;; Recommended way to enable line wrapping.
;; This automatically set world-wrap = t.
(global-visual-line-mode)

;; flash the mode-line instead of ring a bell when unexpected actions occur
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil (lambda () (invert-face 'mode-line)))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default evil-shift-width 2)

;; (setq-default whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark))
;; when using copilot, we have to remove the newline-mark in whitespace-style.
(setq-default whitespace-style '(face tabs tab-mark spaces space-mark trailing newline))

(setq-default whitespace-display-mappings
              '((tab-mark ?\t [?» ?\t])
	              ;;(space-mark ?\s [?·])
	              ;;(space-mark ?\xA0 [?␣])
                (newline-mark ?\n [?↵ ?\n])))
(global-whitespace-mode 1)

;; (setq mode-line-format
;;       '(("%e" mode-line-front-space (:propertize ... display ...)
;;          ;; evil-mode-line-tag
;;          ;; mode-line-frame-identification
;;          mode-line-buffer-identification "   "
;;          mode-line-position
;;          ;; (project-mode-line project-mode-line-format)
;;          ;; (vc-mode vc-mode) "  "
;;          ;; mode-line-modes
;;          ;; mode-line-misc-info
;;          ;; mode-line-end-spaces
;;          ))
;;       )

;; fullscreen
(toggle-frame-maximized)

(add-hook 'window-setup-hook (lambda()
                               (message "Startup completed in %s." (emacs-init-time))
                               ))

(provide 'init-ui)
