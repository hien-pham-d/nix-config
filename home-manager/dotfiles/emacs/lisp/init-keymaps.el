(defun me/gen-sub-keymap (parent-map child-map-key child-map-desc &optional bindings)
  " BINDINGS is a list of keybinding definitions, where each element is a list
of the form (KEY COMMAND DESC)."
  (let ((map (make-sparse-keymap)))
    (keymap-set parent-map child-map-key map)
    (which-key-add-keymap-based-replacements parent-map child-map-key child-map-desc)
    (when bindings
      (dolist (binding bindings)
        (let ((key (nth 0 binding))
              (command (nth 1 binding))
              (key-desc (nth 2 binding)))
          (me/keymap-set-with-desc map key command key-desc))))
    map))

(defun me/keymap-set-with-desc(map key cmd desc)
  "Custom function for config keymap with description."
  (keymap-set map key cmd)
  (which-key-add-keymap-based-replacements map key desc)
  )

(defun me/magit-status()
  (interactive)
  ;; (magit-status-quick)
  ;; (magit-status-here)
  (magit-status)
  )

(defun me/save-buffer()
  (interactive)
  (evil-normal-state)
  (save-buffer))

(defun me/window-vsplit()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun me/window-split()
  (interactive)
  (split-window-below)
  (windmove-down))

;; Change prefix key for help commands.
;; We're going to use the "C-h" for windmove-left.
(keymap-unset global-map "C-h")
(keymap-set global-map "M-h" help-map)

(me/keymap-set-with-desc global-map "C-s" 'me/save-buffer "save-buffer")

(me/keymap-set-with-desc global-map "C-h" 'windmove-left "windmove-left")
(me/keymap-set-with-desc global-map "C-j" 'windmove-down "windmove-down")
(me/keymap-set-with-desc global-map "C-k" 'windmove-up "windmove-up")
(me/keymap-set-with-desc global-map "C-l" 'windmove-right "windmove-right")

;; vterm keybinding customizations
;; allow window nagivation through C-<h,j,k,l>
(keymap-unset vterm-mode-map "C-h")
(keymap-unset vterm-mode-map "C-j")
(keymap-unset vterm-mode-map "C-k")
(keymap-unset vterm-mode-map "C-l")

;; ensure some default behaviors of the terminal
(keymap-set vterm-mode-map "C-u" #'vterm--self-insert)
(keymap-set vterm-mode-map "C-r" #'vterm--self-insert)
(keymap-set vterm-mode-map "C-w" #'vterm--self-insert)

;; prevent some unexpected behaviors from evil-insert-state-map
(keymap-unset evil-insert-state-map "C-k")
(keymap-unset evil-insert-state-map "C-e")
(keymap-unset evil-insert-state-map "C-r")
(keymap-unset evil-insert-state-map "C-w")

(me/keymap-set-with-desc global-map "M-j" #'harpoon-go-to-1 "goto-1")
(me/keymap-set-with-desc global-map "M-k" #'harpoon-go-to-2 "goto-2")
(me/keymap-set-with-desc global-map "M-l" #'harpoon-go-to-3 "goto-3")
(me/keymap-set-with-desc global-map "M-;" #'harpoon-go-to-4 "goto-4")

(setq me-master-map (me/gen-sub-keymap global-map "C-c" ""))

(with-eval-after-load 'evil
  ;; make "SPC" available in magit-status-mode-map
  (keymap-set magit-status-mode-map "SPC" me-master-map)
  ;; make "SPC" available in global-map in normal and visual mode.
  (evil-define-key '(normal visual) global-map (kbd "SPC") me-master-map)

  (evil-define-key '(normal visual) global-map (kbd "g d") #'xref-find-definitions)
  (evil-define-key '(normal visual) global-map (kbd "g t") #'eglot-find-typeDefinition)
  (evil-define-key '(normal visual) global-map (kbd "g r") #'xref-find-references)
  (evil-define-key '(normal visual) global-map (kbd "g i") #'eglot-find-implementation)
  )

;; Quick-Access
(me/keymap-set-with-desc me-master-map "/" 'consult-line "text-in-file")
(me/keymap-set-with-desc me-master-map "SPC" 'project-switch-to-buffer "active-file-in-project")
(me/keymap-set-with-desc me-master-map "M-x" 'execute-extended-command "execute-cmd")
(me/keymap-set-with-desc me-master-map "C-h" help-map "help")

(setq me-help-map
      (me/gen-sub-keymap me-master-map "h" "help"
                         '(
                           ("m" consult-info "manual")
                           ("p" describe-keymap "keymap")
                           ("k" describe-mode "keymap-local")
                           ("K" where-is "keymap-global")
                           ("f" describe-function "func")
                           ("v" describe-variable "var")
                           )))

(setq me-search-map
      (me/gen-sub-keymap me-master-map "s" "search"
                         '(
                           ("w" persp-switch "workspace")
                           ("p" project-switch-project "project")
                           ("T" consult-ripgrep "text-in-project")
                           ("f" project-switch-to-buffer "active-file-in-project")
                           ("d" find-file "file-in-dir")
                           ("F" consult-fd "file-in-project")
                           ("t" consult-line "text-in-file")
                           ("b" persp-switch-to-buffer "buffer")
                           ("s" consult-imenu "symbol-in-file")
                           ("S" xref-find-apropos "symbol-in-project")
                           )))

(setq me-auto-complete-map
      (me/gen-sub-keymap me-master-map "a" "auto-complete"
                         '(
                           ("p" consult-yank-pop "from-clipboard")
                           ("l" cape-line "line")
                           ("D" cape-dict "dict")
                           )))

(setq me-code-map
      (me/gen-sub-keymap me-master-map "c" "code"
                         '(
                           ("l" eglot "lsp-start")
                           ("g d" xref-find-definitions "go-to-def")
                           ("g t" eglot-find-typeDefinition "go-to-type-def")
                           ("g r" xref-find-references "go-to-ref")
                           ("g i" eglot-find-implementation "go-to-impl")
                           )))

(setq me-toggle-map
      (me/gen-sub-keymap me-master-map "t" "toggle"
                         '(
                           ("g" me/magit-status "git-status")
                           ("v" evil-mode "vim-motion")
                           ("a" corfu-mode "auto-completion")
                           ("T" treemacs "explorer")
                           ("o" imenu-list-smart-toggle "outline")
                           ("l" comment-or-uncomment-region "comment-selected")
                           )))
(setq me-window-map
      (me/gen-sub-keymap me-master-map "w" "window"
                         '(
                           ("v" me/window-vsplit "vsplit")
                           ("x" me/window-split "xsplit")
                           ("c" delete-window "close")
                           )))

(setq me-utils-map
      (me/gen-sub-keymap me-master-map "u" "utils"
                         '(
                           ("r" persp-state-load "session-restore")
                           ("x" eval-region "eval-selected")
                           ("X" eval-buffer "eval-file")
                           ("q" save-buffers-kill-emacs "quit")
                           ("c" tab-bar-close-tab "tab-close")
                           )))

(setq me-edit-map
      (me/gen-sub-keymap me-master-map "e" "edit"
                         '(
                           ("r" query-replace-regexp "search-replace-in-file")
                           ("R" project-query-replace-regexp "search-replace-in-project")
                           )))

(setq me-git-map
      (me/gen-sub-keymap me-master-map "g" "git"
                         '(
                           ("p" git-gutter:popup-hunk "hunk-preview")
                           ("r" git-gutter:revert-hunk "hunk-revert")
                           )))

(setq me-bookmark-map
      (me/gen-sub-keymap me-master-map "b" "bookmark"
                         '(
                           ("a" harpoon-add-file "add-file")
                           ("s" harpoon-toggle-file "search")
                           ("1" harpoon-go-to-1 "goto-1")
                           ("2" harpoon-go-to-2 "goto-2")
                           ("3" harpoon-go-to-3 "goto-3")
                           ("4" harpoon-go-to-4 "goto-4")
                           )))

(setq me-file-map
      (me/gen-sub-keymap me-master-map "f" "file"
                         '(
                           ("q" kill-buffer "quit")
                           )))

(setq me-quickfix-map
      (me/gen-sub-keymap me-master-map "q" "quickfix"
                         '(
                           ("e" flymake-show-buffer-diagnostics "compile-error-in-file")
                           ("E" flymake-show-project-diagnostics "compile-error-in-project")
                           )))

(setq me-project-map
      (me/gen-sub-keymap me-master-map "p" "project"))


(setq me-ai-map
      (me/gen-sub-keymap me-master-map "i" "ai"
                         '(
                           ("C-b" copilot-complete "complete-at-point")
                           ("C-y" copilot-accept-completion "accept-complete")
                           )
                         ))

;; git-gutter
(with-eval-after-load 'evil
  (with-eval-after-load 'git-gutter
    (evil-define-key '(normal) global-map "]h" 'git-gutter:next-hunk)
    (evil-define-key '(normal) global-map "[h" 'git-gutter:previous-hunk)
    )
  )

;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook (lambda()
                                        (keymap-set lisp-interaction-mode-map "C-j" 'windmove-down)
                                        ))

(provide 'init-keymaps)
