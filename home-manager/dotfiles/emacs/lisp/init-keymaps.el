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

(defun me/copy-file-path ()
  "Copy the file path to clipboard - relative if in project, full otherwise."
  (interactive)
  (if buffer-file-name
      (let* ((proj (project-current))
             (path (if proj
                       (file-relative-name buffer-file-name (project-root proj))
                     buffer-file-name)))
        (kill-new path)
        (message "Copied: %s" path))
    (message "Buffer is not visiting a file")))

(require 'bash-output-mode)

;; Change prefix key for help commands.
;; We're going to use the "C-h" for windmove-left.
(keymap-unset global-map "C-h")

(me/keymap-set-with-desc global-map "C-s" 'me/save-buffer "save-buffer")

(me/keymap-set-with-desc global-map "C-h" 'windmove-left "windmove-left")
(me/keymap-set-with-desc global-map "C-j" 'windmove-down "windmove-down")
(me/keymap-set-with-desc global-map "C-k" 'windmove-up "windmove-up")
(me/keymap-set-with-desc global-map "C-l" 'windmove-right "windmove-right")

;; harpoon
(me/keymap-set-with-desc global-map "M-h" #'harpoon-go-to-1 "goto-1")
(me/keymap-set-with-desc global-map "M-j" #'harpoon-go-to-2 "goto-2")
(me/keymap-set-with-desc global-map "M-k" #'harpoon-go-to-3 "goto-3")
(me/keymap-set-with-desc global-map "M-l" #'harpoon-go-to-4 "goto-4")

;; Quick Accesss

;; cross-workspace level (C-M-prefix)
;; DONE: switch to workspace (C-M-;)
;; TODO: switch to last workspace (C-M-')
;; DONE: switch to buffer (C-M-SPC)

;; per-workspace level (C-previx)
;; bookmarks
(keymap-set global-map "C-;" #'harpoon-toggle-file)
;; last buffer
;; TODO: does not work in insert mode in terminal emacs
(keymap-set global-map "C-'" #'evil-switch-to-windows-last-buffer)
;; symbols
;; FIXME: does not work
(keymap-set global-map "C-7" #'xref-find-apropos)
;; compile
(keymap-set global-map "C-8" #'compile)
;; buffers
(keymap-set global-map "C-SPC" #'consult-buffer)
(keymap-set global-map "C-@" #'consult-buffer) ;; Terminal Emacs recieves C-SPC as C-@
;; fd
(keymap-set global-map "C-]" #'consult-fd)
;; projects
(keymap-set global-map "C-\\" #'project-switch-project)
;; explorer
(keymap-set global-map "C-," #'treemacs)
;; git
(keymap-set global-map "C-." #'me/magit-status)
;; ripgrep
(keymap-set global-map "C--" #'consult-ripgrep)
;; terminal
(keymap-set global-map "C-/" #'eat-project)

;; per-buffer level (M-prefix)
;; TODO: bookmarks
(keymap-set global-map "M-;" #'consult-line)
;; grep
(keymap-set global-map "M-/" #'consult-line)
(keymap-set global-map "M-SPC" #'consult-line)
;; symbols
;; FIXME: does not work
(keymap-set global-map "M-8" #'consult-outline)
;; prev-hunk
(keymap-set global-map "M-," #'diff-hl-previous-hunk)
;; next-hunk
(keymap-set global-map "M-." #'diff-hl-next-hunk)

(setq me-master-map (me/gen-sub-keymap global-map "C-c" ""))

;; (with-eval-after-load 'magit
;;   ;; make "SPC" available in magit-status-mode-map
;;   (keymap-set magit-status-mode-map "SPC" me-master-map)
;;   )

;; Quick-Access
(me/keymap-set-with-desc me-master-map "/" 'consult-line "text-in-file")
(me/keymap-set-with-desc me-master-map "SPC" 'project-switch-to-buffer "active-file-in-project")
(me/keymap-set-with-desc me-master-map "M-x" 'execute-extended-command "execute-cmd")
(me/keymap-set-with-desc me-master-map "C-h" help-map "help")

(me/keymap-set-with-desc me-master-map "z t" 'evil-scroll-line-to-top "scroll-line-to-top")
(me/keymap-set-with-desc me-master-map "z z" 'evil-scroll-line-to-center "scroll-line-to-center")
(me/keymap-set-with-desc me-master-map "z b" 'evil-scroll-line-to-bottom "scroll-line-to-bottom")

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
                           ;; ("w" persp-switch "workspace")
                           ("p" project-switch-project "project")
                           ("T" consult-ripgrep "text-in-project")
                           ("f" project-switch-to-buffer "active-file-in-project")
                           ("d" find-file "file-in-dir")
                           ("F" consult-fd "file-in-project")
                           ("t" consult-line "text-in-file")
                           ("b" consult-buffer "buffer")
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
                           ("t" projectile-run-vterm "terminal")
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
                           ("C-h" windmove-left "windmove-left")
                           ("C-j" windmove-down "windmove-down")
                           ("C-k" windmove-up "windmove-up")
                           ("C-l" windmove-right "windmove-right")
                           )))

(setq me-utils-map
      (me/gen-sub-keymap me-master-map "u" "utils"
                         '(
                           ;; ("s" persp-state-save "session-save")
                           ("s" desktop-save "session-save")
                           ;; ("r" persp-state-load "session-restore")
                           ("r" desktop-read "session-restore")
                           ("x" eval-region "eval-selected")
                           ("X" eval-buffer "eval-file")
                           (":" eval-expression "eval-expression")
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
                           ("p" diff-hl-show-hunk "hunk-preview")
                           ("r" diff-hl-revert-hunk "hunk-revert")
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
                           ("q" me/kill-this-buffer "quit")
                           ("y" me/copy-file-path "copy-path")
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

(setq me-note-map
      (me/gen-sub-keymap me-master-map "n" "note"
                         '(
                           ("t" org-todo "todo-change-status")
                           ("l" org-todo-list "todo-list")
                           ("a" org-agenda "agenda")
                           ("f" org-toggle-narrow-to-subtree "focus-toggle")
                           ("c" org-capture "Capture")
                           ("s" org-schedule "set-schedule")
                           ("d" org-deadline "set-deadline")
                           ("T" org-set-tags-command "set-tags")
                           ("L" org-store-link "store-link")
                           ("l" org-insert-link "insert-link")
                           )))

;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook
          (lambda()
            (keymap-set lisp-interaction-mode-map "C-j" 'windmove-down)
            ))

;; (with-eval-after-load 'message
;;   (keymap-set messages-buffer-mode-map "SPC" me-master-map)
;;   )

(provide 'init-keymaps)
