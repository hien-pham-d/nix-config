;; POST INSTALLATION

;; (copilot-install-server)

(nerd-icons-install-fonts)

(treesit-auto-install-all)

;; playground
;; mode-line simplification

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

(setq mode-line-format
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
                " C:%c %p"
                ))

(setq mode-line-format-right-align
      '(""
        )
      )
(force-mode-line-update t)

;;
(use-package helix
  :config
  (helix-mode))

;; achirved
(use-package vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local truncate-lines t)
              (visual-line-mode -1)
              ;; (setq-local evil-insert-state-cursor 'box)
              (setq-local evil-insert-state-cursor 'bar)
              ;; (setq mode-line-format default-mode-line-format)
              ;; (vterm-reset-cursor-point)  ;; Refresh terminal display
              ;; (vterm-clear-scrollback)    ;; Clear and re-render
              ))
  (setq vterm-keymap-exceptions nil)
  ;; allow window nagivation through C-<h,j,k,l>
  (keymap-unset vterm-mode-map "C-h")
  (keymap-unset vterm-mode-map "C-j")
  (keymap-unset vterm-mode-map "C-k")
  (keymap-unset vterm-mode-map "C-l")

  ;; global keys
  (keymap-unset vterm-mode-map "C-'")
  (keymap-unset vterm-mode-map "C-]")
  (keymap-unset vterm-mode-map "M-b")
  (keymap-unset vterm-mode-map "M-r")
  (keymap-unset vterm-mode-map "M-s")
  (keymap-unset vterm-mode-map "M-&")
  (keymap-unset vterm-mode-map "M-1")
  (keymap-unset vterm-mode-map "M-2")
  (keymap-unset vterm-mode-map "M-3")
  (keymap-unset vterm-mode-map "M-4")

  (keymap-unset vterm-mode-map "C-SPC")
  (keymap-unset vterm-mode-map "C-@")

  (keymap-unset vterm-mode-map "C-\\")

  ;; ensure some default behaviors of the terminal
  (keymap-set vterm-mode-map "C-u" #'vterm--self-insert)
  (keymap-set vterm-mode-map "C-r" #'vterm--self-insert)
  (keymap-set vterm-mode-map "C-w" #'vterm--self-insert)

  ;; switch exwm workspace
  (keymap-unset vterm-mode-map "M-1")
  (keymap-unset vterm-mode-map "M-2")
  (keymap-unset vterm-mode-map "M-3")
  (keymap-unset vterm-mode-map "M-4")

  (keymap-unset vterm-mode-map "M-,")
  (keymap-unset vterm-mode-map "M-.")
  (keymap-unset vterm-mode-map "M-/")
  (keymap-unset vterm-mode-map "M-;")

  )

(defun my-shared-status ()
  "Return the buffer name of the currently selected window."
  (buffer-name (window-buffer (selected-window))))

(setq tab-line-close-button-show nil)   ; hide close button
(setq tab-line-new-button-show nil)     ; hide new button
(setq tab-line-separator "")            ; no separator
(setq-default mode-line-format nil)     ; kill per-window mode line

(setq tab-line-format
      '((:eval (format "  %s  " (my-shared-status)))))

(setq tab-line-format nil)

(global-tab-line-mode 1)
(global-tab-line-mode nil)


;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :init
;;   (setq copilot-indent-offset-warning-disable t)
;;   ;; prefer using AI's completion mannually in normal mode
;;   ;; (setq copilot-disable-display-predicates '(evil-insert-state-p))

;;   :hook (
;;          (prog-mode . copilot-mode)
;;          (org-mode . copilot-mode)
;;          (markdown-mode . copilot-mode)
;;          )
;;   :bind
;;   (:map copilot-completion-map
;;         ("M-i" . copilot-accept-completion)
;;         ("TAB" . #'indent-for-tab-command)
;;         )
;;   )

(use-package dape
  :disabled
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")
  ;; (setq dape-key-prefix nil)

  ;; :hook
  ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-function 'projectile-project-root)


  (defun dape-jest/find-file-buffer-default ()
    "Read filename at project root, defaulting to current buffer. Return vector of jest args to run said file"
    (let ((file (dape-buffer-default)))
      (if file
          `["--runInBand" "--no-coverage" ,file]
        (user-error "No file found"))))

  (defun dape-jest/ensure (config)
    "Ensure node is available, jest is installed, that the dapDebugServer is installed"

    (dape-ensure-command config))

  (add-to-list 'dape-configs
               `(js-debug-ts-node-me
                 modes (typescript-mode typescript-ts-mode)
                 ensure dape-jest/ensure
                 command "node"
                 command-cwd dape-command-cwd
                 command-args (,(expand-file-name
                                 (file-name-concat dape-adapter-dir
                                                   "js-debug"
                                                   "src"
                                                   "dapDebugServer.js"))
                               :autoport)
                 port :autoport
                 fn dape-config-autoport
                 :type "pwa-node"
                 :request "launch"
                 :name "Launch Test"
                 :runtimeExecutable "yarn"
                 :runtimeArgs ("test:e2e" "-t" "POST.*validation error.*title")
                 :cwd dape-cwd
                 :outputCapture "std"
                 :console "integratedTerminal")))

(with-eval-after-load 'org
  (setq org-directory "~/workspace/repos/second-brain/para/")

  (setq org-agenda-files '("projects/projects.org" "projects/journal.org"))

  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             "BLOCKED(b@)"
                             "IN-PROGRESS(i!)"
                             "PAUSED(p)"
                             "UNDER-REVIEW(r!)"
                             "|"
                             "DONE(d!)"
                             "DELEGATED(D@)"
                             "CANCELED(c@)"
                             )))

  (setq org-todo-keyword-faces '(("TODO" . "#ea76cb")
                                 ("UNDER-REVIEW" . "#ea76cb")
                                 ("BLOCKED" . "#d20f39")
                                 ("IN-PROGRESS" . "#04a5e5")
                                 ("PAUSED" . "#ea76cb")
                                 ("DONE" . "#40a02b")
                                 ("DELEGATED" . "#8c8fa1")
                                 ("CANCELED" . "#4c4f69")))

  (setq org-tag-alist '((:startgroup)
                        ("Locations")
                        (:grouptags)
                        ("@personal" . ?P)
                        ("@work" . ?W)
                        (:endgrouptag)
                        (:endgroup)
                        ;;
                        (:startgroup)
                        ("Priorities")
                        (:grouptags)
                        ("@high" . ?H)
                        ("@medium" . ?M)
                        ("@low" . ?L)
                        (:endgrouptag)
                        (:endgroup)))


  (setq org-default-notes-file (concat org-directory "notes.org"))

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (lambda ()(concat org-directory "projects/projects.org")) "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (lambda () (concat org-directory "projects/journal.org")))
           "* %?\nEntered on %U\n  %i\n  %a")))
  ;; auto-clock when working on an item
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Resolving idle time
  (setq org-clock-idle-time 25)


  ;; disable underline in org link
  (custom-set-faces
   '(org-link ((t (:foreground "royal blue" :underline nil :italic t)))))

  ;; auto-clock-out when idle
  ;; (setq clock-auto-clockout-timer 1200)
  ;; (org-clock-auto-clockout-insinuate)

  ;; auto clock-in/out based on task state
  (add-hook 'org-after-todo-state-change-hook (lambda()
                                                (if (and (member org-state '("IN-PROGRESS"))
                                                         (not (org-clock-is-active)))
                                                    (org-clock-in)
                                                  (org-clock-out))))

  (add-hook 'org-mode-hook #'org-indent-mode)
  ;; (setq display-fill-column-indicator-column 80)
  ;; (add-hook 'org-mode-hook #'display-fill-column-indicator-mode)
  ;; (add-hook 'org-mode-hook #'auto-fill-mode)

  (keymap-unset org-mode-map "C-'")
  (keymap-unset org-mode-map "C-,")
  (keymap-unset org-mode-map "C-h")
  (keymap-unset org-mode-map "C-j")
  (keymap-unset org-mode-map "C-k")
  (keymap-unset org-mode-map "C-l")

  (keymap-unset org-mode-map "M-h")
  (keymap-unset org-mode-map "M-j")
  (keymap-unset org-mode-map "M-k")
  (keymap-unset org-mode-map "M-l")
  )
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(use-package exwm
  :disabled
  :init
  (defun me/exwm-refresh-screen ()
    (interactive)
    (start-process-shell-command
     "xrandr" nil "xrandr --output Virtual-1 --primary --auto --pos 0x0 --rotate normal")
    (exwm-randr-refresh)
    )

  ;; (setq mouse-autoselect-window t)
  ;; (setq focus-follows-mouse 'auto-raise)

  (require 'exwm)
  (setq exwm-workspace-number 1)
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (setq exwm-input-global-keys
        `(
          (,(kbd "C-M-;") . exwm-workspace-switch)
          ;; TODO: add a func to switch to the last workspace instead
          ;; (,(kbd "C-M-'") . evil-switch-to-windows-last-buffer)
          (,(kbd "C-'") . evil-switch-to-windows-last-buffer)
          (,(kbd "C-SPC") . consult-buffer)
          (,(kbd "C-M-SPC") . exwm-workspace-switch-to-buffer)
          (,(kbd "C-M-r") . exwm-reset)
          (,(kbd "C-M-/") . (lambda (cmd)
                              (interactive (list (read-shell-command "$ ")))
                              (start-process-shell-command cmd nil cmd)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "M-%d" i)) . (lambda ()
                                                     (interactive)
                                                     (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 4))
          ))
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "Virtual-1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda()
              (me/exwm-refresh-screen)
              ))
  (exwm-randr-mode 1)
  (exwm-enable)
  )
