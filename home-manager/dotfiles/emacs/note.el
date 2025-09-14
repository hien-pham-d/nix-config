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
      '(""
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
;;
(use-package helix
  :config
  (helix-mode))

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

;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
