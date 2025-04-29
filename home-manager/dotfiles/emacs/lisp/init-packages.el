;; Install a package manager like `straight.el`.
;; https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration with `use-package`
;; https://github.com/radian-software/straight.el?tab=readme-ov-file#integration-with-use-package
(straight-use-package 'use-package)

;; ensure packages are installed if missing
(setq straight-use-package-by-default t)

;; Load env vars from my shell environment (e.g. .zshrc, .bashrc, ...)
(use-package exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  ;; enable noninteractive shell to reduce load time
  ;; (setq exec-path-from-shell-arguments nil)
  ;; select env var to load
  ;; (dolist (var '("PATH"))
  ;;   (add-to-list 'exec-path-from-shell-variables var))
  ;; init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))
  )


;; Enable Vertico for minibuffer fuzzy search
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)

  :bind
  (:map vertico-map
	      ("C-y" . vertico-insert)
	      )
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(setq completion-ignore-case t)


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)          ;; Minimum length of prefix for auto completion
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay 0.25)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook (
  ;;        ;; (prog-mode . corfu-mode)
  ;;        ;; (shell-mode . corfu-mode)
  ;;        ;; (eshell-mode . corfu-mode)
  ;;        )

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; (add-hook 'eshell-mode-hook (lambda ()
  ;;                               (setq-local corfu-auto nil)
  ;;                               (corfu-mode)))

  (keymap-set corfu-map "C-y" 'corfu-complete)
  (keymap-set corfu-map "C-n" 'corfu-next)
  (keymap-set corfu-map "C-p" 'corfu-previous)
  (keymap-set corfu-map "M-SPC" 'corfu-insert-separator)
  (keymap-set corfu-map "TAB" #'indent-for-tab-command)
  (keymap-set corfu-map "RET" #'newline)
  )

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package evil
  :config
  (evil-mode 1)
  (keymap-set global-map "C-u" 'evil-scroll-up)
  (keymap-set global-map "C-d" 'evil-scroll-down)
  (keymap-set evil-insert-state-map "C-b" 'completion-at-point)
  ;; unset conflict keys to corfu-map
  (keymap-unset evil-insert-state-map "C-y")
  (keymap-unset evil-insert-state-map "C-n")
  (keymap-unset evil-insert-state-map "C-p")
  )

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  )

;; became default from Emacs 30
(which-key-mode 1)

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (
         ("C-c M-x" . consult-mode-command)
         ([remap Info-search] . consult-info)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (setq consult-fd-args '("fd"
                          "--full-path --color=never --hidden"
                          "--exclude" ".git"
                          "--exclude" "node_modules"
                          ))

  (setq consult-ripgrep-args '("rg"
                               "--hidden"
                               "--glob !.git"
                               "--glob !node_modules"
                               "--null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"))
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  )

(use-package magit
  :straight (:host github :repo "magit/magit" :branch "main")
  :config
  ;; overwrite magit bindings
  (keymap-set magit-status-mode-map "j" 'next-line)
  (keymap-set magit-status-mode-map "k" 'previous-line)
  (keymap-set magit-status-mode-map "V" 'evil-visual-line)
  (keymap-set magit-status-mode-map "s" 'magit-stage)
  (keymap-set magit-status-mode-map "u" 'magit-unstage)
  (keymap-set magit-status-mode-map "x" 'magit-discard)
  ;; overwrite evil bindings
  (evil-define-key '(normal visual) magit-status-mode-map "s" 'magit-stage)
  (evil-define-key '(normal visual) magit-status-mode-map "u" 'magit-unstage)
  (evil-define-key '(normal visual) magit-status-mode-map "x" 'magit-discard)
  ;; ui
  (defun exec-in-new-tab (orig-fun &rest args)
    (tab-bar-new-tab)
    (apply orig-fun args)
    (delete-other-windows))

  ;; (advice-add 'magit-status :around #'exec-in-new-tab)
  ;; (advice-add 'magit-status-quick :around #'exec-in-new-tab)
  ;; (advice-add 'magit-mode-bury-buffer :after #'tab-bar-close-tab)

  ;; Add info manual of magit to Emacs.
  ;; If a `magit.info` is not available yet,
  ;; please run `make info` in the cloned local repo of magit at `~/.emacs.d/stragight/repos/magit`.
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list "~/.emacs.d/straight/repos/magit/docs"))
  )

(use-package git-gutter
  :after (evil)
  :init
  ;; If you enable global minor mode
  (global-git-gutter-mode t)

  :config
  (evil-define-key '(normal visual) diff-mode-map "q" #'kill-buffer)
  )

(use-package perspective
  ;; :bind
  ;; ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-c u s"))
  :init
  (setq persp-state-default-file (expand-file-name ".persp/master" user-emacs-directory))
  (setq persp-modestring-short t)
  :config
  (persp-mode)
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  (add-hook 'kill-emacs-hook (lambda()
                               (interactive)
                               (when (yes-or-no-p "Save session before exiting? ")
                                 (persp-state-save (expand-file-name ".persp/current" user-emacs-directory)))
                               ))
  )

(use-package rainbow-delimiters
  :hook (prog-mode text-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
	  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	        treemacs-deferred-git-apply-delay        0.5
	        treemacs-directory-name-transformer      #'identity
	        treemacs-display-in-side-window          t
	        treemacs-eldoc-display                   'simple
	        treemacs-file-event-delay                2000
	        treemacs-file-extension-regex            treemacs-last-period-regex-value
	        treemacs-file-follow-delay               0.2
	        treemacs-file-name-transformer           #'identity
	        treemacs-follow-after-init               t
	        treemacs-expand-after-init               t
	        treemacs-find-workspace-method           'find-for-file-or-pick-first
	        treemacs-git-command-pipe                ""
	        treemacs-goto-tag-strategy               'refetch-index
	        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	        treemacs-hide-dot-git-directory          t
	        treemacs-indentation                     2
	        treemacs-indentation-string              " "
	        treemacs-is-never-other-window           nil
	        treemacs-max-git-entries                 5000
	        treemacs-missing-project-action          'ask
	        treemacs-move-files-by-mouse-dragging    t
	        treemacs-move-forward-on-expand          nil
	        treemacs-no-png-images                   nil
	        treemacs-no-delete-other-windows         t
	        treemacs-project-follow-cleanup          nil
	        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	        treemacs-position                        'left
	        treemacs-read-string-input               'from-child-frame
	        treemacs-recenter-distance               0.1
	        treemacs-recenter-after-file-follow      nil
	        treemacs-recenter-after-tag-follow       nil
	        treemacs-recenter-after-project-jump     'always
	        treemacs-recenter-after-project-expand   'on-distance
	        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	        treemacs-project-follow-into-home        nil
	        treemacs-show-cursor                     nil
	        treemacs-show-hidden-files               t
	        treemacs-silent-filewatch                nil
	        treemacs-silent-refresh                  nil
	        treemacs-sorting                         'alphabetic-asc
	        treemacs-select-when-already-in-treemacs 'move-back
	        treemacs-space-between-root-nodes        t
	        treemacs-tag-follow-cleanup              t
	        treemacs-tag-follow-delay                1.5
	        treemacs-text-scale                      nil
	        treemacs-user-mode-line-format           nil
	        treemacs-user-header-line-format         nil
	        treemacs-wide-toggle-width               70
	        treemacs-width                           35
	        treemacs-width-increment                 1
	        treemacs-width-is-initially-locked       t
	        treemacs-workspace-switch-cleanup        nil)

	  ;; The default width and height of the icons is 22 pixels. If you are
	  ;; using a Hi-DPI display, uncomment this to double the icon size.
	  ;;(treemacs-resize-icons 44)

	  (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
	  (treemacs-filewatch-mode t)
	  (treemacs-fringe-indicator-mode 'always)
    ))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; (treemacs-start-on-boot)

;; modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-support-imenu t)
  :custom
  (doom-modeline-height 32)
  (doom-modeline-buffer-encoding nil)
  :hook (after-init . doom-modeline-mode))

;; outline
(use-package imenu-list
  :config
  (evil-define-key '(normal) imenu-list-major-mode-map "q" 'imenu-list-smart-toggle)
  )

;; linter
(with-eval-after-load 'flymake
  ;; (setq-default
  ;;  flycheck-disabled-checkers
  ;;  (append (default-value 'flycheck-disabled-checkers)
  ;;          '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))
  (add-hook 'prog-mode-hook 'flymake-mode)

  )

;; lsp client
(with-eval-after-load 'eglot
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq eldoc-documentation-strategy #'eldoc-documentation-default)
              (setq eldoc-echo-area-use-multiline-p t)
              ))
  )
(require 'eglot)

;; treesitter
(use-package tree-sitter
  :disabled)
(use-package tree-sitter-langs
  :disabled)


;; FIXME: automate the 'tree-sitter-langs-install-grammars
;; (setq treesit-language-source-alist
;;       '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
;;         (c . ("https://github.com/tree-sitter/tree-sitter-c"))
;;         (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;         (css . ("https://github.com/tree-sitter/tree-sitter-css"))
;;         (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
;;         (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
;;         (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
;;         (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
;;         (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil)
;;         (go . ("https://github.com/tree-sitter/tree-sitter-go"))
;;         (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
;;         (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src" nil nil)
;;         (html . ("https://github.com/tree-sitter/tree-sitter-html"))
;;         (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
;;         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
;;         (json . ("https://github.com/tree-sitter/tree-sitter-json"))
;;         (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
;;         (make . ("https://github.com/alemuller/tree-sitter-make"))
;;         (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
;;         (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
;;         (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
;;         (org . ("https://github.com/milisims/tree-sitter-org"))
;;         (python . ("https://github.com/tree-sitter/tree-sitter-python"))
;;         (php . ("https://github.com/tree-sitter/tree-sitter-php"))
;;         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
;;         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
;;         (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
;;         (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
;;         (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
;;         (scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src" nil nil)
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml" "master" "src" nil nil)
;;         (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
;;         (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
;;         (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
;;         (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
;;         (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
;;         (nix . ("https://github.com/nix-community/nix-ts-mode"))
;;         (mojo . ("https://github.com/HerringtonDarkholme/tree-sitter-mojo"))))

;; (setq major-mode-remap-alist
;;       '((bash-mode . bash-ts-mode)
;;         (css-mode . css-ts-mode)
;;         (elisp-mode . elisp-ts-mode)
;;         (elixir-mode . elixir-ts-mode)
;;         (go-mode . go-ts-mode)
;;         (hcl-mode . hcl-ts-mode)
;;         (heex-mode . heex-ts-mode)
;;         (html-mode . html-ts-mode)
;;         (js2-mode . js-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (makefile-mode . make-ts-mode)
;;         (markdown-mode . markdown-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (toml-mode . toml-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         (yaml-mode . yaml-ts-mode)))

;; formatter
(use-package apheleia
  :init
  (apheleia-global-mode +1)
  )

(use-package keycast
  :straight (:host github :repo "tarsius/keycast" :branch "main")
  ;; :config
  ;; (keycast-mode-line-mode)
  )

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

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))

(use-package harpoon
  :after (evil)
  :straight (:host github :repo "otavioschwanck/harpoon.el" :branch "master")
  :config
  (evil-define-key '(normal visual) harpoon-mode-map "q" #'kill-buffer)
  )

(use-package copilot
  :disabled
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :init
  (setq copilot-indent-offset-warning-disable t)
  ;; prefer using AI's completion mannually in normal mode
  (setq copilot-disable-display-predicates '(evil-insert-state-p))
  :hook (
         (prog-mode . copilot-mode)
         (org-mode . copilot-mode)
         (markdown-ts-mode . copilot-mode)
         )
  )

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
              ;; (vterm-send-key "C-l")
              (local-unset-key (kbd "C-h"))
              (local-unset-key (kbd "C-j"))
              (local-unset-key (kbd "C-k"))
              (local-unset-key (kbd "C-l"))
              ))
  (setq vterm-keymap-exceptions nil)
  )

(use-package helm)
(use-package helm-dash
  :init
  (setq dash-docs-enable-debugging nil))

;; after downloading, run this: nerd-icons-install-fonts
(use-package nerd-icons)

(use-package direnv)

(use-package nix-mode)

(use-package treesit-auto
  :config
  ;; prompt to install a new parser when unavailable
  (setq treesit-auto-install 'prompt)
  ;; auto map file extensions to appropriate treesitter language
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(provide 'init-packages)
