;; POST INSTALLATION

;; (copilot-install-server)

(nerd-icons-install-fonts)

(dolist (item treesit-language-source-alist)
  (message "read: %s" (car item))
  (treesit-install-language-grammar (car item))
  )

;; Note
(use-package evil-collection
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package aidermacs
  :bind (("C-c i m" . aidermacs-transient-menu))
  :config
                                        ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  (setenv "ANTHROPIC_API_KEY" "sk-...")
                                        ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
                                        ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))
