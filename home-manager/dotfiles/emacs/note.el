;; POST INSTALLATION

(copilot-install-server)

(nerd-icons-install-fonts)

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


;; ESHELL CUSTOMIZATION
(require 'dash)
(require 's)

(defmacro with-face (str &rest props)
  `(propertize ,str 'face (list ,@props)))

(defmacro esh-section (name icon form &rest props)
  `(setq ,name
         (lambda () (when ,form
                      (-> ,icon
                          (concat esh-section-delim ,form)
                          (with-face ,@props))))))

(defun esh-acc (acc x)
  (--if-let (funcall x)
      (if (s-blank? acc) it (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

;; Separator settings
(setq esh-sep " | ")
(setq esh-section-delim " ")
(setq esh-header "\n")
(setq eshell-prompt-regexp "> ")
(setq eshell-prompt-string "> ")

;; Prompt sections
(esh-section esh-dir
             "" ; folder icon
             (abbreviate-file-name (eshell/pwd))
             :foreground "#005782" :bold t)

(esh-section esh-git
             "on " ; git icon
             (when (fboundp 'magit-get-current-branch)
               (magit-get-current-branch))
             :foreground "#30a80f")

(esh-section esh-python
             "\xe928" ; python icon
             (when (bound-and-true-p pyvenv-virtual-env-name)
               pyvenv-virtual-env-name))

(esh-section esh-clock
             "\xf017" ; clock icon
             (format-time-string "%H:%M" (current-time))
             :foreground "#305d5f")

(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (1+ esh-prompt-num))))

(setq eshell-funcs (list esh-dir esh-git esh-python esh-clock))
(setq eshell-prompt-function 'esh-prompt-func)

;; NOT WORKING CODE
(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  :config
  ;; exwm can not show the dashboardd as expected. I also confirmed that it works on GNOME.
  (dashboard-setup-startup-hook)
  )
