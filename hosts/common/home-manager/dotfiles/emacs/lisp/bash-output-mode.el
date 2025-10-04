(defun me/bash-run-region-or-line ()
  "Run the current region or the current line asynchronously in bash, with output in *Bash Output* buffer."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (script (buffer-substring-no-properties start end))
         (buf (get-buffer-create "*Bash Output*")))

    ;; Clear previous output asynchronously
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))

    ;; Start bash process asynchronously with the script as an argument
    ;; Use start-process to interact asynchronously
    (let ((proc (start-process "bash-run" buf "bash" "-c" script)))
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (string-match-p "finished" event)
           (with-current-buffer (process-buffer process)
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert "\nProcess finished."))))))
      (set-process-filter
       proc
       (lambda (process output)
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (let ((inhibit-read-only t))
               ;; Append output and keep point at end
               (goto-char (point-max))
               (insert output)
               (setq-local scroll-conservatively 101)
               (when (equal (window-buffer) (current-buffer))
                 (goto-char (point-max)))))))))

    ;; Display output buffer in side window
    (display-buffer-in-side-window buf '((side . right) (slot . 0) (window-width . 0.4)))

    ;; Make buffer read-only
    (with-current-buffer buf
      (read-only-mode 1))))

(with-eval-after-load 'sh-script
  (keymap-set bash-ts-mode-map "C-c C-r" #'me/bash-run-region-or-line))

;;
(defvar me/bash-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-this-buffer) ;; TODO: conflict with evil-record-macro
    map)
  "Keymap for bash-output-mode minor mode.")

(define-minor-mode me/bash-output-mode
  "Minor mode for *Bash Output* buffer."
  :lighter " BashOut"
  :keymap me/bash-output-mode-map)

(defun me/enable-bash-output-mode-if-applicable ()
  "Enable `me/bash-output-mode' if buffer name is *Bash Output*."
  (when (string= (buffer-name) "*Bash Output*")
    (me/bash-output-mode 1)))

(add-hook 'buffer-list-update-hook #'me/enable-bash-output-mode-if-applicable)

(provide 'bash-output-mode)
