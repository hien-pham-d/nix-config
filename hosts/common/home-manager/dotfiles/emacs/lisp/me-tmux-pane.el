(defun me/move-window-or-tmux-pane (direction tmux-flag)
  "Move to Emacs window in DIRECTION, or switch to tmux pane using TMUX-FLAG if at edge."
  (let ((current-window (selected-window))
        (windmove-func (intern (format "windmove-%s" direction))))
    (condition-case nil
        (progn
          (funcall windmove-func)
          ;; Check if we actually moved to a different window
          (when (eq current-window (selected-window))
            (unless (display-graphic-p)
              ;; We didn't move, so we're at the edge window
              (call-process-shell-command (format "tmux select-pane %s" tmux-flag)))))
      ;; windmove throws error when no window in that direction
      (error
       (unless (display-graphic-p)
         (call-process-shell-command (format "tmux select-pane %s" tmux-flag)))))))

(defun me/move-right-or-tmux-pane ()
  "Move to right Emacs window, or switch to right tmux pane if at rightmost window."
  (interactive)
  (me/move-window-or-tmux-pane 'right "-R"))

(defun me/move-left-or-tmux-pane ()
  "Move to left Emacs window, or switch to left tmux pane if at leftmost window."
  (interactive)
  (me/move-window-or-tmux-pane 'left "-L"))

(defun me/move-down-or-tmux-pane ()
  "Move to window below, or switch to tmux pane below if at bottom window."
  (interactive)
  (me/move-window-or-tmux-pane 'down "-D"))

(defun me/move-up-or-tmux-pane ()
  "Move to window above, or switch to tmux pane above if at top window."
  (interactive)
  (me/move-window-or-tmux-pane 'up "-U"))

(me/keymap-set-with-desc global-map "C-h" #'me/move-left-or-tmux-pane "windmove-left")
(me/keymap-set-with-desc global-map "C-j" #'me/move-down-or-tmux-pane "windmove-down")
(me/keymap-set-with-desc global-map "C-k" #'me/move-up-or-tmux-pane "windmove-up")
(me/keymap-set-with-desc global-map "C-l" #'me/move-right-or-tmux-pane "windmove-right")

;; lisp-interaction-mode
(add-hook 'lisp-interaction-mode-hook
          (lambda()
            (keymap-set lisp-interaction-mode-map "C-j" #'me/move-down-or-tmux-pane)
            ))

(provide 'me-tmux-pane)
