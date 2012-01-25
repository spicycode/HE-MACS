;; Keyboard bindings
;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-/") 'hippie-expand)

;; MAC specific
(global-set-key [(super o)] 'ido-find-file)
(global-set-key [(super b)] 'ido-switch-buffer)
(global-set-key [(super w)] 'ido-kill-buffer)
(global-set-key [(super /)] 'comment-or-uncomment-region-or-line)

;; SMEX:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Backward kill like bash
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
