;; Keyboard bindings
;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-/") 'hippie-expand)

;; MAC specific
(define-key osx-key-mode-map (kbd "A-o") 'ido-find-file)
(define-key osx-key-mode-map (kbd "A-b") 'ido-switch-buffer)
(define-key osx-key-mode-map (kbd "A-w") 'ido-kill-buffer)
(define-key osx-key-mode-map (kbd "A-/") 'comment-or-uncomment-region-or-line)

;; SMEX:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Backward kill like bash
(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)