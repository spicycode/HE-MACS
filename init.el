(setenv "PATH"
        (concat
         (expand-file-name "~/.bin") ";"
         "/usr/local/bin" ";"
         (getenv "PATH")))

;; Get user local bin on the path first
(push (expand-file-name "~/.bin") exec-path)
(push "/usr/local/bin" exec-path)

(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file 'noerror)

; Setting up the load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(when
    (load
     (expand-file-name "~/.emacs.d/vendor/package.el"))
  (package-initialize))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'load-path (concat dotfiles-dir "/vendor/"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/full-ack/"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari/"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/coffee-mode/"))
(add-to-list 'load-path (concat dotfiles-dir "/elisp/"))

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; MAC KEY MODE
(require 'redo+)
(require 'mac-key-mode)
(mac-key-mode 1)
(add-hook 'mac-key-mode-hook
	  (lambda()
	    (interactive)
	    (if mac-key-mode
		(setq mac-option-modifier 'meta)
	      (setq mac-option-modifier nil))))
(setq mac-key-mode-lighter "mac")

(define-key mac-key-mode-map [(alt l)] 'goto-line)

(define-key mac-key-mode-map [(alt o)] 'ido-find-file)

(define-key mac-key-mode-map [(alt b)] 'ido-switch-buffer)

(define-key mac-key-mode-map [(alt /)] 'comment-or-uncomment-region-or-line)

(define-key mac-key-mode-map [(alt shift t)] 'textmate-goto-symbol)
(define-key mac-key-mode-map [(alt t)] 'textmate-goto-file)

(require 'smex)
(smex-initialize)
;; SMEX:
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "M-/") 'hippie-expand)

(load "config-functions")
(load "config-global")
(load "config-ruby")
(load "config-lisp")
(load "config-coffee-script")
