(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

(set-frame-font "Menlo-14")

(require 'color-theme)
(load  "~/.emacs.d/vendor/color-theme-github.el")
(color-theme-github)

(setq vc-handled-backends nil)

;; SavePlace - remember where I was when I last edited a file
(require 'saveplace)
(setq-default save-place t)

; no startup message please
(setq inhibit-startup-message t)

; no scratch message please
(setq initial-scratch-message nil)

; do not mess with user's mouse!
(setq focus-follows-mouse nil)

(setq-default truncate-lines t)

;; Disable backup files
(setq make-backup-files nil)

(tool-bar-mode -1)

(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)

(require 'linum)
(global-linum-mode 1)
(setq linum-format "%3d  ")

(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	     (turn-on-eldoc-mode)))

(require 'dired)
;; - is `cd ..` (like vim)
(define-key dired-mode-map "-" 'dired-up-directory)

;; all modes
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq show-trailing-whitespace (not buffer-read-only))

(scroll-bar-mode -1)

(require 'textmate)
(textmate-mode)

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

(icomplete-mode t)                       ;; completion in minibuffer
(setq
  icomplete-prospects-height 1           ;; don't spam my minibuffer
  icomplete-compute-delay 0)             ;; don't wait

(setq teach-extended-commands-p t)
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'yes-or-no-p 'y-or-n-p)

;; AutoComplete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

(require 'markdown-mode)
