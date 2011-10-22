;; Global settings
;;;;;;;;;;;;;;;;;;;;

;; Debug on error
(setq debug-on-error t)

;; Do not confirm killing emacs
(setq confirm-kill-emacs nil)

;; Custom file
(setq custom-file "~/.emacs.d/elisp/custom.el")
(load custom-file 'noerror)

;; Auto-revert any buffers if file on disk changes
(global-auto-revert-mode t)

;; Hide startup messages/areas
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; no scratch message please
(setq initial-scratch-message nil)

;; Hide toolbars/menubars/scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Don't go opening new frames
(if (featurep 'aquamacs)
    (one-buffer-one-frame-mode 0))

;; no beeping
(setq visible-bell t)

;; Visually show region
(setq transient-mark-mode t)

;; Line numbers
(setq line-number-mode t)

;; Don't make backup files
(setq make-backup-files nil)

;; Disable VCS backends
(setq vc-handled-backends nil)

;; Blink the cursor
(blink-cursor-mode t)

;; Show trailing whitespace
(setq show-trailing-whitespace (not buffer-read-only))

;; do not confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; Teach me commands
(setq teach-extended-commands-p t)

;; ALIASES
;;;;;;;;;;;;;;;;;;;;
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'yes-or-no-p 'y-or-n-p)

;; FONT
;;;;;;;;;;;;;;;;;;;;
(set-frame-font "Menlo-14")

;; Use fonts everywhere
(setq global-font-lock-mode 1)


;; UTF-8
;;;;;;;;;;;;;;;;;;;;

;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-everywhere t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; uniquify changes conflicting buffer names from file<2> etc
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; SMEX
(require 'smex)
(smex-initialize)

;; Remember where I was
(require 'saveplace)
(setq-default save-place t)

;; Line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%3d ")

;; Dired
(require 'dired)
;; - is `cd ..` (like vim)
(define-key dired-mode-map "-" 'dired-up-directory)

;; AutoComplete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)

;; Color Theme
(require 'color-theme)
(setq color-theme-is-global t)

(setq color-theme-twilight-path
      (expand-file-name "color-theme-twilight-0.1/color-theme-twilight.el" my-elpa-dir))

(setq color-theme-github-path
      (expand-file-name "color-theme-github.el" my-elisp-dir))


(load-file color-theme-github-path)

(color-theme-github)


