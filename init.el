(require 'package)
;; Elpa/Marmalade
;;;;;;;;;;;;;;;;;;;;

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
             (package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(anything anything-config anything-match-plugin clojurescript-mode
			       coffee-mode color-theme color-theme-sanityinc-solarized haml-mode
                               markdown-mode auto-complete
			       paredit rainbow-delimiters sass-mode scss-mode starter-kit-ruby
                               starter-kit-js starter-kit-eshell starter-kit-lisp smex)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; Functions
;;;;;;;;;;;;;;;;;;;;

;; Backward kill like bash
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

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

;; MODES
;;;;;;;;;;;;;;;;;;;;

;; Global hooks
;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Coffee-Script
;;;;;;;;;;;;;;;;;;;;

(require 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"

  ;; CoffeeScript uses two spaces.
  (set (make-local-variable 'tab-width) 2)

  ;; If you don't have js2-mode
  (setq coffee-js-mode 'javascript-mode)

  ;; *Messages* spam
  (setq coffee-debug-mode t)

  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

;; Lisp/Clojure
;;;;;;;;;;;;;;;;;;;;
(require 'rainbow-delimiters)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook '(lambda ()
                                (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Ruby
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(setq rinari-major-modes '(ruby-mode-hook))

;; Markdown
;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))