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

(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))
;; Ruby
;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(setq rinari-major-modes '(ruby-mode-hook))
;; Working around ruby starter kit bug https://github.com/technomancy/emacs-starter-kit/issues/99
(remove-hook 'ruby-mode-hook 'esk-run-coding-hook)

(require 'rspec-mode)

;; Markdown
;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))

(require 'flymake)
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
          (lambda () (flymake-mode t)))

;; Turns on flymake for all files which have a flymake mode
(add-hook 'find-file-hook 'flymake-find-file-hook)