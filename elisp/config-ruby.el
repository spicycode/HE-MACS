;; TODO: Require defer this stuff unless you are in ruby files
(require 'rvm)
(rvm-use-default)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Procfile" . ruby-mode))

(setq rinari-major-modes '(ruby-mode-hook))
