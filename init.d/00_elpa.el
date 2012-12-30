(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(anything anything-config anything-match-plugin clojurescript-mode clojure-mode
			       coffee-mode color-theme color-theme-twilight
                               haml-mode markdown-mode auto-complete
                               ido-ubiquitous
                               ctags-update
			       paredit rainbow-delimiters sass-mode scss-mode starter-kit-ruby
                               starter-kit-js starter-kit-eshell starter-kit-lisp smex
                               yaml-mode haskell-mode gist textmate)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
