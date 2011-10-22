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