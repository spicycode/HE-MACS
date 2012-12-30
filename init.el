;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
(setenv "PATH" (concat "~/.bin:/opt/github/homebrew/bin:/usr/local/bin:" (getenv "PATH")))
(setenv "NODE_PATH" "/usr/local/lib/node")

(if (eq system-type 'darwin)
    (push "/opt/github/homebrew/bin/" exec-path)
    (push "/usr/local/bin" exec-path))

(setq my-emacs-init-file
      (or load-file-name buffer-file-name))

(setq my-emacs-config-dir
      (file-name-directory my-emacs-init-file))

(setq user-emacs-directory my-emacs-config-dir)

(setq my-elisp-dir
      (expand-file-name "elisp" my-emacs-config-dir))

(setq my-elpa-dir
      (expand-file-name "elpa" my-emacs-config-dir))

(setq my-elisp-external-dir
      (expand-file-name "external" my-elisp-dir))

(setq my-init-dir
      (expand-file-name "init.d" my-emacs-config-dir))

;; Add my elisp directory to load path
(add-to-list 'load-path my-elisp-dir)

; Add external projects to load path
(dolist (project (directory-files my-elisp-external-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load all elisp files in ./init.d
(if (file-exists-p my-init-dir)
    (dolist (file (directory-files my-init-dir t "\\.el$"))
      (load file)))
