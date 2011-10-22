(setq my-emacs-init-file
      (or load-file-name buffer-file-name))

(setq my-emacs-config-dir
      (file-name-directory my-emacs-init-file))

(setq user-emacs-directory my-emacs-config-dir)

(setq my-elisp-dir
      (expand-file-name "elisp" my-emacs-config-dir))

(setq my-elisp-external-dir
      (expand-file-name "external" my-elisp-dir))

(setq my-init-dir
      (expand-file-name "init.d" my-emacs-config-dir))

;; Load all elisp files in ./init.d
(if (file-exists-p my-init-dir)
    (dolist (file (directory-files my-init-dir t "\\.el$"))
      (load file)))

