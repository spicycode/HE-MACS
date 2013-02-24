;; Functions
;;;;;;;;;;;;;;;;;;;;

;; Backward kill like bash
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun package-update-all ()
  "Update all packages"
  (interactive)
  (dolist (elt package-alist)
    (let* ((name (car elt))
           (file-name (symbol-name name))
           (available-pkg (assq name package-archive-contents))
           (available-version (and available-pkg
                                   (package-desc-vers (cdr available-pkg))))
           (current-version (package-desc-vers (cdr elt)))
           )
      (when (and available-version
                 (version-list-< current-version available-version))
        (message "Updating to: %s - %s" file-name
                 (package-version-join available-version))
        (package-install name)
        (package-delete file-name (package-version-join current-version))))))

(defun inf-ruby-keys () )
