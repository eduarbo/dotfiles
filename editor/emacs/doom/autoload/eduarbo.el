;;; ~/.dotfiles/editor/emacs/doom/autoload/eduarbo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eduarbo/find-notes-for-major-mode (&optional arg)
  "TODO"
  (interactive "P")
  (let ((default-directory (expand-file-name "code/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat (string-remove-suffix "-mode" (symbol-name major-mode)) ".org"))))))

;;;###autoload
(defun +eduarbo/find-notes-for-project (&optional arg)
  "TODO"
  (interactive "P")
  (let ((project-root (doom-project-name))
        (default-directory (expand-file-name "projects/" org-directory)))
    (if arg
        (call-interactively #'find-file)
      (find-file
       (expand-file-name (concat project-root ".org"))))))

;;;###autoload
(defun +eduarbo/switch-to-last-workspace ()
  "Switch to previously selected workspace, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash +workspace--last
                       *persp-hash* 'non-existent))
    (+workspace/switch-to +workspace--last)))

;;;###autoload
(defun +eduarbo/find-file (dir)
  "Open a file somewhere in DIR via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name dir)))

;;;###autoload
(defun +eduarbo/search-project (dir)
  "Perform a text search on DIR."
  (interactive)
  (require 'org)
  (let ((default-directory dir))
    (+default/search-project-for-symbol-at-point nil "")))
