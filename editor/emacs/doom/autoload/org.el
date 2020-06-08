;;; ~/dev/dotfiles/editor/emacs/doom/autoload/org.el -*- lexical-binding: t; -*-

;; Remove empty LOGBOOK drawers on clock out, from Michael Englehorn's Emacs
;; Configuration
;; https://michael.englehorn.com/config.html
;;
;; This Stack Overflow post shows the fix to the bug in the original function
;; (remove the "LOGBOOK" specification)
;; https://stackoverflow.com/questions/21767471/org-capture-and-time-clocking-misbehaving#21797427
;;;###autoload
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

;; Exclude completed tasks from refile targets, from Michael Englehorn's Emacs
;; Configuration
;; https://michael.englehorn.com/config.html
;;;###autoload
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;;;###autoload
(defun +org-clock-heading-reversed-outline-path ()
  "Return the reversed outline path of the currenty entry"
  (org-format-outline-path
    (reverse (org-get-outline-path t)) nil nil +org-clock-heading-outline-path-separator))

;;;###autoload
(defun +org-get-agenda-project-heading ()
  "Return the parent heading of the current entry decorated with a prefix and suffix"
  (let* ((parent (org-format-outline-path (cdr (org-get-outline-path)) 20)))
    (if (string= parent "")
      ""
      (concat +org-agenda-project-heading-prefix parent +org-agenda-project-heading-suffix))))

;;;###autoload
(defun +org-org-capture-project-file ()
  "Get the path for the project org file"
  (if (doom-project-root)
    (let ((filename (doom-project-name)))
      (expand-file-name (concat filename ".org") +org-default-projects-dir))
    (user-error "Couldn't detect a project")))

;;;###autoload
(defun +org/find-notes-for-project ()
  "Open project org file."
  (interactive)
  (find-file (+org-org-capture-project-file)))

;;;###autoload
(defun +org/emphasize-dwim (&optional char)
  (interactive)
  (unless (region-active-p)
    (forward-word)
    (backward-word)
    (mark-word))
  (org-emphasize char))

;;;###autoload
(defmacro org-emphasize! (fname char)
  "Make function for setting the emphasis in org mode"
  `(defun ,fname () (interactive)
     (+org/emphasize-dwim ,char)))
