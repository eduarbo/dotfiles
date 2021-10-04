;;; editor/emacs/doom/autoload/org.el -*- lexical-binding: t; -*-

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

;; Exclude task from refile targets that match any keyword from the list
;; +org-refile-exclude-keywords
;;;###autoload
(defun +org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (unless (member (nth 2 (org-heading-components)) +org-refile-exclude-keywords) t))

;;;###autoload
(defun +org-clock-heading-reversed-outline-path ()
  "Return the reversed outline path of the currenty entry"
  (org-format-outline-path
   (reverse (org-get-outline-path t)) nil nil +org-clock-heading-outline-path-separator))

;;;###autoload
(defun +org-get-project-heading-or-file-title ()
  "Return the parent heading of the current entry if it's a project task, otherwise return the file title"
  (require 'org-roam)
  (let* ((column-width 20)
         ;; FIXME figure out a way to determine if parent heading is a Project
         ;; (is-project (string= (org-element-property :todo-keyword (org-element-property :parent (org-element-at-point))) "PROJ"))
         (title (cadr (assoc "TITLE" (org-collect-keywords '("title")) #'string-equal)))
         (title-formatted (org-format-outline-path
                           (list title) column-width "■" " ")) ; □
         (parent (car (last (org-get-outline-path))))
         (parent-formatted (org-format-outline-path
                            (list parent) column-width "" " ")))
    ;; (if (and is-project
    ;;          (> (length parent) 0))
    ;;     parent-formatted
    ;;   title-formatted)

    (if (> (length parent) 0)
        parent-formatted
      title-formatted)))

;;;###autoload
(defun +org--capture-project-file ()
  "Get the path for the project org file"
  (if (doom-project-root)
      (let ((filename (doom-project-name)))
        (expand-file-name (concat filename ".org") +org-default-projects-dir))
    (user-error "Couldn't detect a project")))

;;;###autoload
(defun +org/find-notes-for-project ()
  "Open project org file."
  (interactive)
  (find-file (+org--capture-project-file)))

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

;;;###autoload
(defun +org/org-agenda-headlines ()
  "Jump to an Org headline in `org-agenda-files'."
  (interactive)
  (doom-completing-read-org-headings
   "Jump to org headline: " org-agenda-files +org-agenda-search-headlines-depth t))

;;;###autoload
(defun +org/org-notes-headlines ()
  "Jump to an Org headline in `+org-default-notes-dir'."
  (interactive)
  (doom-completing-read-org-headings
   "Jump to org headline: " +org-default-notes-dir +org-notes-search-headlines-depth t))

;;;###autoload
(defun +org/org-notes-search ()
  "Perform a text search on `+org-default-notes-dir'."
  (interactive)
  (require 'org)
  (let ((default-directory +org-default-notes-dir))
    (+default/search-project-for-symbol-at-point "")))

;;;###autoload
(defun +org/find-in-notes ()
  "Find a file under `+org-default-notes-dir', recursively."
  (interactive)
  (unless (bound-and-true-p +org-default-notes-dir)
    (require 'org))
  (doom-project-find-file +org-default-notes-dir))

;;;###autoload
(defun +org/toggle-emphasis (&optional arg)
  "Toggle hiding/showing of org emphasize markers."
  (interactive "p")
  (let ((markers org-hide-emphasis-markers))
    (if markers
        (setq-local org-hide-emphasis-markers nil)
      (setq-local org-hide-emphasis-markers t))
    (when arg
      (font-lock-fontify-buffer))))
