;;; ~/dev/dotfiles/editor/emacs/doom/autoload/eduarbo.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar eduarbo--enlargen-focused-window nil)

;;;###autoload
(defvar eduarbo-agenda-workspace-name "Agenda")


;;;###autoload
(defun +eduarbo--get-unicode-font-size (font-size)
  "Return the size of the unicode font that matches the width of the char
of the default font based on the given FONT-SIZE"
  (let ((base (- font-size 1)))
    (+ (/ (* (+ base (/ base 5)) 5) 6) 1)))

;;;###autoload
(defun +eduarbo/switch-to-last-workspace ()
  "Switch to previously selected workspace, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash +workspace--last
                       *persp-hash* 'non-existent))
    (+workspace/switch-to +workspace--last)))

;;;###autoload
(defun +eduarbo-find-file (dir)
  "Open a file somewhere in DIR via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name dir)))

;;;###autoload
(defun +eduarbo-search-project (dir)
  "Perform a text search on DIR."
  (interactive)
  (require 'org)
  (let ((default-directory dir))
    (+default/search-project-for-symbol-at-point nil "")))

;;;###autoload
(defun +eduarbo/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;;;###autoload
(defun +eduarbo/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows
intelligently.  Intelligently means: region, org-src-block,
org-subtree, or defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (let ((org-src-window-setup 'current-window))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
      ((and (boundp 'org-src-mode) org-src-mode (not p))
        (org-edit-src-exit))
      ((region-active-p)
        (narrow-to-region (region-beginning) (region-end))
        (deactivate-mark))
      ((derived-mode-p 'org-mode)
        (cond ((ignore-errors (org-edit-src-code)))
          ((org-at-block-p)
            (org-narrow-to-block))
          (t (org-narrow-to-subtree))))
      ((derived-mode-p 'prog-mode) (narrow-to-defun))
      (t (error "Please select a region to narrow to")))))


;; Keep focused window enlarged

;;;###autoload
(add-hook! 'doom-switch-window-hook
  (defun eduarbo-focused-window-enlargen-h ()
    (when eduarbo--enlargen-focused-window
      (doom/window-enlargen)
      (evil-window-mru)
      (doom/window-enlargen))))


;; Agenda Workspace

;;;###autoload
(defun eduarbo--switch-to-agenda-workspace (command)
  "Switch to the agenda workspace and dispatch agenda command"
  (+workspace-switch eduarbo-agenda-workspace-name)
  (org-agenda nil command))

;;;###autoload
(defun eduarbo--open-or-switch-to-agenda (command)
  "Switch to agenda workspace if exists or create a new one"
  (condition-case nil
    (eduarbo--switch-to-agenda-workspace command)
    (error
      (+workspace-new eduarbo-agenda-workspace-name)
      (eduarbo--switch-to-agenda-workspace command))))

;;;###autoload
(defun eduarbo/daily-agenda ()
  "Open the daily agenda in a dedicated workspace"
  (interactive)
  (eduarbo--open-or-switch-to-agenda "d"))

;;;###autoload
(defun eduarbo/unscheduled-agenda ()
  "Open the unscheduled agenda in a dedicated workspace"
  (interactive)
  (eduarbo--open-or-switch-to-agenda "u"))

;;;###autoload
(defun eduarbo/add-word-to-dictionary ()
  "Add new word to dictionary"
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

;;;###autoload
(defun eduarbo/configure-web-mode-flycheck-disable-checkers-based-on-engine ()
  "Enable javascript-eslint checker on web-mode but only for svelte files"
  (unless (string= web-mode-engine "svelte")
    (setq-local flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-eslint)))))
