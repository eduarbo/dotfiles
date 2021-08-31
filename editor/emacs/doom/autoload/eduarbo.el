;;; editor/emacs/doom/autoload/eduarbo.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/find-file-in-dotfiles ()
  "Search for a file in `dotfiles-dir'."
  (interactive)
  (doom-project-find-file dotfiles-dir))

;;;###autoload
(defun my/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;;;###autoload
(defun my/narrow-or-widen-dwim (p)
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

;;;###autoload
(defun my-get-buffer-path-relative-to-project ()
  "Get the current buffer's path relative to project."
  (if-let ((root (doom-project-root))
           (filename (or (buffer-file-name (buffer-base-buffer))
                         (bound-and-true-p list-buffers-directory)
                         (+file-templates-get-short-path))))
      (abbreviate-file-name
       (if root
           (file-relative-name filename root)
         filename))
    (+file-templates-get-short-path)))

;;;###autoload
(defun my/toggle-doom-modeline-buffer-file-name-style ()
  "Toggle the style used by doom-modeline-buffer-file-name"
  (interactive)
  (let* ((current-style doom-modeline-buffer-file-name-style)
         (styles '(relative-from-project truncate-with-project))
         (order (cons current-style (remq current-style styles)))
         (next (car (cdr order))))
    (setq doom-modeline-buffer-file-name-style next)
    (message "Switched to %s file name style" (symbol-name next))))

;;;###autoload
(defun my--sort-git-remotes-a (remotes)
  "Move upstream and origin to the top of the list"
  (let ((upstream "upstream")
        (origin "origin"))
    (when (member upstream remotes) (push upstream remotes))
    (when (member origin remotes) (push origin remotes))
    (delete-dups remotes)))

;;;###autoload
(defun my/cycle-ispell-languages ()
  "Switch to the next Ispell dictionary in ‘lang-ring’."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

;;;###autoload
(defun my/yank-buffer-name ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (message "Copied buffer name to clipboard: %s"
           (kill-new (buffer-name))))
