;;; autoload/eduarbo.el -*- lexical-binding: t; -*-

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
(defun my/evil-ex-start-word-search-advice (orig-fun &rest args)
  "Advise `evil-ex-start-word-search' to stay at the original position."
  (apply orig-fun args)
  (evil-ex-search-previous))

;;;###autoload
(defun my/evil-visualstar-begin-search-advice (orig-fun &rest args)
  "Advise `evil-visualstar/begin-search' to stay at the original position."
  (let ((orig-pos (point)))
    (apply orig-fun args)
    (goto-char orig-pos)))

;;;###autoload
(defun my/embrace-js-mode-hook-h ()
  (embrace-add-pair ?$ "${" "}"))

;;;###autoload
(defun my/consult-yank-pop-replace-region (orig-fun &rest args)
  "If the region is active and in visual state, delete it before yanking."
  (if (and (evil-visual-state-p)
           (region-active-p))
      (let ((beg (region-beginning))
            (end (region-end)))
        (delete-region beg end)
        (apply orig-fun args))
    (apply orig-fun args)))
