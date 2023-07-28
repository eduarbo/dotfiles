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
(defun my/embrace-js-mode-h ()
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


;; -- minor mode to enlarge the active window

;;;###autoload
(defun my/yank-buffer-name ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (message "Copied buffer name to clipboard: %s"
           (kill-new (buffer-name))))

(defvar my--window-enlargen-active-p nil
  "Whether or not `my/window-enlargen-mode' is active.")

(defun my--enlarge-active-window (&rest _)
  "Enlarge the active window."
  (when (and my--window-enlargen-active-p
             (not (window-parameter (selected-window) 'popup)))
    (let* ((window (selected-window))
           (dedicated-p (window-dedicated-p window))
           (preserved-p (window-parameter window 'window-preserved-size))
           (ignore-window-parameters t)
           (window-resize-pixelwise nil)
           (frame-resize-pixelwise nil))
      (unwind-protect
          (progn
            (when dedicated-p
              (set-window-dedicated-p window nil))
            (when preserved-p
              (set-window-parameter window 'window-preserved-size nil))
            (maximize-window window))
        (set-window-dedicated-p window dedicated-p)
        (when preserved-p
          (set-window-parameter window 'window-preserved-size preserved-p))))))

;;;###autoload
(define-minor-mode my/window-enlargen-mode
  "A mode to automatically enlarge the active window."
  :init-value nil
  :global t
  (if my/window-enlargen-mode
      (progn
        (setq my--window-enlargen-active-p t)
        (add-hook 'window-selection-change-functions 'my--enlarge-active-window)
        (my--enlarge-active-window))
    (setq my--window-enlargen-active-p nil)
    (remove-hook 'window-selection-change-functions 'my--enlarge-active-window)
    (balance-windows)))

;;;###autoload
(defun my/embrace-emacs-lisp-mode-hook-advice (orig-fun &rest args)
  "Remove the backtick from `evil-embrace-evil-surround-keys' in `emacs-lisp-mode.'"
  (let ((evil-embrace-evil-surround-keys (delq ?` evil-embrace-evil-surround-keys)))
    (apply orig-fun args)))
