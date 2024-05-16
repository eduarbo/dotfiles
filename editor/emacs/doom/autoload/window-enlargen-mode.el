;;; autoload/window-enlargen-mode.el -*- lexical-binding: t; -*-

(defvar my--word-wrap-states (make-hash-table :test 'equal)
  "Hash table to store word wrap states of buffers.")

(defvar my--window-enlargen-active-p nil
  "Whether or not `my/window-enlargen-mode' is active.")

(defun my--manage-word-wrap ()
  "Manage `+word-wrap-mode' for all buffers based on the active state."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (let ((current-state +word-wrap-mode))
        (puthash (current-buffer) current-state my--word-wrap-states)
        (unless (eq buf (window-buffer (selected-window)))
          (when +word-wrap-mode
            (+word-wrap-mode -1)))))))

(defun my--restore-word-wrap ()
  "Restore `+word-wrap-mode' for all buffers."
  (maphash (lambda (buf state)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (and state (not +word-wrap-mode))
                   (+word-wrap-mode 1)))))
           my--word-wrap-states))

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

(defun my--restore-word-wrap-for-buffer ()
  "Restore `+word-wrap-mode' for the current buffer."
  (let ((state (gethash (current-buffer) my--word-wrap-states)))
    (when (and state (not +word-wrap-mode))
      (+word-wrap-mode 1))))

;;;###autoload
(define-minor-mode my/window-enlargen-mode
  "A mode to automatically enlarge the active window."
  :init-value nil
  :global t
  (if my/window-enlargen-mode
      (progn
        (setq my--window-enlargen-active-p t)
        (add-hook 'window-selection-change-functions 'my--enlarge-active-window)
        (add-hook 'window-selection-change-functions 'my--restore-word-wrap-for-buffer)
        (my--enlarge-active-window)
        (my--manage-word-wrap))
    (setq my--window-enlargen-active-p nil)
    (remove-hook 'window-selection-change-functions 'my--enlarge-active-window)
    (remove-hook 'window-selection-change-functions 'my--restore-word-wrap-for-buffer)
    (my--restore-word-wrap)
    (balance-windows)))
