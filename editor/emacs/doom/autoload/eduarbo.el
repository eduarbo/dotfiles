;;; autoload/eduarbo.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar my/original-major-mode nil
  "Variable to store the original major mode before narrowing.")

;;;###autoload
(defvar my/markdown-lang-to-mode-alist
  '(("js" . js2-mode)
    ;; Add more mappings here
    )
  "Alist mapping markdown code block languages to their Emacs major modes.")

;; HACK prevent the cursor from ending outside the code block
;;;###autoload
(defun my/markdown-forward-block (&optional arg)
  "Move forward to the next end of a Markdown block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (markdown-backward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving forward.
      (if (markdown-cur-line-blank-p)
          (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((markdown-code-block-at-point-p)
        (forward-line)
        (while (and (markdown-code-block-at-point-p) (not (eobp)))
          (forward-line)))
       ;; Headings
       ((looking-at markdown-regex-header)
        (goto-char (or (match-end 4) (match-end 2) (match-end 3)))
        (forward-line))
       ;; Horizontal rules
       ((looking-at markdown-regex-hr)
        (forward-line))
       ;; Blockquotes
       ((looking-at markdown-regex-blockquote)
        (forward-line)
        (while (and (looking-at markdown-regex-blockquote) (not (eobp)))
          (forward-line)))
       ;; List items
       ((markdown-cur-list-item-bounds)
        (markdown-end-of-list)
        (forward-line))
       ;; Other
       (t (markdown-forward-paragraph))))
    (skip-syntax-backward "-")
    nil))

;;;###autoload
(defun my/markdown-narrow-to-code-block-content ()
  "Narrow the buffer to the content of the current Markdown fenced code
 block, excluding the delimiters."
  (interactive)
  (save-excursion
    (let (block-start block-end)
      ;; Find the start of the code block
      (markdown-backward-block)
      (when (markdown-code-block-at-point-p)
        ;; Move forward to skip the start delimiter of the block
        (forward-line 1)
        (setq block-start (point)))
      ;; Find the end of the code block
      (my/markdown-forward-block)
      (when (markdown-code-block-at-point-p)
        ;; Move backward to skip the end delimiter of the block
        (forward-line -1)
        (setq block-end (point)))
      ;; Narrow to the region if valid block start and end points are found
      (print block-end)
      (if (and block-start block-end)
          (narrow-to-region block-start block-end)
        (error "Not inside a Markdown fenced code block")))))

;;;###autoload
(defun my/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows
intelligently. Intelligently means: region, org-src-block,
org-subtree, markdown block, or defun, whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (let ((org-src-window-setup 'current-window))
    (cond ((and (buffer-narrowed-p) (not p))
           (widen)
           (when my/original-major-mode
             (funcall my/original-major-mode)
             (setq my/original-major-mode nil)))
          ((and (boundp 'org-src-mode) org-src-mode (not p))
           (org-edit-src-exit))
          ((region-active-p)
           (setq my/original-major-mode major-mode)
           (narrow-to-region (region-beginning) (region-end))
           (deactivate-mark))
          ((derived-mode-p 'org-mode)
           (setq my/original-major-mode major-mode)
           (cond ((ignore-errors (org-edit-src-code)))
                 ((org-at-block-p)
                  (org-narrow-to-block))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'markdown-mode)
           (let ((lang (markdown-code-block-lang)))
             (setq my/original-major-mode major-mode)
             (my/markdown-narrow-to-code-block-content)
             (let ((mode-fn (cdr (assoc lang my/markdown-lang-to-mode-alist))))
               (if (and mode-fn (functionp mode-fn))
                   (funcall mode-fn)
                 (when (and lang (functionp (intern-soft (concat lang "-mode"))))
                   (funcall (intern (concat lang "-mode"))))))))
          ((derived-mode-p 'prog-mode)
           (setq my/original-major-mode major-mode)
           (narrow-to-defun))
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

;;;###autoload
(defun my/yank-buffer-name ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (message "Copied buffer name to clipboard: %s"
           (kill-new (buffer-name))))

;;;###autoload
(defun my/embrace-emacs-lisp-mode-hook-advice (orig-fun &rest args)
  "Remove the backtick from `evil-embrace-evil-surround-keys' in `emacs-lisp-mode.'"
  (let ((evil-embrace-evil-surround-keys (delq ?` evil-embrace-evil-surround-keys)))
    (apply orig-fun args)))


;; -- Markdown: shows markup when editing

(defvar nb/current-line '(0 . 0)
  "(start . end) of current line in current buffer")
(make-variable-buffer-local 'nb/current-line)

;;;###autoload
(defun nb/unhide-current-line (limit)
  "Font-lock function"
  (let ((start (max (point) (car nb/current-line)))
        (end (min limit (cdr nb/current-line))))
    (when (< start end)
      (remove-text-properties start end
                              '(invisible t display "" composition ""))
      (goto-char limit)
      t)))

;;;###autoload
(defun nb/refontify-on-linemove ()
  "Post-command-hook"
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car nb/current-line)))))
    (setq nb/current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 3))))

;;;###autoload
(defun nb/markdown-unhighlight ()
  "Enable markdown concealling"
  (interactive)
  (markdown-toggle-markup-hiding 'toggle)
  (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
  (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

;;;###autoload
(defun my/magit-toggle-diff-buffers-or-quit ()
  "Closes Magit diff/revision buffers, or calls `+magit/quit` if none."
  (interactive)
  (let ((found-and-closed nil))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'magit-diff-mode)
                  (eq major-mode 'magit-revision-mode))
          (setq found-and-closed t)
          (kill-buffer buffer))))
    (unless found-and-closed
      (+magit/quit))))

;;;###autoload
(defun my/comment-divider ()
  "Insert a comment divider with dashes, 60 chars long."
  (interactive)
  (let* ((comment-start (or comment-start "# "))
         (comment-length (length comment-start))
         (divider-length (- 60 comment-length))
         (divider (concat comment-start (make-string divider-length ?-))))
    (save-excursion
      (move-end-of-line 1)
      (newline)
      (insert divider))))
