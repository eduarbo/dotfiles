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

;; HACK: prevent the cursor from ending outside the code block
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
  "Copy the current buffer's name (e.g. *scratch* or filename.ext) to the kill ring."
  (interactive)
  (let ((name (buffer-name)))
    (kill-new name)
    (message "Copied buffer name: %s" name)))

;;;###autoload
(defun my/yank-file-name ()
  "Copy the current file's name (with extension) to the kill ring."
  (interactive)
  (if buffer-file-name
      (let ((fname (file-name-nondirectory buffer-file-name)))
        (kill-new fname)
        (message "Copied file name: %s" fname))
    (message "Not visiting a file!")))

;;;###autoload
(defun my/yank-file-name-no-ext ()
  "Copy the current file's name (without extension) to the kill ring."
  (interactive)
  (if buffer-file-name
      (let* ((fname (file-name-nondirectory buffer-file-name))
             (noext (file-name-sans-extension fname)))
        (kill-new noext)
        (message "Copied file name (no ext): %s" noext))
    (message "Not visiting a file!")))

;;;###autoload
(defun my/yank-directory-path ()
  "Copy the directory of the current buffer file to kill ring."
  (interactive)
  (if buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (kill-new dir)
        (message "Copied directory: %s" dir))
    (message "Not visiting a file!")))

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

;;;###autoload
(defun my/evil-inser-mode-paste ()
  "Paste text relative to the cursor's line position: after if at end, before with cursor adjustment otherwise.
Uses `evil-paste-after` if the cursor is at the end of the line. If not, uses `evil-paste-before` and moves the cursor forward to the end of the pasted text."
  (interactive)
  (if (eolp)  ; Check if the cursor is at the end of the line
      (evil-paste-after 1)
    (progn
      (evil-paste-before 1)
      (evil-forward-char))))

;;;###autoload
(defun my/comment-box (beg end)
  "Toggle a comment box for headers with a fixed width of 80 characters.
Uses `comment-start` and `comment-end` dynamically to determine the comment syntax.
If the selected text is already formatted as a comment box, remove it.
Otherwise, create the formatted comment box.

Formats:
  - Single line: `<comment-char> ─── Title ───────────────────────────────────`
  - Ensures exactly 80 characters.
  - Works in Normal, Visual, and Insert modes.
  - Preserves newlines when needed.
  - In `emacs-lisp-mode`, it uses `;;` instead of `;` to avoid auto-indentation."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)) ;; If a selection exists, use it
     (list (line-beginning-position) (line-end-position)))) ;; Otherwise, use the whole line

  (let* ((text (buffer-substring-no-properties beg end)) ;; Capture the selected text
         (has-newline (string-suffix-p "\n" text)) ;; Check if the selection includes a newline
         (comment-start (or comment-start "")) ;; Get the comment prefix
         (comment-end (or comment-end "")) ;; Get the comment suffix (e.g., `*/` in C-style)
         ;; Special case for Emacs Lisp: Use `;;` instead of `;` to avoid indentation issues
         (comment-prefix (if (eq major-mode 'emacs-lisp-mode)
                             ";;"
                           (string-trim comment-start)))
         (comment-suffix (string-trim comment-end))
         (comment-len (+ (length comment-prefix) (length comment-suffix)))
         (regex (format "^%s ─── \\(.*?\\) ─+%s$"
                        (regexp-quote comment-prefix)
                        (if (string-empty-p comment-suffix) "" (concat " " (regexp-quote comment-suffix))))) ;; Regex for detecting an existing box
         (already-boxed (string-match regex text)))

    (delete-region beg end) ;; Remove the original text

    (if already-boxed
        ;; If already formatted, remove the comment-box and restore the title
        (insert (match-string 1 text))
      ;; Otherwise, apply the comment-box format
      (let* ((title (string-trim text)) ;; Remove extra spaces
             (prefix (concat comment-prefix " ─── " title " ")) ;; Prefix with comment syntax
             (fill-length (- 81 (length prefix) (length comment-suffix))) ;; Ensure 80-char width
             (fill (if (> fill-length 0) (make-string fill-length ?─) "")) ;; Fill with '─'
             (formatted-comment (concat prefix fill
                                        (if (string-empty-p comment-suffix) "" (concat " " comment-suffix))))) ;; Final formatted comment
        (insert formatted-comment)))

    ;; Restore newline if it was present
    (when has-newline (insert "\n"))))

;;;###autoload
(defun my/evilnc-comment-and-stay-in-insert ()
  "Comment/uncomment the current line while staying in insert mode."
  (interactive)
  (save-excursion
    (evilnc-comment-or-uncomment-lines 1))
  (evil-insert-state))

;;;###autoload
(defun my/lsp--eslint-before-save (orig-fun)
  "Run lsp-eslint-fix-all and then run the original lsp--before-save."
  (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))
  (funcall orig-fun))

;;;###autoload
(defun my/eslint-fix-all-maybe-and-format ()
  "Apply ESLint auto-fixes (if applicable) without prompting, then format the buffer/region.
Temporarily sets `lsp-auto-execute-action' to t so that `lsp-eslint-fix-all' executes
the code action without confirmation. Manual invocation outside this function will still prompt."
  (interactive)
  (when (and (derived-mode-p 'js-mode 'typescript-mode 'typescript-tsx-mode)
             (fboundp 'lsp-eslint-fix-all))
    (let ((lsp-auto-execute-action t))
      (lsp-eslint-fix-all)))
  (+format/region-or-buffer))

;;;###autoload
(defun my/dired-toggle-mark ()
  "Toggle mark on the current line in Dired/Dirvish without moving point."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (beginning-of-line)
      (if (eq (char-after) dired-marker-char)
          (dired-unmark 1)
        (dired-mark 1)))))

;;;###autoload
(defun my/posframe-poshandler-simple-smart-margins (info)
  "Smart posframe handler that places frame on side margins or bottom center.

Simple strategy:
- If either side has enough space for the full posframe width: use that side
- Otherwise: bottom center

The structure of INFO can be found in docstring of `posframe-show'."

  (let* ((window-left (plist-get info :parent-window-left))
         (window-width (plist-get info :parent-window-width))
         (frame-width (plist-get info :parent-frame-width))
         (frame-height (plist-get info :parent-frame-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (left-space window-left)
         (right-space (- frame-width window-left window-width))
         (center-y (/ (- frame-height posframe-height) 2))
         (center-x (/ (- frame-width posframe-width) 2))
         (margin 10))

    (cond
     ;; Left side has enough space
     ((>= left-space posframe-width)
      (cons (max margin (- left-space posframe-width margin)) center-y))

     ;; Right side has enough space
     ((>= right-space posframe-width)
      (cons (+ window-left window-width margin) center-y))

     ;; Neither side has enough space: bottom center
     (t
      (cons center-x (- frame-height posframe-height margin))))))

;;;###autoload
(defun my/vertico-posframe-get-size-with-max (buffer)
  "Calculate posframe size with maximum width constraint.

BUFFER is the vertico minibuffer.

Limits width to max 80 characters (or 62% of frame width if smaller),
enabling proper side margin placement.

Returns plist with :height, :width, :min-height, and :min-width."
  (let* ((frame-width (frame-width))
         ;; Maximum width in characters (adjust as needed)
         (max-width 80)
         ;; 62% of frame (vertico default)
         (preferred-width (round (* frame-width 0.62)))
         ;; Use smaller of the two
         (actual-width (min preferred-width max-width))
         ;; Original height logic
         (height (buffer-local-value 'vertico-posframe-height buffer))
         (min-height (or (buffer-local-value 'vertico-posframe-min-height buffer)
                         (let ((h (+ vertico-count 1)))
                           (min h (or height h))))))
    (list
     :height height
     :width (buffer-local-value 'vertico-posframe-width buffer)
     :min-height min-height
     :min-width (or (buffer-local-value 'vertico-posframe-min-width buffer)
                    actual-width))))

;;; PHP PHPCS Integration with LSP for Doom Emacs
;;; Enables PHPCS to work alongside LSP in both php-mode and web-mode

;;;###autoload
(defun my/use-project-phpcs-executable ()
  "Configure Flycheck to use project-local PHPCS executable if available.
Looks for phpcs in the project's vendor/bin directory and sets it as
the local executable for the current buffer."
  (let* ((root (or (projectile-project-root) default-directory))
         (phpcs (expand-file-name "vendor/bin/phpcs" root)))
    (when (file-exists-p phpcs)
      (setq-local flycheck-php-phpcs-executable phpcs))))

;;;###autoload
(defun my/web-mode-is-php-p ()
  "Check if the current web-mode buffer is a PHP file.
Returns t if the buffer is in web-mode and has a PHP extension
or web-mode-engine is set to 'php'."
  (and (eq major-mode 'web-mode)
       (or (string-match-p "\\.php\\'" (or buffer-file-name ""))
           (string-match-p "\\.phtml\\'" (or buffer-file-name ""))
           (equal web-mode-engine "php"))))

;;;###autoload
(defun my/chain-flycheck-after-lsp (checker)
  "Chain CHECKER to run after LSP when both are active.
Adds CHECKER to the list of available checkers and chains it to run
after LSP diagnostics regardless of LSP results."
  (when (and (bound-and-true-p lsp-mode)
             (bound-and-true-p flycheck-mode)
             checker)
    ;; Add checker to the list of available checkers
    (unless (memq checker flycheck-checkers)
      (setq-local flycheck-checkers
                  (append flycheck-checkers (list checker))))
    ;; Chain checker to run after lsp
    ;; The (t . checker) syntax means run checker regardless of lsp result
    (flycheck-add-next-checker 'lsp `(t . ,checker))))

;; ============================================================================
;; Language-specific Setup Functions
;; ============================================================================

;;;###autoload
(defun my/setup-php-flycheck-chain ()
  "Setup Flycheck chain to run PHPCS after LSP for PHP files.
Works in both php-mode and web-mode. Configures the project-local
PHPCS executable and chains it to run after LSP diagnostics."
  (when (or (derived-mode-p 'php-mode)
            (my/web-mode-is-php-p))
    ;; Configure project-local PHPCS executable
    (my/use-project-phpcs-executable)
    ;; Chain PHPCS to run after LSP
    (my/chain-flycheck-after-lsp 'php-phpcs)))

;;;###autoload
(defun my/setup-stylelint-flycheck-chain ()
  "Setup Flycheck chain to run Stylelint after LSP for CSS-like files.
Works with css-mode, scss-mode, less-mode, and web-mode (for CSS).
Chains the appropriate stylelint checker to run after LSP diagnostics."
  (let ((stylelint-checker
         (cond
          ((derived-mode-p 'scss-mode) 'scss-stylelint)
          ((derived-mode-p 'less-mode) 'less-stylelint)
          ((derived-mode-p 'css-mode) 'css-stylelint)
          ;; For web-mode, detect the content type
          ((and (eq major-mode 'web-mode)
                (member web-mode-content-type '("css" "scss" "less")))
           (pcase web-mode-content-type
             ("scss" 'scss-stylelint)
             ("less" 'less-stylelint)
             (_ 'css-stylelint)))
          (t nil))))
    ;; Chain the appropriate stylelint checker
    (my/chain-flycheck-after-lsp stylelint-checker)))
