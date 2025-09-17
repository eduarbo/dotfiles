;;; ../../dev/eduarbo/dotfiles/editor/emacs/doom/+ui.el -*- lexical-binding: t; -*-

;; ─── Fonts ────────────────────────────────────────────────────────────────────

;; make sure that the font exists before using it
(let ((fn (doom-rpartial #'member (font-family-list))))
  (when-let (font (cl-find-if fn '("JetBrains Mono" "Hack")))
    ;; the primary font to use
    (setq doom-font (font-spec :family font :size 16 :weight 'light))
    (setq doom-big-font (font-spec :family font :size 20 :weight 'light)))

  ;; a monospace serif font (where applicable)
  (when-let (font (cl-find-if fn '("IBM Plex Mono" "Fira Code")))
    (setq doom-serif-font (font-spec :family font)))

  ;; a non-monospace font (where applicable)
  (when-let (font (cl-find-if fn '("IBM Plex Mono" "Fira Code")))
    (setq doom-variable-pitch-font (font-spec :family font :weight 'regular))))


;; ─── Theme ────────────────────────────────────────────────────────────────────

;; Workaround to ensure that nothing else gets in front of my `custom-theme-directory' after initialization. Doom's core
;; is supposed to handle this, but it isn't working correctly, so I've mimicked its approach within a hook

(add-hook! 'doom-init-ui-hook
  (defun my/prioritize-custom-theme-directory-h ()
    "Prioritize my custom them path over all other themes"
    (setq custom-theme-load-path
          (cons custom-theme-directory
                (delq 'custom-theme-directory custom-theme-load-path)))))

(setq doom-theme 'doom-oceanic-next)

;; HACK: Fix the output color in the REPL
;; https://github.com/emacs-ess/ESS/issues/1193#issuecomment-1144182009
(add-hook 'comint-mode-hook #'ansi-color-for-comint-mode-filter 'append)

(blink-cursor-mode 1)

(setq doom-themes-treemacs-theme "doom-colors")


;; ─── Modeline ─────────────────────────────────────────────────────────────────

(after! doom-modeline
  (setq
   ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
   ;; truncate-with-project => emacs/l/comint.el
   ;; relative-from-project => emacs/lisp/comint.el
   doom-modeline-buffer-file-name-style 'relative-from-project
   ;; doom-modeline-buffer-file-name-style 'truncate-with-project
   ;; Show buffer encoding
   doom-modeline-buffer-encoding t
   ;; Show modal state icon (`evil', `overwrite', `god', `ryo', `xah-fly-keys', etc)
   doom-modeline-modal-icon t
   ;; Hide buffer modification icon
   doom-modeline-buffer-modification-icon nil
   ;; Show `major-mode' icon
   doom-modeline-major-mode-icon t
   ;; Max length for VCS branch name
   doom-modeline-vcs-max-length 18
   ;; Show GitHub notifications (requires `ghub' package)
   doom-modeline-github t
   ;; only display one number for checker information if applicable
   ;; doom-modeline-check-simple-format t
   )

  ;; customize modeline segments
  (doom-modeline-def-segment clean-matches
    "an alternative to the built-in matches segment without fallback (buffer size indicator)"
    (concat
     ;; 1. the currently recording macro
     (doom-modeline--macro-recording)
     ;; 2. A current/total for the current search term (with `anzu')
     (doom-modeline--anzu)
     (doom-modeline--phi-search)
     ;; 3. The number of substitutions being conducted with `evil-ex-substitute', and/or
     (doom-modeline--evil-substitute)
     ;; 4. The number of active `iedit' regions
     (doom-modeline--iedit)
     ;; 5. The current/total for the highlight term (with `symbol-overlay')
     (doom-modeline--symbol-overlay)
     ;; 6. The number of active `multiple-cursors'.
     (doom-modeline--multiple-cursors)))

  ;; FIXME Freezes Emacs
  ;; (doom-modeline-def-modeline 'main
  ;;   '(hud modals clean-matches checker buffer-info remote-host buffer-position selection-info)
  ;;   '(misc-info github vcs lsp input-method buffer-encoding buffer-size major-mode process " "))
  )

(custom-set-faces!
  '(bold
    :weight semibold)
  '(doom-modeline-project-dir
    :weight regular
    :inherit font-lock-string-face)
  '(doom-modeline-buffer-path
    :weight regular
    :inherit doom-modeline-emphasis)
  '(doom-modeline-buffer-file
    :weight bold)
  '(doom-modeline-buffer-modified
    :weight bold
    :inherit error))


;; ─── Frame ────────────────────────────────────────────────────────────────────

;; Show file path in the title for files with the same base name. For example, the files `/foo/bar/mumble/name'
;; and `/baz/quux/mumble/name' would have the following buffer names:
;; bar/mumble/name    quux/mumble/name
(after! persp-mode
  (setq-hook! 'persp-mode-hook uniquify-buffer-name-style 'forward))


;; ─── Dashboard ────────────────────────────────────────────────────────────────

(setq fancy-splash-image (concat doom-user-dir "banners/berserk-guts-eclipse-1.png"))

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)


;; ─── Flycheck posframe ────────────────────────────────────────────────────────

(after! flycheck-posframe
  (setq flycheck-posframe-border-width 1)
  (setq flycheck-posframe-border-use-error-face nil)

  (custom-set-faces!
    `(flycheck-posframe-border-face     :foreground ,(doom-color 'base3))
    `(flycheck-posframe-background-face :background ,(doom-color 'bg-alt))
    `(flycheck-posframe-face            :box (:line-width ( 8 . 4 ) :color ,(doom-color 'bg-alt)) :foreground ,(doom-color 'fg)))

  (if (modulep! :checkers syntax +icons)
      (setq flycheck-posframe-prefix "󰋽 "
            flycheck-posframe-info-prefix "󰋽 "
            flycheck-posframe-warning-prefix " "
            flycheck-posframe-error-prefix "󰅚 ")
    (setq flycheck-posframe-warning-prefix "[?] "
          flycheck-posframe-info-prefix "[i] "
          flycheck-posframe-error-prefix "[!] ")))


;; ─── GPTel ────────────────────────────────────────────────────────────────────

(custom-set-faces!
  `(gptel-context-highlight-face :extend t :background ,(doom-lighten (doom-color 'bg) 0.05)))


;; ─── Markdown ─────────────────────────────────────────────────────────────────

;; get headings of sizes relative to their level
(setq markdown-header-scaling t)

(custom-set-faces!
  '(markdown-header-face :inherit nil)
  `(markdown-list-face :foreground ,(doom-color 'teal)))

;; better than `markdown-toggle-markup-hiding
;; TODO Remove this, it's pretty slow
;; (add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)


;; ─── Vertico Posframe ─────────────────────────────────────────────────────────

(after! vertico-posframe
  (setq vertico-posframe-border-width  1)
  (setq vertico-posframe-poshandler    #'my/posframe-poshandler-simple-smart-margins)
  (setq vertico-posframe-size-function #'my/vertico-posframe-get-size-with-max)

  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (custom-set-faces!
    `(vertico-group-title     :background ,(doom-color 'bg-alt))
    `(vertico-group-separator :inherit vertico-group-title :foreground ,(doom-color 'base1) :strike-through t)
    `(vertico-posframe        :box (:line-width (8 . 16) :color ,(doom-color 'bg)))
    `(vertico-posframe-border :background ,(doom-color 'base3))))
