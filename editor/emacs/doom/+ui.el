;;; ../../dev/eduarbo/dotfiles/editor/emacs/doom/+ui.el -*- lexical-binding: t; -*-

;; -- Fonts

;; make sure that the font exists before using it
(let ((fn (doom-rpartial #'member (font-family-list))))
  (when-let (font (cl-find-if fn '("JetBrains Mono" "Hack")))
    ;; the primary font to use
    (setq doom-font (font-spec :family font :size 14 :weight 'light))
    (setq doom-big-font (font-spec :family font :size 20 :weight 'light))

    ;; Fallback font for Unicode glyphs
    (setq doom-unicode-font (font-spec :family font :weight 'light)))

  ;; a monospace serif font (where applicable)
  (when-let (font (cl-find-if fn '("IBM Plex Mono" "Fira Code")))
    (setq doom-serif-font (font-spec :family font)))

  ;; a non-monospace font (where applicable)
  (when-let (font (cl-find-if fn '("PT Sans" "Noto Serif" "ETBembo")))
    (setq doom-variable-pitch-font (font-spec :family font))))

;; Emojis font 💅
;; NOTE I prefer Microsoft emojis but they don't have a fixed width, which breaks monospace column alignment 😕
(setq doom-emoji-fallback-font-families '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji" "Noto Emoji"))

;; -- Theme

;; Workaround to ensure that nothing else gets in front of my `custom-theme-directory' after initialization. Doom's core
;; is supposed to handle this, but it isn't working correctly, so I've mimicked its approach within a hook

(add-hook! 'doom-init-ui-hook
  (defun my/prioritize-custom-theme-directory-h ()
    "Prioritize my custom them path over all other themes"
    (setq custom-theme-load-path
          (cons custom-theme-directory
                (delq 'custom-theme-directory custom-theme-load-path)))))

(setq doom-theme 'doom-oceanic-next)

;; HACK Fix the output color in the REPL
;; https://github.com/emacs-ess/ESS/issues/1193#issuecomment-1144182009
(add-hook 'comint-mode-hook #'ansi-color-for-comint-mode-filter 'append)

(blink-cursor-mode 1)

(setq doom-themes-treemacs-theme "doom-colors")


;; -- Modeline

(after! doom-modeline
  (setq
   ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
   ;; truncate-with-project => emacs/l/comint.el
   ;; relative-from-project => emacs/lisp/comint.el
   doom-modeline-buffer-file-name-style 'relative-from-project
   ;; doom-modeline-buffer-file-name-style 'truncate-with-project
   ;; Whether display the buffer encoding
   doom-modeline-buffer-encoding t
   ;; Whether display the modal state icon.
   ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc
   doom-modeline-modal-icon t
   ;; Whether display the modification icon for the buffer
   doom-modeline-buffer-modification-icon nil
   ;; Whether display the icon for `major-mode'
   doom-modeline-major-mode-icon t
   ;; The maximum displayed length of the branch name of version control
   doom-modeline-vcs-max-length 18
   ;; Whether display the GitHub notifications. It requires `ghub' package
   doom-modeline-github t
   ;; If non-nil, only display one number for checker information if applicable
   doom-modeline-checker-simple-format t)

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
  (doom-modeline-def-modeline 'main
    '(hud modals clean-matches checker buffer-info remote-host buffer-position selection-info)
    '(misc-info github vcs lsp input-method buffer-encoding buffer-size major-mode process " ")))


;; -- Frame

;; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; FIXME Disabled for now as it breaks `persp-mode' and there's no way to work around it
;; https://github.com/doomemacs/doomemacs/issues/6205
;;
;; Show file path in the title for files with the same base name. For example, the files `/foo/bar/mumble/name'
;; and `/baz/quux/mumble/name' would have the following buffer names:
;; bar/mumble/name    quux/mumble/name
;; (setq uniquify-buffer-name-style 'forward)


;; -- Dashboard

(setq fancy-splash-image (concat doom-user-dir "banners/berserk-guts-eclipse-1.png"))

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
