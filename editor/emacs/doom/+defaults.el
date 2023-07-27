;;; +defaults.el -*- lexical-binding: t; -*-

;; -- Fonts

;; make sure that the font exists before using it
(let ((fn (doom-rpartial #'member (font-family-list))))
  ;; the primary font to use
  (when-let (font (cl-find-if fn '("JetBrains Mono" "Hack")))
    (setq doom-font (font-spec :family font :size 14 :weight 'light)))

  ;; used for `doom-big-font-mode'; use this for presentations or streaming
  (when-let (font (cl-find-if fn '("JetBrains Mono" "Hack")))
    (setq doom-big-font (font-spec :family font :size 20 :weight 'light)))

  ;; a non-monospace font (where applicable)
  (when-let (font (cl-find-if fn '("PT Sans" "Noto Serif" "ETBembo")))
    (setq doom-variable-pitch-font (font-spec :family font)))

  (setq doom-symbol-fallback-font-families '("Fira Code" "Segoe UI Symbol" "Apple Symbols"))

  ;; for unicode glyphs
  (when-let (font (cl-find-if fn doom-symbol-fallback-font-families))
    (setq doom-unicode-font (font-spec :family font)))

  ;; for the `fixed-pitch-serif' face
  (when-let (font (cl-find-if fn doom-symbol-fallback-font-families))
    (setq doom-serif-font (font-spec :family font)))
  )

;; Emojis font 💅
;; NOTE I prefer Microsoft emojis but they don't have a fixed width, which breaks monospace column
;; alignment 😕
(setq doom-emoji-fallback-font-families '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji" "Noto Emoji"))


;; -- Theme

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-moonlight)
(setq doom-theme 'doom-oceanic-next)


;; -- Sane defaults

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "~/Documents/Docs personales/org"))

;; Line numbers are pretty slow all around. The performance boost of disabling them outweighs the
;; utility of always keeping them on
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'display-line-numbers-mode)
(setq display-line-numbers-type t)

;; Show file path for files with the same base name. For example, the files `/foo/bar/mumble/name'
;; and `/baz/quux/mumble/name' would have the following buffer names:
;; bar/mumble/name    quux/mumble/name
(setq uniquify-buffer-name-style 'forward)

;; Stop in-between "camelCase" words instead of just spaces, hyphens or underscores
(after! subword-mode)
(setq global-subword-mode t)
(add-hook! '(prog-mode-hook conf-mode-hook) #'subword-mode)

;; evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Get some context when scrolling
(setq scroll-margin 10)

;; Show whitespace
(add-hook! '(prog-mode-hook conf-mode-hook) #'doom-enable-show-trailing-whitespace-h)

;; Allow me to insert accents and other symbols
(setq mac-option-modifier 'none)

;; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; Set
(setq projectile-project-search-path '(("~/dev" . 2) ("~/Library/Mobile Documents/iCloud~md~obsidian/Documents" . 2)))

;; pattern matching without jumping
(advice-add 'evil-ex-start-word-search :around #'my/evil-ex-start-word-search-advice)
(advice-add 'evil-visualstar/begin-search :around #'my/evil-visualstar-begin-search-advice)

;; Insert or Replace the active visual region with a yanked entry
(advice-add 'consult-yank-pop :around #'my/consult-yank-pop-replace-region)
