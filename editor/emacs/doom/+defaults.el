;;; +defaults.el -*- lexical-binding: t; -*-

;; -- Sane defaults

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "~/Documents/Docs personales/org"))

;; Tell projectile where my projects are located
(setq projectile-project-search-path '(("~/dev" . 2) ("~/Library/Mobile Documents/iCloud~md~obsidian/Documents" . 2)))

;; Line numbers are pretty slow all around. The performance boost of disabling them outweighs the
;; utility of always keeping them on
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'display-line-numbers-mode)
(setq display-line-numbers-type t)

;; Stop in-between "camelCase" words instead of just spaces, hyphens or underscores
(add-hook! 'after-change-major-mode-hook #'subword-mode)

;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)

;; evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Get some context when scrolling
(setq scroll-margin 10)

;; Show whitespace
(add-hook! '(prog-mode-hook conf-mode-hook) #'doom-enable-show-trailing-whitespace-h)

;; Allow me to insert accents and other symbols
(setq mac-option-modifier 'none)

;; pattern matching without jumping
(advice-add 'evil-ex-start-word-search :around #'my/evil-ex-start-word-search-advice)
(advice-add 'evil-visualstar/begin-search :around #'my/evil-visualstar-begin-search-advice)

;; Insert or Replace the active visual region with a yanked entry
(advice-add 'consult-yank-pop :around #'my/consult-yank-pop-replace-region)

(after! markdown-mode
  (setq markdown-list-indent-width 4))

(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode) 'append)

