;;; +defaults.el -*- lexical-binding: t; -*-

;; ─── Core Directories & Paths ────────────────────────────────────────────────

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "~/Documents/Docs personales/org"))

;; Tell projectile where my projects are located
(setq projectile-project-search-path
      '(("~/dev" . 2)
        ("~/Library/Mobile Documents/iCloud~md~obsidian/Documents" . 2)))
(setq projectile-enable-caching nil)

;; ─── Editor Performance & Display ─────────────────────────────────────────────

;; Line numbers are pretty slow all around. The performance boost of disabling them outweighs the
;; utility of always keeping them on
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'display-line-numbers-mode)
(setq display-line-numbers-type t)

;; Enable scroll margin for better context when scrolling
(setq scroll-margin 10)

;; Show trailing whitespace in code & config
(add-hook! '(prog-mode-hook conf-mode-hook) #'doom-enable-show-trailing-whitespace-h)

;; Word wrap (almost) everywhere
(+global-word-wrap-mode +1)
(add-hook! 'special-mode-hook (+word-wrap-mode +1))

;; ─── Visuals & Icons ─────────────────────────────────────────────────────────

;; Download the emoji images immediately without prompting, otherwise Emacs freezes when opening for the first time
(setq emojify-download-emojis-p t)

;; Auto install nerd-icons font if missing
(unless (member "Symbols Nerd Font Mono" (font-family-list))
  (nerd-icons-install-fonts t))

;; diff-hl: use right fringe
(after! diff-hl (setq diff-hl-side 'right))

;; No Titlebar with square corners
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; No Titlebar with round corners
;; (add-to-list 'default-frame-alist '(undecorated-round . t))

;; ─── Language/Mode-Specific Adjustments ──────────────────────────────────────

;; Org mode
(after! org
  (setq org-startup-indented nil))

;; Markdown
(after! markdown-mode
  (setq markdown-list-indent-width 4))

;; Various file associations
(add-to-list 'auto-mode-alist '("\\editorconfig\\'" . editorconfig-conf-mode))
(add-to-list 'auto-mode-alist '("gitconfig" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . cpp-mode))
(add-to-list 'auto-mode-alist '("\\.clangd\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile" . dockerfile-mode))

;; Treat '=_<> as punctuation in all modes
(add-hook! 'after-change-major-mode-hook
  (defun my-global-word-delimiters-h ()
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> ".")))

;; Python: treat _ as word constituent
(add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))

;; FIXME: Emacs freezes if LSP tries to auto-download a server in nxml-mode
(setq-hook! 'nxml-mode-hook lsp-enable-suggest-server-download nil)

;; ─── User Experience Defaults ────────────────────────────────────────────────

;; Stop in-between "camelCase" words instead of just spaces, hyphens or underscores
(add-hook! 'after-change-major-mode-hook #'subword-mode)

;; Minibuffer: thinner cursor when evaluating/interactive
(setq-hook! 'minibuffer-setup-hook cursor-type 'bar)
(setq-hook! 'eval-expression-minibuffer-setup-hook cursor-type 'bar)

;; Allow me to insert accents and other symbols
(setq mac-option-modifier 'none)

;; Swap Ctrl and Alt on Linux/Windows
(unless (featurep 'macos)
  (setq x-super-keysym 'ctrl)
  (setq x-ctrl-keysym 'super))

;; ─── Editor Enhancements ─────────────────────────────────────────────────────

;; Evil: Preferred window split directions
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Evil: Pattern matching without jumping
(advice-add 'evil-ex-start-word-search :around #'my/evil-ex-start-word-search-advice)
(advice-add 'evil-visualstar/begin-search :around #'my/evil-visualstar-begin-search-advice)

;; Insert or Replace the active visual region with a yanked entry
(advice-add 'consult-yank-pop :around #'my/consult-yank-pop-replace-region)

;; ─── Security & Authentication ───────────────────────────────────────────────

(setq auth-sources '("~/.config/doom/authinfo.gpg")
      auth-source-cache-expiry nil
      epa-file-encrypt-to (getenv "USER_EMAIL")
      ;; Automatically select GPG keys without prompting
      epa-file-select-keys 'silent)
