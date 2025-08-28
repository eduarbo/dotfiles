;;; +defaults.el -*- lexical-binding: t; -*-

;; ─── Sane defaults ────────────────────────────────────────────────────────────

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (expand-file-name "~/Documents/Docs personales/org"))

(after! org
  (setq org-startup-indented nil))

;; Tell projectile where my projects are located
(setq projectile-project-search-path '(("~/dev" . 2) ("~/Library/Mobile Documents/iCloud~md~obsidian/Documents" . 2)))
(setq projectile-enable-caching nil)

;; Line numbers are pretty slow all around. The performance boost of disabling them outweighs the
;; utility of always keeping them on
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) #'display-line-numbers-mode)
(setq display-line-numbers-type t)

;; Stop in-between "camelCase" words instead of just spaces, hyphens or underscores
(add-hook! 'after-change-major-mode-hook #'subword-mode)

;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)
(add-hook! 'special-mode-hook
  (+word-wrap-mode +1))

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

(add-to-list 'auto-mode-alist '("\\editorconfig\\'" . editorconfig-conf-mode))

(add-to-list 'auto-mode-alist '("gitconfig" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.env" . conf-mode))

;; Nunjucks template files
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; Arduino Sketches
(add-to-list 'auto-mode-alist '("\\.ino\\'" . cpp-mode))
(add-to-list 'auto-mode-alist '("\\.clangd\\'" . yaml-mode))

;; Dockerfile
(add-to-list 'auto-mode-alist '("\\Dockerfile" . dockerfile-mode))

;; Treats the `=', `<' and `>' as punctuation for all modes
(add-hook! 'after-change-major-mode-hook
  (defun my-global-word-delimiters-h ()
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> ".")))

(add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))

;; Download the emoji images immediately without prompting, otherwise Emacs freezes when opening for the first time
(setq emojify-download-emojis-p t)

;; Auto install nerd-icons
(unless (member "Symbols Nerd Font Mono" (font-family-list))
  (nerd-icons-install-fonts t))

;; while diff-hl takes the right fringe
(after! diff-hl (setq diff-hl-side 'right))

;; Authentication and encryption settings
(setq auth-sources '("~/.config/doom/authinfo.gpg")
      auth-source-cache-expiry nil
      epa-file-encrypt-to (getenv "USER_EMAIL")
      ;; Automatically select GPG keys without prompting the user
      epa-file-select-keys 'silent)

;; Magit
(setq evil-collection-magit-want-horizontal-movement t)
(setq evil-collection-magit-use-y-for-yank t)

(unless (featurep 'macos)
  ;; Swap Ctrl and Alt
  (setq x-super-keysym 'ctrl)
  (setq x-ctrl-keysym 'super))

(setq desktop-restore-forces-onscreen nil)

;; Set the cursor type to 'bar in the minibuffer when evaluating an expression
(setq-hook! 'minibuffer-setup-hook cursor-type 'bar)
(setq-hook! 'eval-expression-minibuffer-setup-hook cursor-type 'bar)
