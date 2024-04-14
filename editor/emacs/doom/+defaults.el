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

(add-to-list 'auto-mode-alist '("\\editorconfig\\'" . editorconfig-conf-mode))

;; Nunjucks template files
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))

;; Arduino Sketches
(add-to-list 'auto-mode-alist '("\\.ino\\'" . cpp-mode))

;; Treats the `=' as punctuation for all modes
(add-hook! 'after-change-major-mode-hook
  (modify-syntax-entry ?= "."))

(setq evil-collection-magit-want-horizontal-movement t)
(setq evil-collection-magit-use-y-for-yank t)

;; Redefine magit-dispatch transient
(after! magit
  (transient-define-prefix magit-dispatch ()
    "Invoke a Magit command from a list of available commands."
    :info-manual "(magit)Top"
    ["Transient and dwim commands"
     ;; → bound in magit-mode-map or magit-section-mode-map
     ;; ↓ bound below
     [("A" "Apply"          magit-cherry-pick)
      ;; a                  ↓
      ("b" "Branch"         magit-branch)
      ("B" "Bisect"         magit-bisect)
      ("c" "Commit"         magit-commit)
      ("C" "Clone"          magit-clone)
      ("d" "Diff"           magit-diff)
      ("D" "Diff (change)"  magit-diff-refresh)
      ("e" "Ediff (dwim)"   magit-ediff-dwim)
      ("E" "Ediff"          magit-ediff)
      ("f" "Fetch"          magit-fetch)
      ("F" "Pull"           magit-pull)
      ;; g                  ↓
      ;; G                → magit-refresh-all
      ("H" "Section info"   magit-describe-section :if-derived magit-mode)]
     [("i" "Ignore"         magit-gitignore)
      ("I" "Init"           magit-init)
      ("J" "Display buffer" magit-display-repository-buffer)
      ;; k                  ↓
      ;; K                → magit-file-untrack
      ("L" "Log"            magit-log)
      ("m" "Merge"          magit-merge)
      ("M" "Remote"         magit-remote)
      ;; n                → magit-section-forward
      ;; N       reserved → forge-dispatch
      ("o" "Submodule"      magit-submodule)
      ("O" "Subtree"        magit-subtree)
      ;; p                → magit-section-backward
      ("P" "Push"           magit-push)
      ;; q                → magit-mode-bury-buffer
      ("Q" "Command"        magit-git-command)
      ("r" "Rebase"         magit-rebase)]
     [
      ;; R                → magit-file-rename
      ;; s                  ↓
      ;; S                  ↓
      ("t" "Tag"            magit-tag)
      ("T" "Note"           magit-notes)
      ;; u                  ↓
      ;; U                  ↓
      ;; v                  ↓
      ("w" "Apply patches"  magit-am)
      ("W" "Format patches" magit-patch)
      ;; x                → magit-reset-quickly
      ("X" "Reset"          magit-reset)
      ("y" "Show Refs"      magit-show-refs)
      ("Y" "Cherries"       magit-cherry)
      ("z" "Stash"          magit-stash)
      ("Z" "Worktree"       magit-worktree)
      ("⌫" "Revert"         magit-revert)
      ("!" "Run"            magit-run)]]
    ["Applying changes"
     :if-derived magit-mode
     [("a" "Apply"          magit-apply)
      ("." "Reverse"        magit-reverse)
      ("x" "Discard"        magit-discard)]
     [("s" "Stage"          magit-stage)
      ("u" "Unstage"        magit-unstage)]
     [("S" "Stage all"      magit-stage-modified)
      ("U" "Unstage all"    magit-unstage-all)]]
    ["Diff hunks"
     [("0" "Default context"    magit-diff-default-context)]
     [("=" "More context"       magit-diff-more-context)]
     [("-" "Less context"       magit-diff-less-context)]]
    ["Essential commands"
     :if-derived magit-mode
     [("h" "←"    magit-section-backward-sibling)
      ("j" "↓"    magit-section-forward)
      ("k" "↑"    magit-section-backward)
      ("l" "→"    magit-section-forward-sibling)]
     [("g" "Refresh current buffer"   magit-refresh)
      ("q" "Bury current buffer"      magit-mode-bury-buffer)
      ("v" "Visual mode"              evil-visual-char)
      ("V" "Visual Line mode"         evil-visual-line)]
     [("<tab>" "   Toggle section at point"  magit-section-toggle)
      ("<return>" "Visit thing at point"     magit-visit-thing)
      ("C-t" "     Toggle text-mode"         evil-collection-magit-toggle-text-mode)]]))
