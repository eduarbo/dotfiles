;;; +magit.el -*- lexical-binding: t; -*-

;; Redefine everything
(after! (magit evil-collection)
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
      ("h" "help"           transient-quit-one) ; Toggle help popup
      ("H" "Section info"   magit-describe-section :if-derived magit-mode)
      ("i" "Ignore"         magit-gitignore)]
     [("I" "Init"           magit-init)
      ("J" "Jump to section"magit-status-jump  :if-mode     magit-status-mode)
      ("J" "Display status" magit-status-quick :if-not-mode magit-status-mode)
      ;; k                  ↓
      ("K" "Untrack"        magit-file-untrack)
      ("l" "Log"            magit-log)
      ("L" "Log (change)"   magit-log-refresh)
      ("m" "Merge"          magit-merge)
      ("M" "Remote"         magit-remote)
      ;; n                → magit-section-forward
      ;; N       reserved → forge-dispatch
      ("o" "Submodule"      magit-submodule)
      ("O" "Subtree"        magit-subtree)
      ;; p                → magit-section-backward
      ("P" "Push"           magit-push)
      ;; q                → magit-mode-bury-buffer
      ;; Q                → +magit/quit-all
      ("r" "Rebase"         magit-rebase)
      ;; R                → magit-file-rename
      ;; s                  ↓
      ;; S                  ↓
      ("t" "Tag"            magit-tag)
      ("T" "Note"           magit-notes)
      ;; u                  ↓
      ;; U                  ↓
      ;; v                  ↓
      ("V" "Revert"         magit-revert)]
     [("w" "Apply patches"  magit-am)
      ("W" "Format patches" magit-patch)
      ;; x                  ↓
      ("X" "Reset"          magit-reset)
      ("y" "Show Refs"      magit-show-refs)
      ("Y" "Cherries"       magit-cherry)
      ("z" "Stash"          magit-stash)
      ("Z" "Worktree"       magit-worktree)
      ("<" "Command"        magit-git-command)
      (">" "Run"            magit-run)
      ("." "Display buffer" magit-display-repository-buffer)
      ("=" "More context"   magit-diff-more-context)
      ("-" "Less context"   magit-diff-less-context)
      ("0" "Reset context"  magit-diff-default-context)
      ;; TODO find a better binding
      ("C-t" "Text-mode"    evil-collection-magit-toggle-text-mode)
      ]]
    ["Applying changes"
     :if-derived magit-mode
     [("a" "Apply"          magit-apply)
      ("v" "Reverse"        magit-reverse)
      ("x" "Discard"        magit-discard)]
     [("s" "Stage"          magit-stage)
      ("u" "Unstage"        magit-unstage)]
     [("S" "Stage all"      magit-stage-modified)
      ("U" "Unstage all"    magit-unstage-all)]]
    ["Essential commands"
     :if-derived magit-mode
     [("g"        "Refresh current buffer"   magit-refresh)
      ("q"        "Bury current buffer"      magit-mode-bury-buffer)
      ("Q"        "Bury all magit buffers"   +magit/quit-all)
      ("j"        "Next line"                evil-next-visual-line)
      ("k"        "Previous line"            evil-previous-visual-line)]
     [("<space>" " Show diff in other pane"  magit-diff-show-or-scroll-up)
      ("<tab>" "   Toggle section at point"  magit-section-toggle)
      ("<return>" "Visit thing at point"     magit-visit-thing)
      ("C-x m"    "Show all key bindings"    describe-mode)
      ("C-x i"    "Show Info manual"         magit-info)]]))
