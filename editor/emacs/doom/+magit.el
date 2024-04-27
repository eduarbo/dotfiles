;;; +magit.el -*- lexical-binding: t; -*-

(setq evil-collection-magit-use-z-for-folds t)
(setq evil-collection-magit-want-horizontal-movement nil)

;; Redefine the entire help popup
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
      ;; o                  ↓
      ("O" "Revert"         magit-revert)
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
      ("w" "Apply patches"  magit-am)
      ("W" "Format patches" magit-patch)]
     ;; x                  ↓
     [("X" "Reset"          magit-reset)
      ("y" "Show Refs"      magit-show-refs)
      ("Y" "Cherries"       magit-cherry)
      ("z" "Stash"          magit-stash)
      ("Z" "Worktree"       magit-worktree)
      ("." "Display buffer" magit-display-repository-buffer)
      ("~" "Command"        magit-git-command)
      ("!" "Run"            magit-run)
      ("'" "Submodule"      magit-submodule)
      ("\"" "Subtree"       magit-subtree)
      ("<" "Less context"   magit-diff-less-context)
      (">" "More context"   magit-diff-more-context)
      ("=" "Reset context"  magit-diff-default-context)
      ("\\" "Text-mode"     evil-collection-magit-toggle-text-mode)]]
    ["Applying changes"
     :if-derived magit-mode
     [("a" "Apply"          magit-apply)
      ("o" "Reverse"        magit-reverse)
      ("x" "Discard"        magit-discard)]
     [("s" "Stage"          magit-stage)
      ("u" "Unstage"        magit-unstage)]
     [("S" "Stage all"      magit-stage-modified)
      ("U" "Unstage all"    magit-unstage-all)]]
    ["Essential commands"
     :if-derived magit-mode
     [("j"       " Next line"                evil-next-visual-line)
      ("k"       " Previous line"            evil-previous-visual-line)
      ("q"       " Bury current buffer"      magit-mode-bury-buffer)
      ("Q"       " Bury all magit buffers"   +magit/quit-all)
      ("v"       " Visual line"              evil-visual-line)
      ("gr"       "Refresh current buffer"   magit-refresh)
      ("gR"       "Refresh current buffer"   magit-refresh-all)]
     [("[up]" "    Previous sibling section" magit-section-backward-sibling)
      ("[down]" "  Next sibling section"     magit-section-forward-sibling)
      ("[left]" "  Previous section"         magit-section-backward)
      ("[right]" " Next section"             magit-section-forward)
      ("<space>" " Show diff in other pane"  magit-diff-show-or-scroll-up)
      ("<tab>" "   Toggle section at point"  magit-section-toggle)
      ("<return>" "Visit thing at point"     magit-visit-thing)]])

  (map! :map magit-mode-map
        ;; Align bindings in magit mode with the transient's
        :nv   "o"      #'magit-reverse
        :nv   "O"      #'magit-revert
        :nv   "X"      #'magit-reset
        :nv   "J"      #'magit-status-jump

        :nv   "."      #'magit-display-repository-buffer
        :nv   "~"      #'magit-git-command
        :nv   "!"      #'magit-run
        :nv   "<"      #'magit-diff-less-context
        :nv   ">"      #'magit-diff-more-context
        :nv   "="      #'magit-diff-default-context

        :nv   [up]     #'magit-section-backward-sibling
        :nv   [down]   #'magit-section-forward-sibling
        :nv   [left]   #'magit-section-backward
        :nv   [right]  #'magit-section-forward

        ;; Convenient bindings
        :nv   "q"      'my/magit-toggle-diff-buffers-or-quit
        [escape]       'my/magit-toggle-diff-buffers-or-quit))

;; Override other transients to align them with the help popup
(setq evil-collection-magit-popup-changes
      (append
       (when evil-collection-magit-use-z-for-folds
         '((magit-dispatch "Z" "%" magit-worktree)
           (magit-dispatch "z" "Z" magit-stash)))
       (when evil-collection-magit-want-horizontal-movement
         '((magit-dispatch "L" "\C-l" magit-log-refresh)
           (magit-dispatch "l" "L" magit-log)))
       '((magit-branch "x" "X" magit-branch-reset)
         (magit-branch "k" "x" magit-branch-delete)
         (magit-remote "k" "x" magit-remote-remove)
         ;; FIXME: how to properly handle a popup with a key that appears twice (in
         ;; `transient-define-prefix' definition)? Currently we rely on:
         ;; 1. first call to `evil-collection-magit-change-popup-key' changes the first "V"
         ;;    entry of `magit-revert' (the first entry in `transient-define-prefix'
         ;;    definition of `magit-revert'), second call changes the second "V".
         ;; 2. the remapping here are in the same order as in `magit-revert'
         ;;    definition
         (magit-revert "V" "O" magit-revert-and-commit)
         (magit-revert "V" "O" magit-sequencer-continue)
         (magit-revert "v" "o" magit-revert-no-commit)
         (magit-tag    "k" "x" magit-tag-delete))))
