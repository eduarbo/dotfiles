;;; +bindings.el -*- lexical-binding: t; -*-

;; rebind default repeating keys to ← → keys
(setq +evil-repeat-keys (cons [right] [left]))

(map! :i    [S-return]      #'+default/newline-above
      :i    "S-SPC"         #'tab-to-tab-stop

      :n    "SPC"           #'evil-ex-search-word-forward
      :v    "SPC"           #'evil-visualstar/begin-search-forward
      :nv   "S-SPC"         #'+default/search-project
      ;; :nv   "S-SPC"         #'+default/search-project-for-symbol-at-point
      :nv   "C-SPC"         #'lsp-rename

      :v    [S-return]      #'evilnc-comment-operator
      :n    [S-return]      #'evilnc-comment-or-uncomment-lines
      :in   [C-return]      #'lsp-execute-code-action

      :m    "RET"           #'evil-jump-item

      :nv   ";"             #'evil-ex
      :nv   ":"             #'pp-eval-expression

      :m    "k"             #'evil-previous-visual-line
      :m    "j"             #'evil-next-visual-line

      :nv   "H"             #'flycheck-previous-error
      :nv   "L"             #'flycheck-next-error

      :nv   [up]            (λ! (evil-previous-visual-line 10))
      :nv   [down]          (λ! (evil-next-visual-line 10))
      :nv   [left]          #'evil-first-non-blank-of-visual-line
      :nv   [right]         #'evil-end-of-line-or-visual-line

      :m    [S-up]          #'drag-stuff-up
      :m    [S-down]        #'drag-stuff-down
      :m    [S-left]        #'drag-stuff-left
      :m    [S-right]       #'drag-stuff-right

      :n    [backspace]     #'join-line
      :n    [S-backspace]   #'split-line

      :v    "s"             #'evil-surround-region
      :no   "s"             #'evil-surround-edit
      :no   "S"             #'evil-Surround-edit

      ;; expand-region
      :v    "v"             (general-predicate-dispatch 'er/expand-region
                              (eq (evil-visual-type) 'line)
                              'evil-visual-char)
      :v    "V"             #'er/contract-region

      :nv   "X"             #'consult-flycheck

      :n    "="             #'indent-according-to-mode
      :v    "="             #'evil-indent

      :n    ">"             #'evil-shift-right-line
      :n    "<"             #'evil-shift-left-line

      :inv  "C-,"           #'evil-snipe-repeat-reverse
      :inv  "C-."           #'evil-snipe-repeat

      :nv   "C-a"           #'evil-numbers/inc-at-pt
      :nv   "C-x"           #'evil-numbers/dec-at-pt

      :gi   "C-f"           #'forward-word
      :gi   "C-b"           #'backward-word

      ;; :gi   "C-d"           #'sp-delete-word
      :gi   "C-d"           #'kill-line
      (:map evil-markdown-mode-map
       :gi   "C-d"          #'kill-line)

      ;; Window navigation
      (:map general-override-mode-map
       :nvi "C-j"          #'evil-window-down
       :nvi "C-k"          #'evil-window-up
       :nvi "C-l"          #'evil-window-right
       :nvi "C-h"          #'evil-window-left)
      ;; (:map general-override-mode-map
      ;;  :gnvi "C-j"          #'evil-window-down
      ;;  :gnvi "C-k"          #'evil-window-up
      ;;  :gnvi "C-l"          #'evil-window-right
      ;;  :gnvi "C-h"          #'evil-window-left)

      ;; Leaving Emacs state unbound, my accidental visits always turn into a quest for an escape route!
      :im    "C-z"          nil
      (:map magit-mode-map
            "C-z" nil))


(map! :unless (eq system-type 'darwin)
      :gn    "M-<left>"      #'previous-buffer
      :gn    "M-<right>"     #'next-buffer
      (:map helpful-mode-map
       :gn   "M-<left>"      #'help-go-back
       :gn   "M-<right>"     #'help-go-forward)
      (:map Info-mode-map
       :gn   "M-<left>"      #'Info-history-back
       :gn   "M-<right>"     #'Info-history-forward)
      (:map git-timemachine-mode-map
       :gn   "M-<left>"      #'git-timemachine-show-previous-revision
       :gn   "M-<right>"     #'git-timemachine-show-next-revision)

      :gn    "s-S-<iso-lefttab>" #'+workspace/switch-left
      :gn    "s-S-<tab>"     #'+workspace/switch-left
      :gn    "s-<tab>"       #'+workspace/switch-right)

(map! :when (eq system-type 'darwin)
      :gn    "s-["           #'previous-buffer
      :gn    "s-]"           #'next-buffer
      (:map helpful-mode-map
       :gn   "s-["           #'help-go-back
       :gn   "s-]"           #'help-go-forward)
      (:map Info-mode-map
       :gn   "s-["           #'Info-history-back
       :gn   "s-]"           #'Info-history-forward)
      (:map git-timemachine-mode-map
       :gn   "s-["           #'git-timemachine-show-previous-revision
       :gn   "s-]"           #'git-timemachine-show-next-revision)

      :gn    "C-S-<tab>"     #'+workspace/switch-left
      :gn    "C-<tab>"       #'+workspace/switch-right)

;-----------------------------------------------------------
; -- Super (WIN/CMD key) shortcuts

(map!
      :g     "s-;"           #'pp-eval-expression

      :g     "s-h"           #'helpful-key

      :g     "s-,"           #'vertico-repeat
      :g     "s-."           #'+popup/toggle
      :g     [s-backspace]   #'+popup/raise

      ;; map M-x to `general-override-mode-map', which is a special keymap that
      ;; tries to have precedence over most other keymaps (including minor
      ;; modes’), so it doesn’t get overwritten by a careless package/minor mode
      (:map general-override-mode-map
       :g    "s-e"           #'execute-extended-command)

      :g     "s-g"           #'magit-status
      :g     "s-i"           #'+format/region-or-buffer
      :g     "s-j"           #'evil-switch-to-windows-last-buffer
      :g     "s-k"           #'kill-current-buffer
      :g     "s-l"           #'avy-goto-line
      :g     "s-o"           #'projectile-switch-project
      :g     "s-p"           #'+treemacs/toggle
      :n     "s-r"           #'+eval/open-repl-other-window
      :v     "s-r"           #'+eval:region
      :g     "s-s"           #'save-buffer
      :g     "s-t"           #'+workspace/new
      :g     "s-x"           #'doom/open-scratch-buffer
      :g     "s-X"           #'doom/open-project-scratch-buffer
      :g     "s-u"           #'evil-window-mru
      :n     "s-v"           #'evil-paste-after
      :i     "s-v"           #'my/evil-inser-mode-paste
      :v     "s-v"           #'+evil/alt-paste
      :g     "s-y"           #'+default/yank-pop
      :g     "s-w"           #'+workspace/close-window-or-workspace

      (:map with-editor-mode-map
       :g    "s-s"    #'with-editor-finish
       :g    "s-k"    #'with-editor-cancel
       :g    "s-w"    #'with-editor-cancel)
      (:map (wgrep-mode-map grep-mode-map)
       :g    "s-s"    #'wgrep-finish-edit
       :g    "s-k"    #'wgrep-abort-changes
       :g    "s-w"    #'wgrep-abort-changes)
      (:map snippet-mode-map
       :g    "s-s"    #'yas-load-snippet-buffer-and-close
       :g    "s-k"    #'+snippet--abort
       :g    "s-w"    #'+snippet--abort))


;-----------------------------------------------------------
; -- Smart tab: these will only work in GUI Emacs

(map! :i [tab]     (cmds! (and (bound-and-true-p company-mode)
                               (modulep! :completion company))
                          #'company-indent-or-complete-common
                          (and (bound-and-true-p corfu-mode)
                               (modulep! :completion corfu))
                          #'indent-for-tab-command)
      :m [tab]     #'+fold/toggle

      :i [backtab] (cmds! (and (modulep! :editor snippets)
                               (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                          #'yas-expand
                          (and (bound-and-true-p company-mode)
                               (modulep! :completion company))
                          #'company-yasnippet)
      :m [backtab] (cmds! (and (modulep! :editor snippets)
                               (evil-visual-state-p)
                               (or (eq evil-visual-selection 'line)
                                   (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                          #'yas-insert-snippet
                          ;; Fixes #4548: without this, this tab keybind overrides
                          ;; mode-local ones for modes that don't have an evil
                          ;; keybinding scheme or users who don't have :editor (evil
                          ;; +everywhere) enabled.
                          (or (doom-lookup-key
                               [tab]
                               (list (evil-get-auxiliary-keymap (current-local-map) evil-state)
                                     (current-local-map)))
                              (doom-lookup-key
                               (kbd "TAB")
                               (list (evil-get-auxiliary-keymap (current-local-map) evil-state)))
                              (doom-lookup-key (kbd "TAB") (list (current-local-map))))
                          it))

;; :completion (in-buffer)
(map! (:when (modulep! :completion corfu)
        (:after corfu
                (:map corfu-mode-map
                 :i "C-SPC" #'completion-at-point
                 :n "C-SPC" (cmd! (call-interactively #'evil-insert-state)
                                  (call-interactively #'completion-at-point))
                 :v "C-SPC" (cmd! (call-interactively #'evil-change)
                                  (call-interactively #'completion-at-point)))
                (:map corfu-map
                 :i "C-SPC" #'corfu-insert-separator
                 :i "S-SPC" #'corfu-insert-separator

                 "C-u" (cmd! (let (corfu-cycle)
                               (funcall-interactively #'corfu-next (- corfu-count))))
                 "C-d" (cmd! (let (corfu-cycle)
                               (funcall-interactively #'corfu-next corfu-count)))))
        (:after corfu-popupinfo :map (corfu-popupinfo-map corfu-map)
                "C-h"      #'corfu-popupinfo-toggle

                ;; Reversed because popupinfo assumes opposite of what feels intuitive
                ;; with evil.
                "S-<up>"    (cmd!! #'corfu-popupinfo-scroll-down nil (/ corfu-popupinfo-max-height 2))
                "S-<down>"  (cmd!! #'corfu-popupinfo-scroll-up nil (/ corfu-popupinfo-max-height 2))
                "C-k"       (cmd!! #'corfu-popupinfo-scroll-down nil (/ corfu-popupinfo-max-height 2))
                "C-j"       (cmd!! #'corfu-popupinfo-scroll-up nil (/ corfu-popupinfo-max-height 2)))))


;-----------------------------------------------------------
; -- Leader

(map! :leader
      :desc "Switch workspace"              "TAB"   #'persp-switch
      :desc "Eval expression"               ":"     #'pp-eval-expression
      :desc "M-x"                           ";"     #'execute-extended-command
      :desc "Find file in project"          "SPC"   #'projectile-find-file
      :desc "Pop up scratch buffer"         "X"     #'doom/open-project-scratch-buffer

      ;;; <leader> b --- buffer
      :desc "Yank buffer name"              "b y"   #'my/yank-buffer-name
      :desc "Yank buffer"                   "b Y"   #'+default/yank-buffer-contents

      ;;; <leader> c --- code
      :desc "List errors in buffer"         "c x"   #'consult-flycheck
      :desc "List errors in buffer/project" "c X"   #'+default/diagnostics

      ;;; <leader> t --- toggle
      :desc "Window Enlargen mode"          "t e"   #'my/window-enlargen-mode
      :desc "Line numbers mode"             "t l"   #'display-line-numbers-mode
      :desc "Copilot mode"                  "t a"   #'copilot-mode

      ;;; <leader> w --- workspaces/windows
      :desc "Balance windows"               "w b"   #'balance-windows
      :desc "Enlargen mode"                 "w o"   #'my/window-enlargen-mode
      :desc "Enlarge window"                "w O"   #'doom/window-enlargen
      :desc "Select a window"               "w w"   #'switch-window

      ;;; <leader> d --- delete
      :desc "Buffer"                        "d b"   #'kill-current-buffer
      :desc "Kill buried buffers"           "d k"   #'doom/kill-buried-buffers
      :desc "Workspace"                     "d l"   #'+workspace/delete
      :desc "Bookmark"                      "d m"   #'bookmark-delete
      :desc "Other buffers"                 "d o"   #'doom/kill-other-buffers
      :desc "Window or workspace"           "d w"   #'+workspace/close-window-or-workspace
      :desc "Session"                       "d x"   #'+workspace/kill-session

      ;;; <leader> g --- git
      :desc "Magit switch branch"           "g B"   #'magit-branch-checkout
      :desc "Magit blame"                   "g b"   #'magit-blame-addition

      ;; NOTE Reassign entire workspace map to different prefix
      ;;; <leader> l --- workspace (mnemonic for layer)
      (:prefix-map ("l" . "workspace") :when (modulep! :ui workspaces)
       :desc "Display tab bar"              "TAB"   #'+workspace/display
       :desc "Switch workspace"             "."     #'+workspace/switch-to
       :desc "Switch to last workspace"     "`"     #'+workspace/other
       :desc "Kill this workspace"          "k"     #'+workspace/delete
       :desc "New workspace"                "n"     #'+workspace/new
       :desc "New named workspace"          "N"     #'+workspace/new-named
       :desc "Load workspace from file"     "l"     #'+workspace/load
       :desc "Save workspace to file"       "s"     #'+workspace/save
       :desc "Delete session"               "x"     #'+workspace/kill-session
       :desc "Delete this workspace"        "d"     #'+workspace/delete
       :desc "Rename workspace"             "r"     #'+workspace/rename
       :desc "Restore last session"         "R"     #'+workspace/restore-last-session
       :desc "Next workspace"               "]"     #'+workspace/switch-right
       :desc "Previous workspace"           "["     #'+workspace/switch-left)

      ;;; <leader> n --- notes
      :desc "Obsidian notes"                "n SPC" #'obsidian-jump

      ;;; <leader> q --- quit/session
      :desc "Start new Emacs"               "q n"   #'restart-emacs-start-new-emacs)


;-----------------------------------------------------------
; -- goto prefix

(map! :prefix "g"
      :nv "j"     #'avy-goto-line-below
      :nv "k"     #'avy-goto-line-above
      :nv "n"     #'my/narrow-or-widen-dwim
      :nv "o"     #'avy-goto-char-timer
      :nv "O"     (λ! (let ((avy-all-windows t)) (avy-goto-char-timer)))
      :nv "s"     #'evil-snipe-s
      :nv "S"     #'evil-snipe-S
      :nv "w"     #'transpose-words
      :nv "."     #'evil-goto-last-change)


;-----------------------------------------------------------
; -- Modules

(map! :after evil-snipe
      :inv "C-,"     #'evil-snipe-repeat-reverse
      :inv "C-."     #'evil-snipe-repeat

      (:map evil-snipe-parent-transient-map
       :n "o"        #'link-hint-open-link
       :inv "C-,"    #'evil-snipe-repeat-reverse
       :inv "C-."    #'evil-snipe-repeat

       ;; Don't interfere with my bindings
       ";"  nil
       ","  nil))

(map! :map yas-keymap
      [tab]     nil
      "TAB"     nil
      [backtab] nil
      "S-TAB"   nil
      [(shift tab)] nil

      "C-."     #'yas-next-field
      "C-,"     #'yas-prev-field)

(map! :localleader :map markdown-mode-map
      :desc "Bold"              "b" #'markdown-insert-bold
      :desc "Inline code"       "c" #'markdown-insert-code
      :desc "Code block"        "C" #'markdown-insert-gfm-code-block
      :desc "Footnote"          "f" #'markdown-insert-footnote
      :desc "Header dwim"       "h" #'markdown-insert-header-dwim
      :desc "<hr>"              "H" #'markdown-insert-hr
      :desc "Italic"            "i" #'markdown-insert-italic
      :desc "Image"             "I" #'markdown-insert-image
      :desc "Link"              "k" #'markdown-insert-link
      :desc "Kbd"               "K" #'markdown-insert-kbd
      :desc "Wiki link"         "l" #'markdown-insert-wiki-link
      :desc "Table Of Content"  "o" #'markdown-toc-generate-toc
      :desc "Pre"               "P" #'markdown-insert-pre
      :desc "New blockquote"    "q" #'markdown-insert-blockquote
      :desc "Blockquote region" "Q" #'markdown-blockquote-region
      :desc "Strike through"    "s" #'markdown-insert-strike-through
      :desc "Table"             "T" #'markdown-insert-table
      :desc "Checkbox"          "x" #'markdown-insert-gfm-checkbox
      :desc "Heading 1"         "1" #'markdown-insert-header-atx-1
      :desc "Heading 2"         "2" #'markdown-insert-header-atx-2
      :desc "Heading 3"         "3" #'markdown-insert-header-atx-3
      :desc "Heading 4"         "4" #'markdown-insert-header-atx-4
      :desc "Heading 5"         "5" #'markdown-insert-header-atx-5
      :desc "Heading 6"         "6" #'markdown-insert-header-atx-6)

(map! :after vertico :map vertico-map
      "S-SPC"    #'+vertico/embark-preview
      [S-return] #'+vertico/embark-export-write
      "TAB"      #'vertico-insert
      [backtab]  #'vertico-directory-up
      "C-."      #'vertico-next-group
      "C-,"      #'vertico-previous-group
      "C-l"      #'vertico-scroll-up
      "C-h"      #'vertico-scroll-down)

(map! :after embark :map minibuffer-local-map
      "C-SPC"    #'embark-act
      [C-return] #'embark-export)

(map! :after copilot :map copilot-mode-map
      :i    "C-S-l"     #'copilot-complete-or-accept
      :i    "s-<right>" #'copilot-complete-or-accept
      :i    "C-S-j"     #'copilot-accept-completion-by-word
      :i    "s-<down>"  #'copilot-accept-completion-by-word
      :i    "C-S-k"     #'copilot-accept-completion-by-line
      :i    "s-<up>"    #'copilot-accept-completion-by-line
      :i    "C->"       #'copilot-previous-completion
      :i    "s->"       #'copilot-previous-completion
      :i    "C-<"       #'copilot-next-completion
      :i    "s-<"       #'copilot-next-completion
      :i    "C-S-h"     #'copilot-clear-overlay
      :i    "s-<left>"  #'copilot-clear-overlay)
