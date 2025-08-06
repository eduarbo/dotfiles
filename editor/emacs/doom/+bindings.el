;;; +bindings.el -*- lexical-binding: t; -*-

;; rebind default repeating keys to ← → keys
(setq +evil-repeat-keys (cons [right] [left]))

(map!
 :i    [S-return]      #'+default/newline-above
 :i    [C-return]      #'+default/newline-below
 :i    "S-SPC"         #'tab-to-tab-stop

 :nv   "S-SPC"         #'+default/search-project
 ;; :nv   "S-SPC"         #'+default/search-project-for-symbol-at-point

 :v    [S-return]      #'evilnc-comment-operator
 :n    [S-return]      #'evilnc-comment-or-uncomment-lines
 :n    [C-return]      #'lsp-execute-code-action

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

 :nvi  [S-escape]      #'my/comment-box

 :nv   [S-up]          #'drag-stuff-up
 :nv   [S-down]        #'drag-stuff-down
 :nv   [S-left]        #'drag-stuff-left
 :nv   [S-right]       #'drag-stuff-right

 :n    [backspace]     #'join-line
 :n    [S-backspace]   #'split-line

 :v    "s"             #'evil-surround-region
 :no   "s"             #'evil-surround-edit

 :nv   "S"             #'evil-snipe-s

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
  :gnvi "C-j"          #'evil-window-down
  :gnvi "C-k"          #'evil-window-up
  :gnvi "C-l"          #'evil-window-right
  :gnvi "C-h"          #'evil-window-left)

 ;; Leaving Emacs state unbound, my accidental visits always turn into a quest for an escape route!
 :im    "C-z"          nil
 (:map magit-mode-map
       "C-z" nil))

(map! :unless (eq system-type 'darwin)
      :gn    "M-<left>"      #'previous-buffer
      :gn    "M-<right>"     #'next-buffer
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


;; ─── Super (WIN/CMD key) shortcuts ────────────────────────────────────────────

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
 :g     "s-i"           #'my/eslint-fix-all-maybe-and-format
 :g     "s-j"           #'evil-switch-to-windows-last-buffer
 :g     "s-k"           #'kill-current-buffer
 :g     "s-l"           #'avy-goto-line
 :g     "s-o"           #'projectile-switch-project
 :g     "s-p"           #'+dired/dirvish-side-and-follow
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


;; ─── Smart tab: these will only work in GUI Emacs ─────────────────────────────

;; Globally disable default TAB binding to ensure consistent behavior across modes
(global-set-key [remap indent-for-tab-command] nil)

(map! :i [tab]
      `(menu-item "Evil insert smart tab" nil :filter
        (lambda (cmd)
          (cond
           ;; If there's an overriding keybinding, use it
           ((or (doom-lookup-key [tab] overriding-terminal-local-map)
                (doom-lookup-key (kbd "TAB") overriding-terminal-local-map))
            cmd)
           ,@(when (modulep! :editor snippets)
               ;; If yasnippet can expand, do so
               '(((and (bound-and-true-p yas-minor-mode)
                       (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                  #'yas-expand)
                 ;; If inside an active yasnippet field, move to the next field
                 ((memq (bound-and-true-p yas--active-field-overlay)
                        (overlays-in (1- (point)) (1+ (point))))
                  #'yas-next-field))))))

      :v [tab]
      (cmds! (and (modulep! :editor snippets)
                  (evil-visual-state-p)
                  ;; NOTE Disabling check for trailing chars to avoid issues when selecting functions
                  ;; (or (eq evil-visual-selection 'line)
                  ;;     (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\)))))
                  )
             #'yas-insert-snippet)

      (:mode prog-mode
       :n [tab]
       #'+fold/toggle)

      :i [backtab]
      `(menu-item "Evil insert smart backtab" nil :filter
        (lambda (cmd)
          (cond
           ((or (doom-lookup-key [backtab] overriding-terminal-local-map)
                (doom-lookup-key (kbd "S-TAB") overriding-terminal-local-map))
            cmd)
           ,@(when (modulep! :editor snippets)
               ;; If in an active `yasnippet' field, move to previous field
               '(((memq (bound-and-true-p yas--active-field-overlay)
                   (overlays-in (1- (point)) (1+ (point))))
                  #'yas-prev-field)
                 ;; If yasnippet is active and corfu popup is not visible, invoke yasnippet completion
                 ((and (bound-and-true-p yas-minor-mode)
                       (not (and (boundp 'corfu--frame)
                                 (frame-live-p corfu--frame)
                                 (frame-visible-p corfu--frame))))
                  #'yasnippet-capf))))))

      ;; Extend smart tab for specific modes. This way, we process the entire
      ;; smart tab logic and only fall back to these commands at the end.
      (:when (modulep! :lang org)
        (:after org :map org-mode-map
                [remap indent-for-tab-command]
                `(menu-item "Go to the next field" org-table-next-field
                  :filter ,(lambda (cmd) (when (org-at-table-p) cmd))))))

;; :completion (in-buffer)
(map! (:when (modulep! :completion corfu)
        (:after corfu :map corfu-map
                (:unless (bound-and-true-p evil-disable-insert-state-bindings)
                  :i "S-SPC"    #'corfu-insert-separator))
        (:after corfu-popupinfo :map (corfu-popupinfo-map corfu-map)
                "S-<up>"    (cmd!! #'corfu-popupinfo-scroll-down nil (/ corfu-popupinfo-max-height 2))
                "S-<down>"  (cmd!! #'corfu-popupinfo-scroll-up nil (/ corfu-popupinfo-max-height 2)))))


;; ─── Leader ───────────────────────────────────────────────────────────────────

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

      ;;; <leader> k --- workspace (mnemonic: 'k' as in worKspace)
      (:prefix-map ("k" . "workspace") :when (modulep! :ui workspaces)
       :desc "Display tab bar"              "TAB"   #'+workspace/display
       :desc "Switch workspace"             "."     #'+workspace/switch-to
       :desc "Switch to last workspace"     "`"     #'+workspace/other
       :desc "New workspace"                "n"     #'+workspace/new
       :desc "New named workspace"          "N"     #'+workspace/new-named
       :desc "Load workspace from file"     "l"     #'+workspace/load
       :desc "Save workspace to file"       "s"     #'+workspace/save
       :desc "Kill session"                 "x"     #'+workspace/kill-session
       :desc "Kill this workspace"          "d"     #'+workspace/kill
       :desc "Kill this workspace"          "k"     #'+workspace/kill
       :desc "Delete saved workspace"       "D"     #'+workspace/delete
       :desc "Rename workspace"             "r"     #'+workspace/rename
       :desc "Restore last session"         "R"     #'+workspace/restore-last-session
       :desc "Next workspace"               "]"     #'+workspace/switch-right
       :desc "Previous workspace"           "["     #'+workspace/switch-left)

      ;; <leader> l --- LLM (GPTel)
      (:prefix-map ("l" . "llm") :when (modulep! :tools llm)
       :desc "Add text to context"          "a"     #'gptel-add
       :desc "Explain"                      "e"     #'gptel-quick
       :desc "Add file to context"          "f"     #'gptel-add-file
       :desc "Open gptel"                   "l"     #'gptel
       :desc "Send to gptel"                "s"     #'gptel-send
       :desc "Open gptel menu"              "m"     #'gptel-menu
       :desc "Rewrite"                      "r"     #'gptel-rewrite
       :desc "Org: set topic"               "o"     #'gptel-org-set-topic
       :desc "Org: set properties"          "O"     #'gptel-org-set-properties)

      ;;; <leader> y --- Yank (copy)
      (:prefix-map ("y" . "yank/copy")
       :desc "Yank from clipboard history"  "y"     #'+default/yank-pop
       :desc "Yank buffer content"          "c"     #'+default/yank-buffer-contents
       :desc "Yank buffer name"             "b"     #'my/yank-buffer-name
       :desc "Yank directory path"          "d"     #'my/yank-directory-path
       :desc "Yank file name"               "f"     #'my/yank-file-name
       :desc "Yank file name (no ext)"      "F"     #'my/yank-file-name-no-ext
       :desc "Yank file path"               "p"     #'+default/yank-buffer-path
       :desc "Yank file path (project)"     "P"     #'+default/yank-buffer-path-relative-to-project
       (:when (modulep! :lang org)
         :desc "Yank org heading link"      "h"     #'+org/copy-link-to-heading
         :desc "Yank org subtree"           "s"     #'org-copy-subtree)
       )

      ;;; <leader> n --- notes
      :desc "Obsidian notes"                "n SPC" #'obsidian-jump

      ;;; <leader> q --- quit/session
      :desc "Start new Emacs"               "q n"   #'restart-emacs-start-new-emacs)


;; ─── goto prefix ──────────────────────────────────────────────────────────────

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


;; ─── Modules ──────────────────────────────────────────────────────────────────

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

(map! :map dired-mode-map
      :ng   "s-<down>"       #'dired-find-file
      :ng   "s-<up>"         #'dired-up-directory)

(map! :map dirvish-mode-map
      :ng   [backspace]      #'dirvish-history-go-backward
      :ng   [s-backspace]    #'dirvish-history-go-forward)
