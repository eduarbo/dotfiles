;;; +bindings.el -*- lexical-binding: t; -*-

(map! :i    [S-return]      #'+default/newline-above
      :i    "S-SPC"         #'indent-for-tab-command

      :n    "SPC"           #'evil-ex-search-word-forward
      :v    "SPC"           #'evil-visualstar/begin-search-forward
      :nv   "S-SPC"         #'+default/search-project
      ;; :nv   "S-SPC"         #'+default/search-project-for-symbol-at-point

      :v    [S-return]      #'evilnc-comment-operator
      :n    [S-return]      #'evilnc-comment-or-uncomment-lines

      :n    "RET"           #'+fold/toggle

      :nv   ";"             #'evil-ex
      :nv   ":"             #'pp-eval-expression

      ;; :nv   [left]          #'
      ;; :nv   [right]         #'

      :inv  "C-,"           #'evil-snipe-repeat-reverse
      :inv  "C-."           #'evil-snipe-repeat

      :m    "k"             #'evil-previous-visual-line
      :m    "j"             #'evil-next-visual-line

      :nv   "H"             #'evil-first-non-blank-of-visual-line
      :nv   "L"             #'evil-end-of-line-or-visual-line

      :nv   [up]            (λ! (evil-previous-visual-line 10))
      :nv   [down]          (λ! (evil-next-visual-line 10))

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

      :nv   "X"             #'doom/open-project-scratch-buffer

      :n    "="             #'indent-according-to-mode
      :v    "="             #'evil-indent

      :n    ">"             #'evil-shift-right-line
      :n    "<"             #'evil-shift-left-line

      :gi   "C-a"           #'forward-word
      :gi   "C-a"           #'forward-qword

      :nv   "C-a"           #'evil-numbers/inc-at-pt
      :nv   "C-x"           #'evil-numbers/dec-at-pt

      :gi   "C-f"           #'forward-word
      :gi   "C-b"           #'backward-word

      ;; :gi   "C-d"           #'sp-delete-word
      :gi   "C-d"           #'kill-line
      (:map evil-markdown-mode-map
       :gi   "C-d"           #'kill-line)

      :in   "C-j"           #'evil-window-down
      :in   "C-k"           #'evil-window-up
      :in   "C-l"           #'evil-window-right
      :in   "C-h"           #'evil-window-left
      (:map Info-mode-map
       :in   "C-j"           #'evil-window-down
       :in   "C-k"           #'evil-window-up
       :in   "C-l"           #'evil-window-right
       :in   "C-h"           #'evil-window-left)

      ;; Leaving Emacs state unbound. I feel like a mouse in a maze in there!
      :im    "C-z"          nil
      (:map magit-mode-map
            "C-z" nil))


;; -- macOS shortcuts
(map! :when IS-MAC
      :gn    "s-["           #'previous-buffer
      :gn    "s-]"           #'next-buffer
      (:map (helpful-mode-map)
       :gn    "s-["           #'help-go-back
       :gn    "s-]"           #'help-go-forward)
      (:map Info-mode-map
       :gn    "s-["           #'Info-history-back
       :gn    "s-]"           #'Info-history-forward)

      :mi    "s-{"           #'+workspace/switch-left
      :mi    "s-}"           #'+workspace/switch-right

      :g     "s-,"           #'vertico-repeat
      :g     "s-."           #'+popup/toggle
      :g     [s-backspace]   #'+popup/raise
      :g     "s-h"           #'helpful-key

      ;; map M-x to `general-override-mode-map', which is a special keymap that
      ;; tries to have precedence over most other keymaps (including minor
      ;; modes’), so it doesn’t get overwritten by a careless package/minor mode
      (:map general-override-mode-map
       :g     "s-e"           #'execute-extended-command)

      :g     "s-g"           #'magit-status
      :g     "s-i"           #'+format/region-or-buffer
      :g     "s-j"           #'evil-switch-to-windows-last-buffer
      :g     "s-k"           #'kill-current-buffer
      :g     "s-o"           #'projectile-switch-project
      :g     "s-p"           #'treemacs
      :n     "s-r"           #'+eval/open-repl-other-window
      :v     "s-r"           #'+eval:region
      :g     "s-u"           #'evil-window-mru
      :g     "s-y"           #'+default/yank-pop)


;; -- Smart tab: these will only work in GUI Emacs

(map! :i [tab]     #'company-indent-or-complete-common
      :m [tab]     #'evil-jump-item

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


;; -- Leader

(map! :leader
      :desc "Select a window"                   "w w"           #'ace-window
      :desc "Kill this workspace"               "TAB k"         #'+workspace/delete
      :desc "M-x"                               ";"             #'execute-extended-command
      :desc "Find file in project"              "SPC"           #'projectile-find-file
      :desc "Pop up scratch buffer"             "X"             #'doom/open-project-scratch-buffer)


;; -- [g]o-to prefix

(map! :prefix "g"
      :nv "j"     #'avy-goto-line-below
      :nv "k"     #'avy-goto-line-above
      :nv "n"     #'my/narrow-or-widen-dwim
      :nv "o"     #'avy-goto-char-timer
      :nv "O"     (λ! (let ((avy-all-windows t)) (avy-goto-char-timer)))
      :nv "s"     #'evil-snipe-s
      :nv "S"     #'evil-snipe-S
      :nv "w"     #'transpose-words)


;; -- Modules

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
