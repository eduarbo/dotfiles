;;; editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

;;                 ▄▄▄▄· ▪   ▐ ▄ ·▄▄▄▄  ▪   ▐ ▄  ▄▄ • .▄▄ ·
;;                 ▐█ ▀█▪██ •█▌▐███▪ ██ ██ •█▌▐█▐█ ▀ ▪▐█ ▀.
;;                 ▐█▀▀█▄▐█·▐█▐▐▌▐█· ▐█▌▐█·▐█▐▐▌▄█ ▀█▄▄▀▀▀█▄
;;                 ██▄▪▐█▐█▌██▐█▌██. ██ ▐█▌██▐█▌▐█▄▪▐█▐█▄▪▐█
;;                 ·▀▀▀▀ ▀▀▀▀▀ █▪▀▀▀▀▀• ▀▀▀▀▀ █▪·▀▀▀▀  ▀▀▀▀

;;                  == Spacemacs-esque keybinding scheme ==

(defvar +org-log-buffer-mode-map (make-sparse-keymap))
(defvar +org-format-map (make-sparse-keymap))
(defvar +markdown-format-map (make-sparse-keymap))
(defvar +company-omni-completion-map (make-sparse-keymap))

(setq doom-leader-key ","
      doom-localleader-key ", m")

(when (modulep! :editor evil +everywhere)
  ;; NOTE SPC u replaces C-u as the universal argument.

  ;; Minibuffer
  (map! :map (evil-ex-completion-map evil-ex-search-keymap)
        "C-a"      #'evil-beginning-of-line
        "C-b"      #'evil-backward-char
        "C-f"      #'evil-forward-char
        :gi "C-j"  #'next-complete-history-element
        :gi "C-k"  #'previous-complete-history-element)

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-a"    #'move-beginning-of-line
    "C-r"    #'evil-paste-from-register
    "C-u"    #'evil-delete-back-to-indentation
    "C-v"    #'yank
    "C-w"    #'doom/delete-backward-word
    "C-z"    (cmd! (ignore-errors (call-interactively #'undo))))

  (define-key! :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line
    "C-S-j"  #'scroll-up-command
    "C-S-k"  #'scroll-down-command)
  ;; For folks with `evil-collection-setup-minibuffer' enabled
  (define-key! :states 'insert :keymaps +default-minibuffer-maps
    "C-j"    #'next-line
    "C-k"    #'previous-line)
  (define-key! read-expression-map
    "C-j"    #'next-line-or-history-element
    "C-k"    #'previous-line-or-history-element))

;; Easier window navigation
(map! :map (general-override-mode-map ranger-mode-map magit-mode-map comint-mode-map org-agenda-keymap)
      :mnv  "C-h"           #'evil-window-left
      :mnv  "C-j"           #'evil-window-down
      :mnv  "C-k"           #'evil-window-up
      :mnv  "C-l"           #'evil-window-right)

;; Sane defaults
(map! :m    ":"             #'execute-extended-command
      :n    ";"             #'evil-ex

      ;; :nv   "#"             #'comment-dwim
      :n    "#"             #'evilnc-comment-or-uncomment-lines
      :v    "#"             #'evilnc-comment-operator

      :n    "!"             #'async-shell-command

      :nv   "SPC"           #'+default/search-project-for-symbol-at-point
      :nv   "S-SPC"         #'+default/search-project

      :i    "S-SPC"         #'tab-to-tab-stop

      :nv   [S-return]      #'flyspell-correct-at-point
      :gim  "C-s"           #'save-buffer)

;; Text objects
(map! :gi   [C-backspace]  #'delete-forward-char

      :gi   "C-f"          #'forward-word
      :gi   "C-b"          #'backward-word

      :m    "H"            #'sp-backward-symbol
      :m    "L"            #'sp-forward-symbol
      :gim  "C-h"          #'sp-backward-symbol
      :gim  "C-l"          #'sp-forward-symbol

      :gi   "C-d"          #'evil-delete-line
      :gi   "C-S-d"        #'evil-delete-whole-line
      :gi   "C-S-u"        #'evil-change-whole-line
      :gi   "C-S-w"        #'backward-kill-sexp

      :gi   "C-S-a"        #'sp-beginning-of-sexp
      :gi   "C-S-e"        #'sp-end-of-sexp

      :gi   "C-t"          #'transpose-chars
      :nv   "C-a"          #'evil-numbers/inc-at-pt
      :nv   "C-S-a"        #'evil-numbers/dec-at-pt

      :m    "k"            #'evil-previous-visual-line
      :m    "j"            #'evil-next-visual-line
      :m    [down]         #'previous-buffer
      :m    [up]           #'next-buffer
      :m    [right]        #'+workspace/switch-right
      :m    [left]         #'+workspace/switch-left

      :n    "s"            #'evil-surround-edit
      :v    "s"            #'evil-surround-region

      ;; expand-region
      :v    "v"            (general-predicate-dispatch 'er/expand-region
                             (eq (evil-visual-type) 'line)
                             'evil-visual-char)
      :v    "C-v"          #'er/contract-region)


;;
;;; Global keybindings

;; macOS shortcuts
(map!
 :when IS-MAC

 :g     "s-e"           #'execute-extended-command
 :g     "s-y"           #'+default/yank-pop
 :g     "s-`"           #'helpful-key
 :g     "s-."           #'treemacs
 :g     "s->"           #'treemacs-select-window

 (:map org-mode-map
  :g    "s-o"           #'+org/insert-item-below
  :g    "s-O"           #'+org/insert-item-above)

 (:map prog-mode-map
  :gi    "s-o"           #'+default/newline
  :gi    "s-O"           #'+default/newline-above)

 ;; Available keys: bm
 :gn     "s-a"           #'other-window
 :gn     "s-g"           #'magit-status
 :gn     "s-G"           #'magit-status-here
 :gn     "s-h"           #'previous-buffer
 :gn     "s-H"           #'+workspace/switch-left
 :gm     "s-i"           #'org-capture
 :gm     "s-I"           #'org-journal-new-entry
 :gn     "s-j"           #'evil-switch-to-windows-last-buffer
 :gn     "s-J"           #'+workspace/other
 :gn     "s-k"           #'kill-current-buffer
 :gn     "s-K"           #'doom/kill-buried-buffers
 :gn     "s-l"           #'next-buffer
 :gn     "s-L"           #'+workspace/switch-right
 :gn     "s-p"           #'doom/open-project-scratch-buffer
 :gn     "s-P"           #'doom/switch-to-project-scratch-buffer
 :gn     "s-r"           #'+eval/open-repl-other-window
 :gn     "s-R"           #'+eval/open-repl-same-window
 :gv     "s-r"           #'+eval:region
 :gv     "s-R"           #'+eval/buffer
 :gn     "s-u"           #'evil-window-mru
 :gn     "s-U"           #'delete-other-windows
 :gn     "s-x"           #'doom/open-scratch-buffer
 :gn     "s-X"           #'doom/switch-to-scratch-buffer

 :gn     "s-;"           #'doom/reset-font-size
 :gn     "s-:"           #'doom/reset-font-size
 ;; Frame-local font resizing
 :gn     "s-["           #'text-scale-decrease
 :gn     "s-]"           #'text-scale-increase
 ;; Buffer-local font resizing
 :gn     "s-{"           #'doom/decrease-font-size
 :gn     "s-}"           #'doom/increase-font-size

 :gn     "s-,"           #'doom/find-file-in-private-config
 :gn     "s-<"           #'my/find-file-in-dotfiles
 :gm     [s-up]          #'drag-stuff-up
 :gm     [s-down]        #'drag-stuff-down
 :gm     [s-left]        #'drag-stuff-left
 :gm     [s-right]       #'drag-stuff-right)

;; Smart tab, these will only work in GUI Emacs
(map! :i [tab] (cmds! (and (modulep! :editor snippets)
                           (yas-maybe-expand-abbrev-key-filter 'yas-expand))
                      #'yas-expand
                      (and (bound-and-true-p company-mode)
                           (modulep! :completion company))
                      ;; #'+company/complete)
                      #'company-indent-or-complete-common)

      :m [tab] (cmds! (and (modulep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
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
                      it
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      :i [S-tab] +company-omni-completion-map

      :m [S-tab] (cmds! (and (modulep! :editor snippets)
                             (evil-visual-state-p)
                             (or (eq evil-visual-selection 'line)
                                 (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                        #'yas-insert-snippet
                        (and (modulep! :editor fold)
                             (save-excursion (end-of-line) (invisible-p (point))))
                        #'+fold/toggle)

      ;; Smarter C-a/C-e for both Emacs and Evil. C-a will jump to indentation.
      ;; Pressing it again will send you to the true bol. Same goes for C-e, except
      ;; it will ignore comments+trailing whitespace before jumping to eol.
      :gi "C-a" #'doom/backward-to-bol-or-indent
      :gi "C-e" #'doom/forward-to-last-non-comment-or-eol

      (:after treemacs :map treemacs-mode-map
       :g "C-h"     #'evil-window-left
       :g "C-l"     #'evil-window-right)
      (:after help :map help-mode-map
       :n "o"       #'link-hint-open-link)
      (:after helpful :map helpful-mode-map
       :n "o"       #'link-hint-open-link)
      (:after info :map Info-mode-map
       :n "o"       #'link-hint-open-link)
      (:after apropos :map apropos-mode-map
       :n "o"       #'link-hint-open-link
       :n "TAB"     #'forward-button
       :n [tab]     #'forward-button
       :n [backtab] #'backward-button)
      (:after view :map view-mode-map
       [escape]  #'View-quit-all)
      (:after man :map Man-mode-map
       :n "q"    #'kill-current-buffer)
      (:after geiser-doc :map geiser-doc-mode-map
       :n "o"    #'link-hint-open-link)

      (:unless (modulep! :input layout +bepo)
       (:after (evil-org evil-easymotion)
        :map evil-org-mode-map
        :m "gsh" #'+org/goto-visible))

      ;; [g]o-to prefix
      (:prefix "g"
       :n  "."     #'call-last-kbd-macro
       :nv "j"     #'avy-goto-line-below
       :n  "J"     #'join-line
       :nv "k"     #'avy-goto-line-above
       :nv "n"     #'my/narrow-or-widen-dwim
       :nv "o"     #'avy-goto-char-timer
       :nv "O"     (λ! (let ((avy-all-windows t)) (avy-goto-char-timer)))
       :nv "Q"     #'my/unfill-paragraph
       :nv "s"     #'evil-snipe-s
       :nv "S"     #'evil-snipe-S
       :nv "w"     #'transpose-words
       :nv "X"     #'transpose-sexps
       :v  [tab]   #'evil-vimish-fold/create
       :n  [tab]   #'evil-vimish-fold/delete
       :n  [S-tab] #'evil-vimish-fold/delete-all)

      (:when (modulep! :editor multiple-cursors)
       :prefix "gz"
       :nv "d" #'evil-mc-make-and-goto-next-match
       :nv "D" #'evil-mc-make-and-goto-prev-match
       :nv "j" #'evil-mc-make-cursor-move-next-line
       :nv "k" #'evil-mc-make-cursor-move-prev-line
       :nv "m" #'evil-mc-make-all-cursors
       :nv "n" #'evil-mc-make-and-goto-next-cursor
       :nv "N" #'evil-mc-make-and-goto-last-cursor
       :nv "p" #'evil-mc-make-and-goto-prev-cursor
       :nv "P" #'evil-mc-make-and-goto-first-cursor
       :nv "q" #'evil-mc-undo-all-cursors
       :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
       :nv "u" #'+multiple-cursors/evil-mc-undo-cursor
       :nv "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
       :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
       :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)

      (:after evil-snipe
       :n "S"     #'evil-snipe-repeat
       (:map evil-snipe-parent-transient-map
        :n "o"    #'link-hint-open-link
        "L"       #'evil-snipe-repeat
        "H"       #'evil-snipe-repeat-reverse

        "S" (λ! (require 'evil-easymotion)
                (call-interactively
                 (evilem-create 'evil-snipe-repeat
                                :bind ((evil-snipe-scope 'whole-buffer)
                                       (evil-snipe-enable-highlight)
                                       (evil-snipe-enable-incremental-highlight)))))

        ;; Don't interfere with my bindings
        ";"  nil
        ","  nil
        ))

      (:after with-editor :map with-editor-mode-map
       :g "s-s"    #'with-editor-finish
       :g "s-k"    #'with-editor-cancel
       :g "s-w"    #'with-editor-cancel)

      (:after wgrep :map wgrep-mode-map
       :g "s-s"    #'wgrep-finish-edit
       :g "s-k"    #'wgrep-abort-changes
       :g "s-w"    #'wgrep-abort-changes))

;;
;;; Module keybinds

;;; :completion
(map! (:when (modulep! :completion company)
       :i "C-@"    (cmds! (not (minibufferp)) #'company-complete-common)
       :i "C-SPC"  (cmds! (not (minibufferp)) #'company-complete-common)
       (:after company
        (:map +company-omni-completion-map
         "d"    #'company-ispell
         "f"    #'company-files
         "k"    #'+company/dict-or-keywords
         "l"    #'+company/whole-lines
         "]"    #'company-etags
         "s"    #'company-yasnippet
         "o"    #'company-capf
         "n"    #'+company/dabbrev
         "p"    #'+company/dabbrev-code-previous)

        (:map company-active-map
         "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
         "C-e"     #'company-select-last
         "C-a"     #'company-select-first
         "C-n"     #'company-select-next
         "C-p"     #'company-select-previous
         "C-j"     #'company-select-next
         "C-k"     #'company-select-previous
         "C-h"     #'company-show-doc-buffer
         "C-u"     #'company-previous-page
         "C-h"     #'company-previous-page
         "C-d"     #'company-next-page
         "C-l"     #'company-next-page
         "C-s"     #'company-filter-candidates
         "C-S-s"   #'+company/completing-read
         "C-SPC"   #'company-complete-common
         "TAB"     #'company-complete-common-or-cycle
         [tab]     #'company-complete-common-or-cycle
         [backtab] #'company-select-previous
         [S-tab]   #'company-select-previous
         [escape]  #'company-abort
         [f1]      nil)
        (:map company-search-map  ; applies to `company-filter-map' too
         "C-n"     #'company-select-next-or-abort
         "C-p"     #'company-select-previous-or-abort
         "C-j"     #'company-select-next-or-abort
         "C-k"     #'company-select-previous-or-abort
         "C-s"     #'company-filter-candidates
         [escape]  #'company-search-abort)))

      (:when (modulep! :completion ivy)
       (:after ivy
        :map ivy-minibuffer-map
        "C-SPC" #'ivy-call-and-recenter  ; preview file
        "C-l"   #'ivy-alt-done
        "C-v"   #'yank)
       (:after counsel
        :map counsel-ag-map
        "C-SPC"    #'ivy-call-and-recenter ; preview
        "C-l"      #'ivy-done
        [C-return] #'+ivy/git-grep-other-window-action))
      (:when (modulep! :completion helm)
       (:after helm :map helm-map
        [remap next-line]     #'helm-next-line
        [remap previous-line] #'helm-previous-line
        [left]     #'left-char
        [right]    #'right-char
        "C-S-f"    #'helm-previous-page
        "C-S-n"    #'helm-next-source
        "C-S-p"    #'helm-previous-source
        (:when (modulep! :editor evil +everywhere)
         "C-j"    #'helm-next-line
         "C-k"    #'helm-previous-line
         "C-S-j"  #'helm-next-source
         "C-S-k"  #'helm-previous-source)
        "C-u"      #'helm-delete-minibuffer-contents
        "C-s"      #'helm-minibuffer-history
        ;; Swap TAB and C-z
        "TAB"      #'helm-execute-persistent-action
        [tab]      #'helm-execute-persistent-action
        "C-z"      #'helm-select-action)
       (:after helm-ag :map helm-ag-map
        "C--"      #'+helm-do-ag-decrease-context
        "C-="      #'+helm-do-ag-increase-context
        [left]     nil
        [right]    nil)
       (:after helm-files :map (helm-find-files-map helm-read-file-map)
        [C-return] #'helm-ff-run-switch-other-window
        "C-w"      #'helm-find-files-up-one-level
        (:when (modulep! :editor evil +everywhere)
         "C-h"    #'helm-find-files-up-one-level
         "C-l"    #'helm-execute-persistent-action))
       (:after helm-locate :map helm-generic-files-map
        [C-return] #'helm-ff-run-switch-other-window)
       (:after helm-buffers :map helm-buffer-map
        [C-return] #'helm-buffer-switch-other-window)
       (:after helm-occur :map helm-occur-map
        [C-return] #'helm-occur-run-goto-line-ow)
       (:after helm-grep :map helm-grep-map
        [C-return] #'helm-grep-run-other-window-action))

      (:when (modulep! :completion vertico)
       (:after vertico
        :map vertico-map
        [S-return] #'vertico-exit-input
        "TAB"   #'+vertico/embark-preview
        [tab]   #'+vertico/embark-preview
        [S-tab] #'+vertico/embark-export-write
        "C-l"   #'vertico-next-group
        "C-h"   #'vertico-previous-group
        "C-d"   #'vertico-scroll-up
        "C-u"   #'vertico-scroll-down
        "C-j"   #'vertico-next
        "C-k"   #'vertico-previous
        )))


;;; :ui
(map!  (:when (modulep! :ui workspaces)
        :n "C-t"   #'+workspace/new
        :n "C-S-t" #'+workspace/display
        :g "M-1"   #'+workspace/switch-to-0
        :g "M-2"   #'+workspace/switch-to-1
        :g "M-3"   #'+workspace/switch-to-2
        :g "M-4"   #'+workspace/switch-to-3
        :g "M-5"   #'+workspace/switch-to-4
        :g "M-6"   #'+workspace/switch-to-5
        :g "M-7"   #'+workspace/switch-to-6
        :g "M-8"   #'+workspace/switch-to-7
        :g "M-9"   #'+workspace/switch-to-8
        :g "M-0"   #'+workspace/switch-to-final
        (:when IS-MAC
         :g "s-t"   #'+workspace/new
         :g "s-T"   #'+workspace/display
         :n "s-1"   #'+workspace/switch-to-0
         :n "s-2"   #'+workspace/switch-to-1
         :n "s-3"   #'+workspace/switch-to-2
         :n "s-4"   #'+workspace/switch-to-3
         :n "s-5"   #'+workspace/switch-to-4
         :n "s-6"   #'+workspace/switch-to-5
         :n "s-7"   #'+workspace/switch-to-6
         :n "s-8"   #'+workspace/switch-to-7
         :n "s-9"   #'+workspace/switch-to-8
         :n "s-0"   #'+workspace/switch-to-final)))

;;; :editor
(map! (:when (modulep! :editor format)
       :n "gQ" #'+format:region)

      (:when (modulep! :editor rotate-text)
       :n "]r"  #'rotate-text
       :n "[r"  #'rotate-text-backward)

      (:when (modulep! :editor multiple-cursors)
       ;; evil-multiedit
       :v  "R"     #'evil-multiedit-match-all
       :n  "s-d"   #'evil-multiedit-match-symbol-and-next
       :n  "s-D"   #'evil-multiedit-match-symbol-and-prev
       :v  "s-d"   #'evil-multiedit-match-and-next
       :v  "s-D"   #'evil-multiedit-match-and-prev
       :nv "C-M-d" #'evil-multiedit-restore
       (:after evil-multiedit
        (:map evil-multiedit-mode-map
         :nv "M-d" #'evil-multiedit-match-and-next
         :nv "M-D" #'evil-multiedit-match-and-prev
         [return]  #'evil-multiedit-toggle-or-restrict-region)))

      (:when (modulep! :editor snippets)
       ;; auto-yasnippet
       :i  [C-tab] #'aya-expand
       :nv [C-tab] #'aya-create

       (:map snippet-mode-map
        "s-s"           #'yas-load-snippet-buffer-and-close
        "s-k"           #'+snippet--abort
        "s-w"           #'+snippet--abort))

      (:after yasnippet :map yas-keymap
       ;; Do not interfer with company
       [tab]         nil
       "TAB"         nil
       [S-tab]       nil
       "<S-tab>"     nil
       "C-n"         #'yas-next-field
       "C-l"         #'yas-next-field
       "C-p"         #'yas-prev-field
       "C-h"         #'yas-prev-field))

;;; :tools
(map! :after git-timemachine :map git-timemachine-mode-map
      :n "s-h"     #'git-timemachine-show-previous-revision
      :n "s-l"     #'git-timemachine-show-next-revision

      :n "[["   #'git-timemachine-show-previous-revision
      :n "]]"   #'git-timemachine-show-next-revision)


;;
;;; <leader>

(map! :leader
      ;; NOTE available keys: klrvyz
      :desc "Eval expression"       ";"    #'pp-eval-expression
      :desc "M-x"                   ":"    #'execute-extended-command
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Switch to scratch buffer"     "X"  #'doom/switch-to-scratch-buffer
      :desc "Org Captur[e]"         "e"    #'org-capture
      ;; C-u is used by evil
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "window"                "w"    nil
      :desc "help"                  "h"    help-map
      :desc "Jump to Bookmark"      "j"    #'consult-bookmark
      :desc "Delete Bookmark"       "J"    #'bookmark-delete

      :desc "Find file"             "."    #'find-file
      :desc "Find file from here"   ">"    #'+default/find-file-under-here
      :desc "Switch buffer"         ","    #'switch-to-buffer
      (:when (modulep! :ui workspaces)
       :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
       :desc "Switch buffer"           "<" #'switch-to-buffer)
      :desc "Jump to a marker"      "'"    #'consult-mark
      :desc "Resume last search"    "`"
      (cond ((modulep! :completion vertico)    #'vertico-repeat)
            ((modulep! :completion ivy)        #'ivy-resume)
            ((modulep! :completion helm)       #'helm-resume))

      :desc "Find file in project"  "SPC"  #'projectile-find-file
      (:when (modulep! :ui popup)
       :desc "Toggle last popup"    "RET"       #'+popup/toggle
       :desc "Toggle last popup"    [S-return]  #'+popup/raise)

      ;;; <leader> w --- window
      ;; TODO remap C-w too
      (:prefix ("w" . "window")
       ;; Resize
       :desc "Balance windows"          "w"         #'balance-windows
       :desc "Enlarge"                  "o"         #'doom/window-enlargen
       :desc "Enlarge horizontally"     "e"         #'evil-window-set-width
       :desc "Enlarge vertically"       "E"         #'evil-window-set-height
       :desc "Increase width"           "["         #'evil-window-increase-width
       :desc "Decrease width"           "]"         #'evil-window-decrease-width
       :desc "Increase height"          "+"         #'evil-window-increase-height
       :desc "Decrease height"          "-"         #'evil-window-decrease-height

       ;; Zoom
       :desc "Maximize"                 "m"         #'doom/window-maximize-buffer
       :desc "Maximize horizontally"    "f"         #'doom/window-maximize-horizontally
       :desc "Maximize vertically"      "F"         #'doom/window-maximize-vertically

       ;; Navigation
       :desc "Switch to left"           "h"         #'evil-window-left
       :desc "Switch to right"          "l"         #'evil-window-right
       :desc "Switch to upper"          "k"         #'evil-window-up
       :desc "Switch to lower"          "j"         #'evil-window-down
       :desc "Select last used buffer"  "b"         #'evil-window-mru
       :desc "Switch window"            "a"         #'other-window

       ;; Movement
       :desc "Swap left"                "H"         #'+evil/window-move-left
       :desc "Swap right"               "L"         #'+evil/window-move-right
       :desc "Swap upward"              "K"         #'+evil/window-move-up
       :desc "Swap downward"            "J"         #'+evil/window-move-down
       :desc "Rotate downwards"         ","         #'evil-window-rotate-downwards
       :desc "Rotate upwards"           "."         #'evil-window-rotate-upwards
       :desc "Ace swap window"          "A"         #'ace-swap-window

       ;; Misc
       :desc "Delete"                   "d"         #'evil-window-delete
       :desc "Ace delete"               "D"         #'ace-delete-window
       :desc "Undo window changes"      "u"         #'winner-undo
       :desc "Redo window changes"      "r"         #'winner-redo
       :desc "Split new horizontally"   "n"         #'evil-window-new
       :desc "Split new vertically"     "N"         #'evil-window-vnew
       :desc "Split horizontally"       "s"         #'evil-window-split
       :desc "Split vertically"         "v"         #'evil-window-vsplit
       :desc "Split horizontally & focus" "S"       #'+evil/window-split-and-follow
       :desc "Split vertically & focus"   "V"       #'+evil/window-vsplit-and-follow)

      ;;; <leader> TAB --- workspace
      (:when (modulep! :ui workspaces)
       (:prefix-map ("TAB" . "workspace")
        :desc "Display tab bar"           "TAB" #'+workspace/display
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "New named workspace"       "N"   #'+workspace/new-named
        :desc "Load workspace from file"  "l"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Kill this workspace"       "k"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "]"   #'+workspace/switch-right
        :desc "Previous workspace"        "["   #'+workspace/switch-left))

      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
       :desc "Toggle narrowing"            "RET" #'my/narrow-or-widen-dwim
       :desc "Previous buffer"             "["   #'previous-buffer
       :desc "Next buffer"                 "]"   #'next-buffer
       (:when (modulep! :ui workspaces)
        :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
        :desc "Switch buffer"           "B" #'switch-to-buffer
        :desc "ibuffer workspace"       "I" #'+ibuffer/open-for-current-workspace)
       (:unless (modulep! :ui workspaces)
        :desc "Switch buffer"           "b" #'switch-to-buffer)
       :desc "Kill buffer"                 "d"   #'kill-current-buffer
       :desc "ibuffer"                     "i"   #'ibuffer
       :desc "Kill buffer"                 "k"   #'kill-current-buffer
       :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
       :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
       :desc "Set bookmark"                "m"   #'bookmark-set
       :desc "Delete bookmark"             "M"   #'bookmark-delete
       :desc "New empty buffer"            "n"   #'evil-buffer-new
       :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
       :desc "Revert buffer"               "r"   #'revert-buffer
       :desc "Save buffer"                 "s"   #'basic-save-buffer
       :desc "Save all buffers"            "S"   #'evil-write-all
       :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
       :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
       :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
       :desc "Yank buffer name"            "y"   #'my/yank-buffer-name
       :desc "Yank buffer"                 "Y"   #'+default/yank-buffer-contents
       :desc "Bury buffer"                 "z"   #'bury-buffer
       :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers
       :desc "Next buffer"                 "]"   #'next-buffer
       :desc "Previous buffer"             "["   #'previous-buffer)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
       (:when (and (modulep! :tools lsp) (not (modulep! :tools lsp +eglot)))
        :desc "LSP Execute code action" "a" #'lsp-execute-code-action
        :desc "LSP Organize imports" "o" #'lsp-organize-imports
        (:when (modulep! :completion ivy)
         :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
         :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
        (:when (modulep! :completion helm)
         :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
         :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)
        (:when (modulep! :completion vertico)
         :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols
         :desc "Jump to symbol in any workspace"     "J"   (cmd!! #'consult-lsp-symbols 'all-workspaces))
        (:when (modulep! :ui treemacs +lsp)
         :desc "Errors list"                         "X"   #'lsp-treemacs-errors-list
         :desc "Incoming call hierarchy"             "y"   #'lsp-treemacs-call-hierarchy
         :desc "Outgoing call hierarchy"             "Y"   (cmd!! #'lsp-treemacs-call-hierarchy t)
         :desc "References tree"                     "R"   (cmd!! #'lsp-treemacs-references t)
         :desc "Symbols"                             "S"   #'lsp-treemacs-symbols)
        ;; TODO extract useful commands for quick access
        :desc "LSP"                                 "l"   #'+default/lsp-command-map
        :desc "LSP Rename"                          "r"   #'lsp-rename)
       (:when (modulep! :tools lsp +eglot)
        :desc "LSP Execute code action" "a" #'eglot-code-actions
        :desc "LSP Rename" "r" #'eglot-rename
        :desc "LSP Find declaration"                 "j"   #'eglot-find-declaration
        (:when (modulep! :completion vertico)
         :desc "Jump to symbol in current workspace" "j"   #'consult-eglot-symbols))
       :desc "Compile"                               "c"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Jump to references"                    "D"   #'+lookup/references
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'+default/diagnostics)

      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
       :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
       :desc "Copy this file"              "C"   #'doom/copy-this-file
       :desc "Find directory"              "d"   #'+default/dired
       :desc "Delete this file"            "D"   #'doom/delete-this-file
       :desc "Find file in emacs.d"        "e"   #'doom/find-file-in-emacsd
       :desc "Browse emacs.d"              "E"   #'doom/browse-in-emacsd
       :desc "Find file"                   "f"   #'find-file
       :desc "Find file from here"         "F"   #'+default/find-file-under-here
       :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
       :desc "Browse private config"       "P"   #'doom/open-private-config
       :desc "Recent files"                "r"   #'recentf-open-files
       :desc "Rename/move file"            "R"   #'doom/move-this-file
       :desc "Save file"                   "s"   #'save-buffer
       :desc "Save file as..."             "S"   #'write-file
       :desc "Sudo find file"              "u"   #'doom/sudo-find-file
       :desc "Sudo this file"              "U"   #'doom/sudo-this-file
       :desc "Yank file path from project" "y"   #'+default/yank-buffer-path-relative-to-project
       :desc "Yank file path"              "Y"   #'+default/yank-buffer-path
       :desc "Find file in .dotfiles"      "."   #'my/find-file-in-dotfiles)

      ;;; <leader> g --- git/version control
      (:prefix-map ("g" . "git")
       :desc "Revert file"                 "R"   #'vc-revert
       ;; TODO replace it with git-link package
       :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill
       :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
       (:when (modulep! :ui hydra)
        :desc "SMerge"                    "m"   #'+vc/smerge-hydra/body)
       (:when (modulep! :ui vc-gutter)
        (:when (modulep! :ui hydra)
         :desc "VCGutter"                "."   #'+vc/gutter-hydra/body)
        :desc "Revert hunk at point"      "r"   #'+vc-gutter/revert-hunk
        :desc "stage hunk at point"       "s"   #'+vc-gutter/stage-hunk
        :desc "Git time machine"          "t"   #'git-timemachine-toggle
        :desc "Jump to next hunk"         "]"   #'+vc-gutter/next-hunk
        :desc "Jump to previous hunk"     "["   #'+vc-gutter/previous-hunk)
       (:when (modulep! :tools magit)
        :desc "Magit dispatch"            "/"   #'magit-dispatch
        :desc "Magit file dispatch"       "."   #'magit-file-dispatch
        :desc "Forge dispatch"            "'"   #'forge-dispatch
        :desc "Magit blame"               "b"   #'magit-blame-addition
        :desc "Magit status"              "g"   #'magit-status
        :desc "Magit status here"         "G"   #'magit-status-here
        :desc "Magit file delete"         "D"   #'magit-file-delete
        :desc "Magit switch branch"       "B"   #'magit-branch-checkout
        :desc "Magit clone"               "C"   #'magit-clone
        :desc "Magit fetch"               "F"   #'magit-fetch
        :desc "Magit buffer log"          "L"   #'magit-log-buffer-file
        :desc "Git stage file"            "S"   #'magit-stage-file
        :desc "Git unstage file"          "U"   #'magit-unstage-file
        :desc "Timemachine for branch"    "T"   #'git-timemachine-switch-branch
        :desc "Magit diff staged"         "d"   #'magit-diff-buffer-file
        :desc "Magit diff"                "D"   #'magit-diff
        (:prefix ("f" . "find")
         :desc "Find file"                 "f"   #'magit-find-file
         :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
         :desc "Find commit"               "c"   #'magit-show-commit
         :desc "Find issue"                "i"   #'forge-visit-issue
         :desc "Find pull request"         "p"   #'forge-visit-pullreq)
        (:prefix ("o" . "open in browser")
         :desc "Browse file or region"     "o"   #'+vc/browse-at-remote
         :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
         :desc "Browse remote"             "r"   #'forge-browse-remote
         :desc "Browse commit"             "c"   #'forge-browse-commit
         :desc "Browse an issue"           "i"   #'forge-browse-issue
         :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
         :desc "Browse issues"             "I"   #'forge-browse-issues
         :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
        (:prefix ("l" . "list")
         (:when (modulep! :tools gist)
          :desc "List gists"              "g"   #'+gist:list)
         :desc "List repositories"         "r"   #'magit-list-repositories
         :desc "List submodules"           "s"   #'magit-list-submodules
         :desc "List issues"               "i"   #'forge-list-issues
         :desc "List pull requests"        "p"   #'forge-list-pullreqs
         :desc "List notifications"        "n"   #'forge-list-notifications)
        (:prefix ("c" . "create")
         :desc "Initialize repo"           "r"   #'magit-init
         :desc "Clone repo"                "R"   #'magit-clone
         :desc "Commit"                    "c"   #'magit-commit-create
         :desc "Fixup"                     "f"   #'magit-commit-fixup
         :desc "Branch"                    "b"   #'magit-branch-and-checkout
         :desc "Issue"                     "i"   #'forge-create-issue
         :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
       :desc "Emoji"                         "e"   #'emojify-insert-emoji
       :desc "Current file name"             "f"   #'+default/insert-file-path
       :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
       :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
       :desc "From evil register"            "r"   #'evil-show-registers
       :desc "Snippet"                       "s"   #'yas-insert-snippet
       :desc "Unicode"                       "u"   #'insert-char
       :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> l --- language
      (:when (modulep! :config language)
       (:prefix ("l" . "language")
        :desc "Configure translate languages"    ","    #'+language/set-google-translate-languages
        :desc "Translate"                        "t"    #'google-translate-smooth-translate
        :desc "Translate any language"           "a"    #'+language/google-translate-smooth-translate-any
        :desc "Translate from source lang"       "s"    #'google-translate-at-point
        :desc "Translate from destination lang"  "d"    #'google-translate-at-point-reverse))

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
       :desc "Org agenda"                   "a" #'org-agenda
       (:when (modulep! :tools biblio)
        :desc "Bibliographic notes"         "b"
        (cond ((modulep! :completion vertico)  #'citar-open-notes)
              ((modulep! :completion ivy)      #'ivy-bibtex)
              ((modulep! :completion helm)     #'helm-bibtex)))

       :desc "Toggle last org-clock"        "c" #'+org/toggle-last-clock
       :desc "Cancel current org-clock"     "C" #'org-clock-cancel
       :desc "Daily Agenda"                 "d" #'my/daily-agenda
       (:when (modulep! :lang org +noter)
        :desc "Org noter"                   "e" #'org-noter)

       :desc "Find file in notes"           "f" #'+default/find-in-notes
       :desc "Browse notes"                 "F" #'+default/browse-notes
       :desc "Search notes headlines"       "h" #'+org/org-notes-headlines
       :desc "Org store link"               "l" #'org-store-link
       :desc "Tags search"                  "m" #'org-tags-view
       :desc "Org capture"                  "n" #'org-capture
       :desc "Goto capture"                 "N" #'org-capture-goto-target
       :desc "Active org-clock"             "o" #'org-clock-goto
       ;; NOTE This can be replaced by my own agenda templates
       ;; :desc "Todo list"                    "t" #'org-todo-list
       :desc "Search org"                   "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
       :desc "Unscheduled Agenda"           "u" #'my/unscheduled-agenda
       :desc "Org export to md and copy"    "y" (cmd! (+org/export-to-clipboard 'md))
       :desc "Org export to clipboard"      "Y" #'+org/export-to-clipboard
       :desc "Search notes"                 "/" #'+org/org-notes-search

       :desc "Org Roam capture"             "RET"  #'org-roam-capture
       :desc "Find note"                    "SPC"  #'org-roam-node-find

       (:when (modulep! :lang org +roam)
        (:prefix ("r" . "roam")
         :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
         :desc "Org Roam Capture"              "c" #'org-roam-capture
         :desc "Find file"                     "f" #'org-roam-find-file
         :desc "Show graph"                    "g" #'org-roam-graph
         :desc "Insert"                        "i" #'org-roam-insert
         :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
         :desc "Org Roam"                      "r" #'org-roam
         (:prefix ("d" . "by date")
          :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
          :desc "Today"          "t" #'org-roam-dailies-find-today
          :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
          :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday)))

       (:when (modulep! :lang org +roam2)
        (:prefix ("r" . "roam")
         :desc "Open random node"           "a" #'org-roam-node-random
         :desc "Find node"                  "f" #'org-roam-node-find
         :desc "Find ref"                   "F" #'org-roam-ref-find
         :desc "Show graph"                 "g" #'org-roam-graph
         ;; :desc "Insert node"                "i" #'org-roam-node-insert
         :desc "Capture to node"            "n" #'org-roam-capture
         :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
         :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
         :desc "Sync database"              "s" #'org-roam-db-sync
         (:prefix ("d" . "by date")
          :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
          :desc "Capture date"              "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"             "n" #'org-roam-dailies-capture-today
          :desc "Goto today"                "t" #'org-roam-dailies-goto-today
          :desc "Capture today"             "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))

       (:when (modulep! :lang org +journal)
        (:prefix ("j" . "journal")
         :desc "Date journal"        "d" #'org-journal-new-date-entry
         :desc "New Entry"           "j" #'org-journal-new-entry
         :desc "New Scheduled Entry" "s" #'org-journal-new-scheduled-entry
         :desc "Search Forever"      "f" #'org-journal-search-forever)))

      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
       :desc "Org agenda"       "A"  #'org-agenda
       (:prefix ("a" . "org agenda")
        :desc "Agenda"         "a"  #'org-agenda
        :desc "Todo list"      "t"  #'org-todo-list
        :desc "Tags search"    "m"  #'org-tags-view
        :desc "View search"    "v"  #'org-search-view)
       :desc "Default browser"    "b"  #'browse-url-of-file
       :desc "Start debugger"     "d"  #'+debugger/start
       :desc "New frame"          "f"  #'make-frame
       :desc "Select frame"       "F"  #'select-frame-by-name
       :desc "REPL"               "r"  #'+eval/open-repl-other-window
       :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
       :desc "Dired"              "-"  #'dired-jump
       :desc "Shell command"                 "s"        #'async-shell-command
       :desc "Shell command in project root" "S"        #'projectile-run-async-shell-command-in-root

       (:when (modulep! :ui neotree)
        :desc "Project sidebar"                 "p" #'+neotree/open
        :desc "Find file in project sidebar"    "P" #'+neotree/find-this-file)
       (:when (modulep! :ui treemacs)
        :desc "Project sidebar"                 "p" #'treemacs
        ;; :desc "Find in project sidebar"         "P" #'treemacs-find-file
        :desc "Select in project sidebar"       "P" #'treemacs-select-window)
       (:when (modulep! :term shell)
        :desc "Toggle shell popup"    "t" #'+shell/toggle
        :desc "Open shell here"       "T" #'+shell/here)
       (:when (modulep! :term term)
        :desc "Toggle terminal popup" "t" #'+term/toggle
        :desc "Open terminal here"    "T" #'+term/here)
       (:when (modulep! :term vterm)
        :desc "Toggle vterm popup"    "t" #'+vterm/toggle
        :desc "Open vterm here"       "T" #'+vterm/here)
       (:when (modulep! :term eshell)
        :desc "Toggle eshell popup"   "e" #'+eshell/toggle
        :desc "Open eshell here"      "E" #'+eshell/here)
       (:when (modulep! :os macos)
        :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
        :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
        :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
        :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
        :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
        :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar
        :desc "Open in iTerm"              "i" #'+macos/open-in-iterm
        :desc "Open in new iTerm window"   "I" #'+macos/open-in-iterm-new-window)
       (:when (modulep! :tools docker)
        :desc "Docker" "D" #'docker)
       (:when (modulep! :email mu4e)
        :desc "mu4e" "m" #'=mu4e)
       (:when (modulep! :email notmuch)
        :desc "notmuch" "m" #'=notmuch)
       (:when (modulep! :email wanderlust)
        :desc "wanderlust" "m" #'=wanderlust))

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
       :desc "Browse project"               "." #'+default/browse-project
       :desc "Browse other project"         ">" #'doom/browse-in-other-project
       :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
       :desc "Async cmd in project root"    "&" #'projectile-run-async-shell-command-in-root
       :desc "Add new project"              "a" #'projectile-add-known-project
       :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
       :desc "Compile in project"           "c" #'projectile-compile-project
       :desc "Repeat last command"          "C" #'projectile-repeat-last-command
       :desc "Remove known project"         "d" #'projectile-remove-known-project
       :desc "Discover projects in folder"  "D" #'+default/discover-projects
       :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
       :desc "Find file in project"         "f" #'projectile-find-file
       :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
       :desc "Configure project"            "g" #'projectile-configure-project
       :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
       :desc "Kill project buffers"         "k" #'projectile-kill-buffers
       :desc "Open project notes"           "n" #'+org/find-notes-for-project
       :desc "Find other file"              "o" #'projectile-find-other-file
       :desc "Switch project"               "p" #'projectile-switch-project
       :desc "Find recent project files"    "r" #'projectile-recentf
       :desc "Run project"                  "R" #'projectile-run-project
       :desc "Save project files"           "s" #'projectile-save-project-buffers
       :desc "List project todos"           "t" #'magit-todos-list
       :desc "Test project"                 "T" #'projectile-test-project
       :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
       :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
       (:when (and (modulep! :tools taskrunner)
                   (or (modulep! :completion ivy)
                       (modulep! :completion helm)))
        :desc "List project tasks"          "z" #'+taskrunner/project-tasks))

      ;;; <leader> q --- quit/session
      (:prefix-map ("q" . "quit/session")
       :desc "Restart emacs server"         "d" #'+default/restart-server
       :desc "Start new instance of Emacs"  "e" #'restart-emacs-start-new-emacs
       :desc "Delete frame"                 "f" #'delete-frame
       :desc "Clear current frame"          "F" #'doom/kill-all-buffers
       :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
       :desc "Quit Emacs"                   "q" #'save-buffers-kill-terminal
       :desc "Quit Emacs without saving"    "Q" #'evil-quit-all-with-error-code
       :desc "Quick save current session"   "s" #'doom/quicksave-session
       :desc "Restore last session"         "l" #'doom/quickload-session
       :desc "Save session to file"         "S" #'doom/save-session
       :desc "Restore session from file"    "L" #'doom/load-session
       :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
       :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
       :desc "Search buffer"                "b"
       (cond ((modulep! :completion vertico)   #'+default/search-buffer)
             ((modulep! :completion ivy)       #'swiper)
             ((modulep! :completion helm)      #'swiper))
       :desc "Search all open buffers"      "B"
       (cond ((modulep! :completion vertico)   (cmd!! #'consult-line-multi 'all-buffers))
             ((modulep! :completion ivy)       #'swiper-all)
             ((modulep! :completion helm)      #'swiper-all))
       :desc "Search current directory"     "d" #'+default/search-cwd
       :desc "Search other directory"       "D" #'+default/search-other-cwd
       :desc "Search .emacs.d"              "e" #'+default/search-emacsd
       :desc "Jump to symbol"               "i" #'imenu
       :desc "Jump to visible link"         "l" #'link-hint-open-link
       :desc "Jump to link"                 "L" #'ffap-menu
       :desc "Jump list"                    "j" #'evil-show-jumps
       :desc "Jump to bookmark"             "m" #'bookmark-jump
       :desc "Look up online"               "o" #'+lookup/online
       :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
       :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
       :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
       :desc "Search project"               "p" #'+default/search-project
       :desc "Search other project"         "P" #'+default/search-other-project
       :desc "Jump to mark"                 "r" #'evil-show-marks
       :desc "Search buffer"                "s" #'+default/search-buffer
       :desc "Search buffer for thing at point" "S"
       (cond ((modulep! :completion vertico)   #'+vertico/search-symbol-at-point)
             ((modulep! :completion ivy)       #'swiper-isearch-thing-at-point)
             ((modulep! :completion helm)      #'swiper-isearch-thing-at-point))
       :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
       :desc "Thesaurus"                    "T" #'+lookup/synonyms
       :desc "Correct word at point"        "RET" #'flyspell-correct-at-point
       :desc "Add word to dictionary"       "w" #'+spell/add-word
       :desc "Remove word from dictionary"  "W" #'+spell/remove-word)

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
       :desc "Big mode"                     "b" #'doom-big-font-mode
       :desc "Fill Column Indicator"        "c" #'global-display-fill-column-indicator-mode
       :desc "Ispell Dictionary EN/ES"      "d" #'my/cycle-ispell-languages
       :desc "Toggle Org emphasis"          "e" #'+org/toggle-emphasis
       :desc "Flymake"                      "f" #'flymake-mode
       (:when (modulep! :checkers syntax)
        :desc "Flycheck"                   "f" #'flycheck-mode)
       :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
       :desc "Evil goggles"                 "g" #'evil-goggles-mode
       (:when (modulep! :ui indent-guides)
        :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
       :desc "Indent style"                 "I" #'doom/toggle-indent-style
       :desc "Line numbers mode"            "l" #'display-line-numbers-mode
       :desc "Line numbers type"            "L" #'doom/toggle-line-numbers
       (:when (modulep! :ui minimap)
        :desc "Minimap"                      "m" #'minimap-mode)
       :desc "Frame maximized"              "M" #'toggle-frame-maximized
       :desc "Modeline filename style"      "n" #'my/toggle-doom-modeline-buffer-file-name-style
       (:when (modulep! :lang org +present)
        :desc "org-tree-slide mode"        "o" #'org-tree-slide-mode)
       :desc "Project sidebar"              "p" #'treemacs
       (:when (modulep! :lang org +roam2)
        :desc "Roam buffer"                 "r" #'org-roam-buffer-toggle)
       :desc "Read-only mode"               "R" #'read-only-mode
       (:when (and (modulep! :checkers spell) (not (modulep! :checkers spell +flyspell)))
        :desc "Spell checker"              "s" #'spell-fu-mode)
       (:when (modulep! :checkers spell +flyspell)
        :desc "Spell checker"              "s" #'flyspell-mode)
       (:when (modulep! :lang org +pomodoro)
        :desc "Pomodoro timer"             "t" #'org-pomodoro)
       :desc "Visible mode"                 "v" #'visible-mode
       :desc "Visual fill column mode"      "V" #'visual-fill-column-mode
       :desc "Soft line wrapping"           "w" #'visual-line-mode
       :desc "Subword mode"                 "W" #'subword-mode
       (:when (modulep! :editor word-wrap)
        :desc "Soft line wrapping"         "w" #'+word-wrap-mode)
       (:when (modulep! :ui zen)
        :desc "Zen mode"                   "z" #'+zen/toggle
        :desc "Zen mode (fullscreen)"      "Z" #'+zen/toggle-fullscreen)))

(after! which-key
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))

;;
;;; org-mode

;; Setup minor mode for org note log buffers
(define-minor-mode +org-log-buffer-mode
  "Minor mode for org note log buffers"
  :keymap +org-log-buffer-mode-map)
(add-hook! 'org-log-buffer-setup-hook #'+org-log-buffer-mode)

(map!
 (:after org
  ;; log entry buffer (Tracking todo state changes)
  (:map +org-log-buffer-mode-map
   "s-s"           #'org-ctrl-c-ctrl-c
   "s-k"           #'org-kill-note-or-show-branches
   "s-w"           #'org-kill-note-or-show-branches)
  ;; editing source blocks
  (:map org-src-mode-map
   "s-s"           #'org-edit-src-save
   "s-k"           #'org-edit-src-abort
   "s-w"           #'org-edit-src-exit
   (:leader
    :desc "Save file"   "fs"    #'org-edit-src-save))
  ;; capture buffer
  (:map org-capture-mode-map
   "s-s"           #'org-capture-finalize
   "s-k"           #'org-capture-kill
   "s-w"           #'org-capture-kill
   "s-r"           #'org-capture-refile)

  (:map org-mode-map
   :nvi "s-r"      #'org-roam-refile
   :nvi "s-R"      #'+org/refile-to-running-clock

   :n [return]     #'+org/dwim-at-point

   [s-up]          #'org-metaup
   [s-down]        #'org-metadown
   :gi [s-left]    #'org-shiftmetaleft
   :gi [s-right]   #'org-shiftmetaright

   :n "H"          #'org-metaleft
   :n "L"          #'org-metaright)

  (:map +org-format-map
   ;; Basic char syntax:
   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
   "b"   (org-emphasize! +org/bold ?*)
   "i"   (org-emphasize! +org/italic ?/)
   "m"   (org-emphasize! +org/monospace ?~)  ;; monospace/code
   "u"   (org-emphasize! +org/underline ?_)
   "v"   (org-emphasize! +org/verbose ?=)
   "s"   (org-emphasize! +org/strike-through ?+)
   ;; FIXME doesn't work well in normal mode
   "x"   (org-emphasize! +org/restore-format ? )
   "r"   #'org-roam-insert
   "R"   #'org-roam-insert-immediate
   "c"   #'org-cliplink
   "d"   #'org-download-clipboard
   "D"   #'org-download-yank
   "k"   #'org-insert-link
   "K"   #'+org/remove-link))

 (:after evil-org
  (:map evil-org-mode-map
   :n "gk" (cmd! (if (org-on-heading-p)
                     (org-backward-element)
                   (evil-previous-visual-line)))
   :n "gj" (cmd! (if (org-on-heading-p)
                     (org-forward-element)
                   (evil-next-visual-line)))

   :g "s-f"     +org-format-map
   :n "C-i"     #'evil-jump-forward

   (:localleader
    :desc "Add note to the current entry"    "n"   #'org-add-note
    :desc "format"                           "f"   +org-format-map
    :desc "TODO"                             "t"   #'org-todo

    (:prefix ("d" . "date/deadline")
     "T"        #'org-toggle-timestamp-type
     "i"        #'org-time-stamp-inactive)))

  (:after evil-org-agenda
   (:map evil-org-agenda-mode-map
    :m "k"      #'org-agenda-previous-item
    :m "j"      #'org-agenda-next-item))))

;;
;;; markdown

(map!
 (:after markdown-mode
  (:map +markdown-format-map
   "b"   #'markdown-insert-bold
   "h"   #'markdown-insert-header-dwim
   "i"   #'markdown-insert-image
   "i"   #'markdown-insert-italic
   "k"   #'markdown-insert-link
   "K"   #'markdown-insert-kbd
   "m"   #'markdown-insert-code
   "q"   #'markdown-insert-blockquote
   "s"   #'markdown-insert-strike-through
   "v"   #'markdown-insert-pre))

 (:map markdown-mode-map
  :g "s-f"     +markdown-format-map))
