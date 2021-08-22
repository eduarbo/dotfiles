;;; ~/dev/dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

;;                 ▄▄▄▄· ▪   ▐ ▄ ·▄▄▄▄  ▪   ▐ ▄  ▄▄ • .▄▄ ·
;;                 ▐█ ▀█▪██ •█▌▐███▪ ██ ██ •█▌▐█▐█ ▀ ▪▐█ ▀.
;;                 ▐█▀▀█▄▐█·▐█▐▐▌▐█· ▐█▌▐█·▐█▐▐▌▄█ ▀█▄▄▀▀▀█▄
;;                 ██▄▪▐█▐█▌██▐█▌██. ██ ▐█▌██▐█▌▐█▄▪▐█▐█▄▪▐█
;;                 ·▀▀▀▀ ▀▀▀▀▀ █▪▀▀▀▀▀• ▀▀▀▀▀ █▪·▀▀▀▀  ▀▀▀▀

;;                  == Spacemacs-esque keybinding scheme ==


(defvar +org-format-map (make-sparse-keymap))
(defvar +org-log-buffer-mode-map (make-sparse-keymap))

;; Setup minor mode for org note log buffers
(define-minor-mode +org-log-buffer-mode
  "Minor mode for org note log buffers"
  :keymap +org-log-buffer-mode-map)
(add-hook! 'org-log-buffer-setup-hook #'+org-log-buffer-mode)

(setq
 doom-leader-key ","
 doom-localleader-key ", m")


;; ┏━╸╻  ┏━┓┏┓ ┏━┓╻  ┏━┓
;; ┃╺┓┃  ┃ ┃┣┻┓┣━┫┃  ┗━┓
;; ┗━┛┗━╸┗━┛┗━┛╹ ╹┗━╸┗━┛

(map!
 :m    ":"             #'execute-extended-command
 :m    ";"             #'evil-ex

 :n    "#"             #'evilnc-comment-or-uncomment-lines
 :v    "#"             #'evilnc-comment-operator

 :nv   "SPC"           #'+default/search-project-for-symbol-at-point
 :nv   "S-SPC"         #'+default/search-project

 :i    "S-SPC"         #'tab-to-tab-stop


 ;; narrowing and widening
 :nv   [S-return]      #'+eduarbo/narrow-or-widen-dwim


 ;; Easier window navigation

 (:map general-override-mode-map
  :n  "C-h"           #'evil-window-left
  :n  "C-j"           #'evil-window-down
  :n  "C-k"           #'evil-window-up
  :n  "C-l"           #'evil-window-right)
 ;; overrides
 (:after evil-org-agenda :map evil-org-agenda-mode-map
  :m  "C-h"           #'evil-window-left
  :m  "C-j"           #'evil-window-down
  :m  "C-k"           #'evil-window-up
  :m  "C-l"           #'evil-window-right)
 (:map comint-mode-map
  :in "C-h"           #'evil-window-left
  :in "C-j"           #'evil-window-down
  :in "C-k"           #'evil-window-up
  :in "C-l"           #'evil-window-right)
 (:after treemacs-mode :map treemacs-mode-map
  :g  "C-h"           #'evil-window-left
  :g  "C-l"           #'evil-window-right)


 ;; Text objects

 :gi   [C-backspace]  #'delete-forward-char

 :gi   "C-f"          #'forward-word
 :gi   "C-b"          #'backward-word

 :m    "H"            #'sp-backward-symbol
 :m    "L"            #'sp-forward-symbol
 :gi   "C-h"          #'sp-backward-symbol
 :gi   "C-l"          #'sp-forward-symbol

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
 :m    [up]           #'+evil-multi-previous-line
 :m    [down]         #'+evil-multi-next-line

 :n    "s"            #'evil-surround-edit
 :v    "s"            #'evil-surround-region


 ;; expand-region

 :v    "v"            (general-predicate-dispatch 'er/expand-region
                        (eq (evil-visual-type) 'line)
                        'evil-visual-char)
 :v    "C-v"          #'er/contract-region


 ;; CMD Shortcuts

 "s-a"                #'ace-window
 "s-b"                #'consult-bookmark
 "s-B"                #'bookmark-delete
 "s-e"                #'execute-extended-command
 "s-f"                #'projectile-find-file
 "s-F"                #'+default/find-file-under-here
 "s-g"                #'magit-status
 "s-G"                #'magit-status-here
 "s-h"                #'previous-buffer
 "s-H"                #'+workspace/switch-left
 "s-l"                #'next-buffer
 "s-L"                #'+workspace/switch-right
 "s-i"                #'org-capture
 "s-I"                #'org-journal-new-entry
 "s-j"                #'evil-switch-to-windows-last-buffer
 "s-J"                #'+workspace/other
 "s-k"                #'kill-current-buffer
 "s-K"                #'doom/kill-buried-buffers
 "s-m"                nil
 (:map org-mode-map
  "s-o"               #'+org/insert-item-below
  "s-O"               #'+org/insert-item-above)
 (:map prog-mode-map
  "s-o"               #'+evil/insert-newline-below
  "s-O"               #'+evil/insert-newline-above)
 "s-p"                #'projectile-switch-project
 "s-P"                #'doom/find-file-in-other-project
 ;; "s-P"                #'+default/search-other-project
 "s-r"                #'+eval/open-repl-other-window
 "s-R"                #'+eval/open-repl-same-window
 "s-u"                #'evil-window-mru
 "s-U"                #'delete-other-windows
 "s-y"                #'+default/yank-pop

 "s-x"                #'doom/open-project-scratch-buffer
 "s-X"                #'doom/switch-to-project-scratch-buffer
 "s-;"                #'doom/open-scratch-buffer
 "s-:"                #'doom/switch-to-scratch-buffer

 "s-["                #'previous-buffer
 "s-]"                #'next-buffer
 "s-{"                #'+workspace/switch-left
 "s-}"                #'+workspace/switch-right
 "s-`"                #'helpful-key
 "s--"                #'doom/decrease-font-size
 "s-+"                #'doom/increase-font-size
 "s-="                #'doom/reset-font-size
 "s-,"                #'doom/find-file-in-private-config
 "s-<"                (λ! (+eduarbo-find-file dotfiles-dir))
 :ginv "s-/"          #'+default/search-buffer
 "s-?"                #'+default/search-project-for-symbol-at-point
 "s-."                #'treemacs
 "s->"                #'treemacs-select-window

 [s-up]               #'drag-stuff-up
 [s-down]             #'drag-stuff-down
 [s-left]             #'drag-stuff-left
 [s-right]            #'drag-stuff-right)

;;; [g]o-to prefix

(map!
 (:prefix "g"
  :n  "."     #'call-last-kbd-macro
  :nv "j"     #'avy-goto-line-below
  :n  "J"     #'join-line
  :nv "k"     #'avy-goto-line-above
  ;; FIXME
  :nv "m"     #'counsel-mark-ring
  :nv "n"     #'+eduarbo/narrow-or-widen-dwim
  :nv "o"     #'avy-goto-char-timer
  :nv "O"     (λ! (let ((avy-all-windows t)) (avy-goto-char-timer)))
  :nv "Q"     #'+eduarbo/unfill-paragraph
  :nv "s"     #'evil-snipe-s
  :nv "S"     #'evil-snipe-S
  :nv "w"     #'transpose-words
  :nv "X"     #'transpose-sexps
  :v  [tab]   #'evil-vimish-fold/create
  :n  [tab]   #'evil-vimish-fold/delete
  :n  [S-tab] #'evil-vimish-fold/delete-all))


;; ┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┃ ┃┣┳┛┃╺┓╺━╸┃┃┃┃ ┃ ┃┃┣╸
;; ┗━┛╹┗╸┗━┛   ╹ ╹┗━┛╺┻┛┗━╸

(map!
 (:after org
  (:map +org-log-buffer-mode-map
   "s-s"           #'org-ctrl-c-ctrl-c
   "s-k"           #'org-kill-note-or-show-branches
   "s-w"           #'org-kill-note-or-show-branches)

  (:map (org-mode-map org-agenda-mode-map)
   "s-r"           #'org-refile
   "s-R"           #'+org/refile-to-running-clock

   :n [return]     #'+org/dwim-at-point

   [s-up]          #'org-metaup
   [s-down]        #'org-metadown
   :gi [s-left]    #'org-shiftmetaleft
   :gi [s-right]   #'org-shiftmetaright

   :n "H"          #'org-metaleft
   :n "L"          #'org-metaright)

  (:map org-capture-mode-map
   "s-r"           #'org-capture-refile
   "s-w"           #'org-capture-kill
   "s-k"           #'org-capture-kill
   "s-s"           #'org-capture-finalize)

  (:map org-src-mode-map
   "s-k"           #'org-edit-src-abort
   "s-s"           #'org-edit-src-save
   "s-w"           #'org-edit-src-exit

   (:leader
    :desc "Save file"   "fs"    #'org-edit-src-save)))

 (:after evil-org
  (:map evil-org-mode-map
   :n "gk" (cmd! (if (org-on-heading-p)
                     (org-backward-element)
                   (evil-previous-visual-line)))
   :n "gj" (cmd! (if (org-on-heading-p)
                     (org-forward-element)
                   (evil-next-visual-line))))

  (:map +org-format-map
   ;; Basic char syntax:
   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
   "b"   (org-emphasize! +org/bold ?*)
   "i"   (org-emphasize! +org/italic ?/)
   "m"   (org-emphasize! +org/monospace ?~)  ;; monospace/code
   "u"   (org-emphasize! +org/underline ?_)
   "v"   (org-emphasize! +org/verbose ?=)
   "s"   (org-emphasize! +org/strike-through ?+)
   "x"   (org-emphasize! +org/restore-format ? )
   "r"   #'org-roam-insert
   "R"   #'org-roam-insert-immediate
   "c"   #'org-cliplink
   "d"   #'org-download-yank
   "D"   #'org-download-clipboard
   "k"   #'org-insert-link
   "K"   #'+org/remove-link
   "n"   #'org-add-note
   )

  (:map evil-org-mode-map
   "s-m"        +org-format-map
   :n "C-i"     #'evil-jump-forward
   :i "C-o"     #'+org/insert-item-below
   :i "C-S-O"   #'+org/insert-item-above

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

(map! :after org-roam
      :leader
      (:prefix "t"
       :desc "Org Roam"                 "r"  #'org-roam)
      (:localleader :map org-mode-map
       :desc "Toggle Org Roam"          "R"  #'org-roam))

;; Disable bindings for org-super-agenda headers
(after! org-super-agenda
  (setq org-super-agenda-header-map nil))


;; ╻  ┏━╸┏━┓╺┳┓┏━╸┏━┓
;; ┃  ┣╸ ┣━┫ ┃┃┣╸ ┣┳┛
;; ┗━╸┗━╸╹ ╹╺┻┛┗━╸╹┗╸

(map! :leader
      :desc "M-x"                         ":"           #'execute-extended-command
      :desc "Eval expression"             ";"           #'pp-eval-expression
      :desc "Search buffer"               "/"           #'consult-imenu
      :desc "Switch Project"              "RET"         #'+workspace/switch-to
      :desc "Find file from here"         "ESC"         #'projectile-switch-to-buffer
      :desc "Find file from here"         "."           #'find-file
      :desc "Find file"                   ">"           #'+default/find-file-under-here
      :desc "Toggle last popup"           "'"           #'+popup/toggle
      :desc "Resume last completion"      "`"           (cond ((featurep! :completion vertico)  #'vertico-repeat)
                                                              ((featurep! :completion ivy)      #'ivy-resume)
                                                              ((featurep! :completion helm)     #'helm-resume))

      ;;; <leader> l --- language
      (:when (featurep! :config language)
       (:prefix ("l" . "language")
        :desc "Configure translate languages"    ","    #'+language/set-google-translate-languages
        :desc "Translate"                        "t"    #'google-translate-smooth-translate
        :desc "Translate any language"           "a"    #'+language/google-translate-smooth-translate-any
        :desc "Translate from source lang"       "s"    #'google-translate-at-point
        :desc "Translate from destination lang"  "d"    #'google-translate-at-point-reverse))

      ;;; <leader> b --- buffer
      (:prefix ("b" . "buffer")
       :desc "Kill buried buffers"         "K"          #'doom/kill-buried-buffers)

      ;;; <leader> f --- file
      (:prefix ("f" . "file")
       :desc "Find file in private config" ","          #'doom/find-file-in-private-config
       :desc "Find file in .dotfiles"      "."          (λ! (+eduarbo-find-file dotfiles-dir)))

      ;;; <leader> g --- git
      (:prefix ("g" . "git")
       (:when (featurep! :tools magit)
        :desc "Timemachine for branch"    "T"           #'git-timemachine-switch-branch
        :desc "Magit diff staged"         "d"           #'magit-diff-buffer-file
        :desc "Magit diff"                "D"           #'magit-diff))

      ;;; <leader> n --- notes
      (:prefix ("n" . "notes")
       :desc "Search notes"              "S-SPC"        #'+org/org-notes-search
       :desc "Search notes"                "/"          #'+org/org-notes-search
       :desc "Org Roam capture"            "RET"        #'org-roam-capture
       :desc "Find note"                   "SPC"        #'org-roam-node-find

       ;; ;; FIXME
       ;; :desc "Switch to buffer"            ","          #'org-roam-switch-to-buffer
       ;; :desc "Org Roam Insert"             "i"          #'org-roam-node-insert
       ;; ;; FIXME
       ;; :desc "Jump to index"               "I"          #'org-roam-jump-to-index
       ;; :desc "Today's journal"             "j"          #'org-journal-new-entry
       ;; :desc "Date journal"                "J"          #'org-journal-new-date-entry
       ;; :desc "Daily Agenda"                "d"          #'eduarbo/daily-agenda
       ;; :desc "Unscheduled Agenda"          "u"          #'eduarbo/unscheduled-agenda
       ;; :desc "Search org agenda headlines" "A"          #'+org/org-agenda-headlines
       ;; :desc "Search org notes headlines"  "S"          #'+org/org-notes-headlines
       ;; :desc "Open project notes"          "p"          #'+org/find-notes-for-project
       ;; :desc "Today journal"               "t"          #'org-journal-open-current-journal-file
       ;; :desc "Todo list"                   "T"          #'org-todo-list
       )

      ;;; <leader> o --- open
      (:prefix ("o" . "open")
       :desc "Shell command"                 "s"        #'async-shell-command
       :desc "Shell command in project root" "S"        #'projectile-run-async-shell-command-in-root
       :desc "Find and focus in project sidebar"  "p"   #'treemacs-select-window
       :desc "Find in project sidebar"       "P"        #'treemacs-find-file)

      ;;; <leader> TAB --- workspace
      (:prefix ("TAB" . "workspace")
       :desc "Kill this workspace"          "k"         #'+workspace/delete)

      ;;; <leader> p --- project
      (:prefix ("p" . "project")
       :desc "Run cmd in project root"      "!"         #'projectile-run-async-shell-command-in-root
       :desc "Discover projects"            "D"         #'projectile-discover-projects-in-search-path
       :desc "Open project notes"           "n"         #'+org/find-notes-for-project)

      ;;; <leader> q --- quit/session
      (:prefix ("q" . "quit/session")
       :desc "Start new instance of Emacs"  "e"         #'restart-emacs-start-new-emacs)

      ;;; <leader> t --- toggle
      (:prefix ("t" . "toggle")
       :desc "Read-only mode"               "R"         #'read-only-mode
       :desc "Line numbers"                 "l"         #'display-line-numbers-mode
       :desc "Global Line numbers"          "L"         #'global-display-line-numbers-mode
       :desc "Visual fill column mode"      "v"         #'visual-fill-column-mode
       :desc "Subword mode"                 "W"         #'subword-mode
       :desc "Project sidebar"              "p"         #'treemacs
       :desc "Org-Tree-Slide mode"          "o"         #'org-tree-slide-mode
       :desc "Frame maximized"              "m"         #'toggle-frame-maximized)

      ;;; <leader> w --- window
      (:prefix ("w" . "window")
       :desc "Zoom"                         "z"         #'zoom
       :desc "Balance windows"              "b"         #'balance-windows
       :desc "Most recently used buffer"    "w"         #'evil-window-mru))
