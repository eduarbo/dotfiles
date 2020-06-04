;;; ~/dev/dotfiles/editor/emacs/doom/+bindings.el -*- lexical-binding: t; -*-

;;                 ▄▄▄▄· ▪   ▐ ▄ ·▄▄▄▄  ▪   ▐ ▄  ▄▄ • .▄▄ ·
;;                 ▐█ ▀█▪██ •█▌▐███▪ ██ ██ •█▌▐█▐█ ▀ ▪▐█ ▀.
;;                 ▐█▀▀█▄▐█·▐█▐▐▌▐█· ▐█▌▐█·▐█▐▐▌▄█ ▀█▄▄▀▀▀█▄
;;                 ██▄▪▐█▐█▌██▐█▌██. ██ ▐█▌██▐█▌▐█▄▪▐█▐█▄▪▐█
;;                 ·▀▀▀▀ ▀▀▀▀▀ █▪▀▀▀▀▀• ▀▀▀▀▀ █▪·▀▀▀▀  ▀▀▀▀

;;                  == Spacemacs-esque keybinding scheme ==


(defvar my-completion-map (make-sparse-keymap))
(defvar my-org-format-map (make-sparse-keymap))


(setq
  doom-leader-key ","
  doom-localleader-key ", m"
  +evil-repeat-keys '("+" . "-"))


;; ┏━╸╻  ┏━┓┏┓ ┏━┓╻  ┏━┓
;; ┃╺┓┃  ┃ ┃┣┻┓┣━┫┃  ┗━┓
;; ┗━┛┗━╸┗━┛┗━┛╹ ╹┗━╸┗━┛

(map!
  "s-;"                 #'pp-eval-expression
  :gv   "s-x"           #'execute-extended-command
  :m    ":"             #'execute-extended-command
  :m    ";"             #'evil-ex

  :n    "#"             #'evilnc-comment-or-uncomment-lines
  :v    "#"             #'evilnc-comment-operator

  :nv   "SPC"           #'+default/search-project-for-symbol-at-point
  :nv   "S-SPC"         #'+default/search-project

  :i    "S-SPC"         #'tab-to-tab-stop

  ;; Tab

  ;; This allows org-mode to bind org-cycle properly... do not why
  :nvm  [tab]           nil

  (:map prog-mode-map
    :nv [tab]           #'evil-jump-item
    :n  [S-tab]         #'evil-toggle-fold)

  (:when (featurep! :completion company)
    :i  [tab]           #'+company/complete
    :i  [C-tab]         my-completion-map)

  (:when (featurep! :editor snippets)
    :i  [S-tab]         (λ! (unless (call-interactively 'yas-expand)
                              (call-interactively 'company-yasnippet)))
    :v  [S-tab]         #'yas-insert-snippet

    :i  [C-return]      #'aya-expand
    :nv [C-return]      #'aya-create)


  ;; Buffer/Workspace Navigation

  "s-["                 #'previous-buffer
  "s-]"                 #'next-buffer
  "s-h"                 #'previous-buffer
  "s-l"                 #'next-buffer

  (:when (featurep! :ui workspaces)
    (:leader
      :nv ">"           #'persp-switch)

    "s-{"               #'+workspace/switch-left
    "s-}"               #'+workspace/switch-right
    "s-H"               #'+workspace/switch-left
    "s-L"               #'+workspace/switch-right)

  :gn   "s-j"           #'evil-switch-to-windows-last-buffer
  :gn   "s-J"           #'+eduarbo/switch-to-last-workspace


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


  ;; Shortcuts

  :ginv "s-/"           #'counsel-descbinds
  "s-?"                 #'which-key-show-top-level
  "s--"                 #'doom/decrease-font-size
  "s-+"                 #'doom/increase-font-size
  "s-="                 #'doom/reset-font-size
  "s-,"                 #'doom/find-file-in-private-config
  "s-<"                 #'doom/open-private-config

  "s-."                 (cond ((featurep! :completion ivy) #'ivy-resume)
                          ((featurep! :completion helm)    #'helm-resume))
  "s-`"                 (cond ((featurep! :completion ivy) #'ivy-resume)
                          ((featurep! :completion helm)    #'helm-resume))
  "s->"                 #'+popup/toggle
  "s-'"                 #'+popup/toggle

  "s-b"                 #'bookmark-jump
  "s-B"                 #'bookmark-delete
  "s-g"                 #'magit-status
  "s-i"                 #'org-capture
  "s-I"                 #'org-journal-new-entry
  "s-m"                 #'helpful-key
  "s-p"                 #'+treemacs/toggle
  [s-escape]            #'projectile-switch-project
  "s-r"                 #'+eval/open-repl-other-window
  "s-R"                 #'+eval/open-repl-same-window
  "s-u"                 #'winner-undo
  "s-U"                 #'winner-redo
  "s-y"                 #'+default/yank-pop


  ;; Text objects

  :gi   [C-backspace]   #'delete-forward-char

  :gi   "C-f"           #'forward-word
  :gi   "C-b"           #'backward-word

  :m    "H"             #'sp-backward-symbol
  :m    "L"             #'sp-forward-symbol
  :gi   "C-h"           #'sp-backward-symbol
  :gi   "C-l"           #'sp-forward-symbol

  :gi   "C-d"           #'evil-delete-line
  :gi   "C-S-d"         #'evil-delete-whole-line
  :gi   "C-S-u"         #'evil-change-whole-line
  :gi   "C-S-w"         #'backward-kill-sexp

  :gi   "C-S-a"         #'sp-beginning-of-sexp
  :gi   "C-S-e"         #'sp-end-of-sexp

  :gi   "C-t"           #'transpose-chars
  :nv   "C-a"           #'evil-numbers/inc-at-pt
  :nv   "C-S-a"         #'evil-numbers/dec-at-pt

  :m    "k"             #'evil-previous-visual-line
  :m    "j"             #'evil-next-visual-line
  :m    [up]            #'+evil-multi-previous-line
  :m    [down]          #'+evil-multi-next-line

  :n    "s"             #'evil-surround-edit
  :v    "s"             #'evil-surround-region


  ;; expand-region

  :v    "v"             (general-predicate-dispatch 'er/expand-region
                          (eq (evil-visual-type) 'line)
                          'evil-visual-char)
  :v    "C-v"           #'er/contract-region


  ;; Shift text

  :n    "<"             #'evil-shift-left-line
  :n    ">"             #'evil-shift-right-line

  (:after evil-org :map evil-org-mode-map
    :n  "<"             #'+evil/evil-org-<
    :n  ">"             #'+evil/evil-org->)

  ;; don't leave visual mode after shifting
  :v    "<"             #'+evil/visual-dedent  ; vnoremap < <gv
  :v    ">"             #'+evil/visual-indent  ; vnoremap > >gv
)


;;; [g]o-to prefix

(map!
  (:prefix "g"
    :nv "Q"     #'+eduarbo/unfill-paragraph
    :nv "o"     #'avy-goto-char-timer
    :nv "O"     (λ! (let ((avy-all-windows t)) (avy-goto-char-timer)))
    :n  "."     #'call-last-kbd-macro
    :nv "k"     #'avy-goto-line-above
    :nv "j"     #'avy-goto-line-below
    :nv "s"     #'transpose-sexps
    :nv "w"     #'transpose-words
    :nv "a"     #'evil-snipe-s
    :nv "A"     #'evil-snipe-S
    :v  [tab]   #'evil-vimish-fold/create
    :n  [tab]   #'evil-vimish-fold/delete
    :n  [S-tab] #'evil-vimish-fold/delete-all
    ;; narrowing and widening
    :nv "n"     #'+eduarbo/narrow-or-widen-dwim))


;; ┏┳┓┏━┓╺┳┓╻ ╻╻  ┏━╸┏━┓
;; ┃┃┃┃ ┃ ┃┃┃ ┃┃  ┣╸ ┗━┓
;; ╹ ╹┗━┛╺┻┛┗━┛┗━╸┗━╸┗━┛

;;; :completion

(map!
  (:when (featurep! :completion company)
    (:after company
      (:map company-active-map
        [S-tab]   #'company-select-previous
        "C-l"     #'company-complete
        [right]   #'company-complete
        "C-h"     #'company-abort
        [left]    #'company-abort)

      (:map my-completion-map
        "d"      #'+company/dict-or-keywords
        "f"      #'company-files
        "s"      #'company-ispell
        [C-tab]  #'company-yasnippet
        "o"      #'company-capf
        "a"      #'+company/dabbrev)))

  (:when (featurep! :completion ivy)
    (:after ivy :map ivy-minibuffer-map
      "C-n"      #'scroll-up-command
      "C-p"      #'scroll-down-command
      "s-o"      #'hydra-ivy/body)
    (:after counsel :map counsel-ag-map
      [S-tab]  #'+ivy/woccur
      [C-return] #'+ivy/git-grep-other-window-action
      "C-o"      #'+ivy/git-grep-other-window-action)
    (:after swiper :map swiper-map
      [S-tab] #'+ivy/woccur))

  (:when (featurep! :completion helm)
    (:after swiper-helm
      :map swiper-helm-keymap [S-tab] #'helm-ag-edit)
    (:after helm-ag
      :map helm-ag-map
      [S-tab]  #'helm-ag-edit)))


;;; evil-snipe

(map! :after evil-snipe :map (evil-snipe-override-mode-map evil-snipe-parent-transient-map)
  :nv "S" (λ! (require 'evil-easymotion)
          (call-interactively
            (evilem-create #'evil-snipe-repeat
              :bind ((evil-snipe-scope 'whole-buffer)
                      (evil-snipe-enable-highlight)
                      (evil-snipe-enable-incremental-highlight)))))

  ;; Don't interfere with my bindings
  :gm ";"  nil
  :gm ","  nil)


;;; snippets

(map!
  (:when (featurep! :editor snippets)
    :i  [S-tab]    (λ! (unless (call-interactively 'yas-expand)
                         (call-interactively 'company-yasnippet)))
    :v  [S-tab]    #'yas-insert-snippet

    :i  [C-return] #'aya-expand
    :nv [C-return] #'aya-create

    (:after yasnippet :map yas-keymap
      ;; Do not interfer with company
      [tab]         nil
      "TAB"         nil
      [S-tab]       nil
      "<S-tab>"     nil
      "C-n"         #'yas-next-field
      "C-p"         #'yas-prev-field
      "C-l"         #'yas-next-field
      "C-h"         #'yas-prev-field)))


;;; multiple-cursors

(map!
  (:when (featurep! :editor multiple-cursors)
    :n  "s-d"   #'evil-multiedit-match-symbol-and-next
    :n  "s-D"   #'evil-multiedit-match-symbol-and-prev
    :v  "s-d"   #'evil-multiedit-match-and-next
    :v  "s-D"   #'evil-multiedit-match-and-prev
    :nv "s-C-d" #'evil-multiedit-restore

    (:after evil-multiedit :map evil-multiedit-state-map
      "s-d"    #'evil-multiedit-match-and-next
      "s-D"    #'evil-multiedit-match-and-prev
      [return] #'evil-multiedit-toggle-or-restrict-region)))


;;; git-timemachine

(map! :after git-timemachine :map git-timemachine-mode-map
  :n "C-p" #'git-timemachine-show-previous-revision
  :n "C-n" #'git-timemachine-show-next-revision)


;; ┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┃ ┃┣┳┛┃╺┓╺━╸┃┃┃┃ ┃ ┃┃┣╸
;; ┗━┛╹┗╸┗━┛   ╹ ╹┗━┛╺┻┛┗━╸

(map!
  (:after org
    (:map (org-mode-map org-agenda-mode-map)
      "s-r"           #'org-refile
      "s-R"           #'+org/refile-to-running-clock

      :n [S-return]   #'org-todo
      :n [return]     #'+org/dwim-at-point

      :n "H"          #'org-metadown
      :n "L"          #'org-metaup)

    (:map org-capture-mode-map
      "s-r" #'org-capture-refile
      "s-w" #'org-capture-kill
      "s-k" #'org-capture-kill
      "s-s" #'org-capture-finalize)

    (:map org-src-mode-map
      "s-k" #'org-edit-src-abort
      "s-s" #'org-edit-src-save
      "s-w" #'org-edit-src-exit

      (:leader
        :desc "Save file"   "fs"    #'org-edit-src-save)))

  (:after evil-org
    (:map my-org-format-map
      ;; Basic char syntax:
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
      "b"   (λ! (org-emphasize ?*)) ;; bold
      "i"   (λ! (org-emphasize ?/)) ;; italic
      "k"   #'org-insert-link
      "K"   #'+org/remove-link
      "l"   #'org-store-link
      "m"   (λ! (org-emphasize ?~)) ;; monospace/code
      "u"   (λ! (org-emphasize ?_)) ;; underline
      "v"   (λ! (org-emphasize ?=)) ;; verbose
      "s"   (λ! (org-emphasize ?+)) ;; strikethrough
      "r"   (λ! (org-emphasize ?\s)) ;; restore format
      )

    (:map evil-org-mode-map
      :vi "s-f"       my-org-format-map

      :mi "C-o"       #'evil-org-org-insert-heading-respect-content-below

      (:when IS-MAC
        "s-o"   #'+org/insert-item-below
        "s-O"   #'+org/insert-item-above)

      (:localleader
        :desc "format"                           "f"   my-org-format-map
        :desc "Timestamp"                        "t"   #'org-time-stamp
        :desc "Togle Timestamp type"             "T"   #'org-toggle-timestamp-type
        :desc "Inactive Timestamp"               "i"   #'org-time-stamp-inactive))

  (:after evil-org-agenda
    (:map evil-org-agenda-mode-map
    :m "k"          #'org-agenda-previous-item
    :m "j"          #'org-agenda-next-item))))

;; Disable bindings for org-super-agenda headers
(after! org-super-agenda
  (setq org-super-agenda-header-map nil))


;; ╻  ┏━╸┏━┓╺┳┓┏━╸┏━┓
;; ┃  ┣╸ ┣━┫ ┃┃┣╸ ┣┳┛
;; ┗━╸┗━╸╹ ╹╺┻┛┗━╸╹┗╸

(map! :leader
  :desc "M-x"                         ":"         #'execute-extended-command
  :desc "Eval expression"             ";"         #'pp-eval-expression
  :desc "Show marks"                  "/"         #'counsel-evil-marks
  :desc "Switch to scratch buffer"    "X"         #'doom/switch-to-scratch-buffer
  :desc "Escape"                      "ESC"       #'doom/escape
  :desc "Find file in other project"  "RET"       #'doom/find-file-in-other-project
  :desc "Search other project"        "S-SPC"     #'+default/search-other-project
  :desc "Toggle last popup"           "'"         #'+popup/toggle
  :desc "Ivy resume"                  "`"         (cond ((featurep! :completion ivy) #'ivy-resume)
                                                  ((featurep! :completion helm)    #'helm-resume))

      ;;; <leader> l --- language
  (:when (featurep! :config language)
    (:prefix ("l" . "language")
      :desc "Configure translate languages"    ","   #'+language/set-google-translate-languages
      :desc "Translate"                        "t"   #'google-translate-smooth-translate
      :desc "Translate any language"           "a"   #'+language/google-translate-smooth-translate-any
      :desc "Translate from source lang"       "s"   #'google-translate-at-point
      :desc "Translate from destination lang"  "d"   #'google-translate-at-point-reverse))

      ;;; <leader> b --- buffer
  (:prefix ("b" . "buffer")
    :desc "Kill buried buffers"         "K"   #'doom/kill-buried-buffers)

      ;;; <leader> f --- file
  (:prefix ("f" . "file")
    :desc "Find file in other project"  "o" #'doom/find-file-in-other-project
    :desc "Search in other project"     "O" #'+default/search-other-project
    :desc "Find file in private config" "," #'doom/find-file-in-private-config
    :desc "Browse private config"       "<" #'doom/open-private-config
    :desc "Find file in .dotfiles"      "." (λ! (+eduarbo-find-file dotfiles-dir))
    :desc "Search in .dotfiles"         ">" (λ! (+eduarbo-search-project dotfiles-dir))
    )

      ;;; <leader> g --- git
  (:prefix ("g" . "git")
    (:when (featurep! :tools magit)
      :desc "Timemachine for branch"    "T"   #'git-timemachine-switch-branch
      :desc "Magit diff staged"         "d"   #'magit-diff-buffer-file
      :desc "Magit diff"                "D"   #'magit-diff))

      ;;; <leader> n --- notes
  (:prefix ("n" . "notes")
    :desc "New Entry"                     "j" #'org-journal-new-entry
    :desc "Search Forever"                "J" #'org-journal-search-forever
    :desc "Daily Agenda"                  "d" #'eduarbo/daily-agenda
    :desc "Unscheduled Agenda"            "u" #'eduarbo/unscheduled-agenda
    :desc "Open project notes"            "p" #'+org/find-notes-for-project)

      ;;; <leader> o --- open
  (:prefix ("o" . "open")
    :desc "Shell command"                 "s" #'async-shell-command
    :desc "Shell command in project root" "S" #'projectile-run-async-shell-command-in-root)

      ;;; <leader> TAB --- workspace
  (:prefix ("TAB" . "workspace")
    :desc "Kill this workspace"          "k" #'+workspace/delete)

      ;;; <leader> p --- project
  (:prefix ("p" . "project")
    :desc "Run cmd in project root"      "!" #'projectile-run-async-shell-command-in-root
    :desc "Discover projects"            "D" #'projectile-discover-projects-in-search-path
    :desc "Open project notes"           "n" #'+org/find-notes-for-project)

      ;;; <leader> t --- toggle
  (:prefix ("t" . "toggle")
    :desc "Line numbers"                 "l" #'display-line-numbers-mode
    :desc "Global Line numbers"          "L" #'global-display-line-numbers-mode
    :desc "Visual line mode"             "v" #'visual-line-mode
    :desc "Visual fill column mode"      "V" #'visual-fill-column-mode
    :desc "Subword mode"                 "W" #'subword-mode
    :desc "Frame maximized"              "Z" #'toggle-frame-maximized)

      ;;; <leader> w --- window
  (:prefix ("w" . "window")
    "o" #'eduarbo/window-enlargen
    "O" #'eduarbo/focused-window-enlargen
    :desc "Most recently used buffer"    "w" #'evil-window-mru))
