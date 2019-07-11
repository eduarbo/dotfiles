;;; ~/.dotfiles/editor/emacs/doom/+bindings2.el -*- lexical-binding: t; -*-

;; This file defines a Spacemacs-esque keybinding scheme

;;
;;; Global keybindings

(map! "s-;" #'execute-extended-command
      "s-x" #'execute-extended-command
      "s-," #'+nav-flash/blink-cursor
      "s-." #'helpful-key

      "s-[" #'previous-buffer
      "s-]" #'next-buffer

      :i "S-SPC" 'yas-expand
      :v "S-SPC" (general-predicate-dispatch nil
                   (and (bound-and-true-p yas-minor-mode)
                        (or (eq evil-visual-selection 'line)
                            (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                   'yas-insert-snippet)

      [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
              (featurep! :completion company)
              'company-indent-or-complete-common)
              ;; '+company/complete)
      ;; Smart tab, these will only work in GUI Emacs
      ;; :i [tab] (general-predicate-dispatch nil ; fall back to nearest keymap
      ;;            (and (featurep! :completion company)
      ;;                 (+company-has-completion-p))
      ;;            '+company/complete)
      :n [tab] (general-predicate-dispatch nil
                 (fboundp 'evil-jump-item)
                 'evil-jump-item)
      :v [tab] (general-predicate-dispatch nil
                 (fboundp 'evil-jump-item)
                 'evil-jump-item)

      (:when (featurep! :ui workspaces)
        "s-t" #'+workspace/new
        "s-{" #'+workspace/switch-left
        "s-}" #'+workspace/switch-right)

      [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag

      ;; Smarter RET in normal mode
      :n "RET" (general-predicate-dispatch nil
                 (and (bound-and-true-p flyspell-mode)
                      (+flyspell-correction-at-point-p))
                 'flyspell-correct-word-generic)

      ;; misc
      :gnv "s-/"  #'which-key-show-top-level
      :nv ";"     #'evil-ex
      :nv ":"     #'eval-expression

      :n "#"      #'evil-commentary-line
      :v "#"      #'evil-commentary

      ;; Shift text
      :n  "<"     #'evil-shift-left-line
      :n  ">"     #'evil-shift-right-line
      ;; don't leave visual mode after shifting
      :v  "<"     #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"     #'+evil/visual-indent  ; vnoremap > >gv

      ;; FIXME: Ensure they really move to previous/next buffer
      :n  "H"     #'previous-buffer
      :n  "L"     #'next-buffer

      :n  "C-."   (cond ((featurep! :completion ivy)   #'ivy-resume)
                        ((featurep! :completion helm)  #'helm-resume))

      :n  "~"        #'evil-switch-to-windows-last-buffer
      (:map evil-org-mode-map
        :n  "~"        #'evil-switch-to-windows-last-buffer)

      ;; Insert mode
      :gi "C-s"          #'isearch-forward
      (:map isearch-mode-map
        :gi "C-S-s"      #'isearch-repeat-backward)

      ;; Behave like a backspace
      :gi [C-backspace]  #'backward-delete-char-untabify

      ;; :gi [S-backspace]  #'delete-forward-char

      :gi "C-d"          #'evil-delete-line
      :gi "C-S-d"        #'evil-delete-whole-line
      :gi "C-S-u"        #'evil-change-whole-line
      :gi "C-S-w"        #'backward-kill-sexp

      :gi "C-S-a"        #'sp-beginning-of-sexp
      :gi "C-S-e"        #'sp-end-of-sexp

      :gi "C-S-f"        #'sp-forward-sexp
      :gi "C-S-b"        #'sp-backward-sexp

      :gi "C-h"          #'left-char
      :gi "C-l"          #'right-char
      :gi "C-S-h"        #'sp-backward-symbol
      :gi "C-S-l"        #'sp-forward-symbol

      ;; Basic editing
      :i "S-RET"         #'tab-to-tab-stop
      :i [S-return]      #'tab-to-tab-stop
      ;; TODO: Tranpose last two WORDS not those around
      :gi "C-t"          #'transpose-words
      ;; TODO: Tranpose last two SEXPS not those around
      :gi "C-S-t"        #'transpose-sexps

      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt

      ;; Easier window/tab navigation
      :en "C-h"   #'evil-window-left
      :en "C-j"   #'evil-window-down
      :en "C-k"   #'evil-window-up
      :en "C-l"   #'evil-window-right

      ;; expand-region
      :v "v"   (general-predicate-dispatch 'er/expand-region
                 (eq (evil-visual-type) 'line)
                 'evil-visual-char)
      :v "C-v" #'er/contract-region

      :nv "go"    #'avy-goto-char-timer
      :nv "g/"    #'+helm/project-search
      :n  "g."    #'call-last-kbd-macro)

;; help
(map! (:map help-map
        "H"   #'+lookup/documentation))


;;
;;; Module keybinds

;;; :completion
(map! (:when (featurep! :completion company)
        :i [C-return] #'+company/complete
        (:after company
          (:map company-active-map
            [C-return] #'company-complete-common)))

      (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          [C-return] #'ivy-call-and-recenter  ; preview file
          )
        (:after counsel
          :map counsel-ag-map
          [C-return]    #'ivy-call-and-recenter ; preview
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-,"      (+ivy-do-action! #'+ivy-git-grep-other-window-action))
        (:after swiper
          :map swiper-map
          [backtab] #'+ivy/wgrep-occur))

      (:when (featurep! :completion helm)
        (:after helm
          (:after swiper-helm
            :map swiper-helm-keymap [backtab] #'helm-ag-edit)
          (:after helm-ag
            :map helm-ag-map
            [backtab]  #'helm-ag-edit))))

;;; :ui
(map! (:when (featurep! :ui workspaces)
        :n  "C-`"  #'+eduarbo/switch-to-last-workspace
        (:map evil-org-mode-map
          :n  "C-`"  #'+eduarbo/switch-to-last-workspace)

        :n "C-S-l" #'+workspace/switch-right
        :n "C-S-h" #'+workspace/switch-left))

;;; :editor
(map! (:when (featurep! :editor fold)
        :nv [(shift return)] #'+fold/toggle))

;;; :emacs
(map! :map emacs-lisp-mode-map
      :nv "K"  #'helpful-at-point)

;;; :tools
(map! (:when (featurep! :tools flyspell)
        ;; Keybinds that have no Emacs+evil analogues (i.e. don't exist):
        ;;   zq - mark word at point as good word
        ;;   zw - mark word at point as bad
        ;;   zu{q,w} - undo last marking
        ;; Keybinds that evil define:
        ;;   z= - correct flyspell word at point
        ;;   ]s - jump to previous spelling error
        ;;   [s - jump to next spelling error
        :m "]S" #'flyspell-correct-word-generic
        :m "[S" #'flyspell-correct-previous-word-generic)

      (:when (featurep! :tools flycheck)
        :m "]e" #'next-error
        :m "[e" #'previous-error)

      (:when (featurep! :tools gist)
        :after gist
        :map gist-list-menu-mode-map
        :n "RET"    #'+gist/open-current
        :n [return] #'+gist/open-current))

;;; :lang
(map! :map org-mode-map
      :n "SPC" #'org-todo
      (:when IS-MAC
        :ni "s-o"   (λ! (+org/insert-item 'below))
        :ni "s-O"   (λ! (+org/insert-item 'above))))

;; A fresh start
(map! :map org-mode-map
      :after org
      :localleader
      ;; Unmap the whole map
      "" nil)
(map! :map org-mode-map
      :after org
      :localleader

      :desc "Schedule"              :n  "s"   #'org-schedule
      :desc "Set deadline"          :n  "d"   #'org-deadline
      :desc "Set tags"              :n  "t"   #'org-set-tags-command
      ;; Basic char syntax:
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html#Basic-Char-Syntax
      :desc "Bold"                  :v  "b"   (λ! (org-emphasize ?*)) ;; bold
      :desc "Italic"                :v  "i"   (λ! (org-emphasize ?/)) ;; italic
      :desc "Insert link"           :v  "k"   #'org-insert-link
      :desc "Remove link"           :nv "K"   #'+org/remove-link
      :desc "Store link to heading" :n  "l"   #'org-store-link
      :desc "Monospace/code"        :v  "m"   (λ! (org-emphasize ?~)) ;; monospace/code
      :desc "Restore format"        :v  "r"   (λ! (org-emphasize ?\s)) ;; restore format
      :desc "Underline"             :v  "u"   (λ! (org-emphasize ?_)) ;; underline
      :desc "Verbose"               :v  "v"   (λ! (org-emphasize ?=)) ;; verbose
      :desc "Strikethrough"         :v  "x"   (λ! (org-emphasize ?+)) ;; strikethrough

      (:prefix ("c" . "clock/timer")
        :desc "Start timer"                   :n "c" #'org-clock-in
        :desc "Stop timer"                    :n "C" #'org-clock-out
        :desc "Display total time on heading" :n "d" #'org-clock-display
        :desc "Create table report"           :n "d" #'org-clock-report
        :desc "Go to running timer's entry"   :n "g" #'org-clock-goto
        :desc "Select past timers entry"      :n "G" (λ! (org-clock-goto 'select))
        :desc "Cancel running timer"          :n "x" #'org-clock-cancel))

(map! :mode org-journal-mode
      :localleader
      :map org-journal-mode-map
      "n" #'org-journal-open-next-entry
      "p" #'org-journal-open-previous-entry)

(map! :map emmet-mode-keymap
      :after emmet-mode
      :v "S-SPC" #'emmet-wrap-with-markup
      :i "S-SPC" #'+eduarbo/yas-or-emmet-expand)


;;
;;; <leader>

(map! :leader
      :desc "Eval expression"       ":"    #'eval-expression
      :desc "M-x"                   ";"    #'execute-extended-command

      (:when (featurep! :ui workspaces)
        :desc "Switch workspace"            "TAB" #'persp-switch

        ;;; <leader> l --- workspace
        (:prefix-map ("l" . "workspace")
          :desc "Load a past session"       "L"   #'+workspace/load-session
          :desc "Autosave current session"  "S"   #'+workspace/save-session))

      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
        :desc "Kill buried buffers"         "K"   #'doom/kill-buried-buffers)

      ;;; <leader> g --- git
      (:prefix-map ("g" . "git")
        :desc "Git link for line or region" "y"   #'git-link
        (:when (featurep! :tools magit)
          :desc "Magit diff staged"         "d"   #'magit-diff-buffer-file))

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
        :desc "New Journal entry"             "d" #'org-journal-new-entry
        :desc "Open mode notes"               "m" #'+eduarbo/find-notes-for-major-mode
        :desc "Open project notes"            "p" #'+eduarbo/find-notes-for-project)

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
        :desc "Discover projects"            "D" #'projectile-discover-projects-in-search-path)

      ;;; <leader> q --- session
      (:prefix-map ("q" . "session")
        ;; :desc "Quit Emacs"                   "q" #'evil-quit-all
        ;; :desc "Save and quit Emacs"          "Q" #'evil-save-and-quit
        (:when (featurep! :feature workspaces)
          :desc "Quit Emacs & forget session"  "X" #'+workspace/kill-session-and-quit))

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Line numbers"                 "l" #'display-line-numbers-mode
        :desc "Global Line numbers"          "L" #'global-display-line-numbers-mode
        :desc "Visual line mode"             "v" #'visual-line-mode
        :desc "Subword mode"                 "w" #'subword-mode
        :desc "Frame maximized"              "z" #'toggle-frame-maximized))
