;;; ~/.dotfiles/editor/emacs/doom/+gtd.el -*- lexical-binding: t; -*-
;;;###if (featurep! +gtd)

;;       ▄▄▄   ▄▄ •  ▄▄▄·  ▐ ▄ ▪  ·▄▄▄▄• ▄▄▄· ▄▄▄▄▄▪         ▐ ▄
;; ▪     ▀▄ █·▐█ ▀ ▪▐█ ▀█ •█▌▐███ ▪▀·.█▌▐█ ▀█ •██  ██ ▪     •█▌▐█
;;  ▄█▀▄ ▐▀▀▄ ▄█ ▀█▄▄█▀▀█ ▐█▐▐▌▐█·▄█▀▀▀•▄█▀▀█  ▐█.▪▐█· ▄█▀▄ ▐█▐▐▌
;; ▐█▌.▐▌▐█•█▌▐█▄▪▐█▐█ ▪▐▌██▐█▌▐█▌█▌▪▄█▀▐█ ▪▐▌ ▐█▌·▐█▌▐█▌.▐▌██▐█▌
;;  ▀█▄▀▪.▀  ▀·▀▀▀▀  ▀  ▀ ▀▀ █▪▀▀▀·▀▀▀ • ▀  ▀  ▀▀▀ ▀▀▀ ▀█▄▀▪▀▀ █▪
;;  ▄▄ • ▄▄▄▄▄·▄▄▄▄       ▄▄▄·  ▐ ▄ ·▄▄▄▄       ▄▄▄· ▄▄▄· ▄▄▄   ▄▄▄·
;; ▐█ ▀ ▪•██  ██▪ ██     ▐█ ▀█ •█▌▐███▪ ██     ▐█ ▄█▐█ ▀█ ▀▄ █·▐█ ▀█
;; ▄█ ▀█▄ ▐█.▪▐█· ▐█▌    ▄█▀▀█ ▐█▐▐▌▐█· ▐█▌     ██▀·▄█▀▀█ ▐▀▀▄ ▄█▀▀█
;; ▐█▄▪▐█ ▐█▌·██. ██     ▐█ ▪▐▌██▐█▌██. ██     ▐█▪·•▐█ ▪▐▌▐█•█▌▐█ ▪▐▌
;; ·▀▀▀▀  ▀▀▀ ▀▀▀▀▀•      ▀  ▀ ▀▀ █▪▀▀▀▀▀•     .▀    ▀  ▀ .▀  ▀ ▀  ▀
;;
;; Building a Second Brain in Org-mode using GTD and PARA for organization
;;
;; inbox         => where I collect everything (from laptop and mobile)
;; todo          => tasks, filed in Projects or Areas
;; somedaymaybe  => inactive tasks that I might do at some point in the future
;; agendas       => lists of items to discuss with specific people when I see them next
;; goals         => tracking long-term goals, which Tasks, Projects and Areas move towards


;; ╺┳┓┏━╸┏━╸┏━┓╻ ╻╻  ╺┳╸┏━┓
;;  ┃┃┣╸ ┣╸ ┣━┫┃ ┃┃   ┃ ┗━┓
;; ╺┻┛┗━╸╹  ╹ ╹┗━┛┗━╸ ╹ ┗━┛
;; Defaults

(setq
  org-default-notes-file "inbox.org"
  +org-capture-notes-file org-default-notes-file
  )

(after! org
  (add-to-list 'org-global-properties '("Effort_ALL". "5m 15m 30m 1h 2h 3h 4h 8h")))


;; ┏━┓┏━╸┏━╸┏┓╻╺┳┓┏━┓┏━┓
;; ┣━┫┃╺┓┣╸ ┃┗┫ ┃┃┣━┫┗━┓
;; ╹ ╹┗━┛┗━╸╹ ╹╺┻┛╹ ╹┗━┛
;; Agendas

(use-package! org-super-agenda
  :after org-agenda
  :config (org-super-agenda-mode))

(setq
  org-agenda-files
  '("inbox.org" "todo.org" "somedaymaybe.org" "agendas.org" "waiting.org" "goals.org")
  ;; org-agenda-block-separator ""
  org-agenda-inhibit-startup nil
  org-agenda-show-future-repeats nil
  org-agenda-start-on-weekday nil
  org-agenda-skip-deadline-if-done t
  org-agenda-skip-scheduled-if-done t
  )

;; UI
(add-hook! org-mode
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)
  )

(after! org-agenda
  ;; Timeline
  ;; The org-timeline functionality was recently removed. This code, adapted from
  ;; a comment on Reddit, adds similar functionality back.
  ;; https://www.reddit.com/r/orgmode/comments/7hps9j/rip_orgtimeline/dqt4pfs/
  (add-to-list 'org-agenda-custom-commands
    '("L" "Timeline"
       ((agenda
          ""
          ((org-agenda-span 7)
            (org-agenda-prefix-format '((agenda . " %1c %?-12t% s"))))))))

  ;; Unscheduled Tasks
  (add-to-list 'org-agenda-custom-commands
    '("u" "Unscheduled TODOs"
       ((todo ""
          ((org-agenda-overriding-header "\nUnscheduled TODO")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'todo '("DONE" "NOPE" "MAYB" "WAIT" "SOON"))))))) t)

  ;; Delegated and Waiting Tasks
  (add-to-list 'org-agenda-custom-commands
    '("w" "WAIT" todo "WAIT" ((org-agenda-overriding-header "Delegated and/or Waiting"))) t)
  )

;; TODO Agendas should be full screen!


;; ┏━╸┏━┓┏━┓╺┳╸╻ ╻┏━┓┏━╸   ╺┳╸┏━╸┏┳┓┏━┓╻  ┏━┓╺┳╸┏━╸┏━┓
;; ┃  ┣━┫┣━┛ ┃ ┃ ┃┣┳┛┣╸     ┃ ┣╸ ┃┃┃┣━┛┃  ┣━┫ ┃ ┣╸ ┗━┓
;; ┗━╸╹ ╹╹   ╹ ┗━┛╹┗╸┗━╸    ╹ ┗━╸╹ ╹╹  ┗━╸╹ ╹ ╹ ┗━╸┗━┛
;; Capture templates

(after! org
  (setq
    org-capture-templates
    '(("t" "Task" entry (file "inbox.org")
        "* TODO %?\n")
       ("n" "Note" entry (file "inbox.org")
         "* %u %?\n%i\n%a")
       ("p" "Project" entry (file+headline "todo.org" "Projects")
         (file "templates/newprojecttemplate.org"))
       ("w" "Work" entry (file+olp+datetree "work.org")
         "* %?\nAdded: %U\n" :clock-in t :clock-resume t)
       ("j" "Journal" entry (file+olp+datetree "journal.org")
         "* %?\nAdded: %U\n" :clock-in t :clock-resume t)
       ("s" "Someday" entry (file+headline "somedaymaybe.org" "Someday / Maybe")
         "* SOON %?\n")
       ("m" "Maybe" entry (file+headline "somedaymaybe.org" "Someday / Maybe")
         "* MAYB %?\n")
       ("l" "Log" entry (file+olp+datetree "log.org" "Log")
         "** %<%R>\n%?")
       )
    ))


;; ┏━╸╻  ┏━┓┏━╸╻┏ ╻┏┓╻┏━╸
;; ┃  ┃  ┃ ┃┃  ┣┻┓┃┃┗┫┃╺┓
;; ┗━╸┗━╸┗━┛┗━╸╹ ╹╹╹ ╹┗━┛
;; Clocking

(after! org-clock
  (setq
    org-clock-idle-time nil
    org-clock-persist t
    org-clock-in-switch-to-state "DOIN"
    org-clock-report-include-clocking-task t
    org-clock-out-remove-zero-time-clocks t
    org-clock-into-drawer 1
    )
  )

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(defhydra hydra-org-clock (:color blue :hint nil)
  "
  Clock   In/out^     ^Edit^   ^Summary     (_?_)
  -----------------------------------------
          _i_n         _e_dit   _g_oto entry
          _c_ontinue   _q_uit   _d_isplay
          _o_ut        ^ ^      _r_eport
        "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

(defhydra hydra-org-agenda-clock (:color blue :hint nil)
  "
  Clock   In/out^
  -----------------------------------------
          _i_n
          _g_oto entry
          _o_ut
          _q_uit
        "
  ("i" org-agenda-clock-in)
  ("o" org-agenda-clock-out)
  ("q" org-agenda-clock-cancel)
  ("g" org-agenda-clock-goto))


;; ┏━┓┏━╸┏━╸╻╻  ╻┏┓╻┏━╸
;; ┣┳┛┣╸ ┣╸ ┃┃  ┃┃┗┫┃╺┓
;; ╹┗╸┗━╸╹  ╹┗━╸╹╹ ╹┗━┛
;; Refiling

(setq
  org-refile-targets '((("todo.org" "somedaymaybe.org") :maxlevel . 2))
  ;; org-refile-targets
  ;; '((nil :maxlevel . 1)
  ;;    (org-refile-files :maxlevel . 2))
  ;; org-refile-use-cache t
  org-refile-use-outline-path t
  org-refile-target-verify-function 'bh/verify-refile-target
  )


;; ╺┳╸┏━┓┏━┓╻┏    ╻┏ ┏━╸╻ ╻╻ ╻┏━┓┏━┓╺┳┓┏━┓
;;  ┃ ┣━┫┗━┓┣┻┓   ┣┻┓┣╸ ┗┳┛┃╻┃┃ ┃┣┳┛ ┃┃┗━┓
;;  ╹ ╹ ╹┗━┛╹ ╹   ╹ ╹┗━╸ ╹ ┗┻┛┗━┛╹┗╸╺┻┛┗━┛
;; Task keywords

;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
;;      underlying faces like the `org-todo' face does, so we define our own
;;      intermediary faces that extend from org-todo.
(with-no-warnings
  (custom-declare-face '+org-todo-todo '((t (:foreground "#98BE65" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-doing '((t (:foreground "#FF6C6B" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-wait '((t (:foreground "#84DBC7" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-done '((t (:foreground "#5B6268" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-canceled '((t (:foreground "#3F444A" :strike-through t :inherit org-todo))) "")
  (custom-declare-face '+org-todo-next '((t (:foreground "#FCDC7C" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-maybe '((t (:foreground "#B4A1EC" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-someday '((t (:foreground "#B4A1EC" :inherit org-todo))) "")
  )

(after! org
  ;; "@" means to add a note (with time)
  ;; "!" means to record only the time of the state change
  ;; With X and Y being either "@" or "!", "X/Y" means use X when entering the
  ;; state, and use Y when leaving the state if and only if the *target* state
  ;; does not define X. You may omit any of the fast-selection key or X or /Y,
  ;; so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid
  (setq org-todo-keywords
    '((sequence "[-](D)" "[ ](T)" "[?](W)" "|" "[X](X)")
       (sequence "DOIN(d)" "NEXT(n)" "TODO(t)" "WAIT(w@/!)" "|" "DONE(x)" "NOPE(k@)")
       (sequence "SOON(s)" "MAYB(m)" "READ(r)" "VIEW(v)" "|" "DONE(x)" "NOPE(k@)")))

  (setq org-todo-keyword-faces
    '(
       ("[X]" . +org-todo-done)
       ("[-]" . +org-todo-doing)
       ("[?]" . +org-todo-wait)
       ("TODO" . +org-todo-todo)
       ("DOIN" . +org-todo-doing)
       ("NEXT" . +org-todo-next)
       ("WAIT" . +org-todo-wait)
       ("MAYB" . +org-todo-maybe)
       ("SOON" . +org-todo-someday)
       ("DONE" . +org-todo-done)
       ("NOPE" . +org-todo-canceled)
       ("READ" . +org-todo-wait)
       ("VIEW" . +org-todo-wait)
       )
    )
  )


;; ╺┳╸┏━┓┏━╸┏━╸╻┏┓╻┏━╸
;;  ┃ ┣━┫┃╺┓┃╺┓┃┃┗┫┃╺┓
;;  ╹ ╹ ╹┗━┛┗━┛╹╹ ╹┗━┛
;; Tagging

(setq
  org-tag-alist '(
                   ;; Depth
                   ("@immersive" . ?i) ;; "Deep"
                   ("@process" . ?p) ;; "Shallow"
                   ;; Context
                   ("@work" . ?w)
                   ("@home" . ?h)
                   ("@errand" . ?e)
                   ;; Time
                   ("15min" . ?<)
                   ("30min" . ?=)
                   ("1h" . ?>)
                   ;; Energy
                   ("Challenge" . ?1)
                   ("Average" . ?2)
                   ("Easy" . ?3)
                   ))

;; Random Note
(use-package! org-randomnote
  :commands org-randomnote
  :init
  (setq org-randomnote-candidates (list (expand-file-name "todo.org" org-directory))))
