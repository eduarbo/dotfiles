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
;; todo          => Projects, Area tasks and Resources


;; ╺┳┓┏━╸┏━╸┏━┓╻ ╻╻  ╺┳╸┏━┓
;;  ┃┃┣╸ ┣╸ ┣━┫┃ ┃┃   ┃ ┗━┓
;; ╺┻┛┗━╸╹  ╹ ╹┗━┛┗━╸ ╹ ┗━┛
;; Defaults

(after! org
  ;; New stuff collects in this file
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory)))

(defvar org-default-notes-dir (expand-file-name "notes" org-directory)
  "Directory of un-shareable, personal notes")
(defvar org-default-projects-dir (expand-file-name "projects" org-directory)
  "Directory of project notes, usually repos")
(defvar org-default-tasks-file (expand-file-name "todo.org" org-default-notes-dir)
  "Tasks, TODOs and little projects")
(defvar org-default-incubate-file (expand-file-name "incubate.org" org-default-notes-dir)
  "Ideas simmering on back burner")

;; ┏━┓┏━╸┏━╸┏┓╻╺┳┓┏━┓┏━┓
;; ┣━┫┃╺┓┣╸ ┃┗┫ ┃┃┣━┫┗━┓
;; ╹ ╹┗━┛┗━╸╹ ╹╺┻┛╹ ╹┗━┛
;; Agendas

(use-package! org-super-agenda
  :after org-agenda
  :config (org-super-agenda-mode))

(after! org
  (setq
    org-agenda-files (list org-default-tasks-file org-default-projects-dir org-default-notes-dir)
    ;; org-agenda-block-separator ""
    org-agenda-inhibit-startup nil
    org-agenda-show-future-repeats nil
    org-agenda-start-on-weekday nil
    org-agenda-skip-deadline-if-done t
    org-agenda-skip-scheduled-if-done t
    ))

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
    '(
       ("n" "Note" entry
         (file org-default-notes-file)
         (file "templates/new-note.org") :prepend t)
       ("N" "Note From" entry
         (file org-default-notes-file)
         (file "templates/new-note-from.org") :prepend t)
       ("t" "Task" entry
         (file org-default-notes-file)
         (file "templates/new-task.org") :prepend t)
       ("T" "Task From" entry
         (file org-default-notes-file)
         (file "templates/new-task-from.org") :prepend t)
       ("l" "Log" entry
         (file org-default-notes-file)
         (file "templates/new-log.org") :prepend t)
       ("L" "Log From" entry
         (file org-default-notes-file)
         (file "templates/new-log-from.org") :prepend t)
       ("k" "Cliplink" entry
         (fil org-default-notes-file)
         (file "templates/new-cliplink.org") :prepend t)

       ;; Will use {org-default-projects-dir}/{project-root}.org
       ("p" "Templates for projects")
       ("pn" "Note" entry
         (file+headline +eduarbo-org-capture-project-file "Notes")
         (file "templates/new-note.org") :prepend t)
       ("pn" "Note From" entry
         (file+headline +eduarbo-org-capture-project-file "Notes")
         (file "templates/new-note-from.org") :prepend t)
       ("pt" "Task" entry
         (file+headline +eduarbo-org-capture-project-file "Tasks")
         (file "templates/new-task.org") :prepend t)
       ("pT" "Task From" entry
         (file+headline +eduarbo-org-capture-project-file "Tasks")
         (file "templates/new-task-from.org") :prepend t)
       ("pl" "Log" entry
         (file+headline +eduarbo-org-capture-project-file "Log")
         (file "templates/new-log.org") :prepend t)
       ("pL" "Log From" entry
         (file+headline +eduarbo-org-capture-project-file "Log")
         (file "templates/new-log-from.org") :prepend t)
       ("pr" "Resource" entry
         (file+headline +eduarbo-org-capture-project-file "Resources")
         (file "templates/new-resource.org") :prepend t)
       ("pk" "Cliplink" entry
         (file+headline +eduarbo-org-capture-project-file "Resources")
         (file "templates/new-cliplink.org") :prepend t)
       ))
  )


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

(after! org
  (setq
    org-refile-targets
    '((nil :maxlevel . 2)
       (org-agenda-files :maxlevel . 2))
    org-refile-use-outline-path t
    org-refile-target-verify-function 'bh/verify-refile-target
    ))


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
  (add-to-list 'org-global-properties '("Effort_ALL". "5m 15m 30m 1h 2h 3h 4h 8h"))

  ;; "@" means to add a note (with time)
  ;; "!" means to record only the time of the state change
  ;; With X and Y being either "@" or "!", "X/Y" means use X when entering the
  ;; state, and use Y when leaving the state if and only if the *target* state
  ;; does not define X. You may omit any of the fast-selection key or X or /Y,
  ;; so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid
  (setq org-todo-keywords
    '((sequence "DOIN(d)" "NEXT(n)" "|")
       (sequence "TODO(t)" "WAIT(w@/!)" "|")
       (sequence "SOON(s)" "MAYB(m)" "|")
       (sequence "READ(r)" "VIEW(v)" "|")
       (sequence "|" "DONE(x)" "NOPE(k@)")
       (sequence "[ ](T)" "[-](D)" "[?](W)" "|" "[X](X)")))

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

  (setq
    org-highest-priority ?A
    org-default-priority ?B
    org-lowest-priority ?C
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
