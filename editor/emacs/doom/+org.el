;;; ~/dev/dotfiles/editor/emacs/doom/+org.el -*- lexical-binding: t; -*-

;;       ▄▄▄   ▄▄ •      • ▌ ▄ ·.       ·▄▄▄▄  ▄▄▄ .
;; ▪     ▀▄ █·▐█ ▀ ▪     ·██ ▐███▪▪     ██▪ ██ ▀▄.▀·
;;  ▄█▀▄ ▐▀▀▄ ▄█ ▀█▄     █ ▌▐▌▐█· ▄█▀▄ ▐█· ▐█▌▐▀▀▪▄
;; ▐█▌.▐▌▐█•█▌▐█▄▪▐█     ██ ██▌▐█▌▐█▌.▐▌██. ██ ▐█▄▄▌
;;  ▀█▄▀▪.▀  ▀·▀▀▀▀      ▀▀  █▪▀▀▀ ▀█▄▀▪▀▀▀▀▀•  ▀▀▀

;; Building a Second Brain in Org-mode using GTD and PARA for organization


;; ┏━┓┏━┓╺┳╸╻ ╻┏━┓
;; ┣━┛┣━┫ ┃ ┣━┫┗━┓
;; ╹  ╹ ╹ ╹ ╹ ╹┗━┛

(setq org-directory (expand-file-name "~/org"))

(defvar +org-default-notes-dir (expand-file-name "notes" org-directory)
  "Directory of notes")

(defvar +org-default-projects-dir +org-default-notes-dir
  "Directory of project notes, usually repos")

(defvar +org-default-inbox-file (expand-file-name "inbox.org" +org-default-notes-dir)
  "New stuff collects in this file")

(defvar +org-default-todo-file (expand-file-name "todo.org" +org-default-notes-dir)
  "Tasks, TODOs and little projects")

(defvar +org-default-incubate-file (expand-file-name "incubate.org" +org-default-notes-dir)
  "Ideas simmering on back burner")


;; ╺┳┓┏━╸┏━╸┏━┓╻ ╻╻  ╺┳╸┏━┓
;;  ┃┃┣╸ ┣╸ ┣━┫┃ ┃┃   ┃ ┗━┓
;; ╺┻┛┗━╸╹  ╹ ╹┗━┛┗━╸ ╹ ┗━┛

(after! org
  (setq
    org-default-notes-file +org-default-inbox-file
    org-archive-location "archive/%s::"
    org-agenda-files (list +org-default-todo-file +org-default-incubate-file)

    org-log-done 'time
    org-log-into-drawer t

    org-startup-indented t
    org-startup-truncated nil
    org-startup-with-inline-images t

    org-clone-delete-id t
    org-hide-emphasis-markers t
    org-image-actual-width '(600)
    org-imenu-depth 5
    org-outline-path-complete-in-steps nil
    org-pretty-entities t
    org-priority-start-cycle-with-default t

    ;; org-yank-adjusted-subtrees t

    org-ellipsis "  "

    org-highest-priority ?A
    org-default-priority ?B
    org-lowest-priority ?C

    org-file-apps
    '((auto-mode . emacs)
       ("\\.x?html?\\'" . "firefox %s")
       ("\\.pdf\\'" . "open %s"))
    )

  (add-to-list 'org-global-properties '("Effort_ALL". "5m 15m 30m 1h 2h 3h 4h 8h"))
  )

(use-package! org-id ; built-in
  :after org
  :init
  ;; By using unique ID's, links will work even if you move them across files
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; Load org-habit with org.el
(after! org-habit
  (setq org-habit-graph-column 105)
  (add-to-list 'org-modules 'org-habit t))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("𐄙" "𐄚" "𐄛" "𐄜" "𐄝" "𐄞" "𐄟" "𐄠" "𐄡")))

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("" "" "")))


;; ┏━┓   ┏━╸   ┏━┓   ╺┳╸   ╻ ╻   ┏━╸   ╺┳╸   ╻   ┏━╸
;; ┣━┫   ┣╸    ┗━┓    ┃    ┣━┫   ┣╸     ┃    ┃   ┃
;; ╹ ╹   ┗━╸   ┗━┛    ╹    ╹ ╹   ┗━╸    ╹    ╹   ┗━╸

(custom-set-faces!
  '((org-document-info-keyword org-drawer)
     :foreground "#797194")
  `((org-special-keyword org-meta-line)
     :foreground ,(doom-color 'comments))
  '(org-document-title
     :weight normal
     :height 1.4)
  '(outline-1
     :weight normal
     :height 1.2)
  '(outline-2
     :weight normal
     :height 1.1)
  '((link outline-3 outline-4 outline-5 outline-6 outline-7 outline-8)
     :weight normal))


;; ┏━╸┏━┓┏━┓╺┳╸╻ ╻┏━┓┏━╸   ╺┳╸┏━╸┏┳┓┏━┓╻  ┏━┓╺┳╸┏━╸┏━┓
;; ┃  ┣━┫┣━┛ ┃ ┃ ┃┣┳┛┣╸     ┃ ┣╸ ┃┃┃┣━┛┃  ┣━┫ ┃ ┣╸ ┗━┓
;; ┗━╸╹ ╹╹   ╹ ┗━┛╹┗╸┗━╸    ╹ ┗━╸╹ ╹╹  ┗━╸╹ ╹ ╹ ┗━╸┗━┛

(after! org
  (setq
    org-capture-templates
    '(
       ("n" "Note" entry
         (file +org-default-inbox-file)
         (file "templates/new-note.org") :prepend t)
       ("N" "Note From" entry
         (file +org-default-inbox-file)
         (file "templates/new-note-from.org") :prepend t)
       ("t" "Task" entry
         (file +org-default-inbox-file)
         (file "templates/new-task.org") :prepend t)
       ("T" "Task From" entry
         (file +org-default-inbox-file)
         (file "templates/new-task-from.org") :prepend t)
       ("l" "Log" entry
         (file +org-default-inbox-file)
         (file "templates/new-log.org") :prepend t)
       ("L" "Log From" entry
         (file +org-default-inbox-file)
         (file "templates/new-log-from.org") :prepend t)
       ("k" "Cliplink" entry
         (file +org-default-inbox-file)
         (file "templates/new-cliplink.org") :prepend t)

       ;; Will use {org-default-projects-dir}/{project-root}.org
       ("p" "Templates for projects")
       ("pn" "Note" entry
         (file+headline +org-org-capture-project-file "Notes")
         (file "templates/new-note.org") :prepend t)
       ("pn" "Note From" entry
         (file+headline +org-org-capture-project-file "Notes")
         (file "templates/new-note-from.org") :prepend t)
       ("pt" "Task" entry
         (file+headline +org-org-capture-project-file "Tasks")
         (file "templates/new-task.org") :prepend t)
       ("pT" "Task From" entry
         (file+headline +org-org-capture-project-file "Tasks")
         (file "templates/new-task-from.org") :prepend t)
       ("pl" "Log" entry
         (file+headline +org-org-capture-project-file "Log")
         (file "templates/new-log.org") :prepend t)
       ("pL" "Log From" entry
         (file+headline +org-org-capture-project-file "Log")
         (file "templates/new-log-from.org") :prepend t)
       ("pr" "Resource" entry
         (file+headline +org-org-capture-project-file "Resources")
         (file "templates/new-resource.org") :prepend t)
       ("pk" "Cliplink" entry
         (file+headline +org-org-capture-project-file "Resources")
         (file "templates/new-cliplink.org") :prepend t)
       ))
  )


;; ┏━╸╻  ┏━┓┏━╸╻┏ ╻┏┓╻┏━╸
;; ┃  ┃  ┃ ┃┃  ┣┻┓┃┃┗┫┃╺┓
;; ┗━╸┗━╸┗━┛┗━╸╹ ╹╹╹ ╹┗━┛

(defvar +org-clock-heading-outline-path-separator "   ·   "
  "Separator for the clock heading generated from the outline path")

;; override doom's org-clock setup to fix autoload for +org/toggle-clock
(use-package! org-clock ; built-in
  :commands (org-clock-save org-clocking-p org-clock-load)
  :config

  (setq
    ;; Too many clock entries clutter up a heading
    org-clock-into-drawer t

    org-show-notification-handler 'message
    org-clock-clocked-in-display 'frame-title
    org-clock-idle-time nil
    org-clock-persist t
    org-clock-in-switch-to-state "DOIN"
    org-clock-report-include-clocking-task t
    org-clock-out-remove-zero-time-clocks t

    org-clock-heading-function '+org-clock-heading-reversed-outline-path
    )

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)
  )


;; ┏┳┓╻ ╻   ┏━┓┏━┓┏━╸   ┏━╸╻  ╻┏━┓╻  ╻┏┓╻╻┏
;; ┃┃┃┗┳┛   ┃ ┃┣┳┛┃╺┓╺━╸┃  ┃  ┃┣━┛┃  ┃┃┗┫┣┻┓
;; ╹ ╹ ╹    ┗━┛╹┗╸┗━┛   ┗━╸┗━╸╹╹  ┗━╸╹╹ ╹╹ ╹

(use-package! org-cliplink
  :after org)


;; ┏━┓┏━┓┏━╸    ┏┓┏━┓╻ ╻┏━┓┏┓╻┏━┓╻
;; ┃ ┃┣┳┛┃╺┓╺━╸  ┃┃ ┃┃ ┃┣┳┛┃┗┫┣━┫┃
;; ┗━┛╹┗╸┗━┛   ┗━┛┗━┛┗━┛╹┗╸╹ ╹╹ ╹┗━╸
;; org-journal

(setq org-journal-dir (expand-file-name "journal/" +org-default-notes-dir))

(after! org-journal
  ;; Disable default org template for the journal
  (set-file-template! "\\/journal/.+\\.org$" :ignore t)

  (setq
    org-extend-today-until 4 ;; sometimes my days end at 4am
    org-journal-carryover-items nil
    org-journal-file-type 'weekly
    ;; Check the function "format-time-string" for
    org-journal-file-format "%Y/W%W %Y-%m-%d.org"
    org-journal-date-format "%A, %-e %B %Y"
    org-journal-time-format "%-I:%M %p"
    org-journal-time-prefix "** "
    )
  )


;; ┏━┓┏━┓┏━╸   ┏━┓┏━┓┏━┓┏┳┓
;; ┃ ┃┣┳┛┃╺┓╺━╸┣┳┛┃ ┃┣━┫┃┃┃
;; ┗━┛╹┗╸┗━┛   ╹┗╸┗━┛╹ ╹╹ ╹
;; org-roam

(after! org-roam
  (setq
    org-roam-directory +org-default-notes-dir
    org-roam-capture-templates
    '(("d" "default" plain #'org-roam-capture--get-point
        "%?"
        ;; :file-name "%(format-time-string \"%Y-%m-%d--%H-%M-%SZ--${slug}\" (current-time) t)"
        :file-name "${slug}"
        :head "#+TITLE: ${title}\n"
        :unnarrowed t))
    )
  )


;; ┏━┓┏━╸┏━╸╻╻  ╻┏┓╻┏━╸
;; ┣┳┛┣╸ ┣╸ ┃┃  ┃┃┗┫┃╺┓
;; ╹┗╸┗━╸╹  ╹┗━╸╹╹ ╹┗━┛

(after! org
  (setq
    org-refile-targets
    '((nil :maxlevel . 2)
       (org-agenda-files :maxlevel . 2))
    org-refile-use-outline-path t
    org-refile-target-verify-function 'bh/verify-refile-target
    )
  )


;; ╺┳╸┏━┓┏━╸┏━╸╻┏┓╻┏━╸
;;  ┃ ┣━┫┃╺┓┃╺┓┃┃┗┫┃╺┓
;;  ╹ ╹ ╹┗━┛┗━┛╹╹ ╹┗━┛

(setq
  org-tag-alist
  '(
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
     )
  )


;; ╺┳╸┏━┓╺┳┓┏━┓   ╻┏ ┏━╸╻ ╻╻ ╻┏━┓┏━┓╺┳┓┏━┓
;;  ┃ ┃ ┃ ┃┃┃ ┃   ┣┻┓┣╸ ┗┳┛┃╻┃┃ ┃┣┳┛ ┃┃┗━┓
;;  ╹ ┗━┛╺┻┛┗━┛   ╹ ╹┗━╸ ╹ ┗┻┛┗━┛╹┗╸╺┻┛┗━┛

;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
;;      underlying faces like the `org-todo' face does, so we define our own
;;      intermediary faces that extend from org-todo.
(with-no-warnings
  (custom-declare-face '+org-todo-project '((t (:foreground "#2BE1B1" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-doing '((t (:foreground "#FF6C6B" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-next '((t (:foreground "#FCDC7C" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-todo '((t (:foreground "#98BE65" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-read '((t (:foreground "#EDE7D4" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-wait '((t (:foreground "#ABE4F8" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-someday '((t (:foreground "#B4A1EC" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-maybe '((t (:foreground "#B4A1EC" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-done '((t (:foreground "#5B6268" :inherit org-todo))) "")
  (custom-declare-face '+org-todo-canceled '((t (:foreground "#3F444A" :strike-through t :inherit org-todo))) "")
  )

(after! org
  ;; "@" means to add a note (with time)
  ;; "!" means to record only the time of the state change
  ;; With X and Y being either "@" or "!", "X/Y" means use X when entering the
  ;; state, and use Y when leaving the state if and only if the *target* state
  ;; does not define X. You may omit any of the fast-selection key or X or /Y,
  ;; so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid
  (setq org-todo-keywords
    '(
       (sequence
         "TODO(t)"     ; A task that needs doing & is ready to do
         "DOIN(d)"     ; A task that is in progress
         "NEXT(n)"     ; Next task to do in an area or project
         "PROJ(p)"     ; An ongoing project that cannot be completed in one step
         "WAIT(w@/!)"  ; Something is holding up this task; or it is paused
         "|")
       (sequence
         "READ(r)"     ; Read it later
         "VIEW(v)"     ; Review/Watch it later
         "SOON(s)"     ; Someday I will do it
         "MAYB(m)"     ; Maybe I will do it
         "|")
       (sequence
         "|"
         "DONE(x)"     ; Task successfully completed
         "NOPE(k@)"    ; Task was cancelled, aborted or is no longer applicable
         )
       (sequence
         "[-](D)"      ; Task is in progress
         "[ ](T)"      ; A task that needs doing
         "[?](W)"      ; Task is being held up or paused
         "|"
         "[X](X)"      ; Task was completed
         )
       ))

  (setq org-todo-keyword-faces
    '(
       ("[X]" . +org-todo-done)
       ("[-]" . +org-todo-doing)
       ("[?]" . +org-todo-wait)
       ("PROJ" . +org-todo-project)
       ("DOIN" . +org-todo-doing)
       ("NEXT" . +org-todo-next)
       ("TODO" . +org-todo-todo)
       ("READ" . +org-todo-read)
       ("VIEW" . +org-todo-read)
       ("WAIT" . +org-todo-wait)
       ("SOON" . +org-todo-someday)
       ("MAYB" . +org-todo-maybe)
       ("DONE" . +org-todo-done)
       ("NOPE" . +org-todo-canceled)
       )
    )
  )
