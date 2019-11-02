;;; ~/.dotfiles/editor/emacs/doom/+gtd.el -*- lexical-binding: t; -*-

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

(setq org-templates-dir (expand-file-name "templates" org-directory))

(setq org-default-notes-file "inbox.org")
(setq +org-capture-todo-file org-default-notes-file)


;; ┏━┓┏━╸┏━╸┏┓╻╺┳┓┏━┓┏━┓
;; ┣━┫┃╺┓┣╸ ┃┗┫ ┃┃┣━┫┗━┓
;; ╹ ╹┗━┛┗━╸╹ ╹╺┻┛╹ ╹┗━┛
;; Agendas

(setq org-agenda-files
      '("inbox.org" "todo.org" "somedaymaybe.org" "agendas.org" "waiting.org" "goals.org")
      ;; org-agenda-block-separator ""
      org-agenda-inhibit-startup nil
      org-agenda-show-future-repeats nil
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t)

(use-package! org-super-agenda
  :init (org-super-agenda-mode))

;; TODO Setup only useful agendas
;; (setq org-agenda-custom-commands
;;       '(("g" . "GTD contexts")
;;         ;; ("gh" "Home" tags-todo "HOME")
;;         ("gl" "Later" tags-todo "LATER")
;;         ("G" "GTD Block Agenda"
;;          ((todo "ACTIVE")
;;           (todo "NEXT"))
;;          ((org-agenda-prefix-format "[ ] %T: ")
;;           (org-agenda-with-colors nil)
;;           (org-agenda-compact-blocks t)
;;           (org-agenda-remove-tags t)
;;           (ps-number-of-columns 2)
;;           (ps-landscape-mode t))
;;          ;;nil                      ;; i.e., no local settings
;;          )))

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
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'todo '("DONE" "CANCELLED" "MAYBE" "WAITING" "SOMEDAY"))))))) t)

;; Delegated and Waiting Tasks
(add-to-list 'org-agenda-custom-commands
             '("w" "WAITING" todo "WAITING" ((org-agenda-overriding-header "Delegated and/or Waiting"))) t)

;; Agendas should be full screen!
(add-hook 'org-agenda-finalize-hook (lambda () (delete-other-windows)))


;; ┏━╸┏━┓┏━┓╺┳╸╻ ╻┏━┓┏━╸   ╺┳╸┏━╸┏┳┓┏━┓╻  ┏━┓╺┳╸┏━╸┏━┓
;; ┃  ┣━┫┣━┛ ┃ ┃ ┃┣┳┛┣╸     ┃ ┣╸ ┃┃┃┣━┛┃  ┣━┫ ┃ ┣╸ ┗━┓
;; ┗━╸╹ ╹╹   ╹ ┗━┛╹┗╸┗━╸    ╹ ┗━╸╹ ╹╹  ┗━╸╹ ╹ ╹ ┗━╸┗━┛
;; Capture templates

(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "inbox.org" "Tasks")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
        ("n" "Note" entry
         (file+headline "inbox.org" "Notes")
         "* %u %?\n%i\n%a" :prepend t :kill-buffer t)
        ("p" "Project" entry (file+headline "todo.org" "Projects")
         (file "templates/newprojecttemplate.org"))
        ("s" "Someday" entry (file+headline "somedaymaybe.org" "Someday / Maybe")
         "* SOMEDAY %?\n")
        ("m" "Maybe" entry (file+headline "somedaymaybe.org" "Someday / Maybe")
         "* MAYBE %?\n")
        ;; TODO create the template
        ;; ("l" "Log" entry (file+olp+datetree "log.org" "Log")
        ;;  (file "templates/logtemplate.org"))
      ))


;; ┏━╸╻  ┏━┓┏━╸╻┏ ╻┏┓╻┏━╸
;; ┃  ┃  ┃ ┃┃  ┣┻┓┃┃┗┫┃╺┓
;; ┗━╸┗━╸┗━┛┗━╸╹ ╹╹╹ ╹┗━┛
;; Clocking

(setq org-log-done 'time
        org-clock-idle-time nil
        org-clock-continuously nil
        org-clock-persist t
        org-clock-in-switch-to-state "STARTED"
        org-clock-in-resume nil
        org-clock-report-include-clocking-task t
        org-clock-out-remove-zero-time-clocks t
        ;; Too many clock entries clutter up a heading
        org-log-into-drawer t
        org-clock-into-drawer 1)

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
(setq org-refile-files (let ((default-directory org-directory))
                         (cl-mapcar 'expand-file-name '("todo.org" "somedaymaybe.org"))))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-refile-files :maxlevel . 3))
      ;; org-refile-use-cache t
      org-refile-use-outline-path t)

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; ╺┳╸┏━┓┏━┓╻┏    ╻┏ ┏━╸╻ ╻╻ ╻┏━┓┏━┓╺┳┓┏━┓
;;  ┃ ┣━┫┗━┓┣┻┓   ┣┻┓┣╸ ┗┳┛┃╻┃┃ ┃┣┳┛ ┃┃┗━┓
;;  ╹ ╹ ╹┗━┛╹ ╹   ╹ ╹┗━╸ ╹ ┗┻┛┗━┛╹┗╸╺┻┛┗━┛
;; Task keywords

(setq org-todo-keywords
      '((sequence "[ ](i)" "[-](p)" "[?](m)" "|" "[X](x)")
        (sequence "TODO(t)" "ACTIVE(a)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "LATER(l)" "MAYBE(m)" "SOMEDAY(s)" "IDEA(i)" "|" "CANCELLED(c)"))

      org-todo-keyword-faces
      '(("[-]" :inherit font-lock-constant-face :weight bold)
        ("[?]" :inherit warning :weight bold)
        ("TODO" :inherit error :weight bold)
        ("ACTIVE" :inherit warning :weight bold)
        ("NEXT" :inherit success :weight bold)
        ("WAITING" :inherit default :weight bold)
        ("TODAY" :foreground "#dd8844" :weight bold)
        ("LATER" :foreground "#44b9b1" :weight bold)
        ("IDEA" :foreground "#5699AF" :weight bold)
        ("MAYBE" :foreground "#5699AF" :weight bold)
        ("SOMEDAY" :foreground "#5699AF" :weight bold)))


;; ╺┳╸┏━┓┏━╸┏━╸╻┏┓╻┏━╸
;;  ┃ ┣━┫┃╺┓┃╺┓┃┃┗┫┃╺┓
;;  ╹ ╹ ╹┗━┛┗━┛╹╹ ╹┗━┛
;; Tagging

(setq org-tag-alist '(
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
