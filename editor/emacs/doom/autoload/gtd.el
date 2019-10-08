;;; ~/.dotfiles/editor/emacs/doom/autoload/gtd.el -*- lexical-binding: t; -*-


;; ┏━┓┏━╸┏━╸┏┓╻╺┳┓┏━┓┏━┓
;; ┣━┫┃╺┓┣╸ ┃┗┫ ┃┃┣━┫┗━┓
;; ╹ ╹┗━┛┗━╸╹ ╹╺┻┛╹ ╹┗━┛
;; Agendas

;;;###autoload
(defun open-agenda ()
  "Opens the org-agenda."
  (interactive)
  (let ((agenda "*Org Agenda*"))
    (if (equal (get-buffer agenda) nil)
        (org-agenda-list)
      (unless (equal (buffer-name (current-buffer)) agenda)
        (switch-to-buffer agenda))
      (org-agenda-redo t)
      (beginning-of-buffer))))

;;;###autoload
(defun org-buffer-todo ()
  (interactive)
  "Creates a todo-list for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), t (todo-list)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-todo-list)))

;;;###autoload
(defun org-buffer-agenda ()
  (interactive)
  "Creates an agenda for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), a (agenda-list)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)))

;;;###autoload
(defun org-buffer-day-agenda ()
  (interactive)
  "Creates an agenda for the current buffer. Equivalent to the sequence: org-agenda, < (restrict to current buffer), a (agenda-list), d (org-agenda-day-view)."
  (progn
    (org-agenda-set-restriction-lock 'file)
    (org-agenda-list)
    (org-agenda-day-view))) ;; Maybe I should try writing a Emacs Lisp macro for this kind of thing!

(defun my-org-super-agenda ()
  (interactive)
  (let ((org-super-agenda-groups
         '((:name "Schedule"
                  :time-grid t)
           ;; After the last group, the agenda will display items that didn't
           ;; match any of these groups, with the default order position of 99
           ;; To prevent this, add this code:
           ;; (:discard (:anything t))
           )))
    (org-agenda nil "a")))

;;;###autoload
(defun my-org-super-agenda-today ()
  (interactive)
  (progn
    (my-org-super-agenda)
    (org-agenda-day-view)))

;;;###autoload
(defun my-personal-agenda ()
  (interactive)
  (let ((org-super-agenda-groups
         '(;; After the last group, the agenda will display items that didn't
           ;; match any of these groups, with the default order position of 99
           ;; To prevent this, add this code:
           ;; (:discard (:tag ("maple")))
           )))
    (org-agenda nil "a")
    (org-agenda-day-view)))

;; Open Loops

;; https://www.reddit.com/r/orgmode/comments/7kddjq/org_agenda_view_for_scheduled_events_t_2/
;; Thanks Sacha for this custom code!

;;;###autoload
(defun my-org-agenda-recent-open-loops ()
  (interactive)
  (let ((org-agenda-start-with-log-mode t)
        (org-agenda-use-time-grid nil)
        (org-agenda-files '("calendar/gcal.org")))
    (fetch-calendar)
    (org-agenda-list nil (org-read-date nil nil "-2d") 4)
    (beginend-org-agenda-mode-goto-beginning)))

;;;###autoload
(defun my-org-agenda-longer-open-loops ()
  (interactive)
  (let ((org-agenda-start-with-log-mode t)
        (org-agenda-use-time-grid nil)
        (org-agenda-files '("calendar/gcal.org")))
    (fetch-calendar)
    (org-agenda-list 'file (org-read-date nil nil "-14d") 28)
    (beginend-org-agenda-mode-goto-beginning)))


;; ┏━┓┏━┓┏━┓ ┏┓┏━╸┏━╸╺┳╸┏━┓
;; ┣━┛┣┳┛┃ ┃  ┃┣╸ ┃   ┃ ┗━┓
;; ╹  ╹┗╸┗━┛┗━┛┗━╸┗━╸ ╹ ┗━┛
;; Projects

;; A project is "any outcome that will take more than one action step to
;; complete." As a result of implementing Tiago Forte's "PARA" system, I can
;; ensure that I always have an up to date project list.

;;;###autoload
(defun go-to-projects ()
  (interactive)
  (find-file (expand-file-name "todo.org" org-directory))
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* Projects")
  (beginning-of-line))

;;;###autoload
(defun project-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?p)
  (org-columns))

;;;###autoload
(defun project-deadline-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?d)
  (org-columns))

;; The concept of Stuck Projects comes from David Allen's GTD. A stuck project
;; is a project without any action steps or tasks associated with it.

;; Org-Mode has the ability to tell you which subtrees don't have tasks
;; associated with them. You can also configure what it recognizes as a stuck
;; project. Unfortunately, by default, this functionality picks up a lot of
;; noise.

;; This function creates an agenda of stuck projects that is restricted to my
;; "Projects" subtree.

;;;###autoload
(defun my-org-agenda-list-stuck-projects ()
  (interactive)
  (go-to-projects)
  (org-agenda nil "#" 'subtree))


;; ┏━┓┏━┓┏━╸┏━┓┏━┓
;; ┣━┫┣┳┛┣╸ ┣━┫┗━┓
;; ╹ ╹╹┗╸┗━╸╹ ╹┗━┛
;; Areas

;;;###autoload
(defun go-to-areas ()
  (interactive)
  (find-file (expand-file-name "todo.org" org-directory))
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* Areas")
  (beginning-of-line))

;;;###autoload
(defun areas-overview ()
  (interactive)
  (go-to-areas)
  (org-narrow-to-subtree)
  (org-columns))


;; ┏━┓┏━╸╻ ╻╻┏━╸╻ ╻┏━┓
;; ┣┳┛┣╸ ┃┏┛┃┣╸ ┃╻┃┗━┓
;; ╹┗╸┗━╸┗┛ ╹┗━╸┗┻┛┗━┛
;; Reviews

;;;###autoload
(defun my-new-daily-review ()
  (interactive)
  (f-touch "/tmp/reviews.org")
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "/tmp/reviews.org")
                                  (file "templates/dailyreviewtemplate.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

;;;###autoload
(defun my-new-weekly-review ()
  (interactive)
  (f-touch "/tmp/reviews.org")
  (let ((org-capture-templates '(("w" "Review: Weekly Review" entry (file+olp+datetree "/tmp/reviews.org")
                                  (file "templates/weeklyreviewtemplate.org")))))
    (progn
      (org-capture nil "w")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

;;;###autoload
(defun my-new-monthly-review ()
  (interactive)
  (f-touch "/tmp/reviews.org")
  (let ((org-capture-templates '(("m" "Review: Monthly Review" entry (file+olp+datetree "/tmp/reviews.org")
                                  (file "templates/monthlyreviewtemplate.org")))))
    (progn
      (org-capture nil "m")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))


;; ┏┳┓╻┏━┓┏━╸
;; ┃┃┃┃┗━┓┃
;; ╹ ╹╹┗━┛┗━╸
;; Misc

;; Auto Advance

;;;###autoload
(defun org-agenda-set-tags-auto-advance ()
  (interactive)
  (while t
    (call-interactively #'org-agenda-set-tags)
    (org-agenda-next-line)))


;; Remove empty LOGBOOK drawers on clock out, from Michael Englehorn's Emacs
;; Configuration
;; https://michael.englehorn.com/config.html
;;
;; This Stack Overflow post shows the fix to the bug in the original function
;; (remove the "LOGBOOK" specification)
;; https://stackoverflow.com/questions/21767471/org-capture-and-time-clocking-misbehaving#21797427
;;;###autoload
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))


;; Exclude completed tasks from refile targets, from Michael Englehorn's Emacs
;; Configuration
;; https://michael.englehorn.com/config.html
;;;###autoload
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
