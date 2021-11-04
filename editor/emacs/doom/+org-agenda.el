;;; editor/emacs/doom/+agendas.el -*- lexical-binding: t; -*-

;;       ▄▄▄   ▄▄ •      ▄▄▄·  ▄▄ • ▄▄▄ . ▐ ▄ ·▄▄▄▄   ▄▄▄·
;; ▪     ▀▄ █·▐█ ▀ ▪    ▐█ ▀█ ▐█ ▀ ▪▀▄.▀·•█▌▐███▪ ██ ▐█ ▀█
;;  ▄█▀▄ ▐▀▀▄ ▄█ ▀█▄    ▄█▀▀█ ▄█ ▀█▄▐▀▀▪▄▐█▐▐▌▐█· ▐█▌▄█▀▀█
;; ▐█▌.▐▌▐█•█▌▐█▄▪▐█    ▐█ ▪▐▌▐█▄▪▐█▐█▄▄▌██▐█▌██. ██ ▐█ ▪▐▌
;;  ▀█▄▀▪.▀  ▀·▀▀▀▀      ▀  ▀ ·▀▀▀▀  ▀▀▀ ▀▀ █▪▀▀▀▀▀•  ▀  ▀


(after! org
  (setq org-agenda-inhibit-startup nil
        org-agenda-show-future-repeats nil
        org-agenda-skip-deadline-if-done t  ; disable deadline warnings if the task gets scheduled
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 'day
        org-agenda-start-day nil
        org-agenda-start-on-weekday nil
        org-agenda-todo-list-sublevels t
        org-deadline-warning-days 14))


(use-package! org-super-agenda
  :after-call org-agenda
  :config
  (setq org-super-agenda-header-separator "\n")
  ;; Disable bindings for org-super-agenda headers
  (setq org-super-agenda-header-map nil)
  (org-super-agenda-mode))


;; ┏━┓   ┏━╸   ┏━┓   ╺┳╸   ╻ ╻   ┏━╸   ╺┳╸   ╻   ┏━╸
;; ┣━┫   ┣╸    ┗━┓    ┃    ┣━┫   ┣╸     ┃    ┃   ┃
;; ╹ ╹   ┗━╸   ┗━┛    ╹    ╹ ╹   ┗━╸    ╹    ╹   ┗━╸

;; Set bigger line-spacing and center text vertically.
(add-hook! 'org-agenda-mode-hook
  (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))

(after! org
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-block-separator ?—)
  (setq org-agenda-current-time-string "●—————————————————————————————————————————————————")
  (setq org-agenda-deadline-leaders '("鬒  " "鬒  in %2d d.   " "鬒 %2d d. ago   "))
  (setq org-agenda-scheduled-leaders '("  " " %2d d. ago   "))
  (setq org-agenda-prefix-format
        '((agenda . " %i %-15T%?-14t%?-16s%?-25(+org-get-project-heading-or-file-title)")
          (todo   . " %i %-15T%-25(+org-get-project-heading-or-file-title)")
          (tags   . " %?(+org-get-project-heading-or-file-title)")
          (search . " %i %-15T%?(+org-get-project-heading-or-file-title)")))
  (setq org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "······" "––––––––––––––––––––")))

(custom-set-faces!
  '(org-agenda-structure
    :inherit variable-pitch
    :foreground "#84DBC7"
    :weight thin
    :height 1.8)
  '(org-agenda-date-today
    :inherit variable-pitch
    :foreground "#FECE48"
    :weight thin
    :height 1.8)
  '(org-agenda-date
    :inherit variable-pitch
    :foreground "#A48DE8"
    :weight thin
    :height 1.6)
  '(org-agenda-date-weekend
    :inherit variable-pitch
    :foreground "#797194"
    :weight thin
    :height 1.6)
  '(org-super-agenda-header
    :inherit variable-pitch
    :foreground "#7a88cf"
    :weight normal
    :height 1.4))


;; ┏┳┓╻ ╻   ┏━┓┏━╸┏━╸┏┓╻╺┳┓┏━┓┏━┓
;; ┃┃┃┗┳┛   ┣━┫┃╺┓┣╸ ┃┗┫ ┃┃┣━┫┗━┓
;; ╹ ╹ ╹    ╹ ╹┗━┛┗━╸╹ ╹╺┻┛╹ ╹┗━┛

(after! org-agenda
  (setq org-agenda-custom-commands
        '(("d" "Daily Agenda"
           ((agenda
             ""
             (
              (org-agenda-overriding-header "")
              (org-agenda-format-date "%A, %-e %B %Y")
              (org-agenda-span 2)
              (org-agenda-show-log t)
              (org-super-agenda-groups
               '(
                 (:name "💪 Habits"
                  :habit t
                  :order 2)
                 (:name "❌ Overdue"
                  :deadline past
                  :order 1)
                 (:name "📝 Log"
                  :log changed
                  :order 7)
                 (:name "🕓 Timed"
                  :time-grid t
                  :order 3)
                 (:name "⚠︎ Due Soon"
                  :deadline future
                  :order 5)
                 (:name "📆 Scheduled earlier"
                  :scheduled past
                  :order 6)
                 (:name "📅 Day tasks"
                  :anything t
                  :order 4)
                 ))))
            ;; TODO Display number of tasks in the Inbox
            (alltodo
             ""
             ((org-agenda-overriding-header "Unscheduled")
              (org-super-agenda-groups
               '((:discard
                  (:not (:todo ("TODO" "NEXT" "DOIN" "WAIT" "READ" "VIEW" "PROJ"))
                   :deadline t
                   :scheduled t))
                 (:name "🚧 In Progress"
                  :todo "DOIN"
                  :order 1)
                 (:name "⏳ Waiting"
                  :todo "WAIT"
                  :order 2)
                 (:name "📚 To Read/Watch"
                  :todo ("READ" "VIEW")
                  :order 3)
                 (:name "🧻 Backlog"
                  :todo "NEXT"
                  :priority>= "B"
                  :order 4)
                 (:name "🦥 Stuck Habits"
                  :habit t
                  :order 5)
                 (:discard (:anything t))))))
            (alltodo
             ""
             ((org-agenda-overriding-header "Projects")
              (org-super-agenda-groups
               '((:discard
                  (:not (:todo ("PROJ"))))
                 (:name "🚧 In Progress Projects"
                  :and (:todo t :children ("NEXT" "DOIN" "WAIT"))
                  :order 3)
                 (:name "😶 Quiet Projects"
                  :and (:todo t :children todo)
                  :order 2)
                 (:name "⛔ Stuck Projects"
                  :todo t
                  :order 1)
                 (:discard (:anything t))))))))
          ("u" "Unscheduled"
           ((alltodo
             ""
             ((org-agenda-overriding-header "Habits")
              (org-super-agenda-groups
               '((:name none
                  :and (:habit t
                        :scheduled nil))
                 (:discard (:anything t))))))
            (alltodo
             ""
             ((org-agenda-overriding-header "Projects")
              (org-super-agenda-groups
               '((:discard
                  (:not (:todo ("PROJ"))))
                 (:name none
                  :and (:todo "PROJ" :children todo)
                  :order 2)
                 (:name "Stuck Projects"
                  :todo "PROJ"
                  :order 1)))))
            (todo
             ""
             ((org-agenda-overriding-header "Areas")
              (org-agenda-prefix-format
               '((todo . " %i %?t%?s")))
              (org-agenda-todo-list-sublevels nil)
              (org-super-agenda-groups
               '((:discard
                  (:not (:todo ("TODO" "NEXT" "DOIN" "WAIT" "READ" "VIEW"))
                   :scheduled t
                   :habit t))
                 (:auto-outline-path t)))))))
          ("l" "Custom agenda" agenda ""
           ((org-agenda-start-with-log-mode '(closed clock state))
            (org-agenda-archives-mode t)))
          ("w" "Weekly review" agenda ""
           ((org-agenda-start-day "-14d")
            (org-agenda-span 14)
            (org-agenda-start-on-weekday 1)
            (org-agenda-start-with-log-mode '(state))
            (org-agenda-archives-mode t)
            (org-agenda-include-inactive-timestamps 't)
            ))
          )))
