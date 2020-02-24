;;; ~/dev/dotfiles/editor/emacs/doom/+agendas.el -*- lexical-binding: t; -*-

;;       ▄▄▄   ▄▄ •      ▄▄▄·  ▄▄ • ▄▄▄ . ▐ ▄ ·▄▄▄▄   ▄▄▄·
;; ▪     ▀▄ █·▐█ ▀ ▪    ▐█ ▀█ ▐█ ▀ ▪▀▄.▀·•█▌▐███▪ ██ ▐█ ▀█
;;  ▄█▀▄ ▐▀▀▄ ▄█ ▀█▄    ▄█▀▀█ ▄█ ▀█▄▐▀▀▪▄▐█▐▐▌▐█· ▐█▌▄█▀▀█
;; ▐█▌.▐▌▐█•█▌▐█▄▪▐█    ▐█ ▪▐▌▐█▄▪▐█▐█▄▄▌██▐█▌██. ██ ▐█ ▪▐▌
;;  ▀█▄▀▪.▀  ▀·▀▀▀▀      ▀  ▀ ·▀▀▀▀  ▀▀▀ ▀▀ █▪▀▀▀▀▀•  ▀  ▀


(after! org
  (setq
    org-agenda-inhibit-startup nil
    org-agenda-show-future-repeats nil
    org-agenda-skip-deadline-if-done t  ; disable deadline warnings if the task gets scheduled
    org-agenda-skip-deadline-prewarning-if-scheduled t
    org-agenda-skip-scheduled-if-done t
    org-agenda-span 'day
    org-agenda-start-day nil
    org-agenda-start-on-weekday nil
    org-agenda-todo-list-sublevels t
    ;; org-deadline-warning-days 14
    )
  )


(use-package! org-super-agenda
  :after-call org-agenda
  :config
  (setq org-super-agenda-header-separator "\n")
  (org-super-agenda-mode))


;; ┏━┓   ┏━╸   ┏━┓   ╺┳╸   ╻ ╻   ┏━╸   ╺┳╸   ╻   ┏━╸
;; ┣━┫   ┣╸    ┗━┓    ┃    ┣━┫   ┣╸     ┃    ┃   ┃
;; ╹ ╹   ┗━╸   ┗━┛    ╹    ╹ ╹   ┗━╸    ╹    ╹   ┗━╸

(defvar +org-agenda-project-heading-prefix " "
  "Separator for the clock heading generated from the outline path")

(defvar +org-agenda-project-heading-suffix "  "
  "Separator for the clock heading generated from the outline path")


;; Set bigger line-spacing and center text vertically.
(add-hook! org-agenda-mode
  (setq-local default-text-properties '(line-spacing 0.20 line-height 1.20)))

(after! org
  (setq
    org-agenda-dim-blocked-tasks nil
    org-agenda-block-separator ?—
    org-agenda-current-time-string "●—————————————————————————————————————————————————"
    org-agenda-deadline-leaders '("鬒  " "鬒  in %2d d.   " "鬒 %2d d. ago   ")
    org-agenda-scheduled-leaders '("  " " %2d d. ago   ")
    org-agenda-prefix-format
    '((agenda . " %i %-15c%?-14t%?s%?(+org-get-agenda-project-heading)")
       (todo . " %i %-15c%?(+org-get-agenda-project-heading)")
       (tags . " %i %-15c")
       (search . " %i %-15c"))
    org-agenda-time-grid '((daily today require-timed)
                            (800 1000 1200 1400 1600 1800 2000)
                            "······" "––––––––––––––––––––")
    )
  )

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
     :foreground "#64697C"
     :overline "#3D445B"
     :height 1))


;; ┏┳┓╻ ╻   ┏━┓┏━╸┏━╸┏┓╻╺┳┓┏━┓┏━┓
;; ┃┃┃┗┳┛   ┣━┫┃╺┓┣╸ ┃┗┫ ┃┃┣━┫┗━┓
;; ╹ ╹ ╹    ╹ ╹┗━┛┗━╸╹ ╹╺┻┛╹ ╹┗━┛

(after! org-agenda
  (setq org-agenda-custom-commands
    '(("t" "Today's Agenda"
        ((agenda ""
           (
             (org-agenda-overriding-header "")
             (org-agenda-format-date "%A, %-e %B %Y")
             (org-agenda-span 2)
             (org-agenda-show-log t)
             (org-super-agenda-groups
               '(
                  (:name "Habits"
                    :habit t
                    :order 2)
                  (:name "Overdue"
                    :deadline past
                    :order 1)
                  (:name "Log"
                    :log changed
                    :order 7)
                  (:name "Timed"
                    :time-grid t
                    :order 3)
                  (:name "Due Soon"
                    :deadline future
                    :order 5)
                  (:name "Scheduled earlier"
                    :scheduled past
                    :order 6)
                  (:name "Day tasks"
                    :anything t
                    :order 4)
                  ))))
          ;; TODO Display number of tasks in the Inbox
          (alltodo ""
            ((org-agenda-overriding-header "Unscheduled")
              (org-super-agenda-groups
                '((:discard
                    (:not (:todo ("TODO" "NEXT" "DOIN" "WAIT" "READ" "VIEW" "PROJ"))
                      :scheduled t
                      :deadline t
                      ))
                   (:name "In Progress"
                     :todo "DOIN"
                     :order 1)
                   (:name "Waiting"
                     :todo "WAIT"
                     :order 2)
                   (:name "To Read/Watch"
                     :todo ("READ" "VIEW")
                     :order 3)
                   (:name "Stuck Habits"
                     :habit t
                     :order 4)
                   (:name "In Progress Projects"
                     :and (:todo "PROJ" :children ("NEXT" "DOIN" "WAIT"))
                     :order 8)
                   (:name "Quiet Projects"
                     :and (:todo "PROJ" :children todo)
                     :order 7)
                   (:name "Stuck Projects"
                     :todo "PROJ"
                     :order 5)
                   (:name "Next to do"
                     :todo "NEXT"
                     :priority>= "B"
                     :order 6)
                   ;; TODO Show reading books/mangas
                   (:discard (:anything t))))))))
       ("u" "Unscheduled"
         ((alltodo ""
            ((org-agenda-overriding-header "Habits")
              (org-super-agenda-groups
                '((:name none
                    :and (:habit t
                           :scheduled nil
                           :deadline nil))
                   (:discard (:anything t))
                   ))))
           (alltodo ""
             ((org-agenda-overriding-header "Projects")
               (org-super-agenda-groups
                 '((:discard
                     (:not (:todo ("PROJ"))
                       :scheduled t
                       :deadline t))
                    (:name none
                      :and (:todo "PROJ" :children todo)
                      :order 2)
                    (:name "Stuck Projects"
                      :todo "PROJ"
                      :order 1)))))
           (todo ""
             ((org-agenda-overriding-header "Areas")
               (org-agenda-prefix-format
                 '((todo . " %i %?t%?s")))
               (org-agenda-todo-list-sublevels nil)
               (org-super-agenda-groups
                 '((:discard
                     (:not (:todo ("TODO" "NEXT" "DOIN" "WAIT" "READ" "VIEW"))
                       :scheduled t
                       :deadline t
                       :habit t))
                    (:auto-outline-path t)))))))
       )))
