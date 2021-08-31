;;; editor/emacs/doom/autoload/agenda.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar my-agenda-workspace-name "Agenda")


;;;###autoload
(defun my--switch-to-agenda-workspace (command)
  "Switch to the agenda workspace and dispatch agenda command"
  (+workspace-switch my-agenda-workspace-name)
  (org-agenda nil command))

;;;###autoload
(defun my--open-or-switch-to-agenda (command)
  "Switch to agenda workspace if exists or create a new one"
  (condition-case nil
      (my--switch-to-agenda-workspace command)
    (error
     (+workspace-new my-agenda-workspace-name)
     (my--switch-to-agenda-workspace command))))

;;;###autoload
(defun my/daily-agenda ()
  "Open the daily agenda in a dedicated workspace"
  (interactive)
  (my--open-or-switch-to-agenda "d"))

;;;###autoload
(defun my/unscheduled-agenda ()
  "Open the unscheduled agenda in a dedicated workspace"
  (interactive)
  (my--open-or-switch-to-agenda "u"))
