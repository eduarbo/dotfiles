;;; ~/.dotfiles/editor/emacs/doom/+dashboard.el -*- lexical-binding: t; -*-

;;            ·▄▄▄▄   ▄▄▄· .▄▄ ·  ▄ .▄▄▄▄▄·        ▄▄▄· ▄▄▄  ·▄▄▄▄
;;           ██▪ ██ ▐█ ▀█ ▐█ ▀. ██▪▐█▐█ ▀█▪▪     ▐█ ▀█ ▀▄ █·██▪ ██
;;           ▐█· ▐█▌▄█▀▀█ ▄▀▀▀█▄██▀▐█▐█▀▀█▄ ▄█▀▄ ▄█▀▀█ ▐▀▀▄ ▐█· ▐█▌
;;           ██. ██ ▐█ ▪▐▌▐█▄▪▐███▌▐▀██▄▪▐█▐█▌.▐▌▐█ ▪▐▌▐█•█▌██. ██
;;           ▀▀▀▀▀•  ▀  ▀  ▀▀▀▀ ▀▀▀ ··▀▀▀▀  ▀█▄▀▪ ▀  ▀ .▀  ▀▀▀▀▀▀•


;; ┏━╸┏━┓┏┓╻┏━╸╻┏━╸
;; ┃  ┃ ┃┃┗┫┣╸ ┃┃╺┓
;; ┗━╸┗━┛╹ ╹╹  ╹┗━┛
;; Config

(require 'seq)

(defvar +mi-dashboard-banners '("dunder-mifflin"))

(setq +doom-dashboard-functions
      '(+mi--dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; ╻ ╻┏━╸╻  ┏━┓┏━╸┏━┓┏━┓
;; ┣━┫┣╸ ┃  ┣━┛┣╸ ┣┳┛┗━┓
;; ╹ ╹┗━╸┗━╸╹  ┗━╸╹┗╸┗━┛
;; Helpers

(defun +mi--dashboard-widget-banner ()
  "Insert ASCII banner contained in file and center it."
  (let* ((banner-lines (split-string (+mi--dashboard-get-banner) "\n"))
         (banner-width (seq-reduce #'+mi--dashboard-get-banner-width banner-lines 0))
         (margin (max 0 (floor (/ (- +doom-dashboard--width banner-width) 2)))))
    (mapc (lambda (line)
            (insert (propertize (concat (make-string margin ?\s) line)
                                'face 'font-lock-comment-face) "\n"))
          banner-lines)))

(defun +mi--dashboard-get-random-banner ()
  "Get random banner from list"
  (nth (random (length +mi-dashboard-banners)) +mi-dashboard-banners))

(defun +mi--dashboard-get-banner ()
  "Load banner from file and return as a string."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents
         (concat doom-user-dir "banners/" (format "%s.txt" (+mi--dashboard-get-random-banner))))
        (buffer-string))
    (file-error "")))

(defun +mi--dashboard-get-banner-width (banner-width line)
  "Get length of longest line"
  (if (< banner-width (length line))
      (length line)
    banner-width))
