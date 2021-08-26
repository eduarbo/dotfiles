;;; editor/emacs/doom/+defaults.el -*- lexical-binding: t; -*-

;; ·▄▄▄▄  ▄▄▄ .·▄▄▄ ▄▄▄· ▄• ▄▌▄▄▌  ▄▄▄▄▄.▄▄ ·
;; ██▪ ██ ▀▄.▀·▐▄▄·▐█ ▀█ █▪██▌██•  •██  ▐█ ▀.
;; ▐█· ▐█▌▐▀▀▪▄██▪ ▄█▀▀█ █▌▐█▌██▪   ▐█.▪▄▀▀▀█▄
;; ██. ██ ▐█▄▄▌██▌.▐█ ▪▐▌▐█▄█▌▐█▌▐▌ ▐█▌·▐█▄▪▐█
;; ▀▀▀▀▀•  ▀▀▀ ▀▀▀  ▀  ▀  ▀▀▀ .▀▀▀  ▀▀▀  ▀▀▀▀


(setq
  epa-file-encrypt-to user-mail-address

  ;; Allow me to insert accents and other symbols
  mac-option-modifier 'none

  ;; Get some context when scrolling
  scroll-margin 10

  ;; Protecting me from data loss. Save every 20 chars typed (this is the minimum)
  auto-save-visited-interval 20

  ;; evil
  evil-split-window-below t
  evil-vsplit-window-right t

  ;; Which-key
  which-key-idle-delay 0.3
  which-key-idle-secondary-delay 0

  ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
  ;; They're generally unhelpful and only add confusing visual clutter.
  mode-line-default-help-echo nil
  show-help-function nil

  avy-timeout-seconds 0.3

  doom-scratch-initial-major-mode 'org-mode)

;; Stop in-between "camelCase" words instead of just spaces, hyphens or underscores
(global-subword-mode t)

;; treat symbols characters as parts of words: e.g., "this_is_a_symbol" counts as one word
;; (global-superword-mode t)

;; Line numbers are pretty slow all around. The performance boost of disabling them outweighs the
;; utility of always keeping them on
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Hide indent lines
(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)

;; whitespace
(add-hook! (prog-mode conf-mode) #'doom-enable-show-trailing-whitespace-h)

;; Enable visual line mode for text-mode
(add-hook! text-mode #'visual-line-mode)

;; dired
(setq dired-use-ls-dired t)
(when IS-MAC
  ;; use gnu ls to allow dired to sort directories
  (setq insert-directory-program "gls"))

;; let me bind CMD-h please!
(setq mac-pass-command-to-system nil)


;;; Fonts

(setq
 doom-font (font-spec :family "JetBrains Mono" :size 12 :weight 'light)
 ;; doom-font (font-spec :family "Hack Nerd Font" :size 12)
 ;; doom-variable-pitch-font (font-spec :family "Noto Serif" :size 13)
 doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))


;;; Frames/Windows

;; A more useful title
(setq frame-title-format
   '((:eval (buffer-file-name)) "   —   " (:eval (+workspace-current-name))))

;; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))


;;; Theme customization

(setq doom-theme 'doom-moonlight)

;; prevent blinking at startup
(add-to-list 'default-frame-alist '(background-color . "#21242B"))

(with-no-warnings
  (custom-declare-face 'vertico-group-title '((t (:inherit font-lock-comment-face))) ""))
