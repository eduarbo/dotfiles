;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;       ·▄▄▄▄              • ▌ ▄ ·.      ▄▄·        ▐ ▄ ·▄▄▄▪   ▄▄ •
;;       ██▪ ██ ▪     ▪     ·██ ▐███▪    ▐█ ▌▪▪     •█▌▐█▐▄▄·██ ▐█ ▀ ▪
;;       ▐█· ▐█▌ ▄█▀▄  ▄█▀▄ ▐█ ▌▐▌▐█·    ██ ▄▄ ▄█▀▄ ▐█▐▐▌██▪ ▐█·▄█ ▀█▄
;;       ██. ██ ▐█▌.▐▌▐█▌.▐▌██ ██▌▐█▌    ▐███▌▐█▌.▐▌██▐█▌██▌.▐█▌▐█▄▪▐█
;;       ▀▀▀▀▀•  ▀█▄▀▪ ▀█▄▀▪▀▀  █▪▀▀▀    ·▀▀▀  ▀█▄▀▪▀▀ █▪▀▀▀ ▀▀▀·▀▀▀▀
;;
;;                      == Project & Code conventions ==


(defvar dotfiles-dir "~/dev/dotfiles")


;; ╺┳╸╻ ╻╻┏━┓   ╻┏━┓   ┏┳┓┏━╸
;;  ┃ ┣━┫┃┗━┓   ┃┗━┓   ┃┃┃┣╸
;;  ╹ ╹ ╹╹┗━┛   ╹┗━┛   ╹ ╹┗━╸
;; This is me

(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias"
      epa-file-encrypt-to user-mail-address)


;; ┏━┓   ┏━╸   ┏━┓   ╺┳╸   ╻ ╻   ┏━╸   ╺┳╸   ╻   ┏━╸
;; ┣━┫   ┣╸    ┗━┓    ┃    ┣━┫   ┣╸     ┃    ┃   ┃
;; ╹ ╹   ┗━╸   ┗━┛    ╹    ╹ ╹   ┗━╸    ╹    ╹   ┗━╸
;; a e s t h e t i c


;; (load-theme 'doom-one t)
(load-theme 'doom-vibrant t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-oceanic-next t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-challenger-deep t)
;; (load-theme 'doom-moonlight t)
;; (load-theme 'doom-one-light t)

;; Fonts
(setq doom-font (font-spec :family "Hack" :size 14)
      doom-serif-font (font-spec :family "Fira Code")
      doom-variable-pitch-font (font-spec :family "Noto Sans"))

 ;; A more useful title
 (setq frame-title-format '("%b   —   " (:eval (+workspace-current-name))))

;; Overwrite default doom theme faces for todo keywords
(defun eduarbo--set-hl-todo-keyword-faces ()
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success)))))
(add-hook! 'doom-load-theme-hook #'eduarbo--set-hl-todo-keyword-faces)

(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'window-setup-hook #'toggle-frame-maximized))


;; ╺┳┓┏━╸┏━╸┏━┓╻ ╻╻  ╺┳╸┏━┓
;;  ┃┃┣╸ ┣╸ ┣━┫┃ ┃┃   ┃ ┗━┓
;; ╺┻┛┗━╸╹  ╹ ╹┗━┛┗━╸ ╹ ┗━┛
;; Sane defaults

(setq-default
 ;; Enable accents
 ns-alternate-modifier 'none
 ;; Get some context when scrolling
 scroll-margin 10
 ;; Protecting me from data loss. Save every 20 chars typed (this is the minimum)
 auto-save-visited-interval 20)

;;; :editor evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Which-key
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0)

;; Dired
(setq dired-use-ls-dired t)
(when IS-MAC
  ;; use gnu ls to allow dired to sort directories
  (setq insert-directory-program "gls"))

;; Stop in-between "camelCase" words instead of just spaces, hyphens or
;; underscores
(global-subword-mode)

;; http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-keyfreq.el
(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package! nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

;; Hide line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)
;; Hide indent lines
(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)


;; ╺┳╸┏━╸╻ ╻╺┳╸   ┏┳┓┏━┓╺┳┓┏━╸
;;  ┃ ┣╸ ┏╋┛ ┃ ╺━╸┃┃┃┃ ┃ ┃┃┣╸
;;  ╹ ┗━╸╹ ╹ ╹    ╹ ╹┗━┛╺┻┛┗━╸
;; Text mode

;; Enable word wrap only for text-mode
(add-hook! 'text-mode-hook #'turn-on-visual-line-mode)

(after! text-mode
  (set-company-backend! 'text-mode 'company-ispell 'company-capf))


;; ┏━┓┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┣━┛┣┳┛┃ ┃┃╺┓╺━╸┃┃┃┃ ┃ ┃┃┣╸
;; ╹  ╹┗╸┗━┛┗━┛   ╹ ╹┗━┛╺┻┛┗━╸
;; prog-mode

;; whitespace
(defun eduarbo--show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook! (prog-mode conf-mode) #'eduarbo--show-trailing-whitespace)

;; Syntax highlighting for systemd Files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-mode))

(use-package! vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")


;;  ┏┓┏━┓╻ ╻┏━┓┏━┓┏━╸┏━┓╻┏━┓╺┳╸
;;   ┃┣━┫┃┏┛┣━┫┗━┓┃  ┣┳┛┃┣━┛ ┃
;; ┗━┛╹ ╹┗┛ ╹ ╹┗━┛┗━╸╹┗╸╹╹   ╹
;; javascript

;; Normalize indentation level
(set-editorconfig-indent-var! '(rjsx-mode js-indent-level sgml-basic-offset))
(after! js2-mode (setq js-chain-indent nil))

(after! tide
  (setq tide-always-show-documentation nil
        tide-completion-detailed nil)
  ;; Try to ignore case
  (setq completion-ignore-case t
        tide-completion-ignore-case t)

  ;; Disable eldoc for tide, multiline eldoc is very annoying and unfortunately
  ;; I can't disable it by setting the VAR `eldoc-echo-area-use-multiline-p`
  (advice-remove #'tide-setup :after #'eldoc-mode))

(after! (flycheck tide)
  (setq-default flycheck-disabled-checkers '(javascript-tide)))

;; Company is more useful than emmet in these modes, so... fuck off!
(remove-hook! '(rjsx-mode-hook css-mode-hook)
              #'emmet-mode)


;; ┏┳┓╻┏━┓┏━╸
;; ┃┃┃┃┗━┓┃
;; ╹ ╹╹┗━┛┗━╸

(load! "./+modules.el")
(load! "./+dashboard.el")
(load! "./+bindings.el")
