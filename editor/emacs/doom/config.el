;;; ~/.dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;       ·▄▄▄▄              • ▌ ▄ ·.      ▄▄·        ▐ ▄ ·▄▄▄▪   ▄▄ •
;;       ██▪ ██ ▪     ▪     ·██ ▐███▪    ▐█ ▌▪▪     •█▌▐█▐▄▄·██ ▐█ ▀ ▪
;;       ▐█· ▐█▌ ▄█▀▄  ▄█▀▄ ▐█ ▌▐▌▐█·    ██ ▄▄ ▄█▀▄ ▐█▐▐▌██▪ ▐█·▄█ ▀█▄
;;       ██. ██ ▐█▌.▐▌▐█▌.▐▌██ ██▌▐█▌    ▐███▌▐█▌.▐▌██▐█▌██▌.▐█▌▐█▄▪▐█
;;       ▀▀▀▀▀•  ▀█▄▀▪ ▀█▄▀▪▀▀  █▪▀▀▀    ·▀▀▀  ▀█▄▀▪▀▀ █▪▀▀▀ ▀▀▀·▀▀▀▀
;;
;;                      == Project & Code conventions ==
;;
;; https://github.com/hlissner/doom-emacs/issues/839#issuecomment-416209165
;;
;; namespace-symbol-name => public variable or function
;; namespace--symbol-name => private one

;; + `doom/abc` A public, interactive command, designed to be used via `M-x` or
;;   a keybinding.
;; + `doom:abc` A public evil operator, motion or command.
;; + `doom|abc` A public, non-interactive function meant to be used as a hook.
;; + `doom*abc` Functions designed to be used as advice for other functions.
;; + `abc!` A public Doom "autodef" function or macro. An autodef should always
;;   be defined, even if its containing module is disabled (i.e. they will not
;;   throw a void-function error). The purpose of this is to avoid peppering
;;   module configs with conditionals or `after!` blocks before using their
;;   APIs. They should noop if their module is disabled, and should be zero-cost
;;   in the case their module is disabled.

;;   Autodefs usually serve to configure Doom or a module. [and are usually
;;   syntactic sugar]
;; + Functions prefixed with `+abc...` belong to a module, e.g.
;;   `+emacs-lisp|init-hook` is a hook function in the `lang/emacs-lisp` module.
;; + `=abc` An interactive command that invokes an app module.


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

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package! nginx-mode
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))


;; ╺┳╸┏━╸╻ ╻╺┳╸   ┏┳┓┏━┓╺┳┓┏━╸
;;  ┃ ┣╸ ┏╋┛ ┃ ╺━╸┃┃┃┃ ┃ ┃┃┣╸
;;  ╹ ┗━╸╹ ╹ ╹    ╹ ╹┗━┛╺┻┛┗━╸
;; Text mode

;; Hide line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)
;; Hide indent lines
(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)
;; Enable word wrap only for text-mode
(add-hook! 'text-mode-hook #'turn-on-visual-line-mode)


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

(after! tide
  (setq tide-always-show-documentation nil
        tide-completion-detailed nil)
  ;; Try to ignore case
  (setq completion-ignore-case t
        tide-completion-ignore-case t))

(after! (flycheck tide)
  (setq-default flycheck-disabled-checkers '(javascript-tide)))


;; ┏┳┓╻┏━┓┏━╸
;; ┃┃┃┃┗━┓┃
;; ╹ ╹╹┗━┛┗━╸

(load! "./+modules.el")
(load! "./+dashboard.el")
(load! "./+bindings.el")
