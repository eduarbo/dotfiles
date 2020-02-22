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

(load-theme 'doom-one t)
;; (load-theme 'doom-vibrant t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-oceanic-next t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-challenger-deep t)
;; (load-theme 'doom-moonlight t)
;; (load-theme 'doom-one-light t)

;; Fonts
(setq
  doom-font (font-spec :family "Hack Nerd Font" :size 14)
  doom-variable-pitch-font (font-spec :family "Noto Sans" :weight 'light)
  )

 ;; A more useful title
 (setq frame-title-format '("%b   —   " (:eval (+workspace-current-name))))

;; Overwrite default doom theme faces for todo keywords
(defun eduarbo--set-hl-todo-keyword-faces ()
  (setq hl-todo-keyword-faces (("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success)))))
(add-hook! 'doom-load-theme-hook #'eduarbo--set-hl-todo-keyword-faces)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  )


;; ╺┳┓┏━╸┏━╸┏━┓╻ ╻╻  ╺┳╸┏━┓
;;  ┃┃┣╸ ┣╸ ┣━┫┃ ┃┃   ┃ ┗━┓
;; ╺┻┛┗━╸╹  ╹ ╹┗━┛┗━╸ ╹ ┗━┛
;; Sane defaults

(setq
  doom-leader-key ","
  doom-leader-alt-key "C-,"
  doom-localleader-key ", m"
  doom-localleader-alt-key "s-,"
  +evil-repeat-keys '("|" . "\\"))

(setq
  custom-file (concat doom-private-dir "custom.el")
  ;; Enable accents
  mac-option-modifier 'none
  ;; Get some context when scrolling
  scroll-margin 10
  ;; Protecting me from data loss. Save every 20 chars typed (this is the minimum)
  auto-save-visited-interval 20

;;; :editor evil
  evil-split-window-below t
  evil-vsplit-window-right t

  ;; Which-key
  which-key-idle-delay 0.3
  which-key-idle-secondary-delay 0

  doom-scratch-buffer-major-mode 'org-mode
  )

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

;; Hide line numbers
(remove-hook! (prog-mode text-mode conf-mode) #'display-line-numbers-mode)
;; Hide indent lines
(remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode)
(remove-hook! text-mode #'flyspell-mode)


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

(use-package! nginx-mode
  :mode "/nginx/sites-\\(?:available\\|enabled\\)/")

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
        tide-completion-ignore-case t))

;; No multiline eldoc please
(after! lsp
  (setq lsp-eldoc-enable-signature-help nil
        lsp-eldoc-enable-hover nil))

(after! (flycheck tide)
  (setq-default flycheck-disabled-checkers '(javascript-tide)))

;; Company is more useful than emmet in these modes, so... fuck off!
(remove-hook! '(rjsx-mode-hook css-mode-hook)
              #'emmet-mode)


;; ┏━╸┏━┓┏━┓┏━┓╻ ╻┏━┓╻
;; ┃╺┓┣┳┛┣━┫┣━┛┣━┫┃┓┃┃
;; ┗━┛╹┗╸╹ ╹╹  ╹ ╹┗┻┛┗━╸
;; GraphQL

(set-editorconfig-indent-var! '(graphql-mode graphql-indent-level))
(after! graphql-mode
  (add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode)))


;; ┏┳┓╻┏━┓┏━╸
;; ┃┃┃┃┗━┓┃
;; ╹ ╹╹┗━┛┗━╸

(load! "custom.el")
(load! "+modules.el")
(load! "+gtd.el")
(load! "+dashboard.el")
(load! "+bindings.el")
