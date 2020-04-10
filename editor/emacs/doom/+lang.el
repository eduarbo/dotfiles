;;; ~/dev/dotfiles/editor/emacs/doom/+lang.el -*- lexical-binding: t; -*-

;; ▄▄▌   ▄▄▄·  ▐ ▄  ▄▄ • ▄• ▄▌ ▄▄▄·  ▄▄ • ▄▄▄ ..▄▄ ·
;; ██•  ▐█ ▀█ •█▌▐█▐█ ▀ ▪█▪██▌▐█ ▀█ ▐█ ▀ ▪▀▄.▀·▐█ ▀.
;; ██▪  ▄█▀▀█ ▐█▐▐▌▄█ ▀█▄█▌▐█▌▄█▀▀█ ▄█ ▀█▄▐▀▀▪▄▄▀▀▀█▄
;; ▐█▌▐▌▐█ ▪▐▌██▐█▌▐█▄▪▐█▐█▄█▌▐█ ▪▐▌▐█▄▪▐█▐█▄▄▌▐█▄▪▐█
;; .▀▀▀  ▀  ▀ ▀▀ █▪·▀▀▀▀  ▀▀▀  ▀  ▀ ·▀▀▀▀  ▀▀▀  ▀▀▀▀


;; ┏━╸┏━┓┏━┓┏━┓╻ ╻┏━┓╻
;; ┃╺┓┣┳┛┣━┫┣━┛┣━┫┃┓┃┃
;; ┗━┛╹┗╸╹ ╹╹  ╹ ╹┗┻┛┗━╸
;;; GraphQL

(set-editorconfig-indent-var! '(graphql-mode graphql-indent-level))

(after! graphql-mode
  (add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode)))


;;  ┏┓┏━┓╻ ╻┏━┓┏━┓┏━╸┏━┓╻┏━┓╺┳╸
;;   ┃┣━┫┃┏┛┣━┫┗━┓┃  ┣┳┛┃┣━┛ ┃
;; ┗━┛╹ ╹┗┛ ╹ ╹┗━┛┗━╸╹┗╸╹╹   ╹
;;; javascript

;; Normalize indentation level
(set-editorconfig-indent-var! '(rjsx-mode js-indent-level sgml-basic-offset))

(after! js2-mode (setq js-chain-indent nil))

(after! tide
  (setq
    tide-always-show-documentation nil
    tide-completion-detailed nil
    ;; Try to ignore case
    completion-ignore-case t
    tide-completion-ignore-case t
    ))

(after! (flycheck tide)
  (setq-default flycheck-disabled-checkers '(javascript-tide)))

(add-hook! js-mode
  (add-to-list 'evil-embrace-evil-surround-keys ?\`)
  (embrace-add-pair ?\$ "${" "}"))


;; ┏┓╻┏━╸╻┏┓╻╻ ╻
;; ┃┗┫┃╺┓┃┃┗┫┏╋┛
;; ╹ ╹┗━┛╹╹ ╹╹ ╹
;;; Nginx

(use-package! nginx-mode
  :mode "/nginx/sites-\\(?:available\\|enabled\\)/")


;; ┏━┓╻ ╻┏━┓╺┳╸┏━╸┏┳┓╺┳┓
;; ┗━┓┗┳┛┗━┓ ┃ ┣╸ ┃┃┃ ┃┃
;; ┗━┛ ╹ ┗━┛ ╹ ┗━╸╹ ╹╺┻┛
;;; Systemd

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-mode))


;; ╻ ╻╻┏┳┓╻
;; ┃┏┛┃┃┃┃┃
;; ┗┛ ╹╹ ╹┗━╸
;;; VimL

(use-package! vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")