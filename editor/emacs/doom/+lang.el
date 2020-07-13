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


(use-package! graphql-mode
  :mode "\\.g\\(?:raph\\)?ql$"
  :config
  (set-editorconfig-indent-var! '(graphql-mode graphql-indent-level)))


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

(after! flycheck
  (setq-default flycheck-disabled-checkers '(javascript-tide eglot)))

(after! flycheck
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(add-hook! js-mode
  (embrace-add-pair ?\` "`" "`")
  (embrace-add-pair ?\$ "${" "}"))

(use-package! prettier-js
  :commands (prettier-js prettier-js-mode)
  :init
  (map! :after js2-mode :map js2-mode-map
        :localleader
        "p" #'prettier-js))

(use-package! eslintd-fix
  :commands (eslintd-fix eslintd-fix-mode)
  :init
  (map! :after js2-mode :map js2-mode-map
        :localleader
        "l" #'eslintd-fix))


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
