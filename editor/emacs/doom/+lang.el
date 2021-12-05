;;; editor/emacs/doom/+lang.el -*- lexical-binding: t; -*-

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

;; FIXME this slows down the loading of js files (specially R3 files)
(after! flycheck
  (setq-default flycheck-disabled-checkers '(javascript-tide eglot))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; Workaround for eslint loading slow
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  )

(add-hook! 'web-mode-hook
  (defun my/configure-web-mode-flycheck-disable-checkers-based-on-engine ()
    "Enable javascript-eslint checker on web-mode but only for svelte files"
    (unless (string= web-mode-engine "svelte")
      (setq-local flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-eslint))))))

(add-hook! '(js-mode-hook web-mode-hook)
  (embrace-add-pair ?\` "`" "`")
  (embrace-add-pair ?\$ "${" "}"))

(use-package! prettier-js
  :commands (prettier-js prettier-js-mode)
  :init
  (map! :localleader :map (js2-mode-map web-mode-map json-mode-map css-mode-map)
        :desc "Prettier" "p" #'prettier-js))

(use-package! eslintd-fix
  :commands (eslintd-fix eslintd-fix-mode)
  :init
  (map! :after js2-mode :map js2-mode-map
        :localleader
        "l" #'eslintd-fix))

(after! xref-js2
  (setq xref-js2-search-program 'rg))

(set-repl-handler! '(rjsx-mode web-mode) #'+javascript/open-repl)


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
