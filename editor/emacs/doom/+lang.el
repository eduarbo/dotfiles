;;; editor/emacs/doom/+lang.el -*- lexical-binding: t; -*-

;; ▄▄▌   ▄▄▄·  ▐ ▄  ▄▄ • ▄• ▄▌ ▄▄▄·  ▄▄ • ▄▄▄ ..▄▄ ·
;; ██•  ▐█ ▀█ •█▌▐█▐█ ▀ ▪█▪██▌▐█ ▀█ ▐█ ▀ ▪▀▄.▀·▐█ ▀.
;; ██▪  ▄█▀▀█ ▐█▐▐▌▄█ ▀█▄█▌▐█▌▄█▀▀█ ▄█ ▀█▄▐▀▀▪▄▄▀▀▀█▄
;; ▐█▌▐▌▐█ ▪▐▌██▐█▌▐█▄▪▐█▐█▄█▌▐█ ▪▐▌▐█▄▪▐█▐█▄▄▌▐█▄▪▐█
;; .▀▀▀  ▀  ▀ ▀▀ █▪·▀▀▀▀  ▀▀▀  ▀  ▀ ·▀▀▀▀  ▀▀▀  ▀▀▀▀


;; ┏━╸┏━┓┏┓╻┏━╸
;; ┃  ┃ ┃┃┗┫┣╸
;; ┗━╸┗━┛╹ ╹╹
;;; conf/ini/properties files

(add-to-list 'auto-mode-alist '("crontab" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.prettierrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\editorconfig\\'" . editorconfig-conf-mode))


;; ┏━╸┏━┓┏━┓┏━┓╻ ╻┏━┓╻
;; ┃╺┓┣┳┛┣━┫┣━┛┣━┫┃┓┃┃
;; ┗━┛╹┗╸╹ ╹╹  ╹ ╹┗┻┛┗━╸
;;; GraphQL

(after! graphql-mode
  (set-editorconfig-indent-var! '(graphql-mode graphql-indent-level)))


;;  ┏┓┏━┓╻ ╻┏━┓┏━┓┏━╸┏━┓╻┏━┓╺┳╸
;;   ┃┣━┫┃┏┛┣━┫┗━┓┃  ┣┳┛┃┣━┛ ┃
;; ┗━┛╹ ╹┗┛ ╹ ╹┗━┛┗━╸╹┗╸╹╹   ╹
;;; javascript

(when (modulep! :editor file-templates)
  (set-file-template! "\\.stylelintrc.js$" :trigger "__stylelintrc.js" :mode 'js-mode)
  (set-file-template! "\\.eslintrc.js$" :trigger "__eslintrc.js" :mode 'js-mode)
  (set-file-template! "\\.prettierrc.js$" :trigger "__prettierrc.js" :mode 'js-mode)
  (set-file-template! "\\.editorconfig$" :trigger "__editorconfig" :mode 'conf-mode))

(after! tide
  (setq
   tide-always-show-documentation nil
   tide-completion-detailed nil
   ;; Try to ignore case
   completion-ignore-case t
   tide-completion-ignore-case t
   ))

(after! flycheck
  ;; Workaround for eslint loading slow
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode typescript-indent-level web-mode-code-indent-offset))
  ;; Override editorconfig defaults for web-mode to fix indentation
  (setcdr (assq 'web-mode editorconfig-indentation-alist)
          '((web-mode-indent-style lambda (size) 2)
            ;; I prefer the web mode attr indent behavior when it's set to nil
            ;;
            ;; <a href="http://google.com"
            ;;    target="_blank">See how the attributes line up vertically?</a>
            ;;
            ;; web-mode-attr-indent-offset
            ;; web-mode-attr-value-indent-offset

            ;; web-mode-block-padding
            web-mode-code-indent-offset
            web-mode-css-indent-offset
            web-mode-markup-indent-offset
            web-mode-sql-indent-offset
            web-mode-script-padding
            web-mode-style-padding
            standard-indent)))

(after! web-mode
  ;; Disable auto-indentation as it slows down Emacs when pasting
  ;; (setq web-mode-enable-auto-indentation nil)

  ;; TODO Figure out a way to use block comments for JSX blocks and single-line comments for the rest
  (add-to-list 'web-mode-comment-formats '("jsx" . "//" )))

(add-hook! '(js-mode-hook web-mode-hook typescript-mode-hook)
  (embrace-add-pair ?\` "`" "`")
  (embrace-add-pair ?\$ "${" "}"))

(use-package! eslintd-fix
  :commands (eslintd-fix eslintd-fix-mode)
  :init
  (map! :after js2-mode :map js2-mode-map
        :localleader
        "l" #'eslintd-fix))

(after! xref-js2
  (setq xref-js2-search-program 'rg))

(set-repl-handler! '(rjsx-mode web-mode) #'+javascript/open-repl)


;; ┏┳┓┏━┓┏━┓╻┏ ╺┳┓┏━┓╻ ╻┏┓╻
;; ┃┃┃┣━┫┣┳┛┣┻┓ ┃┃┃ ┃┃╻┃┃┗┫
;; ╹ ╹╹ ╹╹┗╸╹ ╹╺┻┛┗━┛┗┻┛╹ ╹
;;; Markdown

(after! markdown-mode
  ;; continue lists when RET is pressed
  (setq markdown-indent-on-enter 'indent-and-new-item))


;; ┏┓╻┏━╸╻┏┓╻╻ ╻
;; ┃┗┫┃╺┓┃┃┗┫┏╋┛
;; ╹ ╹┗━┛╹╹ ╹╹ ╹
;;; Nginx

(use-package! nginx-mode
  :mode "/nginx/sites-\\(?:available\\|enabled\\)/")


;; ╻ ╻╻┏┳┓╻
;; ┃┏┛┃┃┃┃┃
;; ┗┛ ╹╹ ╹┗━╸
;;; VimL

(use-package! vimrc-mode
  :mode "\\.?vim\\(rc\\)?\\'")
