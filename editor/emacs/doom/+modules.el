;;; +modules.el -*- lexical-binding: t; -*-

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;

;; -- Evil Snipe

;; Do not override my bindings!
(after! evil-snipe
  (setq
   evil-snipe-repeat-keys nil
   evil-snipe-enable-incremental-highlight nil
   evil-snipe-override-evil-repeat-keys nil))

;; Unbind default mapping for evil-snipe-S
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)


;; -- LSP

(after! lsp-mode
  (setq
   ;; Follow the instructions to setup ESLint in LSP server:
   ;; https://github.com/emacs-lsp/lsp-mode/wiki/LSP-ESlint-integration#fn1
   lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio")
   lsp-enable-symbol-highlighting nil
   lsp-lens-enable nil
   lsp-headerline-breadcrumb-enable nil
   ;; lsp-modeline-code-actions-enable nil
   ;; lsp-diagnostics-provider :none
   ;; FIXME Disabled until figure out why `lsp-signature-doc-lines' is not
   ;; limiting the number of lines to display in eldoc
   lsp-eldoc-enable-hover nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-diagnostics-scope :file
   lsp-signature-auto-activate nil ;; you could manually request them via `lsp-signature-activate`
   lsp-signature-render-documentation nil
   ;; lsp-completion-provider :none
   ;; lsp-completion-show-detail nil
   ;; lsp-completion-show-kind nil

   lsp-enable-snippet nil
   ;; lsp-signature-doc-lines 5
   lsp-enable-file-watchers nil
   ;; lsp-auto-execute-action nil
   lsp-use-plists t))

;; FIXME Disabling `lsp-eslint' until figure out why LSP is not reporting eslint errors
(after! lsp-mode
  (setq lsp-eslint-enable nil))
(add-hook! 'lsp-after-initialize-hook
  (defun my/js--run-lsp-checker-after-eslint-h ()
    "make sure to run lsp checker (slower) after eslint (faster)"
    (flycheck-add-next-checker 'javascript-eslint 'lsp)))
(add-hook! '(js-mode-hook web-mode-hook)
  (defun my/js--fix-eslint-checker2-h ()
    "set eslint as the default checker"
    (setq flycheck-checker 'javascript-eslint)))

;; If you are in a buffer with `lsp-mode' enabled and a server that supports `textDocument/formatting', it will be used
;; instead of `format-all's formatter. Unfortunately typescript does not seem to be respecting my settings, and is
;; slower than format-all so I prefer to disable it universally.
(after! format-all
  (setq +format-with-lsp nil))


;; -- Company

(after! company
  ;; On-demand code completion
  (setq company-idle-delay nil)

  ;; Sort by occurrence and group by backend (very useful for file completion)
  ;; (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
  )


;; -- Flycheck

(after! flycheck
  ;; The following advice is a workaround for a performance issue when opening files, particularly JavaScript files,
  ;; where the existence check of eslint configuration is causing noticeable delays.  By overriding
  ;; `flycheck-eslint-config-exists-p' to always return true, we bypass the file existence check, thus significantly
  ;; improving file opening times.
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))


;; -- Editorconfig

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


;; -- Evil Surround/Embrace

(after! evil-embrace
  ;; allow the use of backtick as a surround char in all modes by default
  (add-to-list 'evil-embrace-evil-surround-keys ?`)

  ;; Temporarily disables the backtick surround for emacs-lisp-mode to avoid overriding the emacs-lisp-mode specific
  ;; embrace pair that's setup in this hook
  (advice-add 'embrace-emacs-lisp-mode-hook :around #'my/embrace-emacs-lisp-mode-hook-advice))

(add-hook! '(js-mode-hook web-mode-hook typescript-mode-hook) 'my/embrace-js-mode-h)


;; -- REPL

;; Set a default REPL for all the js-related modes
(set-repl-handler! '(rjsx-mode web-mode typescript-mode) #'+javascript/open-repl)

