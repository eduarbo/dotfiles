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

;; -- Word wrap

;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)


;; -- Evil Snipe

;; Do not override my bindings!
(setq
 evil-snipe-repeat-keys nil
 evil-snipe-enable-incremental-highlight nil
 evil-snipe-override-evil-repeat-keys nil)

;; Unbind evil-snipe-S
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)


;; -- Projectile

(setq projectile-project-search-path '(("~/dev" . 2)))


;; -- Modeline

(after! doom-modeline
  (setq
   ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
   ;; truncate-with-project => emacs/l/comint.el
   ;; relative-from-project => emacs/lisp/comint.el
   doom-modeline-buffer-file-name-style 'relative-from-project
   ;; doom-modeline-buffer-file-name-style 'truncate-with-project
   ;; Whether display the buffer encoding
   doom-modeline-buffer-encoding t
   ;; Whether display the modal state icon.
   ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc
   doom-modeline-modal-icon t
   ;; Whether display the modification icon for the buffer
   doom-modeline-buffer-modification-icon nil
   ;; Whether display the icon for `major-mode'
   doom-modeline-major-mode-icon t
   ;; The maximum displayed length of the branch name of version control
   doom-modeline-vcs-max-length 18
   ;; Whether display the GitHub notifications. It requires `ghub' package
   doom-modeline-github t
   ;; If non-nil, only display one number for checker information if applicable
   doom-modeline-checker-simple-format t)

  ;; customize modeline segments
  (doom-modeline-def-segment clean-matches
    "an alternative to the built-in matches segment without fallback (buffer size indicator)"
    (concat
     ;; 1. the currently recording macro
     (doom-modeline--macro-recording)
     ;; 2. A current/total for the current search term (with `anzu')
     (doom-modeline--anzu)
     (doom-modeline--phi-search)
     ;; 3. The number of substitutions being conducted with `evil-ex-substitute', and/or
     (doom-modeline--evil-substitute)
     ;; 4. The number of active `iedit' regions
     (doom-modeline--iedit)
     ;; 5. The current/total for the highlight term (with `symbol-overlay')
     (doom-modeline--symbol-overlay)
     ;; 6. The number of active `multiple-cursors'.
     (doom-modeline--multiple-cursors)))
  (doom-modeline-def-modeline 'main
    '(hud modals clean-matches checker buffer-info remote-host buffer-position selection-info)
    '(misc-info github vcs lsp input-method buffer-encoding buffer-size major-mode process " ")))


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

;; FIXME Disabling lsp-eslint until figure out why LSP is not reporting eslint errors
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

;; If you are in a buffer with `lsp-mode' enabled and a server that supports
;; `textDocument/formatting', it will be used instead of `format-all's
;; formatter. Unfortunately typescript does not seem to be respecting my
;; settings, and is slower than format-all so I prefer to disable it
;; universally.
(after! format-all
  (setq +format-with-lsp nil))


;; -- Company

(after! company
  ;; On-demand code completion
  (setq company-idle-delay nil)

  ;; Sort by occurrence and group by backend (very useful for file completion)
  ;; (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
  )


;; -- Evil Surround/Embrace

(add-hook! '(js-mode-hook web-mode-hook) 'my/embrace-js-mode-hook-h)
(setq evil-embrace-evil-surround-keys '(?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?< ?> ?b ?B ?t ?\C-\[ ?w ?W ?s ?p ?`))


;; -- Ace window

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))


