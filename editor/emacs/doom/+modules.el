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

;; ─── Evil Snipe ───────────────────────────────────────────────────────────────

(after! evil-snipe
  (setq
   evil-snipe-override-evil-repeat-keys nil ;; Do not override my bindings!
   evil-snipe-scope 'whole-visible ;; highlight all visible matches in buffer
   evil-snipe-repeat-keys nil
   evil-snipe-enable-incremental-highlight nil
   ))

;; Unbind default mapping for evil-snipe-S
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)


;; ─── LSP ──────────────────────────────────────────────────────────────────────

(after! lsp-mode
  (setq
   ;; Do not execute the action without user confirmation
   lsp-auto-execute-action nil

   ;; Follow the instructions to setup ESLint in LSP server:
   ;; https://github.com/emacs-lsp/lsp-mode/wiki/LSP-ESlint-integration#fn1
   lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio")
   lsp-eslint-run "onType"
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

   lsp-typescript-preferences-import-module-specifier "non-relative"

   lsp-enable-snippet nil
   ;; lsp-signature-doc-lines 5
   lsp-enable-file-watchers nil
   ;; lsp-auto-execute-action nil
   lsp-use-plists t))

;; If you are in a buffer with `lsp-mode' enabled and a server that supports `textDocument/formatting', it will be used
;; instead of `format-all's formatter. Unfortunately typescript does not seem to be respecting my settings, and is
;; slower than format-all so I prefer to disable it universally.
(setq +format-with-lsp nil)

;; Auto-fix ESLint issues on save
(setq lsp-eslint-auto-fix-on-save t)
;; Incorporate ESLint fixes into the save hook
(advice-add 'lsp--before-save :around #'my/lsp--eslint-before-save)
;; After applying ESLint fixes, run Flycheck
(advice-add 'lsp-eslint-apply-all-fixes :around #'my/lsp-eslint-fix-after)


;; ─── Company ──────────────────────────────────────────────────────────────────

(after! company
  ;; On-demand code completion
  (setq company-idle-delay nil))


;; ─── Corfu ────────────────────────────────────────────────────────────────────

(after! corfu
  (setq corfu-preselect 'valid)
  ;; On-demand code completion
  (setq corfu-auto-delay nil))


;; ─── Flycheck ─────────────────────────────────────────────────────────────────

(after! flycheck
  (setq
   ;; flycheck-check-syntax-automatically '((save idle-change mode-enabled new-line))
   flycheck-idle-change-delay 0.5)

  ;; The following advice is a workaround for a performance issue when opening files, particularly JavaScript files,
  ;; where the existence check of eslint configuration is causing noticeable delays.  By overriding
  ;; `flycheck-eslint-config-exists-p' to always return true, we bypass the file existence check, thus significantly
  ;; improving file opening times.
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

;; Allow flycheck to display on the left fringe
(after! flycheck
  (setq flycheck-indication-mode 'left-fringe)
  ;; A non-descript, right-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [8 12 14 15 14 12 8] 8 7 'center))

;; while diff-hl takes the right fringe
(after! diff-hl (setq diff-hl-side 'right))


;; ─── Editorconfig ─────────────────────────────────────────────────────────────

(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode typescript-indent-level))
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


;; ─── Evil Surround/Embrace ────────────────────────────────────────────────────

(after! evil-embrace
  ;; allow the use of backtick as a surround char in all modes by default
  (add-to-list 'evil-embrace-evil-surround-keys ?`)

  ;; Temporarily disables the backtick surround for emacs-lisp-mode to avoid overriding the emacs-lisp-mode specific
  ;; embrace pair that's setup in this hook
  (advice-add 'embrace-emacs-lisp-mode-hook :around #'my/embrace-emacs-lisp-mode-hook-advice))

(add-hook! '(js-mode-hook web-mode-hook typescript-mode-hook) 'my/embrace-js-mode-h)


;; ─── REPL ─────────────────────────────────────────────────────────────────────

;; Set a default REPL for all the js-related modes
(set-repl-handler! '(rjsx-mode web-mode typescript-mode) #'+javascript/open-repl)


;; ─── Obsidian ─────────────────────────────────────────────────────────────────

(use-package! obsidian
  ;; :demand t
  :config
  (obsidian-specify-path "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Personal")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "notas")
  :bind (:map obsidian-mode-map
              ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Jump to backlinks
              ("C-c C-b" . obsidian-backlink-jump)
              ;; If you prefer you can use `obsidian-insert-link'
              ("C-c C-l" . obsidian-insert-wikilink)))


;; ─── String inflection: underscore -> UPCASE -> CamelCase ─────────────────────

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-java-style-cycle
             string-inflection-python-style-cycle
             string-inflection-elixir-style-cycle
             string-inflection-ruby-style-cycle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase
             string-inflection-kebab-case)

  :init
  (map! :prefix ("g SPC" . "Convert case")
        :desc "cycle"              :nv "n"     #'string-inflection-all-cycle
        :desc "toggle"             :nv "t"     #'string-inflection-toggle
        :desc "PascalCase"         :nv "p"     #'string-inflection-camelcase
        :desc "camelCase"          :nv "c"     #'string-inflection-lower-camelcase
        :desc "kebab-case"         :nv "k"     #'string-inflection-kebab-case
        :desc "snake_case"         :nv "s"     #'string-inflection-underscore
        :desc "Capital_Snake_Case" :nv "S"     #'string-inflection-capital-underscore
        :desc "UP_CASE"            :nv "u"     #'string-inflection-upcase))


;; ─── Yasnippet: Template System for Code Expansion ────────────────────────────

(after! yasnippet
  ;; NOTE Add the following in `.dir-locals.el` (at root level) to enable semicolons on js-mode snippets:
  ;; ((nil . ((doom-snippets-js-semi . t))))

  (setq yas-indent-line 'auto)
  (setq yas-triggers-in-field t))


;; ─── Drag stuff (words, region, lines) around ─────────────────────────────────

(use-package! drag-stuff
  :commands (drag-stuff-up
             drag-stuff-down
             drag-stuff-left
             drag-stuff-right))


;; ─── AI-powered assistant for Emacs (experimental) ────────────────────────────

(use-package aider
  :config
  ;; (setq aider-args '("--model" "o3-mini"))
  (setq aider-args '("--model" "gpt-4o-mini"))
  (setenv "OPENAI_API_KEY" (getenv "OPENAI_API_KEY")))
