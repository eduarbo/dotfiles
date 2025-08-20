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


;; ─── Company ──────────────────────────────────────────────────────────────────

(after! company
  ;; On-demand code completion
  (setq company-idle-delay nil))


;; ─── Corfu ────────────────────────────────────────────────────────────────────

(after! corfu
  (setq corfu-preselect 'valid)
  ;; On-demand code completion
  (setq corfu-auto-delay nil))


;; ─── Drag stuff (words, region, lines) around ─────────────────────────────────

(use-package! drag-stuff
  :commands (drag-stuff-up
             drag-stuff-down
             drag-stuff-left
             drag-stuff-right))


;; ─── Dirvish ──────────────────────────────────────────────────────────────────

;; It's bugged
;; (dirvish-side-follow-mode 1)


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


;; HACK: Force web-mode padding values via hook, as the overrides above don't work
(add-hook! 'web-mode-hook
  (lambda ()
    (setq-local web-mode-style-padding 2
                web-mode-script-padding 2)))


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


;; ─── Evil Surround/Embrace ────────────────────────────────────────────────────

(after! evil-embrace
  ;; allow the use of backtick as a surround char in all modes by default
  (add-to-list 'evil-embrace-evil-surround-keys ?`)

  ;; Temporarily disables the backtick surround for emacs-lisp-mode to avoid overriding the emacs-lisp-mode specific
  ;; embrace pair that's setup in this hook
  (advice-add 'embrace-emacs-lisp-mode-hook :around #'my/embrace-emacs-lisp-mode-hook-advice))

(add-hook! '(js-mode-hook web-mode-hook typescript-mode-hook) 'my/embrace-js-mode-h)


;; ─── Flycheck ─────────────────────────────────────────────────────────────────

;; PHP mode hooks
(add-hook 'php-mode-hook #'my/use-project-phpcs-executable)
(add-hook 'lsp-after-open-hook #'my/setup-php-flycheck-chain)

;; CSS-like mode hooks
(dolist (mode-hook '(css-mode-hook scss-mode-hook less-mode-hook))
  (add-hook mode-hook
            (lambda ()
              (my/find-stylelint-config)
              (add-hook 'lsp-after-open-hook #'my/setup-stylelint-flycheck-chain nil t))))
(add-hook 'lsp-after-open-hook #'my/setup-stylelint-flycheck-chain)

;; Web-mode hooks (handles both PHP and CSS)
(add-hook 'web-mode-hook
          (lambda ()
            (cond
             ;; PHP files in web-mode
             ((my/web-mode-is-php-p)
              (my/use-project-phpcs-executable)
              (add-hook 'lsp-after-open-hook #'my/setup-php-flycheck-chain nil t))
             ;; CSS-like files in web-mode
             ((member web-mode-content-type '("css" "scss" "less"))
              (my/find-stylelint-config)
              (add-hook 'lsp-after-open-hook #'my/setup-stylelint-flycheck-chain nil t)))))

(after! flycheck
  (flycheck-add-mode 'php-phpcs 'web-mode)
  (flycheck-add-mode 'html-tidy 'web-mode)

  ;; (setq flycheck-stylelintrc
  ;;       (expand-file-name "stylelint.config.js" (projectile-project-root)))

  ;; Prefer eslint_d to ESLint
  ;; See https://github.com/mantoni/eslint_d.js
  (when (executable-find "eslint_d")
    (setq flycheck-javascript-eslint-executable "eslint_d"))

  ;; Allow flycheck to display on the left fringe
  (setq flycheck-indication-mode 'left-fringe)

  ;; A non-descript, right-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [8 12 14 15 14 12 8] 8 7 'center)

  (setq
   ;; Stop cluttering my project dirs with temp flycheck files by moving everything to a temp dir
   flycheck-temp-prefix (concat temporary-file-directory "flycheck")
   ;; flycheck-check-syntax-automatically '((save idle-change mode-enabled new-line))
   flycheck-idle-change-delay 0.5)

  ;; HACK: The following advice is a workaround for a performance issue when
  ;; opening files, particularly JavaScript files, where the existence check of
  ;; eslint configuration is causing noticeable delays.  By overriding
  ;; `flycheck-eslint-config-exists-p' to always return true, we bypass the file
  ;; existence check, thus significantly improving file opening times.
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))


;; ─── GPTel ────────────────────────────────────────────────────────────────────

;; (defvar my-gptel-buffer-prefix "GPTel: "
;;   "Prefix for gptel buffers.")

;; (defun my-gptel-buffer-name (name)
;;   (if (string-prefix-p my-gptel-buffer-prefix name)
;;       name
;;     (concat my-gptel-buffer-prefix name)))

;; (advice-add 'gptel :around
;;             (lambda (orig-fn name &rest args)
;;               (apply orig-fn (my-gptel-buffer-name name) args)))

(after! gptel
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :side 'right
    :size 0.35
    :width 120
    :select t
    :modeline t
    :quit nil
    :ttl nil
    :vslot -1)
  (set-popup-rule! "^*doom:vterm-popup:" :sice 'bottom :size 0.25 :width 40 :height 0.16 :vslot -4 :select t :quit t)

  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)
  (setq gptel-default-mode 'org-mode)

  (setq gptel-model 'gpt-4.1)
  (setq gptel-magit-model 'gpt-5-mini)
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-magit-commit-prompt
        "You are an expert at writing Git commit messages. Output exactly one commit message in the format `<optional type>: <summary>`, where type ∈ {build,chore,ci,docs,feat,fix,perf,refactor,style,test} and only include it if it fits naturally without exceeding limits. Use imperative mood, capitalize the first word of the summary, keep it ≤50 characters (rewrite/shorten if needed), and omit ending punctuation. Add a body only if it is absolutely essential, separated by one blank line, wrapped at ≤72 characters. Do not include anything except the commit message.")

  (gptel-make-preset 'base-preset
    :description "Common style rules."
    :system "Do not add explanations, introductions, labels, comments, separators, or any extra output.
Always output only the final text, with nothing else.")

  (gptel-make-preset 'base-preset
    :description "Common style rules."
    :system "Do not use dashes (—) or semicolons as separators.
Do not use typographic quotes or smart quotes; always use plain ASCII ' for single quotes and \" for double quotes.
Always output only the final text, with nothing else.")

  (gptel-make-preset 'translate
    :model 'gpt-4o
    :parents '(base-preset)
    :description "Translate EN ↔ ES."
    :system "You are an expert English-Spanish translator.
If the text is in English, translate it into Spanish.
If it is in Spanish, translate it into English.
Maintain the tone, style, and nuances of the original text.")

  (gptel-make-preset 'translate-refine
    :model 'gpt-4o
    :parents '(base-preset)
    :description "Translate EN ↔ ES and refine wording."
    :system "You are an expert translator and editor.
Translate EN ↔ ES as needed, and reorganize the text to be clear, coherent, and concise.
Improve grammar, vocabulary, and style, while keeping the intended meaning.")

  (gptel-make-preset 'refine
    :model 'gpt-4o
    :parents '(base-preset)
    :description "Refine writing (clarity, conciseness, coherence)."
    :system "You are an expert editor.
Improve the wording of the text so it is clearer, more concise, and coherent while preserving the same meaning and tone.")

  (gptel-make-preset 'cr
    :model 'gpt-4.1
    :parents '(base-preset)
    :description "Line-by-line code review."
    :system "You are a senior code reviewer. Review the provided snippet line by line as needed.
Format:
- For each finding, start with Line X: then a one-line title, followed by a short explanation and fix.
Provide minimal corrected snippets or unified diff if necessary. Keep it concise and actionable."))


;; ─── LSP ──────────────────────────────────────────────────────────────────────

(after! lsp-mode
  (setq
   ;; lsp-clients-typescript-prefer-use-project-ts-server t

   ;; Follow the instructions to setup ESLint in LSP server:
   ;; https://github.com/emacs-lsp/lsp-mode/wiki/LSP-ESlint-integration#fn1
   lsp-eslint-download-url "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/dbaeumer/vsextensions/vscode-eslint/3.0.10/vspackage" ;; latest VSCode eslint extension from marketplace
   lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio")
   ;; lsp-eslint-server-command '("~/.vscode/extensions/dbaeumer.vscode-eslint-3.0.10/server/out/eslintServer.js" "--stdio")
   lsp-eslint-run "onType"
   ;; lsp-eslint-auto-fix-on-save t

   ;; lsp-enable-symbol-highlighting nil
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
   lsp-auto-execute-action nil ;; Do not execute the action without user confirmation
   lsp-use-plists t)

  ;; HACK: Don't prompt the user for the project root every time we open a new
  ;; lsp-worthy file, instead, try to guess it with projectile
  ;; This behavior was previously disabled as it was too confusing for beginners 🤷‍♂️:
  ;; https://github.com/doomemacs/doomemacs/commit/8c3f24f14c148f24d08aee6b6ae7a2a48e42853b#diff-b03b8e6a7215aedaf8f74b36b04158bba60d3e4b145790fd00857d7ea56969c0
  (setq lsp-auto-guess-root t)

  (defadvice! +lsp-prompt-if-no-project-a (session file-name)
    "Prompt for the project root only if no project was found."
    :after-until #'lsp--calculate-root
    (cond ((not lsp-auto-guess-root)
           nil)
          ((cl-find-if (lambda (dir)
                         (and (lsp--files-same-host dir file-name)
                              (file-in-directory-p file-name dir)))
                       (lsp-session-folders-blacklist session))
           nil)
          ((lsp--find-root-interactively session))))

  ;; HACK: Auto-fix ESLint issues on save when `lsp-eslint-auto-fix-on-save' is
  ;; non-nil since lsp-mode doesn't support this natively. See:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/1842
  (advice-add 'lsp--before-save :around #'my/lsp--eslint-before-save))


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


;; ─── REPL ─────────────────────────────────────────────────────────────────────

;; Set a default REPL for all the js-related modes
(set-repl-handler! '(rjsx-mode web-mode typescript-mode) #'+javascript/open-repl)


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
