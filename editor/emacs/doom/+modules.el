;;; editor/emacs/doom/+modules.el -*- lexical-binding: t; -*-

;; • ▌ ▄ ·.       ·▄▄▄▄  ▄• ▄▌▄▄▌  ▄▄▄ ..▄▄ ·
;; ·██ ▐███▪▪     ██▪ ██ █▪██▌██•  ▀▄.▀·▐█ ▀.
;; ▐█ ▌▐▌▐█· ▄█▀▄ ▐█· ▐█▌█▌▐█▌██▪  ▐▀▀▪▄▄▀▀▀█▄
;; ██ ██▌▐█▌▐█▌.▐▌██. ██ ▐█▄█▌▐█▌▐▌▐█▄▄▌▐█▄▪▐█
;; ▀▀  █▪▀▀▀ ▀█▄▀▪▀▀▀▀▀•  ▀▀▀ .▀▀▀  ▀▀▀  ▀▀▀▀


;; ┏━┓┏━╸┏━╸   ╻ ╻╻┏┓╻╺┳┓┏━┓╻ ╻
;; ┣━┫┃  ┣╸ ╺━╸┃╻┃┃┃┗┫ ┃┃┃ ┃┃╻┃
;; ╹ ╹┗━╸┗━╸   ┗┻┛╹╹ ╹╺┻┛┗━┛┗┻┛

(after! ace-window
  (setq aw-ignore-current t))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))


;; ┏━╸┏━┓┏┳┓┏━┓┏━┓┏┓╻╻ ╻
;; ┃  ┃ ┃┃┃┃┣━┛┣━┫┃┗┫┗┳┛
;; ┗━╸┗━┛╹ ╹╹  ╹ ╹╹ ╹ ╹

(after! company
  ;; This slows down company
  ;; (setq company-box-doc-enable nil)

  ;; On-demand code completion
  (setq company-idle-delay nil)
  ;; Sort by occurrence and group by backend (very useful for file completion)
  (setq company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance)))


;; ┏━╸┏┳┓┏┳┓┏━╸╺┳╸
;; ┣╸ ┃┃┃┃┃┃┣╸  ┃
;; ┗━╸╹ ╹╹ ╹┗━╸ ╹

(after! emmet-mode
  ;; Company is more useful than emmet in these modes, so... fuck off!
  (remove-hook! '(rjsx-mode-hook css-mode-hook) #'emmet-mode))


;; ┏━╸╻ ╻╻╻     ┏━┓┏┓╻╻┏━┓┏━╸
;; ┣╸ ┃┏┛┃┃  ╺━╸┗━┓┃┗┫┃┣━┛┣╸
;; ┗━╸┗┛ ╹┗━╸   ┗━┛╹ ╹╹╹  ┗━╸

;; Do not override my bindings!
(setq
 evil-snipe-repeat-keys nil
 evil-snipe-enable-incremental-highlight nil
 evil-snipe-override-evil-repeat-keys nil)

;; Unbind evil-snipe-S
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)


;; ┏━╸╻┏━╸╻  ┏━╸╺┳╸
;; ┣╸ ┃┃╺┓┃  ┣╸  ┃
;; ╹  ╹┗━┛┗━╸┗━╸ ╹

(use-package! figlet
  :config
  (setq figlet-default-font "Future"))


;; ╻  ┏━┓┏━┓
;; ┃  ┗━┓┣━┛
;; ┗━╸┗━┛╹
;; lsp

;; A guide on disabling/enabling lsp-mode features:
;;   https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; Settings:
;;   https://emacs-lsp.github.io/lsp-mode/page/settings/

(after! lsp-ui
  (setq
   ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so disable it by default
   lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
   lsp-ui-doc-enable nil        ; slow and redundant with K

   lsp-ui-doc-include-signature t

   ;; lsp-ui-doc-use-webkit t
   lsp-ui-doc-max-width 100
   lsp-ui-doc-max-height 12)
  ;; If an LSP server isn't present when I start a prog-mode buffer, you don't need to tell me. I
  ;; know. On some systems I don't care to have a whole development environment for some ecosystems.
  +lsp-prompt-to-install-server 'quiet)

(after! lsp-mode
  (setq
   lsp-signature-doc-lines 5
   ;; lsp-signature-render-documentation nil

   ;; FIXME Disabled until figure out why `lsp-signature-doc-lines' is not
   ;; limiting the number of lines to display in eldoc
   lsp-eldoc-enable-hover nil

   ;; Disable lsp checker b/c annoying
   lsp-diagnostics-provider :none

   lsp-enable-indentation nil
   ;; lsp-enable-symbol-highlighting nil
   ;; lsp-enable-file-watchers nil

   lsp-modeline-diagnostics-enable nil
   lsp-modeline-code-actions-enable nil))


;; ┏┳┓┏━┓┏━╸╻╺┳╸
;; ┃┃┃┣━┫┃╺┓┃ ┃
;; ╹ ╹╹ ╹┗━┛╹ ╹
;; magit

(after! magit
  (setq
   magit-repository-directories '(("~/dev" . 1) ("~/work" . 1))
   ;; Don't restore the wconf after quitting magit, it's jarring
   magit-inhibit-save-previous-winconf t
   transient-values '((magit-rebase "--autosquash" "--autostash")
                      (magit-pull "--rebase" "--autostash"))))


;; ┏┳┓┏━┓╺┳┓┏━╸╻  ╻┏┓╻┏━╸
;; ┃┃┃┃ ┃ ┃┃┣╸ ┃  ┃┃┗┫┣╸
;; ╹ ╹┗━┛╺┻┛┗━╸┗━╸╹╹ ╹┗━╸
;; modeline

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


;; ┏━┓┏━┓┏━┓ ┏┓┏━╸┏━╸╺┳╸╻╻  ┏━╸
;; ┣━┛┣┳┛┃ ┃  ┃┣╸ ┃   ┃ ┃┃  ┣╸
;; ╹  ╹┗╸┗━┛┗━┛┗━╸┗━╸ ╹ ╹┗━╸┗━╸

(after! projectile
  (setq projectile-project-search-path '("~/dev" "~/work")))


;; ┏━┓┏━┓┏━╸╻  ╻
;; ┗━┓┣━┛┣╸ ┃  ┃
;; ┗━┛╹  ┗━╸┗━╸┗━╸

(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default dictionary' even though multiple
  ;; dictionaries will be configured in next line
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB,en_US,es_ANY")
  ;; ispell-set-spellchecker-params has to be called before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (when (featurep! +hunspell)
    ;; ispell-set-spellchecker-params has to be called before ispell-hunspell-add-multi-dic
    (ispell-hunspell-add-multi-dic ispell-dictionary))
  (setq ispell-personal-dictionary
        (expand-file-name (concat "personal_dict") doom-etc-dir)))


;; ╺┳╸┏━┓┏━╸┏━╸┏┳┓┏━┓┏━╸┏━┓
;;  ┃ ┣┳┛┣╸ ┣╸ ┃┃┃┣━┫┃  ┗━┓
;;  ╹ ╹┗╸┗━╸┗━╸╹ ╹╹ ╹┗━╸┗━┛

(after! treemacs
  ;; Prefer a monospace font for treemacs
  (setq doom-themes-treemacs-enable-variable-pitch nil))


;; ╺━┓┏━╸┏┓╻
;; ┏━┛┣╸ ┃┗┫
;; ┗━╸┗━╸╹ ╹

(setq +zen-text-scale 3)

(add-hook! 'writeroom-mode-hook
  (defun +zen-better-line-spacing-mode-h ()
    "Set bigger line-spacing and center text vertically"
    (setq-local default-text-properties (if writeroom-mode '(line-spacing 0.6 line-height 1.6) nil)))

  (defun +zen-fix-mixed-pitch-mode-h ()
    "`solaire-mode' and make `mixed-pitch-mode' are incompatible since both
     remaps faces. To fix that disable `solaire-mode' when enabling `mixed-pitch-mode'"
    ;; https://www.reddit.com/r/DoomEmacs/comments/l9jy0h/how_does_variablepitchmode_work_and_why_does_it/gljibj9
    (solaire-mode (if writeroom-mode -1 +1))))

(after! mixed-pitch
  (pushnew! mixed-pitch-fixed-pitch-faces
            'org-hide
            'org-drawer
            'org-done
            'org-ellipsis
            'hl-todo
            'warning
            'success
            'error))
