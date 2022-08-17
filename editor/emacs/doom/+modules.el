;;; editor/emacs/doom/+modules.el -*- lexical-binding: t; -*-

;; • ▌ ▄ ·.       ·▄▄▄▄  ▄• ▄▌▄▄▌  ▄▄▄ ..▄▄ ·
;; ·██ ▐███▪▪     ██▪ ██ █▪██▌██•  ▀▄.▀·▐█ ▀.
;; ▐█ ▌▐▌▐█· ▄█▀▄ ▐█· ▐█▌█▌▐█▌██▪  ▐▀▀▪▄▄▀▀▀█▄
;; ██ ██▌▐█▌▐█▌.▐▌██. ██ ▐█▄█▌▐█▌▐▌▐█▄▄▌▐█▄▪▐█
;; ▀▀  █▪▀▀▀ ▀█▄▀▪▀▀▀▀▀•  ▀▀▀ .▀▀▀  ▀▀▀  ▀▀▀▀


;; ┏┓ ┏━┓┏━┓╻ ╻┏━┓┏━╸   ┏━┓╺┳╸   ┏━┓┏━╸┏┳┓┏━┓╺┳╸┏━╸
;; ┣┻┓┣┳┛┃ ┃┃╻┃┗━┓┣╸ ╺━╸┣━┫ ┃ ╺━╸┣┳┛┣╸ ┃┃┃┃ ┃ ┃ ┣╸
;; ┗━┛╹┗╸┗━┛┗┻┛┗━┛┗━╸   ╹ ╹ ╹    ╹┗╸┗━╸╹ ╹┗━┛ ╹ ┗━╸

(after! browse-at-remote
  ;; HACK `browse-at-remote--get-remotes' returns a list of remotes in alphabetical order. Sometimes
  ;; when I want to copy the URL of the current file with `+vc/browse-at-remote-kill' it selects the
  ;; remote at the top of the list which I don't want. If it can't determine the correct remote it
  ;; should select "origin" as fallback.
  (advice-add 'browse-at-remote--get-remotes :filter-return #'my--sort-git-remotes-a))


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

;; A guide on disabling/enabling lsp-mode features:
;;   https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; Settings:
;;   https://emacs-lsp.github.io/lsp-mode/page/settings/

;; If an LSP server isn't present when I start a prog-mode buffer, you don't need to tell me. I
;; know. On some systems I don't care to have a whole development environment for some ecosystems.
(setq +lsp-prompt-to-install-server 'quiet)

;; Follow the instructions to setup ESLint in LSP server:
;; https://github.com/emacs-lsp/lsp-mode/wiki/LSP-ESlint-integration#fn1
(setq lsp-eslint-server-command
      `("node" ,(expand-file-name (car (last (file-expand-wildcards "~/.vscode/extensions/dbaeumer.vscode-eslint-*/server/out/eslintServer.js")))) "--stdio"))

;; A guide on disabling/enabling lsp-mode features:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq
 ;; lsp-diagnostics-provider :none

 lsp-signature-doc-lines 5
 ;; lsp-signature-render-documentation nil

 ;; FIXME Disabled until figure out why `lsp-signature-doc-lines' is not
 ;; limiting the number of lines to display in eldoc
 ;; lsp-eldoc-enable-hover nil

 ;; lsp-enable-symbol-highlighting nil
 ;; lsp-enable-file-watchers nil

 lsp-auto-execute-action nil

 lsp-modeline-diagnostics-enable nil
 lsp-modeline-code-actions-enable nil)

;; If you are in a buffer with `lsp-mode' enabled and a server that supports
;; `textDocument/formatting', it will be used instead of `format-all's
;; formatter. Unfortunately typescript does not seem to be respecting my
;; settings, and is slower than format-all so I prefer to disable it
;; universally.

(setq +format-with-lsp nil)

;; To disable this behavior in one mode use:
;; (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)


;; ┏┳┓┏━┓┏━╸╻╺┳╸
;; ┃┃┃┣━┫┃╺┓┃ ┃
;; ╹ ╹╹ ╹┗━┛╹ ╹

(after! magit
  (setq
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
   magit-repository-directories '(("~/dev" . 2))
   ;; Don't restore the wconf after quitting magit, it's jarring
   magit-inhibit-save-previous-winconf t
   transient-values '((magit-rebase "--autosquash" "--autostash")
                      (magit-pull "--rebase" "--autostash"))))


;; ┏┳┓┏━┓┏━┓┏━╸╻┏┓╻┏━┓╻  ╻┏━┓
;; ┃┃┃┣━┫┣┳┛┃╺┓┃┃┗┫┣━┫┃  ┃┣━┫
;; ╹ ╹╹ ╹╹┗╸┗━┛╹╹ ╹╹ ╹┗━╸╹╹ ╹

(after! marginalia
  (setq marginalia-field-width 800))


;; ┏┳┓┏━┓╺┳┓┏━╸╻  ╻┏┓╻┏━╸
;; ┃┃┃┃ ┃ ┃┃┣╸ ┃  ┃┃┗┫┣╸
;; ╹ ╹┗━┛╺┻┛┗━╸┗━╸╹╹ ╹┗━╸

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

(setq projectile-project-search-path '(("~/dev" . 2)))


;; ┏━┓┏━┓┏━╸╻  ╻
;; ┗━┓┣━┛┣╸ ┃  ┃
;; ┗━┛╹  ┗━╸┗━╸┗━╸

(after! ispell
  (setq ispell-extra-args '("--sug-mode=ultra" "--run-together" "--camel-case"))
  (setq ispell-dictionary "english")
  (setq ispell-personal-dictionary
        (substitute-in-file-name (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                                                   doom-data-dir))))

(add-hook! '(ispell-change-dictionary-hook flyspell-mode-hook)
  (defun +spell-sync-local-personaly-dictionary-h ()
    "Sync personal dictionary with Ispell's"
    (when-let (lang (or ispell-local-dictionary ispell-dictionary))
      (setq-local ispell-personal-dictionary (expand-file-name (concat "ispell/" lang ".pws")
                                                               doom-data-dir))
      (setq-local ispell-complete-word-dict (expand-file-name (concat "ispell/" lang ".dict")
                                                              doom-data-dir)))))

;; Quickly switch dictionaries
;; Adapted from DiogoRamos' snippet on https://www.emacswiki.org/emacs/FlySpell#h5o-5
(let ((langs '("spanish" "english")))
  (defvar lang-ring (make-ring (length langs))
    "List of Ispell dictionaries you can switch to using `my/cycle-ispell-languages'.")
  (dolist (elem langs) (ring-insert lang-ring elem)))


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
