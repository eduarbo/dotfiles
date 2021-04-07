;;; ~/dev/dotfiles/editor/emacs/doom/+modules.el -*- lexical-binding: t; -*-

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
  ;; This just slow down company
  (setq company-box-doc-enable nil)

  ;; On-demand code completion
  (setq company-idle-delay nil))


;; ┏━╸┏┳┓┏┳┓┏━╸╺┳╸
;; ┣╸ ┃┃┃┃┃┃┣╸  ┃
;; ┗━╸╹ ╹╹ ╹┗━╸ ╹

(after! emmet-mode
  ;; Company is more useful than emmet in these modes, so... fuck off!
  (remove-hook! '(rjsx-mode-hook css-mode-hook) #'emmet-mode))


;; ┏━╸╻ ╻╻╻     ┏━┓┏┓╻╻┏━┓┏━╸
;; ┣╸ ┃┏┛┃┃  ╺━╸┗━┓┃┗┫┃┣━┛┣╸
;; ┗━╸┗┛ ╹┗━╸   ┗━┛╹ ╹╹╹  ┗━╸

(after! evil-snipe
  ;; free up the keys =s= and =S=
  (evil-snipe-mode -1))

;; Do not override my bindings!
(setq
  evil-snipe-repeat-keys nil
  evil-snipe-enable-incremental-highlight nil
  evil-snipe-override-evil-repeat-keys nil)


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
    lsp-ui-sideline-enable nil

    lsp-ui-doc-include-signature t

    ;; lsp-ui-doc-use-webkit t
    lsp-ui-doc-max-width 100
    lsp-ui-doc-max-height 12))

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
    ;; Don't restore the wconf after quitting magit, it's jarring
    magit-inhibit-save-previous-winconf t
    magit-repository-directories '(("~/dev" . 1) ("~/work" . 1))))


;; ┏┳┓┏━┓╺┳┓┏━╸╻  ╻┏┓╻┏━╸
;; ┃┃┃┃ ┃ ┃┃┣╸ ┃  ┃┃┗┫┣╸
;; ╹ ╹┗━┛╺┻┛┗━╸┗━╸╹╹ ╹┗━╸
;; modeline

(after! doom-modeline
  (setq
    doom-modeline-buffer-encoding nil
    doom-modeline-buffer-modification-icon nil
    doom-modeline-major-mode-icon t
    doom-modeline-vcs-max-length 18)

  ;; Remove size indicator
  (remove-hook! doom-modeline-mode #'size-indication-mode))


;; ┏━┓┏━┓┏━┓ ┏┓┏━╸┏━╸╺┳╸╻╻  ┏━╸
;; ┣━┛┣┳┛┃ ┃  ┃┣╸ ┃   ┃ ┃┃  ┣╸
;; ╹  ╹┗╸┗━┛┗━┛┗━╸┗━╸ ╹ ╹┗━╸┗━╸

(after! projectile
  (setq projectile-project-search-path '("~/dev" "~/work")))


;; ┏━┓┏━┓┏━╸╻  ╻
;; ┗━┓┣━┛┣╸ ┃  ┃
;; ┗━┛╹  ┗━╸┗━╸┗━╸

(after! ispell
  (setq ispell-dictionary "en_GB,en_US,es_ANY")
  (ispell-set-spellchecker-params)
  ;; ispell-set-spellchecker-params has to be called before
  ;; ispell-hunspell-add-multi-dic will work
  (ispell-hunspell-add-multi-dic ispell-dictionary)
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
