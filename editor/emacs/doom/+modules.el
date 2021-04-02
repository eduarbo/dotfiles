;;; ~/dev/dotfiles/editor/emacs/doom/+modules.el -*- lexical-binding: t; -*-

;; • ▌ ▄ ·.       ·▄▄▄▄  ▄• ▄▌▄▄▌  ▄▄▄ ..▄▄ ·
;; ·██ ▐███▪▪     ██▪ ██ █▪██▌██•  ▀▄.▀·▐█ ▀.
;; ▐█ ▌▐▌▐█· ▄█▀▄ ▐█· ▐█▌█▌▐█▌██▪  ▐▀▀▪▄▄▀▀▀█▄
;; ██ ██▌▐█▌▐█▌.▐▌██. ██ ▐█▄█▌▐█▌▐▌▐█▄▄▌▐█▄▪▐█
;; ▀▀  █▪▀▀▀ ▀█▄▀▪▀▀▀▀▀•  ▀▀▀ .▀▀▀  ▀▀▀  ▀▀▀▀


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

;; Free the s-l binding
(setq lsp-keymap-prefix nil)
(setq +lsp-auto-install-servers t)

(after! lsp-ui
  (setq
    ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
    ;; disable it by default.
    lsp-ui-sideline-enable nil
    ;; lsp-ui-doc-use-webkit t
    ;; lsp-ui-doc-use-childframe t
    ))

(after! lsp-mode
  (setq
    ;; No multiline eldoc please
    lsp-eldoc-enable-hover nil

    ;; Disable diagnostic b/c annoying
    lsp-flycheck-live-reporting nil
    lsp-diagnostics-provider :none

    ;; lsp-enable-symbol-highlighting nil
    lsp-enable-indentation nil
    lsp-enable-on-type-formatting nil
    lsp-enable-file-watchers nil))


;; ┏┳┓┏━┓┏━╸╻╺┳╸
;; ┃┃┃┣━┫┃╺┓┃ ┃
;; ╹ ╹╹ ╹┗━┛╹ ╹
;; magit

(after! magit
  (setq
    magit-repository-directories '(("~/dev" . 2))
    magit-save-repository-buffers nil
    ;; Don't restore the wconf after quitting magit
    magit-inhibit-save-previous-winconf t))


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

(custom-set-faces!
  '(doom-modeline-info :inherit success)
  '(doom-modeline-buffer-major-mode :inherit mode-line-emphasis))


;; ┏━┓┏━┓┏━┓ ┏┓┏━╸┏━╸╺┳╸╻╻  ┏━╸
;; ┣━┛┣┳┛┃ ┃  ┃┣╸ ┃   ┃ ┃┃  ┣╸
;; ╹  ╹┗╸┗━┛┗━┛┗━╸┗━╸ ╹ ╹┗━╸┗━╸

(after! projectile
  (setq projectile-project-search-path '("~/dev" "~/work")))

(after! helm-projectile
  (setq helm-mini-default-sources
    '(helm-source-buffers-list
       helm-source-projectile-recentf-list
       helm-source-buffer-not-found)))


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


;; ╺━┓┏━╸┏┓╻
;; ┏━┛┣╸ ┃┗┫
;; ┗━╸┗━╸╹ ╹

;; FIXME This kills buffers when switching to other workspaces
;; (add-hook! text-mode
;;   ;; Set bigger line-spacing and center text vertically
;;   (setq-local default-text-properties '(line-spacing 0.3 line-height 1.3))
;;   (setq-local visual-fill-column-mode 80)

;;   (mixed-pitch-mode t)
;;   (visual-fill-column-mode t))

(after! mixed-pitch
  (pushnew! mixed-pitch-fixed-pitch-faces
    'org-hide
    'org-drawer
    'org-done
    'hl-todo
    'warning
    'success
    'error))
