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
  (set-company-backend! 'text-mode 'company-ispell 'company-capf)

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
  ;; Disable evil-snipe-mode but keep incremental highlighting for the f/F/t/T
  ;; motions keys
  (evil-snipe-mode -1))


;; ┏━╸╻  ╻ ╻┏━┓┏━┓┏━╸╻  ╻
;; ┣╸ ┃  ┗┳┛┗━┓┣━┛┣╸ ┃  ┃
;; ╹  ┗━╸ ╹ ┗━┛╹  ┗━╸┗━╸┗━╸

(after! flyspell
  (remove-hook! text-mode #'flyspell-mode))


;; ╻ ╻┏━╸╻  ┏┳┓
;; ┣━┫┣╸ ┃  ┃┃┃
;; ╹ ╹┗━╸┗━╸╹ ╹

(after! helm
  ;; Show hidden files too
  (setq helm-ag-command-option "--hidden"))


;; ╻╻ ╻╻ ╻
;; ┃┃┏┛┗┳┛
;; ╹┗┛  ╹

(after! ivy
  ;; Swap engines since ivy-resume doesn't play well with rg when using options
  (setq +ivy-project-search-engines '(ag rg)))

(after! counsel
  (setq
    counsel-rg-base-command "rg -S --hidden --no-heading --line-number --color never --glob '!.git' %s"
    counsel-ag-base-command "ag -S --hidden --nocolor --nogroup %s"))

(custom-set-faces!
  '(ivy-minibuffer-match-face-1
     :foreground "#83898d"
     :box (:line-width -1)))


;; ╻┏ ┏━╸╺┳╸┏━╸┏━┓┏━╸┏━┓
;; ┣┻┓┣╸  ┃ ┣╸ ┣┳┛┣╸ ┃┓┃
;; ╹ ╹┗━╸ ╹ ╹  ╹┗╸┗━╸┗┻┛

;; http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-keyfreq.el
(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;; ╻  ┏━┓┏━┓
;; ┃  ┗━┓┣━┛
;; ┗━╸┗━┛╹
;; lsp

(after! lsp-ui
  (setq
    ;; disable invasive lsp-ui-sideline
    lsp-ui-sideline-enable nil

    ;; No multiline eldoc please
    lsp-eldoc-enable-hover nil

    ;; Disable diagnostic b/c annoying
    lsp-flycheck-live-reporting nil
    lsp-diagnostic-package :none
    lsp-prefer-flymake :none))


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
    doom-modeline-major-mode-icon t
    doom-modeline-vcs-max-length 18
    doom-modeline-buffer-encoding nil
    size-indication-mode nil
    )
  )

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


;; ╺━┓┏━╸┏┓╻
;; ┏━┛┣╸ ┃┗┫
;; ┗━╸┗━╸╹ ╹

(after! mixed-pitch
  (pushnew! mixed-pitch-fixed-pitch-faces
    'org-hide
    'org-drawer
    'org-done
    'hl-todo
    'warning
    'success
    'error))

(add-hook! mixed-pitch-mode
  ;; Set bigger line-spacing and center text vertically.
  ;; writeroom-mode can also set the line-spacing but won't center the text
  (setq-local default-text-properties '(line-spacing 0.3 line-height 1.3)))

(after! writeroom-mode
  (setq +zen-text-scale 0)
  (setq writeroom-mode-line t))

(add-hook! text-mode #'writeroom-mode)
