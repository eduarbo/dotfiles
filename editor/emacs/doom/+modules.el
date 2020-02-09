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
  ;; On-demand code completion
  (setq company-idle-delay nil))


;; ┏━╸╻ ╻╻╻
;; ┣╸ ┃┏┛┃┃
;; ┗━╸┗┛ ╹┗━╸
;; evil 😈

(defun evil-embrace-js-mode-hook-setup ()
  (add-to-list 'evil-embrace-evil-surround-keys ?\`)
  (embrace-add-pair ?$ "${" "}"))

(after! evil-embrace
  (add-hook 'js-mode-hook 'evil-embrace-js-mode-hook-setup))

;; f/F/t/T/s/S
(after! evil-snipe
  ;; Disable evil-snipe-mode but keep incremental highlighting for the f/F/t/T
  ;; motions keys
  (evil-snipe-mode -1))


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
  (setq +ivy-project-search-engines '(ag rg))

  (custom-set-faces
   '(ivy-minibuffer-match-face-1
     ((t :foreground "#83898d" :box (:line-width -1))))))

(after! counsel
  (setq counsel-rg-base-command "rg -S --hidden --no-heading --line-number --color never --glob '!.git' %s"
        counsel-ag-base-command "ag -S --hidden --nocolor --nogroup %s"))


;; ╻  ┏━┓┏━┓
;; ┃  ┗━┓┣━┛
;; ┗━╸┗━┛╹
;; lsp

(after! lsp-ui
  ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so disable
  ;; it by default
  (setq lsp-ui-sideline-enable nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil)
  ;; Disable lsp-ui flychecker
  (setq lsp-prefer-flymake :none))

;; (after! lsp-mode
;;   (setq lsp-enable-symbol-highlighting nil))


;; ┏┳┓┏━┓┏━╸╻╺┳╸
;; ┃┃┃┣━┫┃╺┓┃ ┃
;; ╹ ╹╹ ╹┗━┛╹ ╹
;; magit

(after! magit
  (setq magit-repository-directories '(("~/dev" . 2))
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
    ;; ~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el
    ;; doom-modeline-buffer-file-name-style 'truncate-upto-project
    ;; ~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/e/lisp/comint.el
    ;; doom-modeline-buffer-file-name-style 'truncate-upto-root
    ;; ~/Projects/FOSS/emacs/lisp/comint.el => emacs/lisp/comint.el
    doom-modeline-buffer-file-name-style 'relative-from-project
    doom-modeline-vcs-max-length 18
    doom-modeline-buffer-encoding nil
    size-indication-mode nil
    )
  )


;; ┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┃ ┃┣┳┛┃╺┓╺━╸┃┃┃┃ ┃ ┃┃┣╸
;; ┗━┛╹┗╸┗━┛   ╹ ╹┗━┛╺┻┛┗━╸
;; org-mode

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Load org-habit with org.el
(setq org-habit-graph-column 105)
(add-to-list 'org-modules 'org-habit t)

;; Paths
(setq
  org-directory (expand-file-name "~/org")
  org-archive-location "archive/%s::datetree/"
  )

;; Options

(setq
  ;; Disable template for .org files to avoid conflicts with capture tools
  +file-templates-alist (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist)
  )

(after! org
  (setq
    org-pretty-entities t
    org-hide-emphasis-markers t

    ;; Too many clock entries clutter up a heading
    org-log-into-drawer t
    org-log-done 'time
    org-startup-indented t
    org-startup-truncated nil
    org-startup-with-inline-images t
    org-imenu-depth 5
    org-outline-path-complete-in-steps nil
    org-highest-priority ?A
    org-default-priority ?B
    org-lowest-priority ?C
    org-image-actual-width '(600)
    org-show-notification-handler 'message
    org-clock-clocked-in-display 'frame-title

    org-clone-delete-id t

    ;; org-src-fontify-natively t
    ;; org-yank-adjusted-subtrees t

    org-file-apps
    '((auto-mode . emacs)
       ("\\.x?html?\\'" . "firefox %s")
       ("\\.pdf\\'" . "open %s"))
    )


  ;; Styling
  (setq
    org-ellipsis "  "  ;; ▼ ˅ ⌄ ↓ ⤵ ▼ ↴ ⬎ ⤷
    org-bullets-bullet-list '("𐄙" "𐄚" "𐄛" "𐄜" "𐄝" "𐄞" "𐄟" "𐄠" "𐄡")
    ;; org-bullets-bullet-list '("𝍠" "𝍡" "𝍢" "𝍤" "𝍥" "𝍦" "𝍧" "𝍨")
    ;; org-bullets-bullet-list '("𐄇" "𐄈" "𐄉" "𐄊" "𐄋" "𐄌" "𐄍" "𐄎" "𐄏")
    ;; org-bullets-bullet-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
    )
  )

(add-hook! org-mode
  (custom-set-faces!
    '(link :weight normal)
    '((org-document-title outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7 outline-8)
       :weight normal))

  (set-face-attribute 'variable-pitch nil :font doom-variable-pitch-font)
  ;; FIXME use doom-font instead of hardcoding the font
  ;; (set-face-attribute 'fixed-pitch nil :family (font-get doom-font :family))
  (set-face-attribute 'fixed-pitch nil :family "Hack")
  (set-face-attribute 'org-document-title nil :height 1.4)
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (set-face-attribute 'org-level-2 nil :height 1.1)
  (set-face-attribute 'org-property-value nil :foreground "#83898d" :weight 'bold)
  (dolist (face '(org-document-info-keyword org-drawer org-special-keyword))
    (set-face-attribute face nil :foreground "#5B6268"))
  (set-face-attribute 'org-special-keyword nil :weight 'bold)

  ;; Keep the fixed-pitch for some faces when variable-pitch-mode is enabled
  (dolist (face '(
                   org-block
                   org-code
                   org-document-info-keyword
                   org-indent
                   org-meta-line
                   org-property-value
                   org-special-keyword
                   org-table
                   org-tag
                   org-todo
                   org-hide
                   org-drawer
                   org-done
                   hl-todo
                   org-block-begin-line
                   org-verbatim
                   org-date
                   font-lock-comment-face
                   line-number
                   line-number-current-line
                   ))
    (set-face-attribute face nil :inherit 'fixed-pitch)))

(use-package! org-id ; built-in
  :after org
  :init
  ;; By using unique ID's, links will work even if you move them across files
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

;; Add CREATED date property to TODOs
(use-package! org-expiry ; built-in
  :after org
  :init
  (setq org-expiry-inactive-timestamps t)
  :config
  (org-expiry-insinuate))

;; fix autoload for +org/toggle-clock
(use-package! org-clock ; built-in
  :commands (org-clock-save org-clocking-p org-clock-load))


;; ┏━┓┏━┓┏━╸    ┏┓┏━┓╻ ╻┏━┓┏┓╻┏━┓╻
;; ┃ ┃┣┳┛┃╺┓╺━╸  ┃┃ ┃┃ ┃┣┳┛┃┗┫┣━┫┃
;; ┗━┛╹┗╸┗━┛   ┗━┛┗━┛┗━┛╹┗╸╹ ╹╹ ╹┗━╸
;; org-journal

;; (setq org-journal-dir (expand-file-name "journal" org-directory))

(when (featurep! +journal)
  (after! org-journal
    (setq org-extend-today-until 4 ;; sometimes my days end at 4am
      org-journal-carryover-items nil
      org-journal-file-type 'weekly
      ;; Check ~format-time-string~ help for a list of the formatting symbols
      ;; org-journal-date-format 'org-journal-date-format-func
      ;; org-journal-file-format "%Y/%Y-%m-%d %A.org"
      ;; org-journal-date-prefix "#+TITLE: "
      ;; FIXME Exclude journals from doom file-templates, that is overriding the TITLE
      ;; org-journal-date-format "%A, %d %B %Y"
      ;; org-journal-time-prefix "* "
      ;; (org-journal-time-format "[%F %a %R]")
      ;; org-journal-hide-entries-p nil
      ))

  ;; FIXME exclude journal notes from templates
  ;; (setq +file-templates-alist
  ;;       (cons '("\\(?!/journal/\\).+\\.org$" :trigger "__" :mode org-mode)
  ;;             (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist)))
  )



;; ┏━┓┏━┓┏━┓ ┏┓┏━╸┏━╸╺┳╸╻╻  ┏━╸
;; ┣━┛┣┳┛┃ ┃  ┃┣╸ ┃   ┃ ┃┃  ┣╸
;; ╹  ╹┗╸┗━┛┗━┛┗━╸┗━╸ ╹ ╹┗━╸┗━╸

(after! projectile
  (setq projectile-project-search-path '("~/dev" "~/work")))

(after! helm-projectile
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-projectile-recentf-list
                                    helm-source-buffer-not-found)))


;; ╺┳╸┏━┓┏━╸┏━╸┏┳┓┏━┓┏━╸┏━┓
;;  ┃ ┣┳┛┣╸ ┣╸ ┃┃┃┣━┫┃  ┗━┓
;;  ╹ ╹┗╸┗━╸┗━╸╹ ╹╹ ╹┗━╸┗━┛

(after! treemacs
  (setq treemacs--icon-size 20))

;; Enable custom treemacs theme (all-the-icons must be installed!)
(doom-themes-treemacs-config)
