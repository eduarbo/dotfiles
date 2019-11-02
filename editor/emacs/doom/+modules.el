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


;; ┏━╸╻  ╻ ╻┏━┓┏━┓┏━╸╻  ╻
;; ┣╸ ┃  ┗┳┛┗━┓┣━┛┣╸ ┃  ┃
;; ╹  ┗━╸ ╹ ┗━┛╹  ┗━╸┗━╸┗━╸

(after! flyspell
  ;; default to flyspell prog mode
  (setq flyspell-generic-check-word-predicate #'flyspell-generic-progmode-verify))


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
  (setq doom-modeline-major-mode-icon t
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el => emacs/lisp/comint.el
        ;; doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Given ~/Projects/FOSS/emacs/lisp/comint.el => ~/Projects/FOSS/emacs/l/comint.el
        doom-modeline-buffer-file-name-style 'truncate-upto-project))


;; ┏━┓┏━┓┏━╸   ┏┳┓┏━┓╺┳┓┏━╸
;; ┃ ┃┣┳┛┃╺┓╺━╸┃┃┃┃ ┃ ┃┃┣╸
;; ┗━┛╹┗╸┗━┛   ╹ ╹┗━┛╺┻┛┗━╸
;; org-mode

(add-to-list 'org-modules 'org-habit t)

(after! org
  (setq org-directory (expand-file-name "~/Documents/org"))

  (setq org-ellipsis "  "  ;; ▼ ˅ ⌄ ↓ ⤵ ▼ ↴ ⬎ ⤷
        org-bullets-bullet-list '("#")
        org-pretty-entities t
        org-hide-emphasis-markers t)

  (setq org-log-done t
        org-startup-indented t
        org-startup-truncated nil
        org-startup-with-inline-images t
        org-imenu-depth 5
        org-outline-path-complete-in-steps nil
        org-highest-priority ?A
        org-default-priority ?B
        org-lowest-priority ?C
        org-image-actual-width '(300)

        ;; requires org-expiry
        org-expiry-inactive-timestamps t

        ;; requires org-clock
        org-show-notification-handler 'message

        ;; org-yank-adjusted-subtrees t

        org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . "firefox %s")
          ("\\.pdf\\'" . "open %s")))

  ;; By using unique ID's for links in Org-mode, links will work even if you
  ;; move them across files
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-clone-delete-id t)

  (setq +file-templates-alist
        (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist))

  (add-to-list 'org-global-properties
               ;; TODO make a decision
               ;; '("Effort_ALL" . "1h 2h 4h 6h 8h 16h"))
               '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

  (defun org-config-faces ()
    (set-face-attribute 'org-document-title nil :height 1.4)
    (set-face-attribute 'org-level-1 nil :height 1.2)
    (set-face-attribute 'org-level-2 nil :height 1.1))
  (add-hook 'org-mode-hook 'org-config-faces)

  (load! "./+gtd.el"))


;; ┏━┓┏━┓┏━╸    ┏┓┏━┓╻ ╻┏━┓┏┓╻┏━┓╻
;; ┃ ┃┣┳┛┃╺┓╺━╸  ┃┃ ┃┃ ┃┣┳┛┃┗┫┣━┫┃
;; ┗━┛╹┗╸┗━┛   ┗━┛┗━┛┗━┛╹┗╸╹ ╹╹ ╹┗━╸
;; org-journal

(after! org-journal
  (setq org-journal-dir (expand-file-name "journal" org-directory)
        org-extend-today-until 4 ;; sometimes my days end at 4am
        org-journal-carryover-items nil
        org-journal-file-type 'weekly))

;; FIXME exclude journal notes from templates
;; (setq +file-templates-alist
;;       (cons '("\\(?!/journal/\\).+\\.org$" :trigger "__" :mode org-mode)
;;             (remove '("\\.org$" :trigger "__" :mode org-mode) +file-templates-alist)))


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
