;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)

;; ─── Disabled Packages ────────────────────────────────────────────────────────
;; These packages are disabled because they are not useful in my setup

(package! evil-escape :disable t)  ;; ESC key is easily reachable in my custom kbd layout
(package! lsp-ui :disable t)       ;; Annoying and unnecessary
(package! emmet-mode :disable t)   ;; not needed for my workflow

;; ─── Syntax Highlighting & File Modes ─────────────────────────────────────────
(package! vimrc-mode)  ;; Provides syntax highlighting for Vim configuration files (.vimrc, .vim)

;; ─── Productivity & Writing ───────────────────────────────────────────────────
(package! obsidian)  ;; Integration with Obsidian for note-taking
(package! string-inflection)  ;; Cycle through different naming conventions (camelCase, snake_case, etc.)

;; ─── AI-Assisted Development ──────────────────────────────────────────────────
(package! aider  ;; AI-powered assistant for Emacs (experimental)
  :recipe (:host github :repo "tninja/aider.el" :files ("aider.el" "aider-core.el" "aider-file.el" "aider-code-change.el" "aider-discussion.el" "aider-prompt-mode.el" "aider-doom.el")))

;; ─── Snippet Management ───────────────────────────────────────────────────────
(package! doom-snippets
  ;; enable for debugging and make sure to symlink the repo into $DOOMDIR/snippets
  :recipe (:local-repo "snippets")
  ;; :recipe (:host github
  ;;          :repo "eduarbo/snippets"
  ;;          :files (:defaults "*"))
  ;; :pin nil
  )

;; ─── Editing Enhancements ─────────────────────────────────────────────────────
(package! drag-stuff)  ;; Move lines or text blocks up/down/left/right using shortcuts
