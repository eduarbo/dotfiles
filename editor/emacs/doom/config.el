;;; ~/dev/dotfiles/editor/emacs/doom/config.el -*- lexical-binding: t; -*-

;;       ·▄▄▄▄              • ▌ ▄ ·.      ▄▄·        ▐ ▄ ·▄▄▄▪   ▄▄ •
;;       ██▪ ██ ▪     ▪     ·██ ▐███▪    ▐█ ▌▪▪     •█▌▐█▐▄▄·██ ▐█ ▀ ▪
;;       ▐█· ▐█▌ ▄█▀▄  ▄█▀▄ ▐█ ▌▐▌▐█·    ██ ▄▄ ▄█▀▄ ▐█▐▐▌██▪ ▐█·▄█ ▀█▄
;;       ██. ██ ▐█▌.▐▌▐█▌.▐▌██ ██▌▐█▌    ▐███▌▐█▌.▐▌██▐█▌██▌.▐█▌▐█▄▪▐█
;;       ▀▀▀▀▀•  ▀█▄▀▪ ▀█▄▀▪▀▀  █▪▀▀▀    ·▀▀▀  ▀█▄▀▪▀▀ █▪▀▀▀ ▀▀▀·▀▀▀▀


(defvar dotfiles-dir "~/dev/dotfiles")

;;; This is me
(setq user-mail-address "eduarbo@gmail.com"
      user-full-name    "Eduardo Ruiz Macias")


(load! "+defaults.el")
(load! "+bindings.el")
(load! "+lang.el")
(load! "+modules.el")
(load! "+org.el")
(load! "+org-agenda.el")
(load! "+dashboard.el")
