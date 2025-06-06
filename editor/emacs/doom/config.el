;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name (getenv "USER_FULL_NAME")
      user-mail-address (getenv "USER_EMAIL"))

(setq doom-leader-key ",")
(setq doom-localleader-key "s-m")

(load! "+defaults.el")
(load! "+ui.el")
(load! "+magit.el")
(load! "+modules.el")
(load! "+bindings.el")
