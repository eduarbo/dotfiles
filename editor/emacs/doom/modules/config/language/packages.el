;; -*- no-byte-compile: t; -*-
;;; config/language/packages.el

(package! google-translate)
(when (modulep! :lang org)
  (package! ob-translate))
