;; -*- no-byte-compile: t; -*-
;;; config/language/packages.el

(package! google-translate)
(when (featurep! :lang org)
  (package! ob-translate))
