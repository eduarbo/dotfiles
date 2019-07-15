;;; config/language/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +language/set-google-translate-languages ()
  "Set source language for google translate."
  (interactive)

  (require 'google-translate)
  (require 'google-translate-default-ui)

  (let* ((source-language (google-translate-read-source-language))
         (target-language (google-translate-read-target-language)))
    (setq google-translate-default-source-language source-language
          google-translate-default-target-language target-language
          google-translate-translation-directions-alist '((source-language . target-language)
                                                          (target-language . source-language)))))

;;;###autoload
(defun +language/google-translate-smooth-translate-any (text)
  "Set source language for google translate."
  (interactive "sTranslate: ")

  (require 'google-translate)
  (require 'google-translate-smooth-ui)

  (let* ((source-language (google-translate-read-source-language))
         (target-language (google-translate-read-target-language)))
    (google-translate-translate source-language target-language text)))
