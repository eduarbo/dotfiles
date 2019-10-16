;;; config/language/config.el -*- lexical-binding: t; -*-

(use-package! google-translate
  :commands (+language/set-google-translate-languages
             +language/google-translate-smooth-translate-ask-language
             google-translate-at-point
             google-translate-at-point-reverse
             google-translate-query-translate
             google-translate-query-translate-reverse
             google-translate-smooth-translate
             google-translate-query-translate-using-directions)
  :init
  ;; Workaround to fix an issue where any translation attempt is failing with
  ;; error: Search failed: ",tkk:'"
  ;; See https://github.com/atykhonov/google-translate/issues/52
  (setq google-translate-backend-method 'curl)

  (setq google-translate-enable-ido-completion t
        google-translate-show-phonetic t
        google-translate-default-source-language "en"
        google-translate-default-target-language "es"
        google-translate-translation-directions-alist
        '(("en" . "es") ("es" . "en"))))

(use-package! ob-translate
  :hook org-mode)
