;;; editor/emacs/doom/+fonts.el -*- lexical-binding: t; -*-

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 12 :weight 'light))
;; (setq doom-variable-pitch-font (font-spec :family "Noto Serif"))
;; (setq doom-variable-pitch-font (font-spec :family "ETBembo" :style "RomanOSF"))

;; make sure that the font exists before using it
(let ((fn (doom-rpartial #'member (font-family-list))))
  ;; Main font
  (when-let (font (cl-find-if fn '("JetBrains Mono" "Hack")))
    (setq doom-font (font-spec :family font :size 12 :weight 'light)))

  ;; Variable pitch font
  (when-let (font (cl-find-if fn '("Noto Serif" "ETBembo")))
    (setq doom-variable-pitch-font (font-spec :family font)))

  ;; Symbols font
  (setq doom-symbol-fallback-font-families '("Fira Code" "Segoe UI Symbol" "Apple Symbols"))
  (when-let (font (cl-find-if fn doom-symbol-fallback-font-families))
    (setq doom-unicode-font (font-spec :family font)))

  ;; Emojis font 💅
  ;; NOTE I prefer Microsoft emojis but they don't have a fixed width, which breaks monospace column
  ;; alignment 😕
  (setq doom-emoji-fallback-font-families '("Segoe UI Emoji" "Apple Color Emoji" "Noto Color Emoji" "Noto Emoji"))

  ;; check this to find out the unicode blocks range:
  ;; https://github.com/rolandwalker/unicode-fonts/blob/e3942fe40b418bfb2dc4e73633e09195437fef01/unicode-fonts.el#L610
  (add-hook! 'after-setting-font-hook
    (when-let (font (cl-find-if fn doom-emoji-fallback-font-families))
      (set-fontset-font t #x2B50 font)  ;; ⭐
      (set-fontset-font t #x2B55 font)  ;; ⭕
      (set-fontset-font t '(#x231A . #x231B) font)  ;; ⌚ ⌛
      (set-fontset-font t '(#x23E9 . #x23F3) font)  ;; ⏩ ⏪ ⏫ ⏬ ⏭ ⏮ ⏯ ⏰ ⏱ ⏲ ⏳
      (set-fontset-font t '(#x23F8 . #x23FA) font)  ;; ⏸ ⏹ ⏺
      (set-fontset-font t '(#x2600 . #x26FF) font)  ;; Miscellaneous Symbols
      (set-fontset-font t '(#x2700 . #x27BF) font)  ;; Dingbats
      (set-fontset-font t '(#x1F600 . #x1F64F) font)  ;; Emoticons
      (set-fontset-font t '(#x1F300 . #x1F5FF) font)  ;; Miscellaneous Symbols and Pictographs
      (set-fontset-font t '(#x1F900 . #x1F9FF) font)  ;; Supplemental Symbols and Pictographs
      (set-fontset-font t '(#x1F680 . #x1F6FF) font)  ;; Transport and Map Symbols
      )))
