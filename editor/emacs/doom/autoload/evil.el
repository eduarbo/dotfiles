;;; editor/emacs/doom/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :editor evil)

;;;###autoload (autoload '+evil-multi-next-line "autoload/evil" nil nil)
(evil-define-motion +evil-multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode 'magit-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+evil-multi-previous-line "autoload/evil" nil nil)
(evil-define-motion +evil-multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode 'magit-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+evil/evil-org-> "autoload/evil" nil nil)
(evil-define-command +evil/evil-org-> (count)
  "Shift the current line COUNT times to the right."
  (interactive "<c>")
  (evil-org-> (line-beginning-position) (line-beginning-position 2) count))

;;;###autoload (autoload '+evil/evil-org-< "autoload/evil" nil nil)
(evil-define-command +evil/evil-org-< (count)
  "Shift the current line COUNT times to the left."
  (interactive "<c>")
  (evil-org-< (line-beginning-position) (line-beginning-position 2) count))
