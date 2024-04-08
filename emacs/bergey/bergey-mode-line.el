(use-package diminish :ensure t
  :commands diminish
  )

(defun bergey/diminish-vc-mode ()
  (setq mode-line-format (--remove (equal it '(vc-mode vc-mode)) mode-line-format))
  )

(use-package smart-mode-line
  :ensure t
  :defer 2
  :config
  (setq sml/theme 'dark)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/mode-width 0)
  (sml/setup)
  (setq rm-blacklist "ElDoc\\|counsel\\|Projectile.*")

  (use-package smart-mode-line-powerline-theme
    :ensure t)
  )

(provide 'bergey-mode-line)
