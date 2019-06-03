(use-package python :ensure t
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (setq python-mode-hook '(
                           flycheck-mode
                           whitespace-mode
                           electric-pair-local-mode
                           ))
  )

(provide 'dmb-python)
