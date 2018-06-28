(use-package python
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-mode-hook '(
                           flycheck-mode
                           whitespace-mode
                           ))
  )

(provide 'dmb-python)
