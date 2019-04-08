(use-package python
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (setq python-mode-hook '(
                           flycheck-mode
                           whitespace-mode
                           ))
  )

(provide 'dmb-python)
