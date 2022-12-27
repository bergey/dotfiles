(use-package python :ensure t
  :mode "\\.pyc?"
  :commands run-python
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (setq python-mode-hook '(
                           flycheck-mode
                           whitespace-mode
                           electric-pair-local-mode
                           ))
  )

(use-package py-isort :ensure t
  :after python
  :config
  (bind-key "C-c C-," 'py-isort-buffer python-mode-map))

(provide 'bergey-python)
