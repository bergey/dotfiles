(use-package python
  :mode "\\.pyc?"
  :commands run-python
  :config
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  (setq python-mode-hook '(
                           whitespace-mode
                           electric-pair-local-mode
                           ;; (setq-local counsel-dash-docsets '("Python 3"))
                           ))
  :dash (python-mode "Python 3")
  )

(use-package py-isort
  :after python
  :config
  (bind-key "C-c C-," 'py-isort-buffer python-mode-map))

(provide 'bergey-python)
