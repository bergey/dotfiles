(use-package rust-mode :ensure t
  :mode ("\\.rs\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-," . rust-format-buffer))
  :config
  (add-hook 'rust-mode-hook '(lambda ()  (flycheck-mode t)))
  (add-hook 'rust-mode-hook #'electric-pair-local-mode))

(use-package flycheck-rust :ensure t
  :commands (flycheck-rust-setup)
  :after (rust-mode)
  :init (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  :config
  ;; upstream doesn't cope when there's more on the line than the subcommand
  (defun flycheck-rust-cargo-has-command-p (command)
    (let ((cargo (funcall flycheck-executable-find "cargo")))
      (member command
              (mapcar
               (lambda (s) (car (s-split-words s)))
               (ignore-errors (process-lines cargo "--list"))))))
  )

(use-package toml-mode :ensure t
  :mode "\\.toml\\|Cargo.lock" )

(provide 'bergey-rust)
