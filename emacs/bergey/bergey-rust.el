(use-package rust-mode :ensure t
  :mode ("\\.rs\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-," . rust-format-buffer))
  :config
  (use-package flycheck-rust :ensure t
    :config
    ;; upstream doesn't cope when there's more on the line than the subcommand
    (defun flycheck-rust-cargo-has-command-p (command)
      (let ((cargo (funcall flycheck-executable-find "cargo")))
        (member command
          (mapcar
           (lambda (s) (car (s-split-words s)))
           (ignore-errors (process-lines cargo "--list"))))))
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (add-hook 'rust-mode-hook '(lambda ()  (flycheck-mode t))))

(use-package toml-mode :ensure t
  :mode "\\.toml\\|Cargo.lock" )

(provide 'bergey-rust)
