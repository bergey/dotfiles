(use-package rust-mode :ensure t
  :mode ("\\.rs\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-," . rust-format-buffer)
              ("C-c i" . bergey/rust-navigate-imports)
              ("C-c m" . bergey/rust-yank-module-name)
              )
  :config
  (add-hook 'rust-mode-hook #'electric-pair-local-mode)
  (add-hook 'rust-mode-hook #'(lambda () (setq-local evil-shift-width 4)))
  )

(use-package flymake-clippy
  :hook (rust-mode . flymake-clippy-setup-backend))

(use-package toml-mode :ensure t
  :mode "\\.toml\\|Cargo.lock" )

(defun bergey/rust-navigate-imports()
  "move point to the beginning of the first import line"
  (interactive)
  (xref-push-marker-stack) ;; so we can return
  (goto-char (point-min))
  (re-search-forward "^use")
  (goto-char (line-beginning-position))
  )

(defun bergey/rust-yank-module-name ()
  "copy the module name to the kill ring"
  (interactive)
  (let* (
         (package-root (file-name-directory (rust-buffer-project)))
         (module-path-src (s-chop-prefix package-root (buffer-file-name)))
         ;; TODO what about other top-level folders?
         (module-path (s-chop-suffix ".rs" (s-chop-prefix "src/" module-path-src)))
         (module-name (s-replace "/" "::" module-path))
         )
    (kill-new module-name))
  )

(define-skeleton rust-insert-test ""
  "name: "
  > "#[test]\n"
  > "fn " str "() {\n"
  > _ "\n"
  > "}")

(provide 'bergey-rust)
