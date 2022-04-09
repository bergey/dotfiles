;; starting point from https://github.com/amake/.emacs.d/blob/master/init.el

(use-package ruby-mode :ensure t
  :hook ((ruby-mode . lsp-deferred)
         (ruby-mode . amk-lsp-format-on-save))
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  )

(use-package inf-ruby)

(use-package ruby-test-mode
  :after ruby-mode
  :diminish ruby-test-mode
  :config
  (defun amk-ruby-test-pretty-error-diffs (old-func &rest args)
    "Make error diffs prettier."
    (let ((exit-status (cadr args)))
      (apply old-func args)
      (when (> exit-status 0)
        (diff-mode)
        ;; Remove self
        (advice-remove #'compilation-handle-exit #'amk-ruby-test-pretty-error-diffs))))
  (defun amk-ruby-test-pretty-error-diffs-setup (old-func &rest args)
    "Set up advice to enable pretty diffs when tests fail."
    (advice-add #'compilation-handle-exit :around #'amk-ruby-test-pretty-error-diffs)
    (apply old-func args))
  (advice-add #'ruby-test-run-command :around #'amk-ruby-test-pretty-error-diffs-setup)
  )


(provide 'bergey-ruby)
