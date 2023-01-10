;; starting point from https://github.com/amake/.emacs.d/blob/master/init.el

(use-package ruby-mode :ensure t
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  :config
  (setq ruby-mode-hook
        '(
          flycheck-mode
          whitespace-mode
          ))
  )

(use-package inf-ruby
  :bind
  (:map inf-ruby-minor-mode-map

        ("C-c C-s" . inf-ruby-console-auto)))

(use-package ruby-test-mode :ensure t
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

(use-package rspec-mode :ensure t
  :init
  (defun bergey/try-ruby-mode ()
    (if (s-ends-with? "spec.rb" (buffer-file-name))
        (rspec-mode)))
  (add-to-list 'ruby-mode-hook #'bergey/try-ruby-mode)
  )

(defun bergey/ruby-navigate-imports ()
  "move point to the beginning of the first import line"
  (interactive)
  (xref-push-marker-stack) ;; so we can return
  (goto-char (point-min))
  (re-search-forward "^\\(require\\|load\\)")
  (goto-char (line-beginning-position))
  )
(bind-key "C-c i" #'bergey/ruby-navigate-imports ruby-mode-map)

(defun bergey/ruby-yank-module-name ()
  "copy the module name to the kill ring"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^module +\\([a-zA-Z0-9.]+\\)")
    (kill-new (match-string 1))
    (message (match-string 1))
    ))
(bind-key "C-c m" #'bergey/ruby-yank-module-name ruby-mode-map)

(defun bergey/ruby-yank-class-name  ()
  "copy the class enclosing point to the kill ring"
  (interactive)
  (save-excursion
    (re-search-backward "^ *class \\([a-zA-Z]+\\)")
    (kill-new (match-string 1))
    (message (match-string 1))
    ) )
(bind-key "C-c c" #'bergey/ruby-yank-class-name ruby-mode-map)

(provide 'bergey-ruby)
