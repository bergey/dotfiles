;; starting point from https://github.com/amake/.emacs.d/blob/master/init.el

(use-package ruby-mode
  :mode (rx ".rb" string-end)
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  :bind
  (:map ruby-mode-map
        ("C-c i" . bergey/ruby-navigate-imports)
        ("C-c m" . bergey/ruby-yank-module-name)
        ("C-c c" . bergey/ruby-yank-class-name)
        ("C-c C-d" . bergey/ruby-do-outline)
        )

  :config
  (setq ruby-mode-hook
        '(
          (lambda () (setq-local flymake-diagnostic-functions '(ruby-flymake-auto eglot-flymake-backend)))
          rubocop-mode
          bergey/try-ruby-mode
          ))

  (defun bergey/ruby-navigate-imports ()
    "move point to the beginning of the first import line"
    (interactive)
    (xref-push-marker-stack) ;; so we can return
    (goto-char (point-min))
    (re-search-forward "^\\(require\\|load\\)")
    (goto-char (line-beginning-position))
    )

  (defun bergey/ruby-yank-module-name ()
    "copy the module name to the kill ring"
    (interactive)
    (bergey/ruby-yank-class-or-module-name t nil))

  (defun bergey/ruby-yank-class-or-module-name (include-module include-class)
    "Find all occurrences of the regex `module [a-z]` before the first occurrence of the regex `^ *class` and concatenate the matches."
    (let ((buf (current-buffer))
          (case-fold-search nil)
          )
      (save-excursion
        (let ((search-end (point))
              (words nil)
              (regex (cond
                      ;; these differ only by keyword
                      ((and include-module include-class) "^ *\\(?:class\\|module\\) \\([a-zA-Z0-9]+\\)")
                      (include-module "^ *module \\([a-zA-Z0-9]+\\)")
                      (include-class "^ *class \\([a-zA-Z0-9]+\\)"))))
          (goto-char (point-min))
          (while (re-search-forward regex search-end t)
            (setq words (cons (match-string-no-properties 1) words)))
          (let ((ret (s-join "::"(reverse words))))
            (kill-new ret)
            (message "%s" ret )
            )))))

  (defun bergey/ruby-yank-class-name (arg)
    "Find all occurrences of the regex `module [a-z]` before the first occurrence of the regex `^ *class` and concatenate the matches."
    (interactive "P")
    (bergey/ruby-yank-class-or-module-name arg t))

  (defun bergey/ruby-do-outline ()
    "occur buffer with all do block headings"
    (interactive)
    (occur "do$"))

  (defun bergey/ruby-align-left-class ()
    "align the first class keyword to the leftmost column of window"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp " *\\(class\\)")
      (goto-char (match-beginning 1))
      (bergey/scroll-align-left))
    )

(use-package inf-ruby
  :commands inf-ruby
  :bind
  (:map inf-ruby-minor-mode-map

        ("C-c C-s" . inf-ruby-console-auto)))

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

(use-package rspec-mode
  :commands rspec-mode
  :init
  (defun bergey/try-ruby-mode ()
    (if (s-ends-with? "spec.rb" (buffer-file-name))
        (rspec-mode)))

  :bind
  (:map rspec-mode-map
        ("C-c r" . bergey/ruby-yank-rspec-command)
        ("C-c C-o" . bergey/ruby-rspec-outline)
        )

  :config
  (defun bergey/ruby-yank-rspec-command (arg)
    "copy an rspec command which can be run in the shell to run the tests in the current file"
    (interactive "p")
    (let ((prefix (if (s-match "/domains/" (buffer-file-name)) "^\\(.*\\)domains/" "^\\(.*\\)spec/")))
      (kill-new (s-replace-regexp prefix (if (= arg 4) " " "bundle exec rspec ") (buffer-file-name) nil nil 1)))
    )

  (defun bergey/ruby-rspec-outline ()
    "occur buffer with the main headings of an rspec file"
    (interactive)
    (occur "^ *\\(context\\|\\(RSpec\\.\\)?describe\\|shared_examples\\|it_behaves_like\\)\\>")
    )
  )

(use-package feature-mode
  :mode "\\.feature"
  :config
  :bind
  (:map feature-mode-map)
  ("M-." . picklebush-line)
  )

(use-package rubocop
  :commands rubocop-mode
  :custom
  (rubocop-prefer-system-executable t)
  :config
  :bind (:map ruby-mode-map ("C-c C-," . rubocop-autocorrect-current-file))
  )

(use-package rbs-mode
  :mode (rx ".rbs" string-end)
  )

(provide 'bergey-ruby)
