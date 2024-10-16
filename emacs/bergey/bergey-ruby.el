;; starting point from https://github.com/amake/.emacs.d/blob/master/init.el

(use-package ruby-mode :ensure t
  :custom
  (ruby-insert-encoding-magic-comment nil "Not needed in Ruby 2")
  :config
  (setq ruby-mode-hook
        '(
          (lambda () (setq-local flymake-diagnostic-functions '(ruby-flymake-auto eglot-flymake-backend)))
          flycheck-mode
          whitespace-mode
          ))
  ;; :bind (:map ruby-mode-map ("M-." . bergey/ruby-rg-definition))
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

(use-package feature-mode :ensure t
  :mode "\\.feature"
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
  (bergey/ruby-yank-class-or-module-name t nil))
(bind-key "C-c m" #'bergey/ruby-yank-module-name ruby-mode-map)

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
(bind-key "C-c c" #'bergey/ruby-yank-class-name ruby-mode-map)

(defun bergey/ruby-yank-rspec-command ()
  "copy an rspec command which can be run in the shell to run the tests in the current file"
  (interactive)
  (kill-new (s-replace-regexp "^.*spec/" "bundle exec rspec spec/" (buffer-file-name)))
  )
(bind-key "C-c r" #'bergey/ruby-yank-rspec-command rspec-mode-map)

(defun bergey/ruby-rg-definition ()
  (interactive)
  (let ((case-fold-search nil )
        (table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      (let* ((name (thing-at-point 'word 'no-attributes))
             (is-const (s-matches? "^[A-Z_]+$" name))
             (is-class (s-matches? "^[A-Z]" name))
             (query (cond
                     (is-const (format "%s *=" name))
                     (is-class (format "(class|module) %s" name))
                     (t (format "def (self.)?%s" name)))))
        (message "%s" query)
        (rg-run query "ruby" (rg-project-root buffer-file-name) )
        ))))

(use-package rubocop :ensure t
  :config
    (add-hook 'ruby-mode-hook #'rubocop-mode)
    (setq rubocop-prefer-system-executable t)
  ;; maybe also rubocop-format-current-file ?
  :bind (:map ruby-mode-map ("C-c C-," . rubocop-autocorrect-current-file))
  )

(defun bergey/ruby-rspec-outline ()
    "occur buffer with the main headings of an rspec file"
    (interactive)
    (occur "^ *\\(context\\|\\(RSpec\\.\\)?describe\\|shared_examples\\|it_behaves_like\\)\\>")
    )
(bind-key "C-c C-o" #'bergey/ruby-rspec-outline rspec-mode-map)

(defun bergey/ruby-do-outline ()
  "occur buffer with all do block headings"
  (interactive)
  (occur "do$"))
(bind-key "C-c C-d" #'bergey/ruby-do-outline)

(use-package rbs-mode)

(provide 'bergey-ruby)
