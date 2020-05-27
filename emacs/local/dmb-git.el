(setq magit-repo-dirs (list "~/code"))

(setq vc-follow-symlinks t)

(use-package git-annex :ensure t
  :commands git-annex
  :defer 20)

(use-package magit :ensure t
  :bind ("C-x g" . magit-status)
  :diminish auto-revert-mode
  :init (defvar magit-log-edit-confirm-cancellation nil)
  :config (progn
            ;; (use-package magit-annex :ensure t)
            (use-package orgit :ensure t)
            (setq magit-push-always-verify nil)
            (setq magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))

            (bind-keys :map magit-blame-mode-map
                       ;; mostly because RET, the default binding, is used by haskell-indentation
                       ("TAB" . magit-show-commit))
            )
  )

(defun search-forward-diff-lines ()
  (interactive)
  (re-search-forward "<<<<<<<\\|=======\\|>>>>>>>"))

(defun bergey/magit-copy-current-branch ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(use-package git-link
  :ensure t)

(provide 'dmb-git)
