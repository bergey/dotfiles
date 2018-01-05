(setq magit-repo-dirs (list "~/code"))

(setq vc-follow-symlinks t)

(use-package git-annex
  :ensure git-annex
  :commands git-annex
  :defer 20)

(use-package magit
  :ensure magit
  :ensure magit-annex
  :bind ("C-x g" . magit-status)
  ;; :diminish magit-auto-revert-mode
  :init (defvar magit-log-edit-confirm-cancellation nil)
  :config (progn
            (use-package magit-annex
              :ensure t)
            (use-package orgit
              :ensure t)
            (setq magit-push-always-verify nil)
            (setq magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))
            )
  )

(defun search-forward-diff-lines ()
  (interactive)
  (re-search-forward "<<<<<<<\\|=======\\|>>>>>>>"))

(provide 'dmb-git)
