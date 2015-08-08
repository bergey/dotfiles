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
            (use-package magit-annex)
            (setq magit-push-always-verify nil))
  )

(provide 'dmb-git)
