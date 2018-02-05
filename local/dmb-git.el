(setq magit-repo-dirs (list "~/code"))

(setq vc-follow-symlinks t)

(use-package git-annex
  :commands git-annex
  :defer 20)

(use-package magit
  :bind ("C-x g" . magit-status)
  ;; :diminish magit-auto-revert-mode
  :init (defvar magit-log-edit-confirm-cancellation nil)
  :config (progn
            (use-package magit-annex)
            (use-package orgit)
            (setq magit-push-always-verify nil)
            (setq magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))
            )
  )

(defun search-forward-diff-lines ()
  (interactive)
  (re-search-forward "<<<<<<<\\|=======\\|>>>>>>>"))

(provide 'dmb-git)
