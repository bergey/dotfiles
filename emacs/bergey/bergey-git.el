(setq magit-repo-dirs (list "~/code"))

(setq vc-follow-symlinks t)

(use-package magit :ensure t
  :bind ("C-. g" . magit-status)
  :diminish auto-revert-mode
  :config
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M:%S" magit-log-margin-width t 18))
  (add-to-list 'magit-main-branch-names "develop")

  :bind
  (
   ("M-g b" . magit-blame-addition)
   ("M-g l" . magit-log-buffer-file)
   :map magit-blame-mode-map
   ;; mostly because RET, the default binding, is used by haskell-indentation
   ("TAB" . magit-show-commit)
   :map magit-mode-map
   ("M-w" . nil)
   :map magit-status-mode-map
   ("," . magit-section-show-level-2)
   ("." . magit-section-show-level-4)
   ("M-n" . nil) ;; conflicts with window switching; was magit-section-forward-sibling
   ))

(use-package orgit :ensure t :defer t)

(defun bergey/search-forward-diff-lines ()
  (interactive)
  (re-search-forward "<<<<<<<\\|=======\\|>>>>>>>"))
(bind-key "M-s d" #'bergey/search-forward-diff-lines)

(defun bergey/magit-copy-current-branch ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is not current branch"))))

(use-package git-link :ensure t
  :bind ("M-g M-l" . git-link)
  )

;; TODO move these in to use-package above, exec on load
(defun bergey/git-link-main ()
  (interactive)
  (let ((git-link-default-branch (magit-main-branch)))
    (call-interactively #'git-link)))

(bind-key "M-g M-d" #'bergey/git-link-main)

(provide 'bergey-git)
