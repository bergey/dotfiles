;; elisp libraries & variables
;; loaded after use-package but before anything else

(use-package dash )
(use-package f )
(use-package s )

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name "bash")
  ;; PATH is set for non-interactive shell, in .bash_env
  (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'bergey-environment)
