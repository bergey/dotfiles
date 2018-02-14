;; elisp libraries & variables
;; loaded after use-package but before anything else

(use-package f)
(use-package dash)
(use-package dash-functional)
(use-package s)

(use-package exec-path-from-shell
  :config
 (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))

(provide 'dmb-environment)
