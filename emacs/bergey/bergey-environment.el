;; elisp libraries & variables
;; loaded after use-package but before anything else

(use-package f :ensure t)
(use-package dash :ensure t)
(use-package dash-functional :ensure t)
(use-package s :ensure t)

(use-package exec-path-from-shell :ensure t
  :config
 (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))

(provide 'bergey-environment)
