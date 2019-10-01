;; New modes I want to try out, but don't want to bury in my long-term config

(use-package avy :ensure t
  )

(use-package cperl-mode :ensure t
  )

(use-package swift-mode :ensure t
  )

;; (use-package perspective :ensure t
;;   :init
;;   (setq persp-mode-prefix-key (kbd "C-x w"))
;;   :config
;;   ;;(bind-key "C-c w" 'perspective-map persp-mode-map)
;;   ;;(bind-keys* :prefix-map perspective-map :prefix "C-x w")
;;   )

(use-package company-c-headers :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(provide 'dmb-trial)
