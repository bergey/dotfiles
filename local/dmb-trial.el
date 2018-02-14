;; New modes I want to try out, but don't want to bury in my long-term config

(use-package avy
  )

(use-package cperl-mode
  )

(use-package swift-mode
  )

;; (use-package perspective
;;   :init
;;   (setq persp-mode-prefix-key (kbd "C-x w"))
;;   :config
;;   ;;(bind-key "C-c w" 'perspective-map persp-mode-map)
;;   ;;(bind-keys* :prefix-map perspective-map :prefix "C-x w")
;;   )

(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

(provide 'dmb-trial)
