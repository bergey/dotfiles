;;; *** programming languages that I use occasionally

;; TODO 2022-06-08 get working again?
;; (use-package agda2)

;; Coq
(use-package proof-site
  :defer t
  :config
  (use-package coq
    :mode "\\.v\\'"
    :config
      (setq coq-compile-before-require t)
    )
  )

(use-package dockerfile-mode :ensure t
  :mode "Dockerfile")

(use-package groovy-mode :ensure t
  :mode "\\.\\(gradle\\|groovy\\|gvy\\|gy\\|gsh\\)\\|Jenkinsfile")

(use-package idris-mode :ensure t
  :mode "\\.idr"
  )

(use-package just-mode :ensure t
  :mode "Justfile")

(use-package kotlin-mode :ensure t
  :mode "\\.kt[sm]?")

;; ocaml
(use-package merlin :ensure t
  :mode "\\.ml"
  :config
  (use-package tuareg :ensure t))

(use-package nix-mode :ensure t
  :mode "\\.nix")

;; POVRay input files
(use-package pov-mode :ensure t
  :mode "\\.pov\\'"
  :config (setq pov-indent-level 4)
  )

;; purescript
(use-package purescript-mode :ensure t
  :mode "\\.ps$"
  :config
    (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
    )

(use-package swift-mode :ensure t
  :mode "\\.swift"
  )

(use-package systemd :ensure t
  :mode "\\.service\\|\\.unit")

(use-package yaml-mode :ensure t
  :mode "\\.yaml$\\|\\.yml$"
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (use-package outline-indent)
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
)

(use-package coffee-mode :ensure t
  :mode "\\.js.coffee")

(use-package go-mode :ensure t
  :mode "\\.go$")

(require 'alloy-mode)

(use-package sed-mode :ensure t)

(use-package terraform-mode :ensure t)

(use-package nginx-mode)

(use-package capnp-mode :ensure t)

(use-package pest-mode :ensure t)

(provide 'bergey-languages)
