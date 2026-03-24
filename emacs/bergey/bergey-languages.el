;;; *** programming languages that I use occasionally

;; TODO 2022-06-08 get working again?
;; (use-package agda2)

;; Coq
(use-package proof-site
  :defer t
  :config
  (use-package coq
    :mode (rx ".v" string-end)
    :config
      (setq coq-compile-before-require t)
    )
  )

(use-package dockerfile-mode :ensure t
  :mode "Dockerfile")

(use-package groovy-mode :ensure t
 :mode (rx (or ".gradle" ".groovy" ".gvy" ".gy" ".gsh" "Jenkinsfile") string-end))

(use-package idris-mode :ensure t
  :mode (rx ".idr" string-end)
  )

(use-package just-mode :ensure t
  :mode "Justfile")

(use-package kotlin-mode :ensure t
  :mode (rx ".kt[sm]?" string-end))

;; ocaml
(use-package merlin :ensure t
  :mode (rx ".ml" string-end)
  :config
  (use-package tuareg :ensure t))

(use-package nix-mode :ensure t
  :mode (rx ".nix" string-end))

;; POVRay input files
(use-package pov-mode :ensure t
  :mode (rx ".pov" string-end)
  :config (setq pov-indent-level 4)
  )

;; purescript
(use-package purescript-mode :ensure t
  :mode (rx ".ps" string-end)
  :config
    (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
    )

(use-package swift-mode :ensure t
  :mode (rx ".swift" string-end)
  )

(use-package systemd :ensure t
  :mode (rx (or ".service" ".unit") string-end))

(use-package yaml-mode :ensure t
  :mode (rx (or ".yaml" ".yml") string-end)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (use-package outline-indent)
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
)

(use-package coffee-mode :ensure t
  :mode (rx ".js.coffee" string-end))

(use-package go-mode :ensure t
  :mode (rx ".go" string-end))

(require 'alloy-mode)
(add-to-list 'auto-mode-alist `(,(rx ".alloy" string-end) . alloy-mode)) ; mode only has .als

(use-package sed-mode :ensure t)

(use-package terraform-mode :ensure t
  :mode (rx ".tf" string-end)
  )

(use-package nginx-mode
  :mode (rx string-start "nginx.conf" string-end)
  )

(use-package capnp-mode :ensure t
  :mode (rx ".capnp" string-end)
  )

(use-package pest-mode :ensure t
  :mode (rx ".pest" string-end)
  )

(use-package bsv-mode
  :mode (rx ".bsv" string-end))

(provide 'bergey-languages)
