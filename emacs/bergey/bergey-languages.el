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

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package groovy-mode
 :mode (rx (or ".gradle" ".groovy" ".gvy" ".gy" ".gsh" "Jenkinsfile") string-end))

(use-package idris-mode
  :mode (rx ".idr" string-end)
  )

(use-package just-mode
  :mode "Justfile")

(use-package kotlin-mode
  :mode (rx ".kt[sm]?" string-end))

;; ocaml
(use-package merlin
  :mode (rx ".ml" string-end)
  :config
  (use-package tuareg ))

(use-package nix-mode
  :mode (rx ".nix" string-end))

;; POVRay input files
(use-package pov-mode
  :mode (rx ".pov" string-end)
  :config (setq pov-indent-level 4)
  )

;; purescript
(use-package purescript-mode
  :mode (rx ".ps" string-end)
  :config
    (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
    )

(use-package swift-mode
  :mode (rx ".swift" string-end)
  )

(use-package systemd
  :mode (rx (or ".service" ".unit") string-end))

(use-package outline-indent
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼")
  :hook ((yaml-mode ruby-mode json-mode json5-ts-mode web-mode) . outline-indent-minor-mode)
  )

(use-package yaml-mode
  :mode (rx (or ".yaml" ".yml") string-end)
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
)

(use-package coffee-mode
  :mode (rx ".js.coffee" string-end))

(use-package go-mode
  :mode (rx ".go" string-end))

(require 'alloy-mode)
(add-to-list 'auto-mode-alist `(,(rx ".alloy" string-end) . alloy-mode)) ; mode only has .als

(use-package sed-mode )

(use-package terraform-mode
  :mode (rx ".tf" string-end)
  )

(use-package nginx-mode
  :mode (rx string-start "nginx.conf" string-end)
  )

(use-package capnp-mode
  :mode (rx ".capnp" string-end)
  )

(use-package pest-mode
  :mode (rx ".pest" string-end)
  )

(use-package bsv-mode
  :mode (rx ".bsv" string-end))

(provide 'bergey-languages)
