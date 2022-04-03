;;; *** programming languages that I use occasionally

(use-package agda2)

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

;; broken 2019-09-30
;; (use-package erlang :ensure t
;;   :mode "\\.erl"
;;   )

(use-package fstar-mode :ensure t
  :mode "\\.fst")

(use-package groovy-mode :ensure t
  :mode "\\.\\(gradle\\|groovy\\|gvy\\|gy\\|gsh\\)\\|Jenkinsfile")

(use-package idris-mode :ensure t
  :mode "\\.idr"
  :config
  )

(use-package just-mode :ensure t)

(use-package kotlin-mode :ensure t
  :ensure t)

;; ocaml
(use-package merlin :ensure t
  :mode "\\.ml"
  :config
  (use-package tuareg :ensure t))

(use-package nix-mode :ensure t)

;; POVRay input files
(use-package pov-mode :ensure t
  :mode "\\.pov\\'"
  :config (setq pov-indent-level 4)
  )

;; purescript
(use-package purescript-mode :ensure t
  :mode "\\.ps$"
  :config (progn
            (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
            ))

;; scala
(use-package scala-mode :ensure t
  ;; :ensure scala-mode
  ;; :ensure sbt-mode
  ;; :ensure ensime
  :mode ("\\.scala\'" . scala-mode)
  :config (progn
            (use-package sbt-mode :ensure t)
            (use-package ensime :ensure t) ;; 2014-07-30 upstream broken
            (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
            (add-hook 'scala-mode-hook 'whitespace-mode)))

(use-package soutei-mode)

(use-package swift-mode :ensure t
  :mode "\\.swift"
  )

(use-package systemd :ensure t)

(use-package thrift :ensure t
  :mode ("\\.thrift\\'" . thrift-mode))

(use-package yaml-mode :ensure t
  :mode "\\.yaml$"
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'outline-minor-mode)
  (add-hook 'yaml-mode-hook
            (lambda ()
              ;; length of regexp match sets section depth
              (set (make-local-variable 'outline-regexp) " *\\(- \\)?")
              ))
  (bind-key "TAB" 'origami-recursively-toggle-node yaml-mode-map)
  )

(provide 'bergey-languages)
