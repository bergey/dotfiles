;;; *** programming languages that I use occasionally

;; (use-package modelica-mode
;;   :mode "\.mo$"
;;   )

;; POVRay input files
(use-package pov-mode
  :mode "\\.pov\\'"
  :config (setq pov-indent-level 4)
  )

;; vala is created and maintained by Gnome.
;; It looks like Java or C#, but compiles down to C instead of using it's own VM.
(use-package vala-mode
  :mode "\\.vala\\'")

(use-package thrift
  :mode ("\\.thrift\\'" . thrift-mode))

;; QML / Qt Quick
(use-package qml-mode
  :mode "\\.qml\'"
  :config
  (add-hook 'qml-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent))))

;; scala
(use-package scala-mode
  ;; :ensure scala-mode
  ;; :ensure sbt-mode
  ;; :ensure ensime
  :mode ("\\.scala\'" . scala-mode)
  :config (progn
            (use-package sbt-mode)
            (use-package ensime) ;; 2014-07-30 upstream broken
            (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
            (add-hook 'scala-mode-hook 'whitespace-mode)))

;; rust
(use-package rust-mode
  :mode ("\\.rs\'" . rust-mode)
  :config (add-hook 'rust-mode-hook '(lambda ()  (flycheck-mode t))))

;; Coq
(use-package proof-site
:config
  (use-package coq
    :defer t
    )
  )

;; purescript
(use-package purescript-mode
  :mode "\\.ps$"
  :config (progn
            (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
            ))

;; R stats
(use-package ess-site
  :commands R)

(use-package systemd)

(use-package yaml-mode
  :mode "\\.yaml$"
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  )

;; C language
;; custom, based on k&r
(c-add-style
 "kar"
 '("k&r"
   (c-basic-offset . 4)))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "kar")))

(setq c-mode-hook
      '(whitespace-mode
        smartparens-mode
        flycheck-mode
        ;; helm-gtags-mode
        dmb-company-short-idle))

(defun my-c-mode-insert-lcurly ()
  (interactive)
  (insert "{")
  (let ((pps (syntax-ppss)))
    (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
      (c-indent-line)
      (insert "\n\n}")
      (c-indent-line)
      (forward-line -1)
      (c-indent-line))))

(use-package toml-mode
  :mode "\\.toml" )

(use-package groovy-mode
  :mode "\\.\\(gradle\\|groovy\\|gvy\\|gy\\|gsh\\)\\|Jenkinsfile")

(use-package erlang
  :mode "\\.erl"
  )

(use-package idris-mode
  :mode "\\.idr"
  :config
  (use-package helm-idris)
  )

(use-package fstar-mode
  :mode "\\.fst")

(use-package merlin
  :mode "\\.ml"
  :config
  (use-package  tuareg))

(use-package kotlin-mode
  :ensure t)

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package swift-mode
  :mode "\\.swift"
  )

;; (use-package bazel-mode
;;   )

(provide 'dmb-languages)
