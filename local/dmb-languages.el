;;; *** programming languages that I use occasionally

;; (use-package modelica-mode
;;   :mode "\.mo$"
;;   :ensure t)

;; POVRay input files
(use-package pov-mode
  :mode "\\.pov\\'"
  :config (setq pov-indent-level 4)
  :ensure t)

;; vala is created and maintained by Gnome.
;; It looks like Java or C#, but compiles down to C instead of using it's own VM.
(use-package vala-mode
  :ensure vala-mode
  :mode "\\.vala\\'")

(use-package thrift
  :ensure thrift
  :mode ("\\.thrift\\'" . thrift-mode))

;; QML / Qt Quick
(use-package qml-mode
  :ensure qml-mode
  :mode "\\.qml\'"
  :config
  (add-hook 'qml-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent))))

;; scala
(use-package scala-mode
  :ensure scala-mode
  :ensure sbt-mode
  :ensure ensime
  :mode ("\\.scala\'" . scala-mode)
  :config (progn
            (use-package sbt-mode)
            (use-package ensime) ;; 2014-07-30 upstream broken
            (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
            (add-hook 'scala-mode-hook 'whitespace-mode)))

;; rust
(use-package rust-mode
  :ensure rust-mode
  :ensure flycheck-rust
  :mode ("\\.rs\'" . rust-mode)
  :config (add-hook 'rust-mode-hook '(lambda ()  (flycheck-mode t))))

;; Coq
;; (if (eq system-type 'gnu/linux)
;;     (progn
;;       (use-package proof
;;         ;; not in MELPA as of 2016-01-19, installed via aptitude
;;         :mode ("\\.v\'" . Coq))

;;       (load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")))

;; purescript
(use-package purescript-mode
  :ensure purescript-mode
  :mode "\\.ps$"
  :config (progn
            (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
            ))

;; R stats
(use-package ess-site
  :ensure ess
  :commands R)

(use-package systemd
  :ensure t
  )

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml$" )

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
        helm-gtags-mode
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
  :ensure t
  :mode "\\.toml" )

(use-package groovy-mode
  :ensure t
  :mode "\\.\\(gradle\\|groovy\\|gvy\\|gy\\|gsh\\)\\|Jenkinsfile")

(use-package erlang
  :mode "\\.erl"
  :ensure t)

(use-package idris-mode
  :mode "\\.idr"
  :ensure t

  :config
  (progn
    (use-package helm-idris
      :ensure t)))

(use-package fstar-mode
  :ensure t
  :mode "\\.fst")

(use-package merlin
  :ensure t)
(use-package  tuareg
  :ensure t)

(use-package kotlin-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile")

(use-package swift-mode
  :ensure t)


(provide 'dmb-languages)
