;;; *** programming languages that I use occasionally

;; (use-package modelica-mode :ensure t
;;   :mode "\.mo$"
;;   )

;; POVRay input files
(use-package pov-mode :ensure t
  :mode "\\.pov\\'"
  :config (setq pov-indent-level 4)
  )

(use-package thrift :ensure t
  :mode ("\\.thrift\\'" . thrift-mode))

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

;; rust
(use-package rust-mode :ensure t
  :mode ("\\.rs\'" . rust-mode)
  :bind (:map rust-mode-map
              ("C-c C-," . rust-format-buffer))
  :config
  (use-package flycheck-rust :ensure t
    :config
    ;; upstream doesn't cope when there's more on the line than the subcommand
    (defun flycheck-rust-cargo-has-command-p (command)
      (let ((cargo (funcall flycheck-executable-find "cargo")))
        (member command
          (mapcar
           (lambda (s) (car (s-split-words s)))
           (ignore-errors (process-lines cargo "--list"))))))
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (add-hook 'rust-mode-hook '(lambda ()  (flycheck-mode t))))

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

;; purescript
(use-package purescript-mode :ensure t
  :mode "\\.ps$"
  :config (progn
            (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
            ))

(use-package systemd :ensure t)

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

(add-to-list 'auto-mode-alist '("\\.cl" . c-mode))

(setq c-mode-hook
      '(whitespace-mode
        smartparens-mode
        flycheck-mode
        ;; helm-gtags-mode
        bergey/company-short-idle))

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

(use-package toml-mode :ensure t
  :mode "\\.toml\\|Cargo.lock" )

(use-package groovy-mode :ensure t
  :mode "\\.\\(gradle\\|groovy\\|gvy\\|gy\\|gsh\\)\\|Jenkinsfile")

;; broken 2019-09-30
;; (use-package erlang :ensure t
;;   :mode "\\.erl"
;;   )

(use-package idris-mode :ensure t
  :mode "\\.idr"
  :config
  )

(use-package fstar-mode :ensure t
  :mode "\\.fst")

(use-package merlin :ensure t
  :mode "\\.ml"
  :config
  (use-package tuareg :ensure t))

(use-package kotlin-mode :ensure t
  :ensure t)

(use-package dockerfile-mode :ensure t
  :mode "Dockerfile")

(use-package swift-mode :ensure t
  :mode "\\.swift"
  )

(use-package soutei-mode)

(use-package agda2)

(use-package just-mode :ensure t)

(use-package nix-mode :ensure t)

(provide 'bergey-languages)
