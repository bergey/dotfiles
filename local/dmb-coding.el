; *** programming languages that I use occasionally

;; tags

(setq-default tab-width 4)
(setq indent-tabs-mode nil)

(use-package yasnippet
  :ensure yasnippet
  :diminish yas-minor-mode
  :config (progn
            (yas-global-mode)
            (bind-key "C-t" 'yas-expand yas-minor-mode-map)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            ))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command (format "ctags %s"  (directory-file-name dir-name))))

(use-package modelica-mode
  :mode ("\.mo$" . modelica-mode))

(add-to-list 'auto-mode-alist '("\.pde" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.ino" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.glsl" . c-mode)) ; OpengGL

;; *** C
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))

(require 'dmb-smartparens)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :commands rainbow-delimiters-mode)

;; minor modes to share in several major modes
(defun dmb-lisp-hook ()
  ;; (paredit-mode +1)
  (subword-mode +1)
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (dmb-company-short-idle)
  (show-paren-mode 1)
  (whitespace-mode)
  )

;; *** clojure
(use-package clojure-mode
  :ensure clojure-mode
  :mode "\\.clj\\'"
  :config (progn
            (add-hook 'clojure-mode-hook 'dmb-lisp-hook)
            (define-key clojure-mode-map (kbd "RET") 'newline-and-indent))

  (use-package cider
    :ensure cider
    :config (add-hook 'nrepl-mode-hook 'dmb-lisp-hook)))

;; *** elisp ***

(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(add-hook 'emacs-lisp-mode-hook 'dmb-lisp-hook)
(add-hook 'lisp-mode-hook 'dmb-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'dmb-lisp-hook)
(add-hook 'scheme-mode-hook 'dmb-lisp-hook)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

;; *** misc ***

(autoload 'tinyprocmail-mode "tinyprocmail" "" t)
(autoload 'aput "assoc")

 ;; Treat ~/.procmailrc and all pm-*.rc files as Procmail files
 (aput 'auto-mode-alist
      "\\.procmailrc\\|pm-.*\\.rc$"
      'turn-on-tinyprocmail-mode)
(put 'upcase-region 'disabled nil)

;; Maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;; ediff customization
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; don't spawn frame
(setq ediff-split-window-function 'split-window-horizontally) ; lines should align!

(setq-default indent-tabs-mode nil)

(use-package flycheck
  :ensure flycheck
  :commands flycheck-mode
  :diminish flycheck-mode
  )

;; POVRay input files
(use-package pov-mode
  :mode "\\.pov\\'"
  :config (setq pov-indent-level 4))

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
(use-package scala-mode2
  :ensure scala-mode2
  :ensure sbt-mode
  :ensure ensime
  :mode ("\\.scala\'" . scala-mode)
  :config (progn
            (require-package 'sbt-mode)
            (require-package 'ensime) ;; 2014-07-30 upstream broken
            (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
            (add-hook 'scala-mode-hook 'whitespace-mode)))

;; rust
(use-package rust-mode
  :ensure rust-mode
  :ensure flycheck-rust
  :mode ("\\.rs\'" . rust-mode)
  :config (add-hook 'rust-mode-hook '(lambda ()  (flycheck-mode t))))

;; Coq
(use-package proof
  ;; not in Elpa, installed through Nix
  :mode ("\\.v\'" . Coq))

;; purescript
(use-package purescript-mode
  :ensure purescript-mode
  :config (progn
            (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
            ))

;; documentation lookup
(use-package :helm-dash
  :ensure helm-dash
  :config (progn
            (setq helm-dash-common-docsets '("Haskell" "Bourbon" "HTML" "CSS"))
            )
  ;; also installed: arduino, bourbon, css, d3.js, haskell, html, javascript, jquery, lo-dash, opengl4, react.
  )


(provide 'dmb-coding)
