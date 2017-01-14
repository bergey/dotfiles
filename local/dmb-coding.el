; *** programming languages that I use occasionally

;; tags

(setq-default tab-width 4)
(setq indent-tabs-mode nil)

;; https://www.emacswiki.org/emacs/NavigatingParentheses#toc2
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis.
vi style of % jumping to matching brace.  If point is immediately
after a paren, and not on a paren, goto the match of the
preceding paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((save-excursion (backward-char 1) (looking-at "\\s\)"))
         (backward-list 1))
        ))
(bind-key"C-%" 'goto-match-paren)

(use-package yasnippet
  :ensure yasnippet
  :diminish yas-minor-mode
  :config (progn
            (yas-global-mode)
            (bind-key "C-t" 'yas-expand yas-minor-mode-map)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
            ))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command (format "ctags %s"  (directory-file-name dir-name))))

;; (use-package modelica-mode
;;   :mode "\.mo$"
;;   :ensure t)

(add-to-list 'auto-mode-alist '("\.pde" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.ino" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.glsl" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.frag" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.vert" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.geom" . c-mode)) ; OpengGL

;; *** C
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
            (define-key c-mode-base-map "{" 'my-c-mode-insert-lcurly)))

(time-package 'dmb-smartparens)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'column)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package highlight-quoted
  :ensure t)

(use-package highlight-escape-sequences
  :ensure t)

;; minor modes to share in several major modes
(defun dmb-lisp-hook ()
  ;; (paredit-mode +1)
  (subword-mode +1)
  (smartparens-strict-mode +1)
  (dmb-company-short-idle)
  (show-paren-mode 1)
  (whitespace-mode)
  (highlight-quoted-mode)
  )

;; *** clojure
(use-package clojure-mode
  :ensure clojure-mode
  :mode "\\.clj\\'"
  :config (progn
            (add-hook 'clojure-mode-hook 'dmb-lisp-hook)
            (add-hook 'clojure-mode-hook 'flycheck-mode)
            (define-key clojure-mode-map (kbd "RET") 'newline-and-indent))

  (use-package flycheck-clojure
    :ensure t)

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

;; for org-mode
(add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
(add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)
;; Check for org mode and existence of buffer
(defun f-ediff-org-showhide (buf command &rest cmdargs)
  "If buffer exists and is orgmode then execute command"
  (when buf
    (when (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
      (save-excursion (set-buffer buf) (apply command cmdargs)))))

(defun f-ediff-org-unfold-tree-element ()
  "Unfold tree at diff location"
  (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
  (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
  (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

(defun f-ediff-org-fold-tree ()
  "Fold tree back to top level"
  (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
  (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
  (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))
;; end for org-mode

(setq-default indent-tabs-mode nil)

(use-package flycheck
  :ensure flycheck
  :commands flycheck-mode
  :diminish flycheck-mode
  )

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
(if (eq system-type 'gnu/linux)
    (progn
      (use-package proof
        ;; not in MELPA as of 2016-01-19, installed via aptitude
        :mode ("\\.v\'" . Coq))

      (load-file "/usr/share/emacs/site-lisp/proofgeneral/generic/proof-site.el")))

;; purescript
(use-package purescript-mode
  :ensure purescript-mode
  :mode "\\.ps$"
  :config (progn
            (add-hook 'purescript-mode-hook 'purescript-indentation-mode)
            ))

;; documentation lookup
(use-package helm-dash
  :ensure t
  :config (progn
            (setq helm-dash-common-docsets '("Haskell" "Bourbon" "HTML" "CSS"))
            )
  ;; also installed: arduino, bourbon, css, d3.js, haskell, html, javascript, jquery, lo-dash, opengl4, react.
  )

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
  :mode "\\.\\(gradle\\|groovy\\|gvy\\|gy\\|gsh\\)")

(use-package erlang
  :mode "\\.erl"
  :ensure t)

(use-package fsharp-mode
  :ensure t
  :mode "\\.fsx?"
  :config
  (add-hook 'fsharp-mode-hook 'whitespace-mode))

(use-package auto-complete
  :ensure t)

(use-package paket-mode
  :mode "paket."
;;  https://github.com/mars888/paket-mode.git
  )

(use-package idris-mode
  :mode "\\.idr"
  :ensure t

  :config
  (progn
    (use-package helm-idris
      :ensure t)))

(use-package fstar-mode
  :ensure t
  :mode )

;; There are at least two modes for typescript
;; `typescript-mode' looks like the most barebones, so good for a start
;; `web-mode' may also have support
(use-package typescript-mode
  :ensure t
  :mode "\\.ts")

(provide 'dmb-coding)
