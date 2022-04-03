;; minor modes to share in several major modes
(defun bergey/lisp-hook ()
  ;; (paredit-mode +1)
  (subword-mode +1)
  (smartparens-strict-mode +1)
  (bergey/company-short-idle)
  (show-paren-mode 1)
  (whitespace-mode)
  (highlight-quoted-mode)
  )

(use-package racket-mode :ensure t
  :mode "\\.rkt'"
  :config
  (add-hook 'racket-mode-hook 'bergey/lisp-hook))

;; *** clojure
(use-package clojure-mode :ensure t
  :mode "\\.clj\\'"
  :config (progn
            (add-hook 'clojure-mode-hook 'bergey/lisp-hook)
            (add-hook 'clojure-mode-hook 'flycheck-mode)
            (define-key clojure-mode-map (kbd "RET") 'newline-and-indent))

  (use-package flycheck-clojure :ensure t)

  )

(add-hook 'emacs-lisp-mode-hook 'bergey/lisp-hook)
(add-hook 'lisp-mode-hook 'bergey/lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'bergey/lisp-hook)
(add-hook 'scheme-mode-hook 'bergey/lisp-hook)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

(provide 'bergey-lisp)
