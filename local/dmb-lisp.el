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

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'dmb-lisp-hook))

;; *** clojure
(use-package clojure-mode
  :mode "\\.clj\\'"
  :config (progn
            (add-hook 'clojure-mode-hook 'dmb-lisp-hook)
            (add-hook 'clojure-mode-hook 'flycheck-mode)
            (define-key clojure-mode-map (kbd "RET") 'newline-and-indent))

  (use-package flycheck-clojure)

  (use-package cider
    :config (add-hook 'nrepl-mode-hook 'dmb-lisp-hook)))

(add-hook 'emacs-lisp-mode-hook 'dmb-lisp-hook)
(add-hook 'lisp-mode-hook 'dmb-lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'dmb-lisp-hook)
(add-hook 'scheme-mode-hook 'dmb-lisp-hook)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

(provide 'dmb-lisp)
