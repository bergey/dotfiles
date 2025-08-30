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
  :bind (:map racket-mode-map
              ("C-c C-," . bergey/indent-buffer))
  :config
  (add-hook 'racket-mode-hook 'bergey/lisp-hook))

;; *** clojure
(use-package clojure-mode :ensure t
  :mode "\\.clj\\'"
  :config (progn
            (add-hook 'clojure-mode-hook 'bergey/lisp-hook)
            (define-key clojure-mode-map (kbd "RET") 'newline-and-indent))
  )

(add-hook 'emacs-lisp-mode-hook 'bergey/lisp-hook)
(add-hook 'lisp-mode-hook 'bergey/lisp-hook)
(add-hook 'lisp-interaction-mode-hook 'bergey/lisp-hook)
(add-hook 'scheme-mode-hook 'bergey/lisp-hook)

(define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent)

(defun bergey/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key "C-c C-," #'bergey/indent-buffer emacs-lisp-mode-map)

(provide 'bergey-lisp)
