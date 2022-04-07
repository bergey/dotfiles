(require 'dired-x)
;; (use-package dired-details :ensure t+ )

(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
(define-key dired-mode-map "." 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-o") 'dired-find-file-other-window)
(define-key dired-mode-map (kbd "C-o") nil)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(define-key dired-mode-map (kbd "C-c C-S-w") 'wdired-change-to-wdired-mode)

(provide 'bergey-dired)
