(require 'dired-x)
(require 'dired-details+)

(define-key dired-mode-map "." 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-o") 'diredp-find-a-file-other-window)
(define-key dired-mode-map (kbd "C-o") nil)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(define-key dired-mode-map "a" 'emms-add-file)
(define-key dired-mode-map "e" 'emms-add-directory)
(define-key dired-mode-map "E" 'emms-add-directory-tree)
(define-key dired-mode-map (kbd "C-c C-w") 'wdired-change-to-wdired-mode)

(provide 'dmb-dired)
