;;; -*- lexical-binding: t -*-

(use-package diminish :ensure t
  :commands diminish
  )

(defun bergey/diminish-vc-mode ()
  (setq mode-line-format (--remove (equal it '(vc-mode vc-mode)) mode-line-format))
  )

(use-package telephone-line :ensure t
  :custom
  (telephone-line-evil-use-short-tag t)
  (telephone-line-lhs
   '((nil telephone-line-evil-tag-segment)
     (accent bergey/telephone-line-position-segment)
     (nil telephone-line-projectile-buffer-segment)))
  (telephone-line-rhs
   '((nil telephone-line-flymake-segment telephone-line-misc-info-segment)
     (accent telephone-line-vc-segment telephone-line-erc-modified-channels-segment telephone-line-process-segment)
     (nil telephone-line-major-mode-segment)
     ))
  :custom-face
  (mode-line ((t (:foreground "#ea84fe" :background "gray10"))))
  (mode-line-inactive ((t (:background "gray20"))))
  (telephone-line-projectile ((t (:foreground ,b-indigo))))
  :config
  (telephone-line-mode t)

  (telephone-line-defsegment* bergey/telephone-line-position-segment (&optional lines columns)
    "Optional args set padding on lines/columns."
    (let* ((l (number-to-string (if lines lines 3)))
           (c (number-to-string (if columns columns 3))))
      (list (concat " %" l "l" ":%" c "c"))))
  )

(provide 'bergey-mode-line)
