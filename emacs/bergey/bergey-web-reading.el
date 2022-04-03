(use-package w3m :ensure t
  :commands w3m)

(defun w3m-browse-current-buffer ()
    (interactive)
    (let ((filename (concat (make-temp-file "w3m-") ".html")))
      (unwind-protect
          (progn
            (write-region (point-min) (point-max) filename)
            (w3m-find-file filename))
        (delete-file filename))))

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-variable-pitch nil) ;; use default face instead
  )

(provide 'bergey-web-reading)
