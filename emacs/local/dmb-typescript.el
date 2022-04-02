(use-package typescript-mode :ensure t
  :mode "\\.ts"
  :config

(use-package tide :ensure t
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    ;; (tide-hl-identifier-mode +1)
    )
  :hook (before-save . tide-format-before-save))
  :bind (:map tide-mode-map
              ("C-c C-," . tide-format))
  )

  (defun typescript-sort-imports ()
  "sort the current region according to typescript import rules"
  (interactive)
  ;; TODO handle all import groups, regardless of position in buffer
  (sort-regexp-fields nil "^.*$" "\".*\"" (point) (mark)))

  (setq typescript-mode-hook
        '(whitespace-mode
          smartparens-strict-mode
          dmb-company-short-idle
          setup-tide-mode
          ))
  )

(use-package add-node-modules-path
  :ensure t
  :init
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'ts-mode-hook #'add-node-modules-path)
  )

(add-hook 'web-mode-hook #'bergey/fixed-pitch-indent)

(provide 'dmb-typescript)
