(use-package typescript-mode
  :bind (:map typescript-mode-map
              ("C-c C-," . prettier-prettify)
              ("C-c i" . bergey/typescript-navigate-imports)
              )
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (typescript-indent-level 2)

  (defun bergey/typescript-navigate-imports ()
    "move point to the beginning of the first import line"
    (interactive)
    (xref-push-marker-stack) ;; so we can return
    (goto-char (point-min))
    (re-search-forward "^import")
    (goto-char (line-beginning-position))
    )
  )

(use-package add-node-modules-path

  :init
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  )

(add-hook 'web-mode-hook #'bergey/fixed-pitch-indent)

(use-package flymake-eslint

  :hook (typescript-mode . flymake-eslint-enable)
  :config
  ;; If Emacs is compiled with JSON support
  (setq flymake-eslint-prefer-json-diagnostics t)
  )

(provide 'bergey-typescript)
