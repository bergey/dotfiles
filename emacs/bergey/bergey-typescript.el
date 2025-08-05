(use-package typescript-mode :ensure t
  :mode "\\.ts"
  :bind (:map typescript-mode-map
              ("C-c C-," . prettier-prettify))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 3)
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  )

(use-package add-node-modules-path
  :ensure t
  :init
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'ts-mode-hook #'add-node-modules-path)
  )

(add-hook 'web-mode-hook #'bergey/fixed-pitch-indent)

(provide 'bergey-typescript)
