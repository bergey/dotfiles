(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "M-l")
  )

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode
  :config
  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   gc-cons-threshold 100000000
   read-process-output-max (* 1024 1024) ;; 1mb
   )
  )

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )

(provide 'bergey-language-server)
