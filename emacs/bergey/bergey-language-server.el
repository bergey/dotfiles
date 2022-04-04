(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "M-l")
  )

(use-package lsp-ui :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  )

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )

(provide 'bergey-language-server)
