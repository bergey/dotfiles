(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'"
  :mode "\\.markdown\\'"
  :config (setq markdown-command "pandoc -f markdown -t html")
  )

; put pandoc-specific code here, rst, etc.
(use-package pandoc-mode
  :config
  (add-hook 'markdown-mode-hook 'turn-on-pandoc))

(provide 'dmb-markup)
