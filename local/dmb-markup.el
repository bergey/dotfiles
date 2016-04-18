(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'"
  :mode "\\.markdown\\'"
  :config (progn
            (setq markdown-command "pandoc -f markdown -t html")
            (bind-key "C-c C-a b" 'markdown-footnote-return markdown-mode-map)
            (bind-key "C-c C-a k" 'markdown-footnote-kill markdown-mode-map)
            )
  )

; put pandoc-specific code here, rst, etc.
(use-package pandoc-mode
  :ensure pandoc-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(provide 'dmb-markup)
