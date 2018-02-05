(use-package markdown-mode
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
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(use-package polymode
  :defer t)

;; TODO poly-markdown interferes with edit-indirect
;; `markdown-get-enclosing-fenced-block-construct' doesn't find the block bounds
(use-package poly-markdown
  :defer t
  :mode ("\\.md" . poly-markdown-mode))

(provide 'dmb-markup)
