;; -*- lexical-binding: t; -*-

(use-package ess
  :mode "\\.R"
  :commands R)

(use-package julia-ts-mode
  :mode "\\.jl$"
  :custom
  (julia-ts-mode-hook '(julia-repl-mode))
  )

(use-package julia-repl
  :functions julia-repl-mode)

(provide 'bergey-stats)
