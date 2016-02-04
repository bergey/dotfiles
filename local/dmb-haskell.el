(add-to-list 'revert-without-query "~/.cabal/logs/*")
(add-to-list 'revert-without-query ".cabal-sandbox/logs/*")

(require 'haskell-mode-autoloads)

(use-package haskell-mode
  :mode "\\.hs\'"
  :mode "\\.lhs\'"
  :mode "\\.cabal\'"
  ;; :init (progn
  ;;         (use-package ghc
  ;;           :commands ghc-init))
  :config
  (progn

    (setq haskell-mode-hook '(
    ;;                           ;;structured-haskell-mode
                              whitespace-mode
                              flycheck-mode
    ;;                           subword-mode
                              interactive-haskell-mode ;; causes annoying project starts?
                              dmb-company-short-idle
    ;;                           smartparens-mode
                              (lambda ()
                                (setq-local helm-dash-docsets '("Haskell")))
                              haskell-auto-insert-module-template
    ;;                           ;; ghc-init  ; doesn't work with GHC-7.10?
                              ))

    (bind-keys :map haskell-mode-map
               ("M-C-h" . haskell-hoogle)
               ("C-c c" . haskell-process-cabal)
               ("C-c i" . haskell-navigate-imports))

    (setq haskell-tags-on-save t
          haskell-stylish-on-save t
          haskell-ask-also-kill-buffers nil
          haskell-process-type 'stack-ghci
    )
  ))

(use-package haskell-interactive-mode
  :commands interactive-haskell-mode
  :config
  (progn

    ;; ghci buffer
    (bind-keys :map haskell-interactive-mode-map
               ("C-c C-n" . haskell-interactive-mode-error-forward)
               ("C-c C-p" . haskell-interactive-mode-error-backward)
               ("<up>" . haskell-interactive-mode-history-previous)
               ("<down>" . haskell-interactive-mode-history-next)
               )

    (setq haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-suggest-hoogle-imports t
          haskell-process-log t
          haskell-process-auto-import-loaded-modules t)))

(use-package haskell-snippets
  :ensure t)

(provide 'dmb-haskell)
