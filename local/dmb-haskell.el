(add-to-list 'revert-without-query "~/.cabal/logs/*")
(add-to-list 'revert-without-query ".cabal-sandbox/logs/*")

;; (require 'haskell-mode-autoloads)

(use-package haskell-mode
  :ensure t
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
                              interactive-haskell-mode
                              dmb-company-short-idle
                              ;;                           smartparens-mode
                              (lambda ()
                                (setq-local helm-dash-docsets '("Haskell")))
                              haskell-auto-insert-module-template
    ;;                           ;; ghc-init  ; doesn't work with GHC-7.10?
                              ))

    (setq haskell-indent-spaces 4)

    (defvar dmb-helm-haskell-language-extensions
      '((name . "Haskell Language Extensions")
        (candidates . haskell-ghc-supported-extensions)
        (action . dmb-haskell-insert-language-pragma)))

    (defun dmb-haskell-insert-language-pragma (extension)
      "Insert a language extension pragma at the top of the current buffer."
      ;; assume that stylish-haskell or other code will sort the
      ;; pragmas.  Just put it at the top of the file.
      (save-excursion
        (goto-char (point-min))
        (insert "{-# LANGUAGE " extension " #-}\n")))

    (defun dmb-helm-haskell-language-pragma ()
      (interactive)
      (helm-other-buffer 'dmb-helm-haskell-language-extensions "*Helm Haskell*"))

    (bind-keys :map haskell-mode-map
               ("M-C-h" . haskell-hoogle)
               ("C-c c" . haskell-process-cabal)
               ("C-c i" . haskell-navigate-imports)
               ("M-." . haskell-mode-tag-find)
               ("C-h C-l" . dmb-helm-haskell-language-pragma)
               )

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
               ("C-M-." . haskell-mode-jump-to-def-or-tag)
               )

    ;; override default
    (bind-keys :map interactive-haskell-mode-map
               ("M-." haskell-mode-tag-find)
               ("C-c C-t" . haskell-mode-show-type-at)
               )

    (setq haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-suggest-hoogle-imports t
          haskell-process-log t
          haskell-process-auto-import-loaded-modules t
          haskell-interactive-popup-errors nil
          )))

(use-package haskell-snippets
  :ensure t)

(provide 'dmb-haskell)
