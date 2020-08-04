(add-to-list 'revert-without-query "~/.cabal/logs/*")
(add-to-list 'revert-without-query ".cabal-sandbox/logs/*")

(use-package haskell-mode :ensure t
  :mode "\\.hs\'"
  :mode "\\.lhs\'"
  :mode "\\.cabal\'"
  :config
  (progn

    ;; better flycheck with info from cabal / hpack files
    (use-package flycheck-haskell
      :ensure t)

    (setq haskell-mode-hook '(
    ;;                           ;;structured-haskell-mode
                              whitespace-mode
                              flycheck-mode
    ;;                           subword-mode
                              interactive-haskell-mode
                              dmb-company-short-idle
                              ;;                           smartparens-mode
                              ;; (lambda ()
                              ;;   (setq-local helm-dash-docsets '("Haskell")))
                              haskell-auto-insert-module-template
    ;;                           ;; ghc-init  ; doesn't work with GHC-7.10?
                              flycheck-haskell-setup
                              ))

    (setq
     haskell-indentation-layout-offset 4
     haskell-indentation-starter-offset 4
     haskell-indentation-left-offset 4
     )

    (setq flycheck-ghc-language-extensions '( "DataKinds" "DeriveDataTypeable" "DeriveFunctor" "DeriveGeneric" "DuplicateRecordFields" "ExtendedDefaultRules" "FlexibleContexts" "FlexibleInstances" "FunctionalDependencies" "GeneralizedNewtypeDeriving" "MultiParamTypeClasses" "OverloadedStrings" "ScopedTypeVariables" "StandaloneDeriving" "TemplateHaskell" "TypeApplications" "TypeFamilies" "TypeOperators" "CPP" ))
    ;; TODO make default args different per-project
    (setq flycheck-ghc-args '("-fno-warn-type-defaults"))
    (setq haskell-font-lock-quasi-quote-modes
          (append
           '(("aritySql" . sql-mode)
             '("qq" . sql-mode))
           haskell-font-lock-quasi-quote-modes))

    (defun dmb-haskell-insert-language-pragma (extension)
      "Insert a language extension pragma at the top of the current buffer."
      ;; assume that stylish-haskell or other code will sort the
      ;; pragmas.  Just put it at the top of the file.
      (save-excursion
        (goto-char (point-min))
        (insert "{-# LANGUAGE " extension " #-}\n")))

    (defun dmb-ivy-haskell-language-pragma ()
      (interactive)
      (ivy-read "Extend LANGUAGE: " haskell-ghc-supported-extensions
                :predicate (lambda (s) (not (string-prefix-p "No" s)))
                :action #'dmb-haskell-insert-language-pragma
                ))

    (defun dmb-haskell-yank-module-name ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        ;; should accept unicode module names?
        (search-forward-regexp "^module +\\([a-zA-Z0-9.]+\\)")
        (kill-new (match-string 1))))

    (defun bergey/simformat ()
      "format buffer with https://github.com/Simspace/simformat"
      (interactive)
      (let ((old-point (point)))
        ;; save-excursion doesn't work, I'm not sure why
        (shell-command-on-region (point-min) (point-max) "simformat" nil t)
        (goto-char old-point)))

    (defun bergey/haskell-no-warn-orphans ()
      "insert pragma to silence warning about orphan instances"
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (insert "{-# OPTIONS_GHC -fno-warn-orphans #-}\n")
        )
      )

    (defun bergey/haskell-hoogle-or-simspace (&optional flag)
      (interactive "P")
      (if flag (let ((haskell-hoogle-url "https://hoogle.simspace.com/?hoogle=%s"))
                 (call-interactively #'haskell-hoogle))
        (call-interactively #'haskell-hoogle))
      )

    (bind-keys :map haskell-mode-map
               ("M-h" . bergey/haskell-hoogle-or-simspace)
               ("C-c c" . haskell-process-cabal)
               ("C-c i" . haskell-navigate-imports)
               ("M-." . haskell-mode-tag-find)
               ("C-h C-l" . dmb-ivy-haskell-language-pragma)
               ("C-c m" . dmb-haskell-yank-module-name)
               )

    (if (executable-find "simformat")
        (bind-keys :map haskell-mode-map ("C-c C-," . bergey/simformat)))

    (add-to-list 'auto-mode-alist '("cabal.project" . haskell-cabal-mode))

    (setq haskell-tags-on-save t
          haskell-stylish-on-save t
          haskell-ask-also-kill-buffers nil
          haskell-process-type 'ghci
    )
    ;; (setq haskell-process-wrapper-function
    ;;     (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

    ;; (use-package lsp-haskell
    ;;   :config
    ;;   (add-hook 'haskell-mode-hook #'lsp))


    ;; redefine so that we make one TAGS file for the whole project
    (advice-add 'haskell-cabal--find-tags-dir :override
                'projectile-project-root)
  ))

(use-package haskell-interactive-mode
  :commands interactive-haskell-mode
  :diminish 'interactive-haskell-mode
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

(use-package haskell-snippets :ensure t)

(provide 'dmb-haskell)
