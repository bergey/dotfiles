;; -*- lexical-binding: true; -*-
(add-to-list 'revert-without-query "~/.cabal/logs/*")
(add-to-list 'revert-without-query ".cabal-sandbox/logs/*")

(use-package haskell-mode :ensure t
  :mode "\\.hs\'"
  :mode "\\.lhs\'"
  :mode "\\.cabal\'"
  :config

  ;; better flycheck with info from cabal / hpack files
  (use-package flycheck-haskell
    :ensure t
    :config

    ;; async get-cabal-configuration
    (defun flycheck-haskell-read-and-cache-configuration (config-file)
      "Read and cache configuration from CABAL-FILE.

Return the configuration."
      ;; (message "in flycheck-haskell-read-and-cache-configuration")
      (let* ((modtime (nth 5 (file-attributes config-file)))
             (continue (lambda (config)
                         (message "in continue: %s" config)
                         (puthash config-file (cons modtime config) flycheck-haskell-config-cache)
                         (flycheck-haskell-configure) ;; this time with cached value
                         )))
        (if (equal "yaml" (file-name-extension config-file))
            (bergey/flycheck-haskell-read-hpack-configuration config-file continue)
          (funcall continue (flycheck-haskell-read-cabal-configuration config-file)))
        ))

    ;; (advice-add #'flycheck-haskell-read-and-cache-configuration
    ;;             :override #'bergey/flycheck-haskell-read-and-cache-configuration)

    (defun bergey/flycheck-haskell-read-hpack-configuration (hpack-file c2)
      "Read the hpack configuration from HPACK-FILE."
      (cl-assert flycheck-haskell-hpack-executable)
      (let ((args (list flycheck-haskell-helper
                        "--hpack-exe" flycheck-haskell-hpack-executable
                        "--hpack-file" (expand-file-name hpack-file))))
        (bergey/flycheck-haskell--read-configuration-with-helper
         (flycheck-haskell-runghc-command args) c2)))

    (defun bergey/flycheck-haskell--read-configuration-with-helper (command c3)
      (let ((continue-or-print-err
             (lambda (proc)
               (message "in continue-or-print-err")
               (pcase (process-exit-status proc)
                 (0
                  (goto-char (point-min))
                  (funcall c3 (read (process-buffer proc))))
                 (retcode
                  (message "Reading Haskell configuration failed with exit code %s and output:\n%s"
                           retcode (with-current-buffer (process-buffer proc) (buffer-string)))
                  nil) ))
             ))
        ;; (message "command=%s" command)
        (apply #'async-start-process (car command) (car command) continue-or-print-err (cdr command))
        '()))

    )

  (setq haskell-mode-hook '(
                            ;;                           ;;structured-haskell-mode
                            whitespace-mode
                            flycheck-mode
                            ;;                           subword-mode
                            ;; interactive-haskell-mode
                            dmb-company-short-idle
                            ;;                           smartparens-mode
                            ;; (lambda ()
                            ;;   (setq-local helm-dash-docsets '("Haskell")))
                            haskell-auto-insert-module-template
                            ;;                           ;; ghc-init  ; doesn't work with GHC-7.10?
                            flycheck-haskell-setup
                            ))

  (setq
   haskell-indentation-layout-offset 2
   haskell-indentation-starter-offset 2
   haskell-indentation-left-offset 2
   )

  (setq flycheck-ghc-language-extensions '( "DataKinds" "DeriveDataTypeable" "DeriveFunctor" "DeriveGeneric" "DuplicateRecordFields" "ExtendedDefaultRules" "FlexibleContexts" "FlexibleInstances" "FunctionalDependencies" "GeneralizedNewtypeDeriving" "MultiParamTypeClasses" "OverloadedStrings" "ScopedTypeVariables" "StandaloneDeriving" "TemplateHaskell" "TypeApplications" "TypeFamilies" "TypeOperators" "CPP" ))
  ;; TODO make default args different per-project
  (setq flycheck-ghc-args '("-fno-warn-type-defaults"))
  (setq haskell-font-lock-quasi-quote-modes
        (append
         '(("validSql" . sql-mode)
           ("select" . sql-mode)
           ("aesonQQ" . js-mode)
           )
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

  (mapcar (lambda (ext) (add-to-list 'haskell-ghc-supported-extensions ext)) '("BlockArguments" "DerivingStrategies"))

  (defun dmb-haskell-yank-module-name ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      ;; should accept unicode module names?
      (search-forward-regexp "^module +\\([a-zA-Z0-9.]+\\)")
      (let ((module-name (kill-new (match-string 1))))
        (if (equal module-name "Main")
            ;; special case, because :l Main does not work in ghci
            (bergey/buffer-file-name-as-kill)
          module-name))))

  (defun bergey/simformat ()
    "format buffer with https://github.com/Simspace/simformat"
    (interactive)
    (let ((old-point (point)))
      ;; save-excursion doesn't work, I'm not sure why
      (shell-command-on-region (point-min) (point-max) "simformat --editor" nil t)
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

  (setq haskell-tags-on-save nil ;; TODO async tags, at most one at once
        haskell-stylish-on-save (not (executable-find "simformat"))
        haskell-ask-also-kill-buffers nil
        haskell-process-type 'ghci
        fill-column 80
        )
  ;; (setq haskell-process-wrapper-function
  ;;     (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

  ;; (use-package lsp-haskell
  ;;   :config
  ;;   (add-hook 'haskell-mode-hook #'lsp))


  ;; redefine so that we make one TAGS file for the whole project
  (advice-add 'haskell-cabal--find-tags-dir :override
              'projectile-project-root)


   )


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
