;; -*- lexical-binding: true; -*-
(add-to-list 'revert-without-query "~/.cabal/logs/*")
(add-to-list 'revert-without-query ".cabal-sandbox/logs/*")

(use-package haskell-mode
  :mode "\\.hs\'"
  :mode "\\.lhs\'"
  :mode "\\.cabal\'"
  :custom
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-suggest-remove-import-lines t)

  :config
  (setq haskell-mode-hook '(
                            bergey/company-short-idle
                            haskell-auto-insert-module-template
                            ))

  (setq
   haskell-indentation-layout-offset 2
   haskell-indentation-starter-offset 2
   haskell-indentation-left-offset 2
   )

  ;; TODO make default args different per-project
  (setq haskell-font-lock-quasi-quote-modes
        (append
         '(("validSql" . sql-mode)
           ("select" . sql-mode)
           ("rawSql" . sql-mode)
           ("aesonQQ" . js-mode)
           )
         haskell-font-lock-quasi-quote-modes))

  (defun bergey/haskell-insert-language-pragma (extension)
    "Insert a language extension pragma at the top of the current buffer."
    ;; assume that stylish-haskell or other code will sort the
    ;; pragmas.  Just put it at the top of the file.
    (save-excursion
      (goto-char (point-min))
      (insert "{-# LANGUAGE " extension " #-}\n")))

  (defun bergey/read-haskell-language-pragma ()
    (interactive)
    (bergey/haskell-insert-language-pragma
     (completing-read
      "Extend LANGUAGE: "
      haskell-ghc-supported-extensions
      (lambda (s) (not (string-prefix-p "No" s)))
      )))

  (mapcar (lambda (ext) (add-to-list 'haskell-ghc-supported-extensions ext)) '("BlockArguments" "DerivingStrategies" "NumericUnderscores"))

  (defun bergey/haskell-yank-module-name ()
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
             ("C-h C-l" . bergey/read-haskell-language-pragma)
             ("C-c m" . bergey/haskell-yank-module-name)
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

(provide 'bergey-haskell)
