(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote xetex))
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "k&r"))))
 '(calendar-date-style (quote iso))
 '(completion-ignored-extensions
   (quote
    (".hi" ".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo")))
 '(custom-enabled-themes (quote (bergey)))
 '(custom-safe-themes
   (quote
    ("5abe7e872b2ddeeb64b23e8d62ffa4a14097e326fa824f46669601b9f22a8a04" "cc6536e3dfdf85901a4c5467ea33da0543734db3470b2b5743ccc2df448c521e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "c49ccc9b83eaa14ea6a08f2878cf399e477eb279d5514c02218747e9713d1ba0" "14e8b670cdf569d2f15a9e13b6310d82f3c351b493d2e8b9cae266fd2c224fdc" "50f1d72c0888326be5c37b21ee19065e753525b02ba6ee3a85830e8118913fba" "466e49f381b194020c0f32907a9d14b8e4eb5c69b4c2a8e77ae5e2092aded705" "978d55d7b755d314d070f5b9ff5f1ef1a6ef3d19f84613ef0e2d16759c8ad36c" "67c5740e30c3e6e683a823fd1035f4204c05688b0f9af0c3ccfc4fde8e7c2577" "388eaf6623d594170b9dde909a4bd1ffb30da38ecf286faa00c5c6d8874580bb" "ab239eac68a686a7770579b9814fc50e0f1a9b20050c35c9b559736dbaf4c21a" "ea96abae49211217138b7fba8373b1ae3db6ba781ceb96b765be66cde96edc46" default)))
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT" "MODE")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom services stamp track truncate)))
 '(eval-expression-print-length nil)
 '(frame-background-mode (quote dark))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   (quote
    (("sh"
      ((size-gt . 4000)
       (size-gt . 2000)))
     ("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(ido-everywhere t)
 '(latex-run-command "pdflatex")
 '(max-lisp-eval-depth 1000)
 '(max-mini-window-height 2)
 '(message-send-mail-partially-limit 10000000)
 '(mm-verify-option (quote known))
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags)))
 '(org-agenda-files (quote ("~/records/org-mode/ets.org")))
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
    (org-plus-contrib org exec-path-from-shell swift-mode window-purpose org-evil yaml-mode windresize window-number web-mode vala-mode use-package unfill toml-mode tide thrift systemd stylus-mode smartparens rust-mode real-auto-save rainbow-mode rainbow-delimiters racket-mode qml-mode python-mode purescript-mode powershell pov-mode password-store pandoc-mode orgit notmuch nodejs-repl nix-mode ng2-mode move-text maxframe markdown-mode magit-annex ledger-mode jade-mode ivy indent-guide idris-mode hl-sexp highlight-quoted highlight-parentheses highlight-indentation highlight-indent-guides highlight-escape-sequences helm-dash haskell-snippets haskell-mode groovy-mode google-this go-mode go-autocomplete git-annex fstar-mode fsharp-mode flycheck-rust fic-mode evil ess erlang ensime emms emmet-mode dired-details+ csv-mode csharp-mode color-theme-modern color-identifiers-mode coffee-mode clojure-mode buffer-move bbdb auto-compile anaconda-mode ag)))
 '(protect-buffer-bury-p nil t)
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system . utf-8-unix)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (haskell-indent-spaces . 4)
     (haskell-process-use-ghci . t)
     (hamlet/basic-offset . 4)
     (python-indent . 8))))
 '(scroll-bar-mode nil)
 '(tramp-default-method "ssh" t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
