(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine 'xetex)
 '(auth-source-save-behavior nil)
 '(c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "k&r")))
 '(calendar-date-style 'iso)
 '(completion-ignored-extensions
   '(".hi" ".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"))
 '(custom-enabled-themes '(bergey))
 '(custom-safe-themes
   '("574cfa79c178975bfad537e30d5bd0b98b204eda06001d95bc8df5af18dd64f1" "6096a4b8ab56a1cf53341906daa0f5f0c497ebd1de98c9177efcae5a19833c40" "e5312690ddd83974c145d64d3908c9be7a3212f6ada6a0b750c8d0d355e67a8a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5abe7e872b2ddeeb64b23e8d62ffa4a14097e326fa824f46669601b9f22a8a04" "cc6536e3dfdf85901a4c5467ea33da0543734db3470b2b5743ccc2df448c521e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "c49ccc9b83eaa14ea6a08f2878cf399e477eb279d5514c02218747e9713d1ba0" "14e8b670cdf569d2f15a9e13b6310d82f3c351b493d2e8b9cae266fd2c224fdc" "50f1d72c0888326be5c37b21ee19065e753525b02ba6ee3a85830e8118913fba" "466e49f381b194020c0f32907a9d14b8e4eb5c69b4c2a8e77ae5e2092aded705" "978d55d7b755d314d070f5b9ff5f1ef1a6ef3d19f84613ef0e2d16759c8ad36c" "67c5740e30c3e6e683a823fd1035f4204c05688b0f9af0c3ccfc4fde8e7c2577" "388eaf6623d594170b9dde909a4bd1ffb30da38ecf286faa00c5c6d8874580bb" "ab239eac68a686a7770579b9814fc50e0f1a9b20050c35c9b559736dbaf4c21a" "ea96abae49211217138b7fba8373b1ae3db6ba781ceb96b765be66cde96edc46" default))
 '(erc-hide-list '("JOIN" "PART" "QUIT" "MODE"))
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom services stamp track truncate))
 '(eval-expression-print-length nil)
 '(frame-background-mode 'dark)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("sh"
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
        (mode . lisp-mode))))))
 '(latex-run-command "pdflatex")
 '(max-lisp-eval-depth 1000)
 '(max-mini-window-height 2)
 '(message-send-mail-partially-limit 10000000)
 '(mm-verify-option 'known)
 '(notmuch-hello-sections
   '(notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags))
 '(org-agenda-files '("~/Dropbox/org-mode/simspace.org"))
 '(org-export-backends '(ascii html icalendar latex md odt confluence))
 '(package-selected-packages
   '(native-complete lsp-haskell lsp-ui protobuf-mode add-node-modules-path lsp-mode counsel ledger-mode w3m dumb-jump default-text-scale docker-tramp flycheck-haskell nix-sandbox persp-projectile perspective smart-mode-line-powerline-theme smart-mode-line yaml-mode windresize window-purpose window-number web-mode vala-mode use-package unfill toml-mode tide thrift systemd swift-mode rust-mode rg real-auto-save rainbow-mode rainbow-delimiters qml-mode py-isort purescript-mode pov-mode poly-markdown password-store pandoc-mode origami orgit org-trello org-evil org-cliplink nov notmuch nix-mode move-text merlin kotlin-mode ivy idris-mode highlight-quoted highlight-indent-guides highlight-escape-sequences haskell-snippets haskell-mode groovy-mode google-this git-link git-annex fstar-mode fsharp-mode font-lock-studio fic-mode eyebrowse exec-path-from-shell ensime emms emmet-mode editorconfig edit-indirect dockerfile-mode diminish csv-mode color-identifiers-mode coffee-mode clojure-mode caps-lock buffer-move bison-mode bbdb auto-complete auto-compile))
 '(protect-buffer-bury-p nil t)
 '(safe-local-variable-values
   '((lexical-binding . true)
     (haskell-process-type . cabal-new-repl)
     (haskell-process-type . ghci)
     (py-shell-local-path . "./virtualenv/bin/ipython")
     (buffer-file-coding-system . utf-8-unix)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (haskell-indent-spaces . 4)
     (haskell-process-use-ghci . t)
     (hamlet/basic-offset . 4)
     (python-indent . 8)))
 '(scroll-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
