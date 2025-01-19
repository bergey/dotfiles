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
   '("79086142afd41373ab832301602de492ba858605178caad2b40cdfe6a2e5d388" default))
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
 '(org-export-backends '(ascii html icalendar latex md odt confluence))
 '(package-selected-packages
   '(rbs-mode pest-mode capnp-mode emacs-dir-treeview rubocop feature-mode ox-reveal ruby-test-mode rspec-mode add-node-modules-path counsel w3m default-text-scale smart-mode-line-powerline-theme smart-mode-line org-evil org-cliplink nov move-text merlin kotlin-mode ivy idris-mode highlight-quoted highlight-indent-guides highlight-escape-sequences haskell-snippets haskell-mode groovy-mode google-this git-link git-annex fstar-mode fsharp-mode font-lock-studio fic-mode eyebrowse exec-path-from-shell ensime emms emmet-mode editorconfig edit-indirect dockerfile-mode diminish csv-mode color-identifiers-mode coffee-mode clojure-mode caps-lock buffer-move bison-mode bbdb auto-complete auto-compile))
 '(protect-buffer-bury-p nil t)
 '(safe-local-variable-values
   '((eglot-server-programs
      (ruby-base-mode "bundle" "exec" "solargraph" "stdio"))
     (lexical-binding . true)
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
 '(warning-suppress-types '((comp) (undo discard-info))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
