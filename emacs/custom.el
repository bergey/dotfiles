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
   '(".hi" ".cm/" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl"
     ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/"
     "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib"
     ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl"
     ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl"
     ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl"
     ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn"
     ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs"
     ".pyc" ".pyo"))
 '(custom-enabled-themes '(bergey))
 '(custom-safe-themes
   '("abfce0dc2f678405f10b981eb74f7cca46837a6b948c1a80efae896380373485"
     "d9703a42210b6bb68e14040a21958b24491e4ac7803c42b3f5f376cb9b2dd0bf"
     "098e47d810de9c0ec1bd7ee8d6783b9d4b27c690110d318de24c1a40b5cdc63a"
     "19c8d43a3e88c6942ef583c054f337dd594a313be6187572d35f76fa0f56d648"
     default))
 '(eval-expression-print-length nil)
 '(frame-background-mode 'dark)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("sh" ((size-gt . 4000) (size-gt . 2000)))
     ("gnus"
      ((or (mode . message-mode) (mode . mail-mode)
	   (mode . gnus-group-mode) (mode . gnus-summary-mode)
	   (mode . gnus-article-mode))))
     ("programming"
      ((or (mode . emacs-lisp-mode) (mode . cperl-mode)
	   (mode . c-mode) (mode . java-mode) (mode . idl-mode)
	   (mode . lisp-mode))))))
 '(latex-run-command "pdflatex")
 '(max-lisp-eval-depth 1000)
 '(max-mini-window-height 2)
 '(message-send-mail-partially-limit 10000000)
 '(mm-verify-option 'known)
 '(package-selected-packages
   '(add-node-modules-path auto-compile auto-complete bbdb bison-mode
			   buffer-move capnp-mode caps-lock
			   clojure-mode code-review coffee-mode
			   color-identifiers-mode counsel csv-mode
			   default-text-scale diminish dockerfile-mode
			   edit-indirect editorconfig
			   emacs-dir-treeview emmet-mode emms ensime
			   exec-path-from-shell eyebrowse feature-mode
			   fic-mode font-lock-studio fsharp-mode
			   fstar-mode git-annex git-link git-review
			   google-this groovy-mode haskell-mode
			   haskell-snippets highlight-escape-sequences
			   highlight-indent-guides highlight-quoted
			   idris-mode ivy kotlin-mode merlin move-text
			   nov org-cliplink org-evil ox-reveal
			   pest-mode powershell rbs-mode rspec-mode
			   rubocop ruby-test-mode smart-mode-line
			   smart-mode-line-powerline-theme w3m))
 '(protect-buffer-bury-p nil t)
 '(safe-local-variable-values
   '((rainbow-r-colors-alist ("b-red" . "#fe0b54")
			     ("b-orange" . "#f6bb2b")
			     ("b-green" . "#0be37a")
			     ("b-cyan" . "#13f2f8")
			     ("b-indigo" . "#5e84fe")
			     ("b-violet" . "#ea84fe")
			     ("dark-red" . "#8c0437")
			     ("dark-orange" . "#8f4a31")
			     ("dark-green" . "#246c01")
			     ("dark-blue" . "#086783")
			     ("dark-indigo" . "#475aac")
			     ("dark-violet" . "#8b418d")
			     ("light-gray" . "gray80"))
     (rainbow-r-colors-alist ("b-red" . "#fe0b54")
			     ("b-orange" . "#f6bb2b")
			     ("b-green" . "#0be37a")
			     ("b-cyan" . "#13f2f8")
			     ("b-indigo" . "#5e84fe")
			     ("b-violet" . "#ea84fe")
			     ("dark-red" . "#8c0437")
			     ("dark-orange" . "#8f4a31")
			     ("dark-green" . "#246c01")
			     ("dark-blue" . "#086783")
			     ("dark-indigo" . "#475aac")
			     ("dark-violet" . "#8b418d"))
     (rainbow-r-colors-alist ("b-red" . "#fe0b54")
			     ("b-orange" . "#f6bb2b")
			     ("b-green" . "#0be37a")
			     ("b-cyan" . "#13f2f8")
			     ("b-indigo" . "#5e84fe")
			     ("b-violet" . "#ea84fe")
			     ("dark-red" . "#8c0437")
			     ("dark-orange" . "#8f4a31")
			     ("dark-green" . "#246c01")
			     ("dark-blue" . "#086783"))
     (rainbow-r-colors-alist ("b-red" . "#fe0b54")
			     ("b-orange" . "#f6bb2b")
			     ("b-green" . "#0be37a")
			     ("b-cyan" . "#13f2f8")
			     ("b-indigo" . "#5e84fe")
			     ("b-violet" . "#ea84fe")
			     ("dark-red" . "#8c0437")
			     ("dark-orange" . "#8f4a31")
			     ("dark-blue" . "#086783"))
     (rainbow-r-colors-alist . bergey-colors)
     (bergey-colors quote
		    (("b-red" . "#fe0b54") ("b-orange" . "#f6bb2b")
		     ("b-green" . "#0be37a") ("b-cyan" . "#13f2f8")
		     ("b-indigo" . "#5e84fe") ("b-violet" . "#ea84fe")
		     ("dark-red" . "#8c0437")
		     ("dark-orange" . "#8f4a31")
		     ("dark-blue" . "#086783")))
     (rainbow-r-colors . t)
     (eglot-server-programs
      (ruby-base-mode "bundle" "exec" "solargraph" "stdio"))
     (lexical-binding . true) (haskell-process-type . cabal-new-repl)
     (haskell-process-type . ghci)
     (py-shell-local-path . "./virtualenv/bin/ipython")
     (buffer-file-coding-system . utf-8-unix)
     (eval font-lock-add-keywords nil
	   `
	   ((,(concat "("
		      (regexp-opt
		       '("sp-do-move-op" "sp-do-move-cl"
			 "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op"
			 "sp-do-del-cl")
		       t)
		      "\\_>")
	     1 'font-lock-variable-name-face)))
     (haskell-indent-spaces . 4) (haskell-process-use-ghci . t)
     (hamlet/basic-offset . 4) (python-indent . 8)))
 '(warning-suppress-types '((comp) (undo discard-info))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
