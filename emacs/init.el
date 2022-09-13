;; from https://github.com/bodil/ohai-emacs
;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))
(setq load-prefer-newer t)

;; load-path
(let ((default-directory dotfiles-dir))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (concat (file-name-as-directory (getenv "HOME"))
                                "code/simspace/avaleryar/tools/emacs")) ;; soutei
;; we want the org-mode from elpa, instead
(delete "/Applications/Emacs.app/Contents/Resources/lisp/org" load-path)

(require 'bergey-package)

;; Load Customize UI settings from `custom.el`.
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(time-packages '(
                 ;; load this stuff early, so I have it even when I
                 ;; introduce bugs in the later config
                 bergey-environment
                 bergey-keyboard
                 bergey-gui
                 bergey-global-bindings
                 bergey-isearch
                 bergey-editing
                 bergey-unicode
                 bergey-evil
                 ;; org mode
                 bergey-org
                 ;; programming
                 bergey-coding
                 bergey-c
                 bergey-fsharp
                 bergey-haskell
                 bergey-lisp
                 bergey-python
                 bergey-rust
                 bergey-typescript
                 bergey-web
                 bergey-languages
                 bergey-git
                 ;; misc
                 bergey-emacsclient
                 bergey-markup
                 bergey-irc
                 bergey-shell
                 bergey-ledger
                 bergey-dired
                 bergey-sql
                 bergey-wifi
                 bergey-google
                 bergey-stats
                 ))

(measure-time "epa-file"
 (use-package epa-file
   :mode "\\.gpg"
   :commands (epa-file-enable epa-file-disable epa-file-select-keys)))

(measure-time "happy-mode"
 (use-package happy-mode
   ;; TODO distinguish happy from all other yacc-like tools
   :mode "\\.y"))

(if (eq system-type 'windows-nt)
    (time-package 'bergey-windows))

(measure-time "final stuff"
              (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura * &")
                                                   ("\\.png\\'" "feh -F * &")
                                                   ("\\.jpg\\'")))
              ;; allow some functions that emacs considers confusing to users
              (put 'dired-find-alternate-file 'disabled nil)
              (put 'narrow-to-region 'disabled nil)
              (put 'set-goal-column 'disabled nil)
              (put 'scroll-left 'disabled nil)
              (put 'downcase-region 'disabled nil))

;; kept seperate so the rest can be shared, succeed even if not present
(with-library 'bergey-passwords)
