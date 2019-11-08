;; (package-initialize) ; in dmb-package instead

;; from https://github.com/bodil/ohai-emacs
;; Figure out the path to our .emacs.d by getting the path part of the
;; current file (`init.el`).
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))

(setq load-prefer-newer t)

;; load-path
(let ((default-directory dotfiles-dir))
  (normal-top-level-add-subdirs-to-load-path))
;; we want the org-mode from elpa, instead
(delete "/Applications/Emacs.app/Contents/Resources/lisp/org" load-path)

(require 'dmb-package)

(time-packages '(
                 ;; load this stuff early, so I have it even when I
                 ;; introduce bugs in the later config
                 dmb-environment
                 dmb-keyboard
                 dmb-gui
                 dmb-global-bindings
                 dmb-isearch
                 dmb-editing
                 dmb-system
                 dmb-unicode
                 ;; org mode
                 dmb-org
                 ;; programming
                 dmb-nix
                 dmb-coding
                 dmb-haskell
                 dmb-lisp
                 dmb-fsharp
                 dmb-typescript
                 dmb-python
                 dmb-languages
                 dmb-git
                 ;; mail
                 dmb-notmuch
                 dmb-bbdb
                 ;; misc
                 dmb-emacsclient
                 dmb-xml
                 dmb-markup
                 dmb-audio
                 ;;dmb-ublog
                 dmb-irc
                 dmb-shell
                 dmb-evil
                 epa-file
                 dmb-ledger
                 dmb-dired
                 dmb-sql
                 dmb-google
                 happy-mode
                 ))

(if (eq system-type 'windows-nt)
    (time-package 'dmb-windows))

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
(with-library 'dmb-passwords)

;; from https://github.com/bodil/ohai-emacs
;; Load Customize UI settings from `custom.el`.
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)
