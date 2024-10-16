;; backup-on-save behavior
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 4
 kept-old-versions 2
 version-control t
 large-file-warning-threshold 1e6
 electric-pair-skip-self 'electric-pair-default-skip-self
 fill-column 90
 )

(use-package unfill :ensure t
  :commands (unfill-paragraph unfill-region))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook (lambda() (diminish 'visual-line-mode)))
(bind-key "C-. v" 'visual-line-mode)

(defun find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo:root@localhost:" ""
                       fname))
        (setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))

(use-package whitespace :ensure t
  :diminish whitespace-mode
  :config (progn
            (setq whitespace-style '(face tabs trailing empty tab-mark lines-tail))
            (setq whitespace-action '(auto-cleanup))
            (setq whitespace-line-column 120)
            ))

(use-package origami :ensure t
  :ensure t
  :config
  (bind-key "C-TAB" 'origami-recursively-toggle-node origami-mode-map)
  (bind-key "M-TAB" 'origami-recursively-toggle-node origami-mode-map)
  )

;;----------------------------------------------------------------------------
;; By default, shift lines up and down with M-up and M-down. When
;; paredit is enabled, it will use those keybindings. For this reason,
;; you might prefer to use M-S-up and M-S-down, which will work even
;; in lisp modes.
;; ----------------------------------------------------------------------------
(use-package move-text :ensure t
  ;; TODO evil bindings?
  :bind ("M-S-<up>" . move-text-up)
  :bind ("M-S-<down>" . move-text-down))

;; view mode bindings
(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map (kbd "j") 'View-scroll-line-forward)
            (define-key view-mode-map (kbd "k") 'View-scroll-line-back)))

;; auto completion
(use-package company :ensure t
  :diminish company-mode
  :defer 10
  :init (defun bergey/company-short-idle () (setq company-idle-delay 0.1))
  :config
    (setq company-show-numbers t)
    ;; (setq company-quick-access-keys '("m" "w" "v" "h" "t" "n" "g" "c" "r")) ;; cf keypad
    ;; (setq company-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
    (global-company-mode)
    (setq company-idle-delay 2) ; set shorter in select modes
    (make-variable-buffer-local 'company-idle-delay)
    (bind-key "M-c" 'company-manual-begin)
    (defun company-begin-dabbrev () (interactive) (company-begin-backend 'company-dabbrev))
    (bind-key "M-/" 'company-begin-dabbrev)
    (setq
     ;; if I start using completion in prose, make these buffer-local
     company-dabbrev-ignore-case nil ; in code, case matters
     company-dabbrev-downcase nil))

(use-package csv-mode :ensure t
  :mode "\\.csv$"
  )

(use-package rg :ensure t
  :commands rg
  :bind ("M-g M-g" . rg-dwim)
  :bind ("M-g t" . rg-project)
  :bind ("M-g M-t" . rg-ruby-not-spec)
  :bind ("M-g r" . rg)
  :bind ("M-g M-r" . rg-menu)
  :config
  (define-key rg-mode-map (kbd "M-o") 'compilation-display-error)
  (setq
   rg-custom-type-aliases '(("puppet" . "*.pp *.erb"))
   rg-builtin-type-aliases (assoc-delete-all "puppet" (rg-list-builtin-type-aliases))
   )
  (rg-define-search rg-ruby-not-spec
    "search the current project for ruby non-spec files"
    :dir project
    :files "ruby"
    :flags ("-g" "'!*_spec.rb'")
    )
  )

(use-package editorconfig :ensure t
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(global-unset-key (kbd "C-t"))

;; from https://www.reddit.com/r/emacs/comments/3vo62x/scroll_so_that_whole_paragraphs_stay_visible/
(defun scroll-up-paragraph ()
  (interactive)
  (goto-char (window-end))
  (backward-paragraph)
  (recenter 0))

(bind-key "C-c C-v" 'scroll-up-paragraph)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

(defalias 'count-lines-region 'count-words-region)

(setq hi-lock-face-defaults
      '("hi-green" "hi-blue" "hi-pink" "hi-aquamarine" "hi-salmon" "hi-yellow" ))

;; https://www.emacswiki.org/emacs/SortWords
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun bergey/scroll-align-left ()
  (interactive)
  (scroll-left (- (current-column) 1) t))

(defun snake-case ()
  (interactive)
  "convert words in region to snake_case"
  (if (use-region-p)
      (let ((end (region-end)) (case-fold-search nil))
        (goto-char (region-beginning))
        (while (re-search-forward "[A-Z]" end)
               (replace-match (format "_%s" (downcase (match-string 0))) t))
        )
    (save-excursion
      (forward-word-strictly)
      (let ((end (point)) (case-fold-search nil))
        (backward-word-strictly)
        (while (re-search-forward "[A-Z]" end)
          (replace-match (format "_%s" (downcase (match-string 0))) t)))
     )
    ))

(provide 'bergey-editing)
