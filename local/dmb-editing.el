;; backup-on-save behavior
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 4
 kept-old-versions 2
 version-control t
 )

(use-package unfill
  :commands (unfill-paragraph unfill-region))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook (lambda() (diminish 'visual-line-mode)))
(bind-key "C-. v" 'visual-line-mode)
(bind-key "M-S-a" 'end-of-sentence) ;; like M-a, since I rebind M-e

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

(use-package whitespace
  :diminish whitespace-mode
  :config (progn
            (setq whitespace-style '(face tabs trailing empty tab-mark lines-tail))
            (setq whitespace-action '(auto-cleanup))))

;;----------------------------------------------------------------------------
;; By default, shift lines up and down with M-up and M-down. When
;; paredit is enabled, it will use those keybindings. For this reason,
;; you might prefer to use M-S-up and M-S-down, which will work even
;; in lisp modes.
;; ----------------------------------------------------------------------------
(use-package move-text
  :bind ("M-S-<up>" . move-text-up)
  :bind ("M-S-<down>" . move-text-down))

;; actually save file, on 10 sec (configurable) timer
;; I'm considering using this with OpenSCAD and Etherpad
(use-package real-auto-save
  :commands (real-auto-save turn-on-real-auto-save))

;; view mode bindings
(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map (kbd "j") 'View-scroll-line-forward)
            (define-key view-mode-map (kbd "k") 'View-scroll-line-back)))

;; alternative to open-line, bound to C-S-o in global-bindings
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

;; auto completion
(use-package company
  :diminish company-mode
  :defer 10
  :init (defun dmb-company-short-idle () (setq company-idle-delay 0.1))
  :config (progn
          (setq company-show-numbers t)
          (global-company-mode)
          (setq company-idle-delay 2) ; set shorter in select modes
          (make-variable-buffer-local 'company-idle-delay)
          (bind-key "M-c" 'company-manual-begin)
          (defun company-begin-dabbrev () (interactive) (company-begin-backend 'company-dabbrev))
          (bind-key "M-/" 'company-begin-dabbrev)
          (setq
           ;; if I start using completion in prose, make these buffer-local
           company-dabbrev-ignore-case nil ; in code, case matters
           company-dabbrev-downcase nil)))

(use-package csv-mode
  :mode "\\.csv$"
  )

(use-package rg
  :commands rg
  )

(use-package editorconfig
  :ensure t
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

(use-package caps-lock
  :bind ("C-S-c i" . caps-lock-mode))

(provide 'dmb-editing)
