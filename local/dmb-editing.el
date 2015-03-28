(require-package 'unfill)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(diminish 'visual-line-mode)
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
  :ensure company
  :diminish company-mode
  :defer 10
  :config (progn
          (setq company-show-numbers t)
          (global-company-mode)
          (setq company-idle-delay 2) ; set shorter in select modes
          (make-variable-buffer-local 'company-idle-delay)
          (defun dmb-company-short-idle () (setq company-idle-delay 0.1))
          (bind-key "M-c" 'company-manual-begin)
          (defun company-begin-dabbrev () (interactive) (company-begin-backend 'company-dabbrev))
          (bind-key "M-/" 'company-begin-dabbrev)
          (setq
           ;; if I start using completion in prose, make these buffer-local
           company-dabbrev-ignore-case nil ; in code, case matters
           company-dabbrev-downcase nil)))

(use-package csv-mode
  :ensure csv-mode
  )

(provide 'dmb-editing)
