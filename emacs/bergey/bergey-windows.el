;; window management

;; never split vertically (into 2 windows above eachother)
(setq split-height-threshold nil)

(use-package window-number :ensure t
  ;; :commands window-number-meta-mode
  ;; :defer 3
  :config
  ;; These are the dvorak letters on the same keys as 1-6 on my numeric keypad layer
  (bind-key "M-m" (lambda () (interactive) (window-number-select 1)))
  (bind-key "M-w" (lambda () (interactive) (window-number-select 2)))
  (bind-key "M-v" (lambda () (interactive) (window-number-select 3)))
  (bind-key "M-h" (lambda () (interactive) (window-number-select 4)))
  (bind-key "M-t" (lambda () (interactive) (window-number-select 5)))
  (bind-key "M-n" (lambda () (interactive) (window-number-select 6)))
  )

(advice-add 'split-window-right :after #'balance-windows)
(advice-add 'split-window-below :after #'balance-windows)
;; causes problems with dired-do-flagged-delete? (advice-add 'delete-window :after #'balance-windows)
(defun bergey/delete-window-rebalance ()
  (interactive)
  (delete-window)
  (balance-windows)
  )
(bind-key "C-x 0" #'bergey/delete-window-rebalance)
(bind-keys :prefix-map bergey/window-management
           :prefix "M-r"
           ("v" . split-window-right)
           ("w" . split-window-below)
           ("m" . delete-other-windows)
           ("h" . bergey/delete-window-rebalance)
           ("g" . (lambda () (interactive) (window-number-select 7)))
           ("c" . (lambda () (interactive) (window-number-select 8)))
           ("r" . (lambda () (interactive) (window-number-select 9)))
           )

(use-package buffer-move :ensure t
  :bind
    ("C-. <left>" . buf-move-left)
    ("C-. <right>" . buf-move-right)
    ("C-. m" . buf-move)
    ;; ("C-. <up>" . buf-move up)
    ;; ("C-. <down>" . buf-move-down)
    )

(use-package perspective
  :commands (persp-switch persp-rename)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-show-modestring nil)
  :config
  :init
  (persp-mode))

(use-package projectile :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("M-o" . projectile-command-map)
              :map projectile-command-map
              ("$ b" . projectile-switch-to-buffer-other-window)
              ("$ f" . projectile-find-file-other-window))
  )

(use-package persp-projectile :ensure t
  :after (perspective projectile)
  :defer 5
  )

(provide 'bergey-windows)
