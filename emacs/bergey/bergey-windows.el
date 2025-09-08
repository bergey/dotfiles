;; window management

;; never split vertically (into 2 windows above eachother)
(setq split-height-threshold nil)

(use-package window-number :ensure t
  ;; :commands window-number-meta-mode
  ;; :defer 3
  :bind
  ;; These are the dvorak letters on the same keys as 1-6 on my numeric keypad layer
  ("M-m" . (lambda () (interactive) (window-number-select 1)))
  ("M-w" . (lambda () (interactive) (window-number-select 2)))
  ("M-v" . (lambda () (interactive) (window-number-select 3)))
  ("M-h" . (lambda () (interactive) (window-number-select 4)))
  ("M-t" . (lambda () (interactive) (window-number-select 5)))
  ("M-n" . (lambda () (interactive) (window-number-select 6)))
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

(defun bergey/display-buffer-in-direction (buffer alist)
  "If there are fewer than N splits, split another.
If there are already N or more windows across this frame (horizontally) use the right / left most one.
Similar to display-buffer-in-direction but adds a window to an existing row, rather than adding an internal window"
  (let* ((direction (or (alist-get 'direction alist) 'left))
         (max-splits (or (alist-get 'max-splits alist) 6))
         (root-window-or-box (car (window-tree)))
         (target-window
          (if (one-window-p)
              (split-window root-window-or-box nil direction)
            (let (
                  (existing (cddr root-window-or-box))
                  (window-at-side ;; actually first / last visible window of first flex-box
                   (pcase direction
                     ('left (nth 2 (car (window-tree))))
                     ('right (car (last (car (window-tree))))))))
              ;; check that root box is for horizontal splits?
              (if (< (length existing) max-splits)
                  (split-window  window-at-side nil direction)
                window-at-side)))))
    (window--display-buffer buffer target-window 'window))
  (balance-windows-area))

(defun bergey/mode-in-direction (mode direction)
  `((derived-mode ,mode)
    (display-buffer-reuse-mode-window bergey/display-buffer-in-direction)
    (direction . ,direction)))

(setq display-buffer-alist
      `(
        ,(bergey/mode-in-direction 'magit-status-mode 'left)
        ,(bergey/mode-in-direction 'flymake-diagnostics-buffer-mode 'right)
        ,(bergey/mode-in-direction 'org-agenda-mode 'right)
        ,(bergey/mode-in-direction 'compilation-mode 'right)
        ,(bergey/mode-in-direction 'help-mode 'left)
        ))

(use-package perspective
  :commands (persp-switch persp-rename)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-show-modestring nil)
  :config
  :init
  (persp-mode)
  ;; this takes ~10s, which is excessive, especially for temp buffers in other commands
  (setq kill-buffer-query-functions (remove #'persp-maybe-kill-buffer kill-buffer-query-functions))
  )

(use-package projectile :ensure t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("M-o" . projectile-command-map)
              :map projectile-command-map
              ("$ b" . projectile-switch-to-buffer-other-window)
              ("$ f" . projectile-find-file-other-window))
  :custom
  ;; buggy nix on macos, and the error window is annoying
  (projectile-git-submodule-command nil)
  :init (projectile-mode)
  )

(use-package persp-projectile :ensure t
  :after (perspective projectile)
  :defer 5
  )

(provide 'bergey-windows)
