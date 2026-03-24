;; window management

;; never split vertically (into 2 windows above eachother)
(setq split-height-threshold nil)

(use-package window-number
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

(use-package buffer-move
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
         (root-window (frame-root-window (selected-frame)))
         (width
          (let ((count 1) (w (window-left-child root-window)))
            (while w (setq count (+ 1 count)) (setq w (window-next-sibling w))) count))
         (right
          (let ((right root-window) (next (window-left-child root-window)))
            (while next
              (setq right next)
              (setq next (window-next-sibling next)))
            right))
         (target-window
          (if (< width max-splits)
              (split-window
               (pcase direction
                 ('left (or (window-left-child root-window) root-window))
                 ('right right))
               nil direction)
            ;; if the leftmost / rightmost box is not a display window, pick the top window within that box
            (pcase direction
              ('left (or (window-top-child (window-left-child root-window))
                         (window-left-child root-window)
                         root-window))
              ('right (or (window-top-child right) right)))
            ))
         )
    (window--display-buffer buffer target-window 'window))
  (balance-windows))

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
        ,(bergey/mode-in-direction 'eww-mode 'right)
        ))

(use-package perspective
  :commands (persp-switch persp-rename)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-show-modestring nil)
  :config
  :init
  ;; this takes ~10s, which is excessive, especially for temp buffers in other commands
  (setq kill-buffer-query-functions (remove #'persp-maybe-kill-buffer kill-buffer-query-functions))
  )

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("M-o" . projectile-command-map)
              :map projectile-command-map
              ("$ b" . projectile-switch-to-buffer-other-window)
              ("$ f" . projectile-find-file-other-window))
  :custom
  ;; buggy nix on macos, and the error window is annoying
  (projectile-git-submodule-command nil)
  :defer 5
  :config (projectile-mode)
  )

(use-package persp-projectile
  :after (perspective projectile)
  :defer 5
  )

(provide 'bergey-windows)
