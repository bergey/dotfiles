;; never split vertically (into 2 windows above eachother)
(setq split-height-threshold nil)

;; fonts
(measure-time
 "gentium"

 (setq gentium (-first (-partial '-contains? '("Gentium" "GentiumPlus" "Gentium Plus")) (font-family-list)))
 (set-face-font 'default gentium)
 )

;; https://github.com/purcell/default-text-scale
(use-package default-text-scale
  ;; binds C-M-= and C-M--
  :ensure t
  )
(default-text-scale-mode)


;; colors
(measure-time "theme" (load-theme 'bergey t))

;; set this before ansi-color is loaded, and it gets picked up automaticaly
(setq ansi-color-names-vector
      '["black" "red3" "green3" "yellow3" "deep sky blue" "magenta3" "turquoise" "gray90"]
)
;; C-x C-e next line when testing changes to above
;; (setq ansi-color-map (ansi-color-make-color-map))

(use-package rainbow-mode :ensure t
  )

;; clean up interface
(measure-time "interface"
              (menu-bar-mode -1) ; hide menu bar
              (tool-bar-mode -1) ; hide toolbar (buttons)
              (fset 'yes-or-no-p 'y-or-n-p) ; reply to all prompts with one letter
              (blink-cursor-mode 0)
              (setq visible-bell 1) ; silence audio bell
              (setq use-dialog-box nil)
              (setq initial-scratch-message nil)
              (setq scroll-conservatively 101)
              )

;; windows
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

(use-package perspective
  :commands (persp-switch persp-rename)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-show-modestring nil)
  :config
  :init
  (persp-mode))

(use-package projectile :ensure t
  :commands (projectile-mode projectile-find-file)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("M-o" . projectile-command-map)
              :map projectile-command-map
              ("$ b" . projectile-switch-to-buffer-other-window)
              ("$ f" . projectile-find-file-other-window))
  :config
  (projectile-mode +1)
  )

(use-package persp-projectile :ensure t
  :after (perspective projectile)
  :defer 5
  )

(use-package buffer-move :ensure t
  :bind
    ("C-. <left>" . buf-move-left)
    ("C-. <right>" . buf-move-right)
    ("C-. m" . buf-move)
    ;; ("C-. <up>" . buf-move up)
    ;; ("C-. <down>" . buf-move-down)
    )

(setq column-number-mode t)

(setq mouse-yank-at-point t) ; middle mouse button inserts at point, not at mouse pointer

; better buffer names when >=2 open files (or dirs?) have same name
; http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)

;; Tramp needs to call `ls` and `id` but it will not find them on a
;; NixOS machine without additional configuration because they are not
;; in the usual places.
;;(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")

(autoload 'buffer-face-mode-invoke "face-remap" )

(defun monospace-mode (&optional arg)
  "Fixed-pitch default-face mode.
An interface to `buffer-face-mode' which uses the `monospace' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke 'fixed-pitch arg
                           (called-interactively-p 'interactive)))

(add-hook 'calendar-mode-hook 'monospace-mode)

;; Use system default web browser
(if (memq system-type '(gnu gnu/linux))
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")
  ;; browse-url-default-browser calls lynx even if not installed, on Debian, -- 2017-02-23
  (setq browse-url-browser-function 'browse-url-default-browser)
  )

(use-package ivy :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode)
  (bind-key "C-<return>" 'ivy-immediate-done ivy-minibuffer-map)
  (setq ivy-extra-directories '())
  (ido-everywhere -1) ;; disable
  )

;; (use-package counsel :ensure t
;;   :config
;;   (counsel-mode)
;;   (ivy-configure 'counsel-M-x :initial-input "test")
;;   )

(use-package diminish :ensure t
  :commands diminish
  )

(use-package smart-mode-line
  :ensure t
  :defer 2
  :config
  (setq sml/theme 'dark)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/mode-width 0)
  (sml/setup)
  (setq rm-blacklist "ElDoc\\|counsel\\|Projectile.*")

  (use-package smart-mode-line-powerline-theme
    :ensure t)
  )

(defun bergey/buffer-file-name-as-kill (arg)
  (interactive "p")
  (kill-new
   (cl-case arg
     ('4 (file-name-nondirectory (buffer-file-name)))
     (t (buffer-file-name)))
   ))

(provide 'bergey-gui)
