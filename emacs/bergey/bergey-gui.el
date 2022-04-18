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
(setq ansi-color-names-vector
      '["black" "red3" "green3" "yellow3" "deep sky blue" "magenta3" "turquoise" "gray90"]
)
(setq ansi-color-map (ansi-color-make-color-map))

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
              (setq initial-scratch-message nil))

;; windows
(use-package window-number :ensure t
  ;; :commands window-number-meta-mode
  ;; :defer 3
  :config (window-number-meta-mode 1)) ; meta-# shortcuts

(advice-add 'split-window-right :after #'balance-windows)
(advice-add 'split-window-below :after #'balance-windows)
;; causes problems with dired-do-flagged-delete? (advice-add 'delete-window :after #'balance-windows)
(defun bergey/delete-window-rebalance ()
  (interactive)
  (delete-window)
  (balance-windows)
  )
(bind-key "C-x 0" #'bergey/delete-window-rebalance)

(use-package windresize :ensure t
  :commands windresize)

(use-package perspective
  :ensure t
  :config
  (setq persp-show-modestring nil)
  (use-package persp-projectile :ensure t)
  )
(persp-mode)

(use-package projectile :ensure t
  :config
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (projectile-mode +1)
  )

(use-package buffer-move :ensure t
  :bind
    ("C-. <left>" . buf-move-left)
    ("C-. <right>" . buf-move-right)
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
   (case arg
     ('4 (file-name-nondirectory (buffer-file-name)))
     (t (buffer-file-name)))
   ))

(provide 'bergey-gui)
