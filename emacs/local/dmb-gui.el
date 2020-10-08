;; never split vertically (into 2 windows above eachother)
(setq split-height-threshold nil)

;; this is more annoying than convenient when Emacs starts on the wrong monitor
;; (toggle-frame-fullscreen)

;; fonts
(measure-time
 "gentium"

 (setq gentium (-first (-partial '-contains? '("Gentium" "GentiumPlus" "Gentium Plus")) (font-family-list)))
 (set-face-font 'default gentium)


 ;; Based on http://arnab-deka.com/posts/2012/09/emacs-change-fonts-dynamically-based-on-screen-resolution/
 ;; and https://gist.github.com/MatthewDarling/8c232b1780126275c3b4
 (defun fontify-frame ()
   "set frame font & font size based on OS & display size"
   (interactive)
   (if window-system
       (set-frame-parameter (window-frame) 'font
                            (cl-case system-type
                              ('gnu/linux
                               (if (> (x-display-pixel-height) 900)
                                   (format "-unknown-%s-normal-normal-normal-*-15-*-*-*-*-0-iso10646-1" gentium)
                                 (format "-unknown-Gentium Plus-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1" gentium)
                                 ))
                              ;; ('windows-nt (set-face-font 'default "Gentium Plus"))
                              ('darwin
                               (cond
                                ((>= (x-display-mm-height) 250) "Gentium Plus-8")
                                ((>= (x-display-pixel-height) 1080) "Gentium Plus-14")
                                (t "Gentium Plus-12")
                                )))
                            )))

;;; Fontify current frame (so that it happens on startup; may be unnecessary if you use focus-in-hook)
 (fontify-frame))

;; https://github.com/purcell/default-text-scale
(use-package default-text-scale
  ;; binds C-M-= and C-M--
  :ensure t
  )
(default-text-scale-mode)


;; colors
(measure-time "theme" (load-theme 'bergey t))

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
  (use-package persp-projectile
    :ensure t)
  (bind-key "C-x x w" #'persp-switch)
  )
(persp-mode)

(use-package buffer-move :ensure t
  :bind
    ("C-. <left>" . buf-move-left)
    ("C-. <right>" . buf-move-right)
    ;; ("C-. <up>" . buf-move up)
    ;; ("C-. <down>" . buf-move-down)
    )

(use-package window-purpose :ensure t
  :config
  (setq purpose-user-mode-purposes
        '((Eshell . shell)
          (Shell . shell)
          (magit-status-mode . git)
          (magit-log-mode . git)
          (magit-diff-mode . git)
          (magit-refs-mode . git)
          (Help . help)
          (Info . help)))
  (purpose-compile-user-configuration)
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

(use-package projectile :ensure t
  :config
  (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (projectile-mode +1)
  )

;; nicer rectangle selection, without other CUA bindings
(bind-key "C-x r h" 'cua-set-register-mark)

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

(defun bergey/buffer-file-name-as-kill ()
  (interactive)
  (kill-new (buffer-file-name)))

;; mode line
;; (setq mode-line-percent-position nil)
;; (setq-default mode-line-format
;;             '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-remote
;;               mode-line-buffer-identification
;;               ;; "%f"
;;               " " evil-mode-line-tag mode-line-position
;;  (vc-mode vc-mode)
;;  "  <" mode-name "> " mode-line-misc-info mode-line-end-spaces))

(provide 'dmb-gui)
