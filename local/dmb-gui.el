(defun first-member (candidates available)
  (if (null candidates) nil
    (if (member (car candidates) available) (car candidates)
      (first-member (cdr candidates) available))))

;; fonts
(measure-time
 "gentium"

 (set-face-font 'default
                (first-member
                 '("Gentium" "GentiumPlus" "Gentium Plus") (font-family-list)))

 ;; Based on http://arnab-deka.com/posts/2012/09/emacs-change-fonts-dynamically-based-on-screen-resolution/
 ;; and https://gist.github.com/MatthewDarling/8c232b1780126275c3b4
 (defun fontify-frame ()
   "set frame font & font size based on OS & display size"
   (interactive)
   (if window-system
       (cl-case system-type
         ('gnu/linux
          (let ((font-name (first-member
                            '("Gentium" "GentiumPlus" "Gentium Plus") (font-family-list))))
            (if (> (x-display-pixel-height) 1080)
                (set-frame-parameter (window-frame) 'font
                                     (format "-unknown-%s-normal-normal-normal-*-15-*-*-*-*-0-iso10646-1" font-name))
              (set-frame-parameter (window-frame) 'font
                                   (format "-unknown-Gentium Plus-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1" font-name))
              )))
         ;; ('windows-nt (set-face-font 'default "Gentium Plus"))
         ('darwin
          (if (> (x-display-pixel-height) 1080)
              (set-frame-parameter (window-frame) 'font "Gentium Plus-14")
            (set-frame-parameter (window-frame) 'font "Gentium Plus-12")
            )
          ))))

;;; Fontify current frame (so that it happens on startup; may be unnecessary if you use focus-in-hook)
 (fontify-frame)

;;; Only in Emacs 24.4 (currently available as a pretest)
; see http://emacsredux.com/blog/2014/03/22/a-peek-at-emacs-24-dot-4-focus-hooks/
(add-hook 'focus-in-hook 'fontify-frame))

;; colors
(measure-time "theme" (load-theme 'bergey t))

(use-package rainbow-mode
  :ensure t)

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
(use-package window-number
  :ensure window-number
  ;; :commands window-number-meta-mode
  ;; :defer 3
  :config (window-number-meta-mode 1)) ; meta-# shortcuts

(use-package windresize
  :ensure windresize
  :commands windresize)

(use-package buffer-move
  :ensure t)

(use-package window-purpose
  :ensure t
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

(setq column-number-mode 1)

(setq mouse-yank-at-point t) ; middle mouse button inserts at point, not at mouse pointer

; better buffer names when >=2 open files (or dirs?) have same name
; http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)

(use-package helm
  :ensure helm
  :bind (("C-x b" . helm-buffers-list))
  ;; :config (require 'helm-config)
  )

(use-package helm-imenu
  :bind ("C-c C-m" . helm-imenu))

;; (use-package ido
;;   :demand t
;;   :bind ("C-x b" . ido-switch-buffer)
;;   :config (ido-mode t))

;; Tramp needs to call `ls` and `id` but it will not find them on a
;; NixOS machine without additional configuration because they are not
;; in the usual places.
;;(add-to-list 'tramp-remote-path "/run/current-system/sw/bin")

(defun monospace-mode (&optional arg)
  "Fixed-pitch default-face mode.
An interface to `buffer-face-mode' which uses the `monospace' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode)
  (buffer-face-mode-invoke 'monospace arg
                           (called-interactively-p 'interactive)))

(add-hook 'calendar-mode-hook 'monospace-mode)

;; Use system default web browser
(if (eq system-type 'windows-nt)
    (setq browse-url-browser-function 'browse-url-default-browser)
  ;; browse-url-default-browser calls lynx even if not installed, on Debian, -- 2017-02-23
  (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open"))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config (ivy-mode))

(provide 'dmb-gui)
