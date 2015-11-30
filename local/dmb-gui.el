;; fonts
(set-face-font 'default (if (member "Gentium" (font-family-list)) "Gentium" "GentiumPlus"))
(add-to-list 'default-frame-alist '(font . "-unknown-Gentium-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1"))

;; colors
(load-theme 'bergey t)

;; clean up interface
(menu-bar-mode -1) ; hide menu bar
(tool-bar-mode -1) ; hide toolbar (buttons)
(fset 'yes-or-no-p 'y-or-n-p) ; reply to all prompts with one letter
(blink-cursor-mode 0)
(setq visible-bell 1) ; silence audio bell
(setq use-dialog-box nil)
(setq initial-scratch-message nil)

;; windows
(use-package window-number
  :ensure window-number
  :commands window-number-meta-mode
  :defer 3
  :config (window-number-meta-mode 1)) ; meta-# shortcuts

(use-package windresize
  :ensure windresize
  :commands windresize)

(use-package buffer-move
  :ensure t)

(setq column-number-mode 1)

;; (require 'buffer-extension)

(setq mouse-yank-at-point t) ; middle mouse button inserts at point, not at mouse pointer

; better buffer names when >=2 open files (or dirs?) have same name
; http://www.emacswiki.org/emacs/uniquify
(require 'uniquify)

(use-package helm
  :ensure helm
  :commands helm-other-buffer
  ;; :config (require 'helm-config)
  )

(use-package helm-imenu
  :bind ("C-c C-m" . helm-imenu))

(use-package ido
  :demand t
  :bind ("C-x b" . ido-switch-buffer)
  :config (ido-mode t))

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

;; use chromium as browser
;; TODO set this system-wide via XDG or something
(setq browse-url-browser-function 'browse-url-chromium)
;; (setq frame-title-format
;;       '(multiple-frames erc-modified-channels-object org-mode-line-string
;;                        ("" invocation-name "@" system-name)))
;;
;; (setq mode-line-format
;;       ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;  (vc-mode vc-mode)
;;  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(provide 'dmb-gui)
