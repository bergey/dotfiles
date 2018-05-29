(setq
 mac-command-modifier 'control
 mac-control-modifier 'super
 mac-option-modifier 'meta
 mac-pass-command-to-system nil
 )

(toggle-frame-fullscreen)

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
       (cl-case system-type
         ('gnu/linux
          (if (> (x-display-pixel-height) 900)
              (set-frame-parameter (window-frame) 'font
                                   (format "-unknown-%s-normal-normal-normal-*-15-*-*-*-*-0-iso10646-1" gentium))
            (set-frame-parameter (window-frame) 'font
                                 (format "-unknown-Gentium Plus-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1" gentium))
            ))
         ;; ('windows-nt (set-face-font 'default "Gentium Plus"))
         ('darwin
          (if (>= (x-display-pixel-height) 1080)
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
(use-package window-number
  ;; :commands window-number-meta-mode
  ;; :defer 3
  :config (window-number-meta-mode 1)) ; meta-# shortcuts

(use-package windresize
  :commands windresize)

(use-package buffer-move
  :bind          ("C-. <left>" . buf-move-left)
  ("C-. <right>" . buf-move-right)
         ;; ("C-. <up>" . buf-move up)
         ;; ("C-. <down>" . buf-move-down)
         )

(use-package window-purpose
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
(if (memq system-type '(gnu gnu/linux))
    (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open")
  ;; browse-url-default-browser calls lynx even if not installed, on Debian, -- 2017-02-23
  (setq browse-url-browser-function 'browse-url-default-browser)
  )

(use-package ivy
  :diminish ivy-mode
  :config (ivy-mode))

(use-package diminish
  :commands diminish
  )

(use-package projectile)

;; nicer rectangle selection, without other CUA bindings
(bind-key "C-x r h" 'cua-set-register-mark)

(provide 'dmb-gui)
