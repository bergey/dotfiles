;; fonts
(measure-time
 "gentium"

 (setq gentium (-first (-partial '-contains? '("Gentium" "GentiumPlus" "Gentium Plus")) (font-family-list)))
 (set-face-font 'default gentium)
 )

(defun monospace-mode (&optional arg)
  "Fixed-pitch default-face mode.
An interface to `buffer-face-mode' which uses the `monospace' face.
Besides the choice of face, it is the same as `buffer-face-mode'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (buffer-face-mode-invoke 'fixed-pitch arg
                           (called-interactively-p 'interactive)))

(add-hook 'calendar-mode-hook 'monospace-mode)

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
(use-package ivy :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode)
  (bind-key "C-<return>" 'ivy-immediate-done ivy-minibuffer-map)
  (setq ivy-extra-directories '())
  (ido-everywhere -1) ;; disable
  )

(defun bergey/buffer-file-name-as-kill (arg)
  (interactive "p")
  (kill-new
   (cl-case arg
     (4 (file-name-nondirectory (buffer-file-name)))
     (t (buffer-file-name)))
   ))

(define-fringe-bitmap 'small-right-arrow
  [#x00 #x00 #x00 #x00
        #b00001000
        #b00111100
        #b00001000
        #x00
        ]
  )
(define-fringe-bitmap 'small-left-arrow
  [#x00 #x00 #x00 #x00
        #b00010000
        #b00111100
        #b00010000
        #x00
        ]
  )
(setcdr (assoc 'continuation fringe-indicator-alist) '(small-left-arrow small-right-arrow))
(setcdr (assoc 'truncation fringe-indicator-alist) '(small-left-arrow small-right-arrow))
(bind-key "C-x x f" #'follow-mode)

(provide 'bergey-gui)
