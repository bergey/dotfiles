; *** misc editing commands

(defun shell-date ()
    "Show today's date in the status bar."
  (interactive)
  (shell-command "date"))

(defun datestamp (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%d %H:%M:%S")
            (format-time-string "%Y-%m-%d"))))

(bind-keys*
 ("C-x t" . revert-buffer)
 ("C-o" . other-window)
 ;; ("C-x w" . evil-window-map)
 ("C-S-o" . vi-open-line-above)
 ;;("C-x x" . execute-extended-command)
 ;; M-; is hard to type on my keymapping; rebind to something easier
 ;; -- inspired by haskell comment char, and negative-argument is bound to several keys
 ("C--" . comment-dwim)
 ("C-x C-b" . ibuffer)
 ("C-. t" . shell-date)
 ("C-. d" . datestamp)
 ("C-. C-f" . find-file-at-point)
 ("C-. ." . helm-dash))

;; Jump to various buffers
(bind-keys* :prefix-map dmb-jump-keymap
            :prefix "C-. b"
            ("u" . erc-iswitchb)
            ("o" . helm-org-agenda-files-headings))

(defmacro bind-keys-switch-buffer (bindings &optional prefix-map)
  "Define named functions to switch to the given buffers, and
bind them to the specified keys."
  `(progn ,@(mapcar (lambda (binding)
                      (let* ((keys (car binding))
                             (buffer-name (cdr binding))
                             (function-name (intern (format "switch-to-%s-buffer" buffer-name)))
                             )
                        `(progn
                           (defun ,function-name ()
                             ,(format "switch to the buffer \"%s\"" buffer-name)
                             (interactive)
                             (switch-to-buffer ,buffer-name))
                           (bind-key ,keys ',function-name ,prefix-map)
                           )
                        ;; `(message ,buffer-name)
                        ))
                    bindings
                    )))

(bind-keys-switch-buffer
 (("b". "&bitlbee")
  ("y" . "*Ipython*")
  ("U" . "#diagrams")
  ("S" . "*scratch*"))
 dmb-jump-keymap)

(bind-keys* ("C-c C-x C-j" . org-clock-goto)
            ("C-. g" . magit-status)
            ("C-. P" . emms-pause)
            ("M-i" . ispell-word)
            ("C-. M-f" . dired-x-find-file)
            ;; after M-%, query-replace, which I use all the time
            ("M-$" . replace-regexp)
            ("M-#" . replace-string)
            ("C-. n" . (lambda () (interactive) (scroll-down 1)))
            ("C-. p" . (lambda () (interactive) (scroll-up 1)))
            ;; interferes with org-time-stamp
            ("C-. C-." . kmacro-call-macro)
            ("M-l" . company-show-location)
            ("C-. C-p" . password-store-copy)
            )

(defun window-number-select-1 () (interactive) (window-number-select 1))
(defun window-number-select-2 () (interactive) (window-number-select 2))
(defun window-number-select-3 () (interactive) (window-number-select 3))
(bind-keys*
 ("M-S-a" . backward-sentence)
 ("M-S-e" . forward-sentence)
 ("M-o" . window-number-select-1)
 ("M-e" . window-number-select-2)
 ("M-u" . window-number-select-3)
 )

;; move case-change commands under a prefix
(bind-keys* :prefix-map dmb-case-keymap
            :prefix "C-S-c"
            ("u" . upcase-word)
            ("l" . downcase-word)
            ("c" . capitalize-word)
            ("C-S-u" . upcase-region)
            ("C-S-l" . downcase-region)
            ("C-S-c" . capitalize-region))

(use-package google-this
  :ensure google-this
  :bind ("C-. w" . google-this-mode-submap)
  )

(provide 'dmb-global-bindings)
