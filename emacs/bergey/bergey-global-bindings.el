; *** misc editing commands

(defun shell-date ()
    "Show today's date in the status bar."
  (interactive)
  (shell-command "date"))

(defun datestamp (arg)
  (interactive "p")
  (insert (format-time-string
           (cl-case arg
             ('16 "%Y-%m-%dT%H:%M:%S")
             ('4 "%Y-%m-%d %H:%M:%S")
             (t "%Y-%m-%d")))))

(override-global-mode t)

(bind-keys*
 ("C-x t" . revert-buffer)
 ("C-o" . other-window)
 ;; M-; is hard to type on my keymapping; rebind to something easier
 ;; -- inspired by haskell comment char, and negative-argument is bound to several keys
 ("C--" . comment-dwim)
 ("C-x C-b" . ibuffer)
 ("C-. t" . shell-date)
 ("C-. d" . datestamp)
 ("C-. C-f" . find-file-at-point)
 )

;; Jump to various buffers
(bind-keys* :prefix-map bergey/jump-keymap
            :prefix "C-. b"
            ("u" . erc-iswitchb)
            ("o" . org-switchb))

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
 (("y" . "*Ipython*")
  ("S" . "*scratch*"))
 bergey/jump-keymap)

;; 2021-06-03 upstream password-store-copy is broken
(defun bergey/password-store-copy (entry)
  (interactive (list (password-store--completing-read t)))
  (kill-new (auth-source-pass-get 'secret entry))
  )

(defun bergey/store-or-edit-password (arg)
  (interactive "p")
  (cl-case arg
    ('4 (call-interactively 'password-store-edit))
    (t (call-interactively 'bergey/password-store-copy)))
  )

(bind-keys*
  ("C-c C-x C-j" . org-clock-goto)
  ("M-i" . ispell-word)
  ("M-I" . ispell-complete-word)
  ("C-. M-f" . dired-x-find-file)
  ;; after M-%, query-replace, which I use all the time
  ("M-$" . replace-regexp)
  ("M-#" . replace-string)
  ("M-@" . query-replace-regexp)
  ;; interferes with org-time-stamp
  ("C-. C-." . kmacro-call-macro)
  ("M-l" . company-show-location)
  ("C-. C-p" . bergey/store-or-edit-password)
  )

;; move case-change commands under a prefix
(bind-keys* :prefix-map bergey/case-keymap
            :prefix "C-S-c"
            ("u" . upcase-word)
            ("l" . downcase-word)
            ("c" . capitalize-word)
            ("C-S-u" . upcase-region)
            ("C-S-l" . downcase-region)
            ("C-S-c" . capitalize-region))
(put 'upcase-region 'disabled nil)

(defun bergey/browse-url-or-xref ()
  (interactive)
  (let* ((url (thing-at-point 'url))
        (backend (xref-find-backend))
        (id (xref-backend-identifier-at-point backend)))
  (cond
   (url (browse-url url))
   (id (xref--find-definitions id nil))
   (t (call-interactively 'xref-find-definitions)))))
(bind-key "M-." 'bergey/browse-url-or-xref)

(provide 'bergey-global-bindings)
