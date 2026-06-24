; *** misc editing commands

(override-global-mode t)

(bind-keys*
 ("C-x t" . revert-buffer)
 ("C-x s" . save-all-buffers)
 ("C-o" . other-window)
 ;; M-; is hard to type on my keymapping; rebind to something easier
 ;; -- inspired by haskell comment char, and negative-argument is bound to several keys
 ("C--" . comment-dwim)
 ("C-x C-b" . ibuffer)
 ("C-c C-x C-j" . org-clock-goto)
 ("M-i" . ispell-word)
 ("M-I" . ispell-complete-word)
 ;; after M-%, query-replace, which I use all the time
 ("M-$" . replace-regexp)
 ("M-#" . replace-string)
 ("M-@" . query-replace-regexp)
 ("M-l" . company-show-location)
 )

(bind-keys*
 :prefix "C-."
 :prefix-map bergey/global-keymap
 ("t" . shell-date)
 ("d" . datestamp)
 ("C-f" . find-file-at-point)
 ("M-f" . dired-x-find-file)
 ;; interferes with org-time-stamp
 ("C-." . kmacro-call-macro)
 (" " . (lambda () (insert-char " ")))
 ("-" . comment-dwim)
 ("s" . bergey/goto-shell-prompt)
 )

(evil-define-key '(normal visual) override-global-map " " bergey/global-keymap)

;; Jump to various buffers
(bind-keys*
 :map bergey/global-keymap
 :prefix "b"
 :prefix-map bergey/jump-keymap
 ("o" . org-switchb)
 ("g" . bergey/switch-buffer-magit)
 ("p" . previous-buffer)
 ("b" . ivy-switch-buffer)
 )

(cl-loop for binding in
         '(
           ("y" . "*Ipython*")
           ("S" . "*scratch*"))
         do
         (let* ((keys (car binding))
                (buffer-name (cdr binding))
                (function-name (intern (format "switch-to-%s-buffer" buffer-name)))
                )
           (eval
            `(defun ,function-name ()
               ,(format "switch to the buffer \"%s\"" buffer-name)
               (interactive)
               (switch-to-buffer ,buffer-name))
            )
           (eval `(bind-key keys function-name bergey/jump-keymap))
           ))

(defun shell-date ()
  "Show today's date in the status bar."
  (interactive)
  (shell-command "date"))

(defun iso-time-formats (arg)
  (cl-case arg
    ;; could use %F %T to simplify
    (16 "%Y-%m-%dT%H:%M:%SZ")
    (4 "%Y-%m-%d %H:%M:%S%z")
    (t "%Y-%m-%d")))

(defun datestamp (arg)
  (interactive "p")
  (insert (format-time-string
           (iso-time-formats arg) nil (eq arg 16))))

(defun iso-time-at-point (arg)
  (interactive "p")
  (message
   (format-time-string (iso-time-formats arg) (thing-at-point 'number 'no-attributes) (eq arg 16)))
  )

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(defun bergey/switch-buffer-regex (regex)
  (switch-to-buffer
   (completing-read
    "switch to buffer: "
    #'internal-complete-buffer
    (lambda (s) (s-matches? regex (car s))))))

(defun bergey/switch-buffer-magit ()
  (interactive)
  (bergey/switch-buffer-regex "^magit:"))

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

(use-package helpful
  :bind ("C-h k" . helpful-key))

(provide 'bergey-global-bindings)
