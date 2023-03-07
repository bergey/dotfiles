;; everything web: HTML,javascript, css
(use-package web-mode :ensure t
  :mode "\\.\\(p?html\\|tsx?\\|tpl\\|php\\|erb\\|mustach\\|e?jsx?\\|json\\|s?css\\|sass\\|fsproj\\|csproj\\|xml\\|tmpl\\)\\'"
  :config
  (progn
    (define-key web-mode-map (kbd "C-c C-l") 'w3m-browse-current-buffer)
    (setq web-mode-hook '(
                          bergey/yas-by-file-extension
                          bergey/auto-quote-by-file-extension
                          bergey/prettier-parsers-by-file-extension
                          color-identifiers-mode
                          bergey/company-short-idle
                          ;; emmet-mode
                          bergey/configure-web-mode-flycheck-checkers
                          ;; bergey/lsp-for-typescript
                          prettier-mode
                          whitespace-mode
                          ))

    (bind-keys*
     :map web-mode-map
     ("C-j" . nil)
     ("RET" . newline-and-indent)
     ("C-c C-r" . run-mocha)
     ("C--" . web-mode-comment-or-uncomment)
     ("C-c C-m" . nil) ;; masks imenu
     ("C-c C-," . prettier-prettify))
    (setq web-mode-code-indent-offset 2)

    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

(defmacro string-case (actual &rest branches)
  `(pcase ,actual ,@(mapcar (lambda (b) `((pred (string-equal ,(car b))) ,(cadr b))) branches))
  )

(use-package prettier :ensure t
  :commands prettier-mode
  :config
  ;; better would be to show in the usual buffer, but leave that buffer off-screen with display-buffer-alist
  (defun prettier--show-error (string &rest objects)
    (message "prettier: %s" (car (s-lines (apply #'format string objects)))))
  )

(use-package emmet-mode :ensure t
  :commands emmet-mode
  )

(use-package color-identifiers-mode :ensure t
  :commands color-identifiers-mode
  :config
  (push
   '(web-mode
     "</?!?"
     "\\_</?!?\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
     (nil web-mode-html-tag-face))
   color-identifiers:modes-alist)
  )

;; https://github.com/emacsmirror/nodejs-mode
(use-package nodejs-repl :ensure t
  :commands nodejs-repl
  )

(defun bergey/yas-by-file-extension ()
  (string-case (downcase (file-name-extension (buffer-file-name)))
    ("js"  (yas-activate-extra-mode 'js-mode))
    ("html" (yas-activate-extra-mode 'html-mode))
    ;; ("tsx" (message "tsx")) ;; for debugging
    )
  )

(defun bergey/auto-quote-by-file-extension ()
  (if (string-match "tsx\\|jsx" (downcase (file-name-extension (buffer-file-name))))
      (setq web-mode-enable-auto-quoting nil)
      )
  )

(defun bergey/prettier-parsers-by-file-extension ()
  (string-case (downcase (file-name-extension (buffer-file-name)))
               ("tsx" (setq prettier-parsers '(typescript)))))

;; https://emacs.stackexchange.com/questions/32900/how-to-use-web-mode-engine-specific-checkers-in-flycheck
(defun bergey/configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (cl-flet ((enable (checker)
                  (flycheck-mode)
                  (flycheck-select-checker checker)))
    (string-case web-mode-content-type
                 ("jsx" (enable 'javascript-eslint))
                 ("typescript" (enable 'javascript-eslint))))
  )

(defun bergey/lsp-for-typescript ()
  (string-case web-mode-content-type
               ("typescript" (lsp))
               ("jsx" (lsp)) ;; .tsx files look this way
               ))

;; mocha runner code from https://gist.github.com/lazywithclass/1582626
(defun run-mocha()
  "Runs all the tests in the current buffer"
  (interactive)
  (let* (command result exit-value)
    (setq command (concat "mocha " (buffer-file-name)))
    (setq exit-value (shell-command command))
    (color-modeline exit-value)))

(defun color-modeline(exit-value)
  "Colors the modeline, green success red failure"
  (interactive)
  (let ((test-result-color
          (if (= exit-value 0)
              "Green"
            "Red")))
    (set-face-background 'mode-line test-result-color)
    (run-at-time "1 sec" nil 'no-color-modeline)))

(defun no-color-modeline()
  "No color for the modeline"
  (interactive)
  (set-face-background 'mode-line "gray"))

(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun jq-line ()
  "format the current line with jq"
  (interactive)
  (shell-command-on-region (line-beginning-position) (line-end-position) "jq ." t t)
  )

(provide 'bergey-web)
