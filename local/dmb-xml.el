(use-package w3m
  :ensure w3m
  :commands w3m)

(defun w3m-browse-current-buffer ()
    (interactive)
    (let ((filename (concat (make-temp-file "w3m-") ".html")))
      (unwind-protect
          (progn
            (write-region (point-min) (point-max) filename)
            (w3m-find-file filename))
        (delete-file filename))))

;; everything web: HTML,javascript, css
(use-package web-mode
  :ensure web-mode
  :mode "\\.\\(p?html\\|tpl\\|php\\|erb\\|mustach\\|jsx?\\|json\\|s?css\\|sass\\)\\'"
  :config
  (progn
    (define-key web-mode-map (kbd "C-c C-l") 'w3m-browse-current-buffer)
    (setq web-mode-hook '(
                          flycheck-mode
                          whitespace-mode
                          dmb-company-short-idle
                          smartparens-strict-mode
                          (lambda () (setq-local helm-dash-docsets '("HTML" "CSS" "Sass" "Bourbon" "Neat")))))
  ;; (add-hook 'web-mode-hook 'flycheck-mode)
  ;; (add-hook 'web-mode-hook 'whitespace-mode)
  ;; (add-hook 'web-mode-hook 'dmb-company-short-idle)
  ;; (add-hook 'web-mode-hook 'smartparens-strict-mode)
  ;; (add-hook 'web-mode-hook '(lambda () (setq-local helm-dash-docsets '("HTML" "CSS" "Sass" "Bourbon" "Neat"))))
    (define-key web-mode-map (kbd "C-j") 'newline)
    (define-key web-mode-map (kbd "RET") 'newline-and-indent)
    (define-key web-mode-map (kbd "C-c C-r") 'run-mocha)
    (bind-keys :map web-mode-map
               ("C--" . web-mode-comment-or-uncomment))
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

(use-package coffee-mode
  :ensure coffee-mode
  :mode "\\.coffee\\'")

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

;; https://github.com/emacsmirror/nodejs-mode
(use-package nodejs-repl
  :commands nodejs-repl
  :ensure nodejs-repl)

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

(provide 'dmb-xml)
