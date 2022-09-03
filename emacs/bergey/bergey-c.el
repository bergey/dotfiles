;; C language
;; custom, based on k&r
(c-add-style
 "kar"
 '("k&r"
   (c-basic-offset . 4)))

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "kar")))

(add-to-list 'auto-mode-alist '("\\.cl" . c-mode))

(setq c-mode-hook
      '(whitespace-mode
        smartparens-mode
        flycheck-mode
        bergey/company-short-idle))

;; *** C
(defun bergey/c-mode-insert-lcurly ()
  (interactive)
  (insert "{")
  (let ((pps (syntax-ppss)))
    (when (and (eolp) (not (or (nth 3 pps) (nth 4 pps)))) ;; EOL and not in string or comment
      (c-indent-line)
      (insert "\n\n}")
      (c-indent-line)
      (forward-line -1)
      (c-indent-line))))

(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
            (define-key c-mode-base-map "{" 'bergey/c-mode-insert-lcurly)))

(add-to-list 'auto-mode-alist '("\.pde" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.ino" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.glsl" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.frag" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.vert" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.geom" . c-mode)) ; OpengGL

(provide 'bergey-c)
