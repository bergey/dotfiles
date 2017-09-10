;; *** python ***
(use-package python-mode
  :ensure python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (setq ipython-command "/usr/bin/ipython")
    (setq py-python-command-args '("--pylab"))
    (setq py-shell-name "/usr/bin/ipython")

    (add-hook 'python-mode-hook #'(lambda ()
                                    (setq py-python-command-args '( "--pylab"))
                                    (setq indent-tabs-mode nil)))
    (add-hook 'python-mode-hook 'whitespace-mode)

    ;; (add-hook 'python-mode-hook
    ;;           '(lambda () (eldoc-mode 1)))

    (add-hook 'python-mode-hook
              flycheck-mode
              anaconda-mode
              )

    (when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (local-file (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name))))
          (list "epylint" (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.py\\'" flymake-pylint-init)))

    (define-key python-mode-map (kbd "C-c C-h") 'py-help-at-point)
    ;; TODO should be for all flymake
    (define-key python-mode-map (kbd "C-c C-e") 'flymake-goto-next-error)))

(use-package anaconda-mode
  :commands anaconda-mode
  :ensure t)

(provide 'dmb-python)
