(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (mapcar (lambda (key-string)
            (define-key evil-motion-state-map (kbd key-string) nil)
            (define-key evil-normal-state-map (kbd key-string) nil)
            (define-key evil-insert-state-map (kbd key-string) nil)
            (define-key evil-visual-state-map (kbd key-string) nil)
            )
          '("C-a" "C-d" "C-e" "C-k" "C-o" "C-v" "C-w" "C-y" "C-."))

  (setcdr evil-insert-state-map nil)

  ;; bindings that should only apply in certain evil states, not all
  (define-key evil-insert-state-map (kbd evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "Q") 'call-last-kbd-macro)
  (define-key evil-visual-state-map (kbd "Q") 'call-last-kbd-macro)

  ;; new bindings
  (bind-key "M-<down>" 'evil-scroll-line-down evil-motion-state-map)
  (bind-key "M-<up>" 'evil-scroll-line-up evil-motion-state-map)

  ;;; per-mode customization
  ;; (evil-set-initial-state 'org-capture-mode 'insert) ;; did not work 2017-07-03
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  )

(provide 'dmb-evil)
