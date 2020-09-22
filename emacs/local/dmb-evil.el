(use-package evil :ensure t
  :config
  (evil-mode 1)
  (global-undo-tree-mode -1)

  (mapcar (lambda (key-string)
            (define-key evil-motion-state-map (kbd key-string) nil)
            (define-key evil-normal-state-map (kbd key-string) nil)
            (define-key evil-insert-state-map (kbd key-string) nil)
            (define-key evil-visual-state-map (kbd key-string) nil)
            )
          '("C-a" "C-d" "C-e" "C-k" "C-o" "C-v" "C-w" "C-y" "C-." "TAB"))

  (setcdr evil-insert-state-map nil)

  ;; bindings that should only apply in certain evil states, not all
  (define-key evil-insert-state-map (kbd evil-toggle-key) 'evil-emacs-state)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "Q") 'call-last-kbd-macro)
  (define-key evil-visual-state-map (kbd "Q") 'call-last-kbd-macro)

  ;; replaces 'evil-repeat-pop-next'; I haven't decided where to rebind that
  (define-key evil-normal-state-map (kbd "M-.") nil)

  (evil-define-key 'normal magit-blame-mode-map "q" 'magit-blame-quit)

  ;; new bindings
  (bind-key "M-<down>" 'evil-scroll-line-down evil-motion-state-map)
  (bind-key "M-<up>" 'evil-scroll-line-up evil-motion-state-map)

  ;;; per-mode customization
  ;; (evil-set-initial-state 'org-capture-mode 'emacs) ;; did not work 2017-07-03 or 2019-04-25
  (evil-set-initial-state 'org-mode 'emacs)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  (setq evil-insert-state-cursor '(bar . 1))
  (setq evil-cross-lines t)
  (setq evil-shift-width 2)

  ;; This makes the cursor position more like emacs position.  It also
  ;; makes `sp-forward-sexp' work, for some reason.  I only care about
  ;; the latter.  In practice, with `evil-move-cursor-back' nil,
  ;; prefer `i' to `a', so that `i<esc>' brings you back where you
  ;; started.
  (setq evil-move-cursor-back nil)

  ;; My habit is to type `:q' when I'm done with a file, so rebind
  ;; `:q' to make that work.  I never learned vim window split
  ;; commands (using vim in terminal in `screen').
  (evil-ex-define-cmd "q" 'kill-this-buffer)

  (use-package org-evil :ensure t :ensure)

  )

(provide 'dmb-evil)
