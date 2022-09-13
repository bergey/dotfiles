(use-package evil :ensure t
  :init
  (use-package undo-fu :ensure t
    :config
    (setq evil-undo-system 'undo-fu)
    )
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (mapcar (lambda (key-string)
            (define-key evil-motion-state-map (kbd key-string) nil)
            (define-key evil-normal-state-map (kbd key-string) nil)
            (define-key evil-insert-state-map (kbd key-string) nil)
            (define-key evil-visual-state-map (kbd key-string) nil)
            )
          '("C-a" "C-d" "C-e" "C-k" "C-o" "C-w" "C-y" "C-." "TAB"))

  (setcdr evil-insert-state-map nil)
  (define-key evil-motion-state-map (kbd "RET") nil)

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
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (evil-set-initial-state 'deadgrep-mode 'emacs)

  (setq evil-insert-state-cursor '(bar . 1))
  (setq evil-cross-lines t)

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

  ;; (use-package org-evil :ensure t :ensure)

  (setq-default evil-shift-width 2))

(use-package evil-collection
  :after evil
  :config
  (setq bergey/evil-collection-original-mode-list evil-collection-mode-list)
  (setq evil-collection-mode-list '(deadgrep))
  (evil-collection-init))

(provide 'bergey-evil)
