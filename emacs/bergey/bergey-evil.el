(use-package evil
  :bind (
         :map evil-normal-state-map
         ("s" . evil-forward-char)
         ("Q" call-last-kbd-macro)
         :map evil-visual-state-map
         ("Q" call-last-kbd-macro)
         :map evil-motion-state-map
         ("M-<down>" evil-scroll-line-down )
         ("M-<up>" evil-scroll-line-up)
         ;; bindings that should only apply in certain evil states, not all
         :map evil-insert-state-map
         (evil-toggle-key evil-emacs-state)
         ("<escape>" evil-normal-state)
         ;; replaces 'evil-repeat-pop-next'; I haven't decided where to rebind that
         (define-key evil-normal-state-map (kbd "M-.") nil)
         )
  :init
  (use-package undo-fu
    :config
    (setq evil-undo-system 'undo-fu)
    )

  :custom
  (evil-cross-lines t)
  (evil-insert-state-cursor '(bar . 1))
  (evil-want-keybinding nil)
  (evil-want-minibuffer t)

  ;; This makes the cursor position more like emacs position.  It also
  ;; makes `sp-forward-sexp' work, for some reason.  I only care about
  ;; the latter.  In practice, with `evil-move-cursor-back' nil,
  ;; prefer `i' to `a', so that `i<esc>' brings you back where you
  ;; started.
  (evil-move-cursor-back nil)

  :config
  (evil-mode 1)

  (mapcar (lambda (key-string)
            (define-key evil-motion-state-map (kbd key-string) nil)
            (define-key evil-normal-state-map (kbd key-string) nil)
            (define-key evil-insert-state-map (kbd key-string) nil)
            (define-key evil-visual-state-map (kbd key-string) nil)
            )
          '("C-o" "C-." "TAB" ";"))
  (setcdr evil-insert-state-map nil)
  (define-key evil-motion-state-map (kbd "RET") nil)

  (evil-define-key 'normal magit-blame-mode-map "q" 'magit-blame-quit)

  ;;; per-mode customization
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  ;; My habit is to type `:q' when I'm done with a file, so rebind
  ;; `:q' to make that work.  I never learned vim window split
  ;; commands (using vim in terminal in `screen').
  (evil-ex-define-cmd "q" 'kill-this-buffer)

  (setq-default evil-shift-width 2)
  )

(use-package evil-collection
  ;; magit, rg, maybe dired benefit from this
  :after evil
  :diminish evil-collection-unimpaired-mode
  :custom
  (bergey/evil-collection-original-mode-list evil-collection-mode-list)
  :config
  (evil-collection-init)
  (evil-define-key 'normal Info-mode-map (kbd "M-h") nil)
  )

(provide 'bergey-evil)
