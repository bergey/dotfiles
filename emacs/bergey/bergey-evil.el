(use-package evil
  :commands evil-mode
  :bind (
         :map evil-normal-state-map
         ("s" . evil-forward-char)
         ("Q" . call-last-kbd-macro)
         :map evil-visual-state-map
         ("Q" . call-last-kbd-macro)
         :map evil-motion-state-map
         ("M-<down>" . evil-scroll-line-down)
         ("M-<up>" . evil-scroll-line-up)
         ;; bindings that should only apply in certain evil states, not all
         :map evil-insert-state-map
         ("C-z" . evil-emacs-state)
        ("<escape>" . evil-normal-state)
         )
  :init
  (evil-mode 1)
  (use-package undo-fu
    :custom
    (evil-undo-system 'undo-fu)
    )

  :custom
  (evil-cross-lines t)
  (evil-insert-state-cursor '(bar . 1))
  (evil-want-keybinding nil)
  (evil-want-minibuffer t)
  (evil-bigword "^,;() \t\r\n\f")

  ;; This makes the cursor position more like emacs position.  It also
  ;; makes `sp-forward-sexp' work, for some reason.  I only care about
  ;; the latter.  In practice, with `evil-move-cursor-back' nil,
  ;; prefer `i' to `a', so that `i<esc>' brings you back where you
  ;; started.
  (evil-move-cursor-back nil)

  :config

  (mapcar (lambda (key-string)
            (define-key evil-motion-state-map (kbd key-string) nil)
            (define-key evil-normal-state-map (kbd key-string) nil)
            (define-key evil-insert-state-map (kbd key-string) nil)
            (define-key evil-visual-state-map (kbd key-string) nil)
            )
          '("C-o" "C-." "TAB" ";"))
  (setcdr evil-insert-state-map nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  ;; replaces 'evil-repeat-pop-next'; I haven't decided where to rebind that
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (evil-define-key 'normal help-mode-map (kbd "C-o") nil)

  (evil-define-key 'normal magit-blame-mode-map "q" 'magit-blame-quit)

  ;;; per-mode customization
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  ;; My habit is to type `:q' when I'm done with a file, so rebind
  ;; `:q' to make that work.  I never learned vim window split
  ;; commands (using vim in terminal in `screen').
  (evil-ex-define-cmd "q" 'kill-this-buffer)

  (setq-default evil-shift-width 2)

  ;; https://github.com/emacs-evil/evil/issues/1450
  (defun forward-evil-WORD (&optional count)
    "Move forward COUNT \"WORDS\".
Moves point COUNT WORDS forward or (- COUNT) WORDS backward if
COUNT is negative. Point is placed after the end of the WORD (if
forward) or at the first character of the WORD (if backward). A
WORD is a sequence of non-whitespace characters
'[^\\n\\r\\t\\f ]', or an empty line matching ^$."
    (evil-forward-nearest count
                          #'(lambda (&optional cnt)
                              (evil-forward-chars evil-bigword cnt))
                          #'forward-evil-empty-line))
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
