;;; Minor modes & definitions used in multiple programming modes

;; tags

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; https://www.emacswiki.org/emacs/NavigatingParentheses#toc2
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis.
vi style of % jumping to matching brace.  If point is immediately
after a paren, and not on a paren, goto the match of the
preceding paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        ((save-excursion (backward-char 1) (looking-at "\\s\)"))
         (backward-list 1))
        ))
(bind-key"C-%" 'goto-match-paren)

(use-package yasnippet
  :ensure yasnippet
  :diminish yas-minor-mode
  :config (progn
            (yas-global-mode)
            (bind-key "C-t" 'yas-expand yas-minor-mode-map)
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
            ))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command (format "ctags %s"  (directory-file-name dir-name))))

;; *** C
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
            (define-key c-mode-base-map "{" 'my-c-mode-insert-lcurly)))

(add-to-list 'auto-mode-alist '("\.pde" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.ino" . c-mode)) ; arduino
(add-to-list 'auto-mode-alist '("\.glsl" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.frag" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.vert" . c-mode)) ; OpengGL
(add-to-list 'auto-mode-alist '("\.geom" . c-mode)) ; OpengGL

(time-package 'dmb-smartparens)

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'column)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package highlight-quoted
  :ensure t)

(use-package highlight-escape-sequences
  :ensure t)

(use-package fic-mode
  :ensure t
  :commands fic-mode
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package eldoc
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

;; *** misc ***

(autoload 'tinyprocmail-mode "tinyprocmail" "" t)
(autoload 'aput "assoc")

 ;; Treat ~/.procmailrc and all pm-*.rc files as Procmail files
 (aput 'auto-mode-alist
      "\\.procmailrc\\|pm-.*\\.rc$"
      'turn-on-tinyprocmail-mode)
(put 'upcase-region 'disabled nil)

;; Maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
(setq imaxima-use-maxima-mode-flag t)

;; ediff customization
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; don't spawn frame
(setq ediff-split-window-function 'split-window-horizontally) ; lines should align!

;; for org-mode
(add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
(add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)
;; Check for org mode and existence of buffer
(defun f-ediff-org-showhide (buf command &rest cmdargs)
  "If buffer exists and is orgmode then execute command"
  (when buf
    (when (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
      (save-excursion (set-buffer buf) (apply command cmdargs)))))

(defun f-ediff-org-unfold-tree-element ()
  "Unfold tree at diff location"
  (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
  (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
  (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

(defun f-ediff-org-fold-tree ()
  "Fold tree back to top level"
  (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
  (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
  (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))
;; end for org-mode

(use-package flycheck
  :ensure flycheck
  :commands flycheck-mode
  :diminish flycheck-mode
  )

;; documentation lookup
(use-package helm-dash
  :ensure t
  :config (progn
            (setq helm-dash-common-docsets '("Haskell" "Bourbon" "HTML" "CSS"))
            )
  ;; also installed: arduino, bourbon, css, d3.js, haskell, html, javascript, jquery, lo-dash, opengl4, react.
  )
(use-package auto-complete
  :ensure t)

(use-package edit-indirect
  :ensure t)

(provide 'dmb-coding)
