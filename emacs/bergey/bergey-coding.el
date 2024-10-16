;;; Minor modes & definitions used in multiple programming modes

;; tags

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(bind-key "C-c C-m" 'imenu)
(setq imenu-max-item-length nil)

(setq tags-revert-without-query t)

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((ctags
         (if (equal system-type 'windows-nt)
             "c:/ProgramData/chocolatey/bin/ctags"
           "ctags")))
    (shell-command (format "%s %s"  ctags (directory-file-name dir-name)))))

(time-package 'bergey-smartparens)

(use-package rainbow-delimiters :ensure t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :custom (rainbow-delimiters-max-face-count 6)
  )

(use-package highlight-indent-guides :ensure t
  :commands highlight-indent-guides-mode
  :diminish highlight-indent-guides-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; :bind doesn't put the binding in override-global-map, and it doesn't show up in Haskell
  (bind-key "C-c C-x h" #'highlight-indent-guides-mode override-global-map)

  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\┆)
  (setq highlight-indent-guides-auto-enabled nil)

  (defface indent-guides-depth-1 '((t (:foreground "#8c0437" ))) "lch 30 54 13")
  (defface indent-guides-depth-2 '((t (:foreground  "#8f4a31"))) "lch 40 40 45")
  (defface indent-guides-depth-3 '((t (:foreground  "#246c01"))) "lch 40 57 130")
  (defface indent-guides-depth-4 '((t (:foreground  "#086783"))) "lch 40 29 233")
  (defface indent-guides-depth-5 '((t (:foreground  "#475aac"))) "lch 40 49 285")
  (defface indent-guides-depth-6 '((t (:foreground  "#8b418d"))) "lch 40 49 325")
  ;; These colors are really too aggressive; I should make a darker
  ;; set, or maybe even a spectrum that doesn't vary much in hue.
  ;; This shows how to do it, though.
  (defun bergey/highlight-indent (level responsive display)
    (cl-case (+ 1 (mod level 6))
      (1 'indent-guides-depth-1)
      (2 'indent-guides-depth-2)
      (3 'indent-guides-depth-3)
      (4 'indent-guides-depth-4)
      (5 'indent-guides-depth-5)
      (6 'indent-guides-depth-6)
      ))
  (setq highlight-indent-guides-highlighter-function #'bergey/highlight-indent)
  )

(defun bergey/fixed-pitch-indent ()
  (font-lock-add-keywords nil '(("^[ -] *" . 'fixed-pitch))))

(font-lock-add-keywords 'yaml-mode '(("^ [ -]*" . 'fixed-pitch)))

(defun bergey/configure-eglot-flymake ()
  (cl-case major-mode
    (ruby-mode (setq-local flymake-diagnostic-functions '(ruby-flymake-auto eglot-flymake-backend)))))

(use-package eglot
  :ensure t
  :defer 3
  :hook
  ((
    go-mode
    js-mode
    ruby-mode
    rust-mode
    typescript-mode
    ) . eglot-ensure)
  :bind (:map eglot-mode-map (("C-c C-," . eglot-format-buffer)))
  :config
  (setq eglot-managed-mode-hook
        '(bergey/configure-eglot-flymake
          bergey/diminish-vc-mode
          ))
  (setq eglot-menu-string "")
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode 1)
  :bind ("M-y" . yas-expand)
  )

(use-package highlight-quoted :ensure t
  :commands highlight-quoted-mode
  )

(use-package highlight-escape-sequences :ensure t
  )

(use-package fic-mode :ensure t
  :commands fic-mode
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package eldoc :ensure t
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (setq eldoc-echo-area-display-truncation-message nil)
  )

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

(use-package flymake
  :diminish flymake-mode
  :bind (:map flymake-mode-map
              ;; I use M-n for window switching, so can't bind M-n / M-p
              ("C-c ! n" . flymake-goto-next-error) ;; flycheck
              ("C-! n" . flymake-goto-next-error) ;; shorter
              ("C-! j" . flymake-goto-next-error) ;; shorter
              ("M-j" . flymake-goto-next-error) ;; shorter
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-! p" . flymake-goto-prev-error)
              ("C-! k" . flymake-goto-prev-error)
              ("M-k" . flymake-goto-prev-error)
              ("C-! l" . flymake-show-buffer-diagnostics)
              ("C-! L" . flymake-show-project-diagnostics)
              )
  )

;; TODO remove this everywhere, if flymake works well enough
;; 2024-08-15 flycheck works in Rust, I haven't gotten flymake / eglot set up yet
(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish flycheck-mode
  :config
  ;; TODO show message in minibuffer, but truncate to fit
  (setq flycheck-display-errors-function nil)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; unset before rebinding, from docs for flycheck-keymap-prefix
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-!"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map)
)

;; https://emacs.stackexchange.com/questions/22091/how-to-jump-up-or-down-to-first-non-whitespace-character-in-same-column
(defun jump-to-non-whitespace-char-in-same-column (up-or-down)
  (let* ((col (current-column))
         (go-up-or-down (lambda ()
                          (funcall up-or-down)
                          (evil-goto-column col))))
    (funcall go-up-or-down)
    (while (or (= (char-after (point)) 32)
               (= (char-after (point)) 10)
               (< (current-column) col))
      ;; (message "%s %s" (line-number-at-pos) (current-column)) ;; debug
      (funcall go-up-or-down))))

(defun jump-down-to-non-whitespace-char-in-same-column ()
  (interactive)
  (jump-to-non-whitespace-char-in-same-column #'evil-next-line))

(defun jump-up-to-non-whitespace-char-in-same-column ()
  (interactive)
  (jump-to-non-whitespace-char-in-same-column #'evil-previous-line))

(bind-key "C-j" #'jump-down-to-non-whitespace-char-in-same-column)
(bind-key "C-k" #'jump-up-to-non-whitespace-char-in-same-column)

(use-package edit-indirect :ensure t
  :commands edit-indirect-region
  :defer 10
  )

(use-package protobuf-mode :ensure t
  :mode "\\.proto\\'")

(use-package direnv
  :config
  (direnv-mode))

(use-package dumb-jump :ensure t
  :commands dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-force-searcher 'rg) ;; avoid git-grep
  ;; (setq dumb-jump-debug t)
  ;; (setq evil-goto-definition-functions '(evil-goto-definition-xref))
  )
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(provide 'bergey-coding)

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
