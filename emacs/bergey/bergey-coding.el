;;; Minor modes & definitions used in multiple programming modes

;; tags

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(bind-key "C-c C-m" 'imenu)

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
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
  )

(defun bergey/fixed-pitch-indent ()
  (font-lock-add-keywords nil '(("^[ -] *" . 'fixed-pitch))))
(add-hook 'prog-mode-hook 'bergey/fixed-pitch-indent)

(font-lock-add-keywords 'yaml-mode '(("^ [ -]*" . 'fixed-pitch)))

(use-package highlight-quoted :ensure t
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
   'paredit-close-round))

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

(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish flycheck-mode
  :config
  ;; TODO show message in minibuffer, but truncate to fit
  (setq flycheck-display-errors-function nil)
)

;; https://emacs.stackexchange.com/questions/22091/how-to-jump-up-or-down-to-first-non-whitespace-character-in-same-column
(defun jump-down-to-non-whitespace-char-in-same-column ()
  (interactive)
  (evil-next-line)
  (while (or (= (char-after (point)) 32)
            (= (char-after (point)) 10))
    (evil-next-line)))

(defun jump-up-to-non-whitespace-char-in-same-column ()
  (interactive)
  (evil-previous-line)
  (while (or (= (char-after (point)) 32)
            (= (char-after (point)) 10))
    (evil-previous-line)))
(bind-key "C-j" #'jump-down-to-non-whitespace-char-in-same-column)
(bind-key "C-k" #'jump-up-to-non-whitespace-char-in-same-column)

(use-package edit-indirect :ensure t
  )

(use-package protobuf-mode :ensure t
  :mode "\\.proto\\'")

(provide 'bergey-coding)
