(require-package 'evil)
(evil-mode 1)

(define-key evil-motion-state-map (kbd "C-o") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-v") nil)
(define-key evil-motion-state-map (kbd "C-w") nil)
(define-key evil-motion-state-map (kbd "C-y") nil)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (kbd evil-toggle-key) 'evil-emacs-state)
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
;; (define-key evil-insert-state-map (kbd "C-o") nil)
;; (define-key evil-insert-state-map (kbd "C-e") nil)
;; (define-key evil-insert-state-map (kbd "C-k") nil)
;; (define-key evil-insert-state-map (kbd "C-n") nil)
;; (define-key evil-insert-state-map (kbd "C-p") nil)
;; (define-key evil-insert-state-map (kbd "C-r") nil)
;; (define-key evil-insert-state-map (kbd "C-w") nil)
;; (define-key evil-insert-state-map (kbd "C-y") nil)

(define-key evil-normal-state-map (kbd "C-e") nil)
(define-key evil-visual-state-map (kbd "C-e") nil)
(define-key evil-motion-state-map (kbd "C-e") nil)
(define-key evil-normal-state-map (kbd "C-f") nil)
(define-key evil-insert-state-map (kbd "C-f") nil)
(define-key evil-normal-state-map (kbd "C-b") nil)
(define-key evil-visual-state-map (kbd "C-b") nil)
(define-key evil-normal-state-map (kbd "C-d") nil)
(define-key evil-visual-state-map (kbd "C-d") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-visual-state-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-visual-state-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-w") nil)
(define-key evil-visual-state-map (kbd "C-w") nil)
(define-key evil-normal-state-map (kbd "C-y") nil)
(define-key evil-visual-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map (kbd "C-k") nil)
(define-key evil-visual-state-map (kbd "C-k") nil)
(define-key evil-normal-state-map (kbd "Q") 'call-last-kbd-macro)
(define-key evil-visual-state-map (kbd "Q") 'call-last-kbd-macro)

(provide 'dmb-evil)
