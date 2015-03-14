; in isearch, after RET, point should be at start of word
; this is how I expect it to behave for navigation
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
   (when isearch-forward (goto-char isearch-other-end)))

; make isearch loop to top of buffer
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; In emacs 24.4, icomplete takes over most of the functionality of ido
;; though defaults & keybindings differ
;; ETA: I'm not convinced of the above; keep experimenting
(icomplete-mode 99)
(setq completion-cycle-threshold nil)  ; TAB cycles
(provide 'dmb-isearch)
