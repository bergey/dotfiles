; in isearch, after RET, point should be at start of word
; this is how I expect it to behave for navigation
(add-hook 'isearch-mode-end-hook 'bergey/goto-match-beginning)
(defun bergey/goto-match-beginning ()
   (when isearch-forward (goto-char isearch-other-end)))

; make isearch loop to top of buffer
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(provide 'bergey-isearch)
