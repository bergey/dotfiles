

;; I use the following elisp function when on the zotero://select// item in emacs:

(defun smc/zot-select ()
  "select on current 'zotero://select//HASH' using fresno"
  (interactive)
  (save-excursion
    (let (zot-select js l)
      (re-search-backward " \\|^")
      (if (re-search-forward "\\(zotero:\\/\\/select\\/\\/0_[[:alnum:]]\\{8\\}\\)" (line-end-position) t)
          (progn
            (setq zot-select (match-string-no-properties 1))
            ;; (message zot-select)
            (shell-command (concat "fresno -p \"" zot-select "\"")))
        (message "no match")))))
