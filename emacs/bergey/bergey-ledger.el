(use-package ledger-mode :ensure t
  :mode "\\.ledger\\'"
  :config
  (bind-key "M-m" 'ledger-set-month ledger-mode-map)
  (setq ledger-reports
        '(("bal" "ledger --strict -f %(ledger-file) bal")
          ("reg" "ledger -f %(ledger-file) reg")
          ;; ("payee" "ledger -f %(ledger-file) reg -- %(payee)")
          ("account" "ledger -f %(ledger-file) reg %(account)")
          ("assets" "ledger -f %(ledger-file) -s bal assets liabilities -V")
          ("expenses" "ledger -f %(ledger-file) -s bal expenses --begin 2015")
          ;; ("cleared" "ledger -f %(ledger-file) -s bal -C assets liabilities")
          ("brokerage" "ledger -f %(ledger-file) bal fidelity")
          ("monthly" "ledger -f %(ledger-file) reg expenses -M --period-sort \"(-amount)\"")
          ("income" "ledger -f %(ledger-file) -Y  register '^Income:[^S]' --begin=$(date +%Y)-01-01")
          ("tikun" "ledger -f %(ledger-file) reg expenses:tikun --begin=$(date +%Y)-01-01")
          ("budget" "ledger -f %(ledger-file) --budget --monthly register ^expenses")
          ))
  (add-to-list 'ledger-mode-hook
               (lambda () (setq-local tab-always-indent 'complete)))
  )

(defun date-format-usa-to-iso ()
  (interactive)
  (while (re-search-forward "\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{4\\}\\)" nil t)
    (replace-match "\\3-\\1-\\2")))

(provide 'bergey-ledger)
