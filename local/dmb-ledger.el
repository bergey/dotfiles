(use-package ledger-mode
  :ensure ledger-mode
  :mode "\\.ledger\\'"
  :config (progn
            (bind-key "M-m" 'ledger-set-month ledger-mode-map)
            (setq ledger-reports
                  '(("bal" "ledger -f %(ledger-file) bal")
                    ("reg" "ledger -f %(ledger-file) reg")
                    ("payee" "ledger -f %(ledger-file) reg -- %(payee)")
                    ("account" "ledger -f %(ledger-file) reg %(account)")
                    ("assets" "ledger -f %(ledger-file) --price-db prices.db -s bal assets liabilities -V")
                    ("expenses" "ledger -f %(ledger-file) -s bal expenses --begin 2015")
                    ("cleared" "ledger -f %(ledger-file) -s bal -C assets liabilities")
                    ("brokerage" "ledger -f %(ledger-file) bal fidelity")
                    ("monthly" "ledger -f %(ledger-file) reg expenses -M --period-sort \"(-amount)\"")))))

(defun date-format-usa-to-iso ()
  (interactive)
  (while (re-search-forward "\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{4\\}\\)" nil t)
    (replace-match "\\3-\\1-\\2")))

(provide 'dmb-ledger)
