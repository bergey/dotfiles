(defvar netlist-mode-hook
  '(
    highlight-indent-guides-mode
    ))

(defvar netlist-mode-map (make-keymap))

(defvar netlist-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" synTable)
    synTable))

(defconst netlist-font-lock-keywords
  '(
    ("\\.[a-z]*" . font-lock-keyword-face)
    ;; ("^ *X[[:alnum:]]*" . font-lock-variable-name-face)
    ;; ,(rx line-start (* whitespace) "X" (* anychar) whitespace (group (+ alnum))) . (1 font-lock-function-call-face)
    ))

(define-derived-mode netlist-mode fundamental-mode "NL"
  "Major mode for circuit netlists"
  (set-syntax-table netlist-mode-syntax-table)
  (setq-local font-lock-defaults '(netlist-font-lock-keywords))
  (setq-local comment-start "*")
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ((rx line-start (* whitespace) (group "*")) (1 "<"))
               ((rx (group "//")) (1 "<"))
               ((rx (group "\n")) (1 ">"))
               ))
  )

(provide 'netlist-mode)
