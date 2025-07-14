;; https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode

;; https://github.com/dwwmmn/alloy-mode
;; provided the syntax table

;; sudo make install in tree-sitter-alloy
(treesit-language-available-p 'alloy)

(define-derived-mode alloy-ts-mode prog-mode "Alloy[ts]"
  "Major mode for Alloy 6 specifications"
  :syntax-table alloy-mode-syntax-table

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'alloy)
    (treesit-parser-create 'alloy)
    (alloy-setup))
  )

(defun alloy-setup ()
  "Set up treesit for alloy-ts-mode"

  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules alloy-font-lock-rules))
  (setq-local treesit-simple-indent-rules alloy-indent-rules)

  (treesit-major-mode-setup)
  )

(defvar alloy-mode-syntax-table
  (let (( syntax-table (make-syntax-table)))
    (--each '(
              (?\( "()")
              (?\) ")(")
              (?\[ "(]")
              (?\] ")[")
              (?\{ "(}")
              (?\} "){")
              ;; Add operator symbols misassigned in the std table
              (?\$ ".")
              (?\% ".")
              (?\& ".")
              (?\* ".")
              (?\+ ".")
              (?\- ".")
              (?\/ ".")
              (?\< ".")
              (?\= ".")
              (?\> ".")
              (?\| ".")

              (?\_ "w")
              (?\' "w")
              (?\" "w")
              ;; backquote is open and close paren
              (?\` "$")
              ;; comment delimiters
              (?/  ". 124b")
              (?*  ". 23")
              (?-  ". 12b")
              (?\n "> b   ")
              (?  "    ")
              (?\t "    ")
              (?\r "    ")
              (?\f "    "))
      )
    (modify-syntax-entry (car it) (cadr it) syntax-table))
  syntax-table
  "Syntax table used in `alloy-mode' buffers.")

(setq-local tree-sit-font-lock-feature-list
            '((comment)
              (paragraph logical temporal)
              (prime relational)
              (operator)))
