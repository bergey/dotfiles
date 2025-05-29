(defvar prism-mode-hook
  '(
    rainbow-delimiters-mode
    highlight-indent-guides-mode
    ))

(defvar prism-mode-map (make-keymap))

(defvar prism-mode-syntax-table
  (let ((syn-table (make-syntax-table)))
    ;; not working
    (modify-syntax-entry ?: "_" syn-table)
    (modify-syntax-entry ?\; "_" syn-table)
    (modify-syntax-entry ?\( "()" syn-table)
    (modify-syntax-entry ?\[ "(]" syn-table)
    (modify-syntax-entry ?\n ">" syn-table)
    (modify-syntax-entry ?/ "< 12" syn-table)
    syn-table))

(defface prism-operator-face
  '((t (:inherit font-lock-operator-face :foreground "white")))
  "PRISM operators")

(defconst prism-font-lock-keywords
  '(
    ("module\\|endmodule\\|init" . font-lock-keyword-face)
    ("dtmc\\|ctmc\\|mdp\\|pta\\|pomdp\\|popta" . font-lock-keyword-face)
    ("bool\\|int\\|double" . font-lock-type-face)
    ("[:;'&]" . font-lock-preprocessor-face)
    ("->" . font-lock-function-name-face)
    ("\\[[^].]*\\]" . font-lock-function-name-face)
    ))

(define-derived-mode prism-mode fundamental-mode "PR"
  "Major mode for PRISM models"
  ;; (set-syntax-table prism-mode-syntax-table)
  (setq-local font-lock-defaults '(prism-font-lock-keywords))
  (setq-local comment-start "//")
  (setq-local comment-use-syntax t)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ((rx line-start (* whitespace) (group "//")) (1 "<"))
               ((rx (group "\n")) (1 ">"))
               ((rx (group ":")) (1 "_"))
               ))
  )
