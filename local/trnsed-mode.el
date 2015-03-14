(defvar trnsed-mode-hook nil)

(defvar trnsed-mode-map (make-keymap)
  "Keymap for TRNSED major mode")

(add-to-list 'auto-mode-alist '("\\.trd\\'" . trnsed-mode))
(add-to-list 'auto-mode-alist '("\\.TRD\\'" . trnsed-mode)) ; is there a better way to be case-insensitive?


(regexp-opt '("EQUATIONS" "CONSTANTS" "UNIT" "TYPE" "PARAMETERS" "INPUTS" "END" "SIMULATION" "TOLERANCE" "LIMITS" "WIDTH") t)

(defconst trnsed-font-lock-keywords
  (list
   '("\\<\\(assign\\|ASSIGN\\|CONSTANTS\\|E\\(?:ND\\|QUATIONS\\)\\|INPUTS\\|LIMITS\\|PARAMETERS\\|SIMULATION\\|T\\(?:\\(?:OLERANC\\|YP\\)E\\)\\|UNIT\\|WIDTH\\)\\>" . font-lock-builtin-face)
   '("^\\*$\\|^\\*|[*#].*\\|^\\*[^|].*\\|!.*" . font-lock-comment-face)
   '("^*|.*" . font-lock-preprocessor-face)))

(define-derived-mode trnsed-mode fundamental-mode "TRNSED"
  "Major mode for editing TRD (TRNSED) files."
  (set (make-local-variable 'font-lock-defaults) '(trnsed-font-lock-keywords))
)

(provide 'trnsed-mode)
