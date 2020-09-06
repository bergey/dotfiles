(defvar happy-mode-hook nil)

(defvar happy-mode-map (make-keymap)
  "Keymap for happy major mode")

(defvar happy-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" synTable)
    (modify-syntax-entry ?: "_" synTable)
    (modify-syntax-entry ?| "_" synTable)
    (modify-syntax-entry ?% "_" synTable)
    (modify-syntax-entry ?{ "(} 1b" synTable)
    (modify-syntax-entry ?} "){4" synTable)
    (modify-syntax-entry ?- "_ 12b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)
    synTable)
  ;; "Syntax table for `happy-mode'"
  )

(defface happy-haskell-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Haskell block embedded in Happy")

(defface happy-directive-face
  '((t (:inherit font-lock-builtin-face)))
  "Happy directives (begin with %)")
;; (set-face-attribute 'happy-directive-face nil :inherit font-lock-builtin-face)

(defface happy-grammer-rule-face
  '((t (:inherit font-lock-variable-name-face)))
  "Operators introducing Happy grammar rules")

(defconst happy-font-lock-keywords
  '(
    ("%[a-z]*\\>" . 'happy-directive-face)
    ("[:|]" . 'happy-grammer-rule-face)
    ("[{}]" . 'happy-haskell-face)
    ("%%" . 'font-lock-comment-face)
    ("^[a-zA-Z0-9_]*\\>" . 'font-lock-type-face)
    ))

(define-derived-mode happy-mode fundamental-mode "Y"
  "Major mode for editing Happy files (Haskell parser generator)"
  (set-syntax-table happy-mode-syntax-table)
  (set 'font-lock-defaults '(happy-font-lock-keywords)))

;; highlight Haskell inside {} with `haskell-mode'
(define-hostmode poly-happy-hostmode
  :mode 'happy-mode)

(define-innermode poly-happy-haskell-innermode
  :mode #'haskell-mode
  :head-matcher "{"
  ;; this is very crude, and breaks if you have any {} blocks in the Haskell snippet
  ;; in practice, I define types in other modules, so this works out
  :tail-matcher "[^-]}"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-happy-mode
  :hostmode 'poly-happy-hostmode
  :innermodes '(poly-happy-haskell-innermode))

(add-to-list 'auto-mode-alist '("\\.y\\'" . poly-happy-mode))


(provide 'happy-mode)
