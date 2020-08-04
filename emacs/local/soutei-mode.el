(defvar soutei-mode-hook nil)

(defvar soutei-mode-map (make-keymap)
  "Keymap for soutei major mode")

(defvar soutei-mode-syntax-table
  (let ((synTable (make-syntax-table)))
    (modify-syntax-entry ?- "_" synTable)
    (modify-syntax-entry ?\; "<" synTable)
    (modify-syntax-entry ?\n ">" synTable)
    synTable)
  "Syntax table for `soutei-mode'"
  )

(defface soutei-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Soutei keywords")

(defface soutei-operator-face
  '((t (:inherit font-lock-builtin-face)))
  "Soutei builtin operators")

(defconst soutei-font-lock-keywords
  '(
    (";.*" . 'font-lock-comment-face)
    ("says" . 'soutei-keyword-face)
    (":-" . 'soutei-operator-face)
    ("," . 'soutei-operator-face)
    ("\\." . 'soutei-operator-face)
    ("#[tf]" . 'font-lock-constant-face)
    ("\\?[a-zA-Z!@$%&*/<=>~_^][a-zA-Z0-9!@$%&*/<=>~_^?+-]*" . 'font-lock-variable-name-face)
    ))

(define-derived-mode soutei-mode prog-mode "Soutei"
  "Major mode for editing Soutei files"
  (set-syntax-table soutei-mode-syntax-table)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) ";+ *")
  (set 'font-lock-defaults '(soutei-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.ava\\'" . soutei-mode))


(provide 'soutei-mode)
