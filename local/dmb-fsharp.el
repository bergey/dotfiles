(use-package fsharp-mode
  :ensure t
  :mode "\\.fsx?"
  :config
  (add-hook 'fsharp-mode-hook 'whitespace-mode))

;; generate to / from JSON boilerplate
(defun fsharp-find-record-fields ()
  "Returns a list of strings.  If point is on a record
definition, return the field names.  Assumes one particular
formatting convention."
  (interactive)
  (save-excursion
    (search-backward-regexp "type \\([a-zA-Z0-9]\\)+ *= *{")
    (forward-line)
    (let ( (type-name (match-string 1))
          (fields ())
          (start (point)))
      (search-forward "}")
      (while (< start (point))
        (search-backward-regexp "^ *\\([a-zA-Z0-9]*\\) *:")
        (let ((name (match-string 1)))
          (set-text-properties 0 (length name) nil name)
          (setq fields (cons name fields))
          ))
      (cons type-name fields))))

(defun fsharp-insert-json-instances ()
  (interactive)
  (let* ((found (fsharp-find-record-fields))
        (type-name (car found))
        (fields (cdr found)))
    (search-forward "}")
    (forward-line)
    (insert "\nwith static member ToJson (r : "
            type-name ") =\n"
            "    JsonValue.Record [|\n")
    (mapcar (lambda (s)
              (insert (format "        \"%s\" .= r.%s\n" s s))) fields)
    (insert "    |]\n\n"
            "static member FromJson =\n"
            "    parseObj <| fun json -> jsonParse {\n")
    (mapcar (lambda (s)
              (insert (format "        let! %s = json .@ \"%s\"\n" s s))) fields)
    (insert "        return {\n")
    (mapcar (lambda (s)
              (insert (format "            %s = %s\n" s s))) fields)
    (insert "        }\n    }\n")
    ))

(use-package paket-mode
  :mode "paket."
;;  https://github.com/mars888/paket-mode.git
  )

(provide 'dmb-fsharp)
