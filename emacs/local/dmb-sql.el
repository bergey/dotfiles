(use-package sql
  :config
  ;; matches \set PROMPT1 '%`date +%H:%M:%S`%R%# ' in ~/.psqlrc
  (sql-set-product-feature 'postgres :prompt-regexp "^[_:[:alnum:]]*[=][#>] ")

  (setq sql-connection-alist
        '(
          (local-portal
           (sql-product 'postgres)
           (sql-user "range-data-server")
           (sql-database "range-data-server")
           (sql-server "localhost")
           (sql-port 5432))
          (kubes-beta
           (sql-product 'postgres)
           (sql-user "range-data-server")
           (sql-database "range-data-server")
           (sql-server "localhost")
           (sql-port 54320))
          (qa-portal
           (sql-product 'postgres)
           (sql-user "range-data-server")
           (sql-database "range-data-server")
           (sql-server "localhost")
           (sql-port 54321))
          (prod-portal
           (sql-product 'postgres)
           (sql-user "readonly")
           (sql-database "range-data-server")
           (sql-server "localhost")
           (sql-port 54322))
          (prod-portal-superuser
           (sql-product 'postgres)
           (sql-user "postgres")
           (sql-database "range-data-server")
           (sql-server "localhost")
           (sql-port 54322))
          (docker-localhost
           (sql-product 'postgres)
           (sql-user "postgres")
           (sql-database "postgres")
           (sql-server "localhost")
           (sql-port 5432))
          (load-test
           (sql-product 'postgres)
           (sql-user "range-data-server")
           (sql-database "range-data-server")
           (sql-server "localhost")
           (sql-port 54323))
          ))

  (defun sql-kubes-beta ()
    (interactive)
    (bergey/password-store-copy "simspace/kubes-beta-postgres")
    (sql-connect 'kubes-beta))

  (defun sql-scalability ()
    (interactive)
    (bergey/password-store-copy "simspace/scalability-postgres")
    (sql-connect 'load-test))

  (defun sql-qa ()
    (interactive)
    (bergey/password-store-copy "simspace/qa-postgres")
    (sql-connect 'qa-portal))

  (defvar my-sql-replacements nil)
  (make-variable-buffer-local 'my-sql-replacements)

  (defun bergey/sql-send-buffer-replace ()
    (interactive)
    (let ((string (buffer-substring-no-properties (point-min) (point-max))))
      (while (string-match "[$][0-9]+" string)
        (let* ((placeholder (match-string 0 string))
               (replacement (or (cdr (assoc placeholder my-sql-replacements))
                                (read-string (format "Replacement for %s: " placeholder)))))
          (unless (assoc placeholder my-sql-replacements)
            (push (cons placeholder replacement) my-sql-replacements))
          (setq string (replace-regexp-in-string (regexp-quote placeholder) replacement string))))
      (sql-send-string string)))


  (defun bergey/sql-send-string-replace ()
    (interactive)
    ;; TODO handle \" in quoted strings
    (save-excursion
      (let* ((string (buffer-substring-no-properties
                      (progn (search-backward "\"") (forward-char) (point))
                      (progn (search-forward "\"") (backward-char) (point))))
             (sql-replacements nil))
        (while (string-match "[$][0-9]+" string)
          (let* ((placeholder (match-string 0 string))
                 (replacement (or (cdr (assoc placeholder sql-replacements))
                                  (read-string (format "Replacement for %s: " placeholder)))))
            (unless (assoc placeholder sql-replacements)
              (push (cons placeholder replacement) sql-replacements))
            (setq string (replace-regexp-in-string (regexp-quote placeholder) replacement string))))
        (sql-send-string string))))

  (define-key sql-interactive-mode-map (kbd "C-c C-w") nil)
  (define-key sql-interactive-mode-map (kbd "C-c C-x") nil) )

(provide 'dmb-sql)
