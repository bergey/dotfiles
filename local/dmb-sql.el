(setq sql-connection-alist
      '((logicalbuildings (sql-product 'postgres)
                          (sql-user "bergey")
                          (sql-database "data_api_prod")
                          (sql-server "localhost")
                          (sql-port 3306))
        (logicalbuildings-su (sql-product 'postgres)
                          (sql-user "su_bergey")
                          (sql-database "data_api_prod")
                          (sql-server "localhost")
                          (sql-port 3306))))

(defun sql-logicicalbuildings ()
  (interactive)
  (password-store-copy "ets-postgresql-bergey")
  (sql-connect 'logicalbuildings))

(defun sql-logicalbuildings-su ()
    (interactive)
  (password-store-copy "ets-postgresql-bergey")
  (sql-connect 'logicalbuildings-su))

(provide 'dmb-sql)
