;; http://teallabs.org/picklebush/
;; TODO better xref integration
(defvar picklebush-processes '()
  "an assoc list whose keys are directory names and values are emacs process objects")

(defun picklebush-find-or-start-process (dir)
  (or
   (let ((process (cdr (assoc dir picklebush-processes))))
     (if (and process (eq 'run (process-status process))) process))
   (lexical-let ((process (make-process
                           :name (format "picklebush %s" dir)
                           :command `("picklebush" "--dir" ,dir)
                           :stderr (get-buffer-create (format "picklebush errors %s" dir))
                           :buffer nil)))
     (push (cons dir process) picklebush-processes)
     ;; set up the callback before we send our query
     (set-process-filter process
                         (lambda (p response)
                           (if (equal p process) ; ignore stderr
                               (let* ((file-line (s-split "::" response))
                                      (file (car file-line))
                                      (line (string-to-number (cadr file-line))))
                                 (find-file-other-window file)
                                 (goto-char (point-min))
                                 (beginning-of-line line)))))
     process)))

(defun picklebush (text &optional dir)
  (interactive "Mtext to match:")
  (let* ((directory (or dir (projectile-project-root) (file-name-directory (buffer-file-name))))
         (pickle-process (picklebush-find-or-start-process directory)))
    (xref-push-marker-stack)
    ;; now send the query
    (cond
     ((listp text)
      (process-send-region pickle-process (car text) (cdr text)))
     ((stringp text) (process-send-string pickle-process text))
     (t (message "bad arg %s" text)))))

(defun picklebush-line ()
  (interactive)
  (save-excursion
    (goto-char (pos-bol))
    (search-forward-regexp "Given \\|And \\|Then ")
    (picklebush (cons (point) (pos-bol 2)))))

(provide 'picklebush)
