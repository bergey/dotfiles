(use-package mu4e
  :custom
  (mu4e-mu-binary "/home/bergey/.nix-profile/bin/mu")
  (mu4e-get-mail-command "mbsync -a")
  ;; for all contexts / email addresses
  (user-full-name "Daniel Bergey" )
  (smtpmail-smtp-service 587)
  (mu4e-contexts
   `( ,(make-mu4e-context
        :name "Clematis"
        ;; we match based on the contact-fields of the message
        :match-func (lambda (msg)
                      (when msg (mu4e-message-contact-field-matches msg :to "bergey@clematis.systems")))
        :vars '( ( user-mail-address . "bergey@clematis.systems"  )
                 (smtpmail-smtp-server . "smtp.fastmail.com")
                 ))
      ,(make-mu4e-context
        :name "gmail"
        ;; we match based on the maildir of the message
        ;; this matches maildir /gmail and its sub-directories
        :match-func (lambda (msg)
                      (when msg (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        :vars '( ( user-mail-address       . "dmbergey@gmail.com" )
                 (smtpmail-smtp-server . "smtp.gmail.com")
                 ))
      ,(make-mu4e-context
        :name "Teal Labs"
        :match-func (lambda (msg)
                      (when msg (mu4e-message-contact-field-matches msg :to "bergey@teallabs.org")))
        :vars '( ( user-mail-address . "bergey@teallabs.org")
                 (smtpmail-smtp-server . "smtp.fastmail.com")
                 ))
      ))
  )

(provide 'bergey-email)
