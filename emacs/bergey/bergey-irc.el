(use-package password-store :ensure t
  :commands (password-store--completing-read)
  :config
  ;; TODO put this patch somewhere
  (defun password-store--run-1 (callback &rest args)
  "Run pass with ARGS.

Nil arguments are ignored.  Calls CALLBACK with the output on success,
or outputs error message on failure."
  (let ((output ""))
    (make-process
     :name "password-store-gpg"
     :command (cons password-store-executable (delq nil args))
     :connection-type 'pipe
     :stderr nil
     :noquery t
     :filter (lambda (process text)
               (setq output (concat output text)))
     :sentinel (lambda (process state)
                 (cond
                  ((string= state "finished\n")
                   (funcall callback output))
                  ((string= state "open\n") (accept-process-output process))
                  (t (error (concat "password-store: " state)))))))))

(use-package erc :ensure t
  :commands erc
  :init
  (defun bergey/erc ()
    (interactive)
    (setq erc-nickserv-passwords
          `((freenode (("bergey" . ,(if (equal system-type 'windows-nt)
                                        (read-passwd "freenode password: ")
                                        (progn
                                          (message "prompting for GPG password")
                                          (password-store-get "shared/freenode"))))))))
    (erc :server "irc.freenode.net" :nick "bergey")
    (erc :server "irc.mozilla.org" :nick "bergey")
    ;;  (erc :server "localhost"        :nick "bergey" :password (assoc 'bitlbee bergey-passwords))
    )
  :config
  (progn

    (setq erc-join-buffer 'bury)
    (setq erc-prompt-for-nickserv-password nil)

    (add-hook 'erc-join-hook 'bitlbee-identify)
    (defun bitlbee-identify ()
      "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
      (when (and (string= "localhost" erc-session-server)
                 (string= "&bitlbee" (buffer-name)))
        (erc-message "PRIVMSG" (format "%s identify %s"
                                       (erc-default-target)
                                       (assoc 'bitlbee bergey-passwords)))))

    ;; retain channel preference across sessions
    ;; within session, reconnect automatically after first connect
    ;; (autojoin module enabled by default)
    (setq erc-autojoin-timing 'ident)
    (setq erc-autojoin-channels-alist
          '(;("localhost" "&bitlbee")
            ( "freenode.net" "#haskell" "#haskell-emacs" "#ghc" )
            ))

    (setq erc-hide-list '("JOIN" "PART" "QUIT" "MODE"))
    (setq erc-modules
          '(autojoin button completion fill irccontrols list
          match menu move-to-prompt netsplit networks noncommands
          readonly ring scrolltobottom services stamp track
          truncate))

    (defvar erc-insert-post-hook)
    (add-hook 'erc-insert-post-hook
              'erc-truncate-buffer)
    (setq erc-truncate-buffer-on-save t)

    (bind-key "C-c C-o" 'browse-url erc-mode-map)

    (defun bergey/shorten-haskell (name)
      (replace-regexp-in-string "haskell" "h"
                                (replace-regexp-in-string "^#haskell$" "#H"
                                                          name)))

    (defun bergey/erc-track-shorten-haskell (names)
      (-map 'bergey/shorten-haskell
            (erc-track-shorten-names names)))

    (setq erc-track-shorten-function 'bergey/erc-track-shorten-haskell)

    (defun irc-activity-string ()
      (if (functionp erc-track-shorten-function)
          (funcall
           erc-track-shorten-function
           (mapcar 'buffer-name (mapcar 'car erc-modified-channels-alist)))
        (nil)))

    ))

(provide 'bergey-irc)
