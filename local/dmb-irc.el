(use-package erc
  :commands erc
  :init
  (defun dmb-erc ()
    (interactive)
    (erc :server "irc.freenode.net" :nick "bergey" :password (assoc 'freenode dmb-passwords))
    ;; (erc :server "irc.mozilla.org" :nick "bergey")
    ;;  (erc :server "localhost"        :nick "bergey" :password (assoc 'bitlbee dmb-passwords))
    )
  :config
  (progn
    (add-hook 'erc-join-hook 'bitlbee-identify)
    (defun bitlbee-identify ()
      "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
      (when (and (string= "localhost" erc-session-server)
                 (string= "&bitlbee" (buffer-name)))
        (erc-message "PRIVMSG" (format "%s identify %s"
                                       (erc-default-target)
                                       (assoc 'bitlbee dmb-passwords)))))

    ;; retain channel preference across sessions
    ;; within session, reconnect automatically after first connect
    ;; (autojoin module enabled by default)
    (setq erc-autojoin-channels-alist
          '(;("localhost" "&bitlbee")
            ("freenode.net" "#diagrams" "#haskell" "#ghcjs" "#purescript"
              "#numerical-haskell" "#haskell-lens" "#haskell-game")
            ("irc.mozilla.org" "#rust")))

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

    (defun dmb-shorten-haskell (name)
      (replace-regexp-in-string "haskell" "h"
                                (replace-regexp-in-string "^#haskell$" "#H"
                                                          name)))

    (defun dmb-erc-track-shorten-haskell (names)
      (-map 'dmb-shorten-haskell
            (erc-track-shorten-names names)))

    (setq erc-track-shorten-function 'dmb-erc-track-shorten-haskell)

    ))

(provide 'dmb-irc)
