(use-package mu4e
  :custom
  (mu4e-mu-binary "/home/bergey/.nix-profile/bin/mu")
  (mu4e-get-mail-command "mbsync -a")
  )

(provide 'bergey-email)
