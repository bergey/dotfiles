(use-package emms
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-mode-line-mode -1)
  (emms-playing-time-mode -1)

  :custom
  (emms-player-list '(emms-player-vlc))
  (emms-info-functions '(emms-info-native))
  (emms-source-file-default-directory "/home/spaceways/music/0 sorted/")

  :bind
  ( :map bergey/global-keymap
    ("p" . emms-pause)
   :map bergey/jump-keymap
   ("m" . emms)
   )
  :config
  (evil-define-key 'normal emms-playlist-mode-map "a" #'emms-add-directory-tree)
  )

(provide 'bergey-music)
