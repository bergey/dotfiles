(use-package emms :ensure t
  :commands emms-add-directory-tree
             :commands emms-add-directory
             :commands emms-add-file
             :init
             (bind-key "C-. e" (lambda () (interactive) (switch-to-buffer " *EMMS Playlist*")))
             :config
             (progn
               (require 'emms-setup)
               (emms-standard)
               (emms-default-players)

               ;; same as default, use current directory
               (setq emms-source-file-default-directory nil)

               ;; faster recursive descent
               (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
               (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal)

               (define-key emms-playlist-mode-map (kbd "x") 'emms-pause)
               (define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
               (define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
               (define-key emms-playlist-mode-map (kbd "R") 'emms-toggle-repeat-playlist)

               (add-hook 'emms-player-paused-hook
                         (lambda ()
                           (if emms-player-paused-p
                               (message "EMMS Paused")
                             (message "EMMS Resumed"))))))

(provide 'dmb-audio)
