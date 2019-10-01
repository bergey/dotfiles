;; bind C-x k to end emacsclient (eg, git) instead of C-x #, which is awkward to type
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

;; start server, so I can call emacsclient from bash
(unless (server-running-p) (server-start))
(add-hook 'mail-mode-hook '(lambda ()
                             (auto-fill-mode 1)
                             (setq fill-column 72)))

(provide 'dmb-emacsclient)
