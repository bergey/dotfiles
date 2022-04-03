; address book
(use-package bbdb :ensure t
  :commands bbdb-search-simple
  :commands bbdb-get-addresses

  :config (progn
                     (bbdb-initialize)
                     (setq
                      bbdb-offer-save 1                        ;; 1 means save-without-asking
                      bbdb-use-pop-up t                        ;; allow popups for addresses
                      bbdb-electric-p t                        ;; be disposable with SPC
                      bbdb-popup-target-lines  1               ;; very small
                      bbdb-dwim-net-address-allow-redundancy t ;; always use full name
                      bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
                      bbdb-always-add-addresses t              ;; add new addresses to existing...
                      ;; ...contacts automatically
                      bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
                      bbdb-completion-type nil                 ;; complete on anything
                      bbdb-complete-name-allow-cycling t       ;; cycle through matches
                      ;; this only works partially
                      bbbd-message-caching-enabled t           ;; be fast
                      bbdb-use-alternate-names t               ;; use AKA
                      bbdb-elided-display t                    ;; single-line addresses
                      )
                     (add-hook 'bbdb-load-hook
                               (lambda () (setq bbdb-file-coding-system 'utf-8-unix)))
                     (setq file-coding-system-alist
                           (cons '("\\.bbdb" utf-8 . utf-8)
                                 file-coding-system-alist))
                     ;; bbdb-insert-new-field is bound by default to C-o, which I use for switching windows
                     ;; In turn, the obvious i binding shadows bbdb-info
                     (define-key bbdb-mode-map (kbd "i") 'bbdb-insert-new-field)
                     (define-key bbdb-mode-map (kbd "I") 'bbdb-info)
                     )
  )

(provide 'bergey-bbdb)
