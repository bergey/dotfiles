;; smartparens is like paredit, for many languages

(use-package smartparens :ensure t
  :commands (smartparens-strict-mode smartparens-mode)
  :diminish smartparens-mode
  :config

  ;; default config is a good start
  (require 'smartparens-config)

  ;; copied from https://github.com/Fuco1/smartparens/wiki/Example-configuration

  (defhydra hydra-smartparens (evil-normal-state-map ";")
    "smartparens bindings suitable for evil normal mode"

    ("f" sp-forward-sexp "forward")
    ("b" sp-backward-sexp "back")

    ("j" sp-down-sexp "down ->")
    ("k" sp-backwards-up-sexp "<- up")
    ("J" sp-backwards-down-sexp "<- down")
    ("K" sp-up-sexp "up ->")

    ("^" sp-beginning-of-sexp "start")
    ("$" sp-end-of-sexp "end")

    ("t" sp-transpose-sexp)
    ("<backspace>" sp-backward-unwrap-sexp "< unwrap")
    ("x" sp-unwrap-sexsp "unwrap >")
    ("d" sp-kill-sexp "kill")

    ("l" sp-forward-slurp-sexp "slurp >")
    ("h" sp-forward-barf-sexp "barf >")
    ("H" sp-backward-slurp-sexp "< slurp")
    ("L" sp-backward -barf-sexp "< barf")
    ("X" sp-splice-sexp "splice")

    ;; rebind the function vim puts on ; and some friends
    (";" evil-repeat-find-char "fc;")
    ("," evil-repeat-find-char-reverse "fc,")
    ("." evil-repeat-find-char "fc;") ;; . next to , on dvorak
    )

  (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

  ;; smartparens uses a remap to insert sp-kill-region,
  ;; which is great except that I want to bind both. Remove
  ;; the remap before setting fresh bindings.
  (define-key smartparens-strict-mode-map [remap kill-region] nil)
  (bind-key "C-w" 'sp-kill-region smartparens-strict-mode-map)
  (bind-key "C-S-w" 'kill-region smartparens-strict-mode-map)


  ;; markdown-mode
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

(provide 'dmb-smartparens)
