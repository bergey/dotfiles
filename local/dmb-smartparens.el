;; smartparens is like paredit, for many languages

(use-package smartparens
  :commands (smartparens-strict-mode smartparens-mode)
  :diminish smartparens-mode
  :config (progn

            ;; default config is a good start
            (require 'smartparens-config)

            ;; copied from https://github.com/Fuco1/smartparens/wiki/Example-configuration

            (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
            (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

            (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
            (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
            (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
            (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

            (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
            (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
            (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
            (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

            (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
            (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

            (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
            (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

            (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
            (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

            (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
            (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
            (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
            (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

            (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
            (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
            (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
            (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

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
              (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))))

(provide 'dmb-smartparens)
