(use-package coterm
  :after shell
  :init (coterm-mode))

(use-package shell
  :commands shell
  :init
  (defun named-shell (new-name)
    (interactive "M*shell*<_>:")
    (shell (get-buffer-create (concat "*shell*<" new-name ">"))))

  :bind (
         ("C-. h" . #'named-shell)
         :map shell-mode-map
         ("C-M-n" . #'rename-shell-buffer)
         ("C-r" . comint-history-isearch-backward-regexp) ;; was isearch-backward, but I prefer evil ?
         ("C-s" . #'comint-history-isearch-backward-regexp) ;; works in nav mode
         ("C-c C-w" . nil)
         ("C-c C-x" . nil) ;; was comint-get-next-from-history which sounds useful
         ("M-r" . nil) ;; conflicts with window switching; was comint-history-isearch-backward-regexp
         :map comint-mode-map
         ("<up>" . #'comint-previous-input)
         ("<down>" . #'comint-next-input)
         )

  :config
  (use-package native-complete :after company
    :commands native-complete-setup-bash
    :init
    (native-complete-setup-bash)
    (add-to-list 'company-backends 'company-native-complete)
    )

  (setq
   tramp-default-method "ssh"          ; uses ControlMaster
   comint-scroll-to-bottom-on-input t  ; always insert at the bottom
   comint-scroll-to-bottom-on-output nil ; always add output at the bottom
   comint-scroll-show-maximum-output t ; scroll to show max possible output
   comint-input-ignoredups t           ; no duplicates in command history
   comint-completion-addsuffix t       ; insert space/slash after file completion
   comint-buffer-maximum-size 50000    ; max length of the buffer in lines
   comint-prompt-read-only nil         ; if this is t, it breaks shell-command
   comint-get-old-input (lambda () "") ; what to run when i press enter on a
                                        ; line above the current prompt
   comint-input-ring-size 5000         ; max shell history size
   protect-buffer-bury-p nil
   comint-move-point-for-output nil
   )

  (set-face-attribute 'comint-highlight-prompt nil :inherit nil)

  ;; track current directory
  ;; from: http://www.emacswiki.org/emacs/ShellDirtrackByProcfs
  (defun track-shell-directory/procfs ()
    (shell-dirtrack-mode 0)
    (add-hook 'comint-preoutput-filter-functions
              (lambda (str)
                (prog1 str
                  (when (string-match comint-prompt-regexp str)
                    (cd (file-symlink-p
                         (format "/proc/%s/cwd" (process-id
                                                 (get-buffer-process
                                                  (current-buffer)))))))))
              nil t))

  (if (memq system-type '(gnu gnu/linux))
      (add-hook 'shell-mode-hook 'track-shell-directory/procfs))

  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  (setenv "EDITOR" "emacsclient") ; no -nw; just open in an emacs buffer
  (setenv "PAGER" "cat") ; let emacs take care of scrolling
  (setenv "MANPAGER" "cat") ; same, for programs that use `man' for `--help'

  (defun set-scroll-conservatively ()
    "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
    (set (make-local-variable 'scroll-conservatively) 10))

  (add-hook 'shell-mode-hook 'set-scroll-conservatively)

  (defun bergey/set-bash-completion ()
    (setq completion-at-point-functions '(native-complete-at-point t)))
  (add-hook 'shell-mode-hook 'bergey/set-bash-completion)

  (defun bergey/shell-buffer ()
    (interactive)
    (switch-to-buffer
     (completing-read
      "Switch to shell: "
      #'internal-complete-buffer
      (lambda (buffer-pair) ;; (name . buffer)
        (let ((case-fold-search nil))
          (string-match "^\\*shell.*\\*" (car buffer-pair))))
      ))
    )
  (bind-key "h" #'bergey/shell-buffer bergey/jump-keymap)

  (defun rename-shell-buffer (new-name)
    "rename the current buffer with the form shell<foo>"
    (interactive "M*shell*<_>:")
    (rename-buffer (concat "*shell*<" new-name ">") t))

  ;; whatever lint says, the ' before windows-nt is necessary'
  (pcase system-type
    ('windows-nt
     (progn (setq explicit-shell-file-name
                  "C:/Program Files/Git/bin/bash.exe")
            (setq shell-file-name explicit-shell-file-name)
            (setq explicit-sh.exe-args '("--login" "-i"))
            (setenv "SHELL" shell-file-name)
            (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
            )) ;; 'windows-nt
    ('darwin
     (setq explicit-shell-file-name "~/.nix-profile/bin/bash")
     (setq shell-file-name explicit-shell-file-name)
     ))

  (if (boundp 'warning-suppress-types)
      (add-to-list 'warning-suppress-types '(undo discard-info))
    (setq warning-suppress-types '((undo discard-info)))
    )

  )

(use-package eshell
  :commands eshell
  ;; TODO lazy bind, only when eshell is actually loaded, and variable is declared
  ;; :bind ((:map eshell-mode-map ("C-M-n" . rename-shell-buffer)))
  :config
  (cl-case system-type
    ('windows-nt
     (setq eshell-mode-hook
           #'(lambda ()
               (setq eshell-path-env
                     (concat
                      "C:\\Program Files\\Git\\mingw64\\bin;"
                      "C:\\Program Files\\Git\\usr\\local\\bin;"
                      "C:\TDM-GCC-64\bin;"
                      "C:\\Program Files\\Git\\usr\\bin;"
                      "C:\\Program Files\\Git\\bin;"
                      "C:\\Program Files\\Git\\cmd;"
                      eshell-path-env
                      ";"
                      "C:\\Program Files\\Git\\usr\\bin\\vendor_perl;"
                      "C:\\Program Files\\Git\\usr\\bin\\core_perl"
                      ))))
     (setq eshell-login-script "/Users/bergey/.emacs.d/eshell/windows-login"))
    ('darwin
     (setq eshell-login-script "/Users/bergey/.emacs.d/eshell/macos-login"))
    )
  (setq eshell-buffer-name "*shell*")
  )

(use-package eat
  :config
  (setq eat-semi-char-non-bound-keys
        (append
         '([?\e?m] [?\e?w] [?\e?v] [?\e?h] [?\e?t] [?\e?n] [?\e?r])
         eat-semi-char-non-bound-keys))
  (eat-update-semi-char-mode-map)
  )

(provide 'bergey-shell)
