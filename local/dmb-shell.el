(use-package shell
  :bind ("C-. h" . shell)
  :config
  (progn
    (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
    (define-key comint-mode-map (kbd "<down>") 'comint-next-input)

    (setq
     tramp-default-method "ssh"          ; uses ControlMaster
     comint-scroll-to-bottom-on-input t  ; always insert at the bottom
     comint-scroll-to-bottom-on-output nil ; always add output at the bottom
     comint-scroll-show-maximum-output t ; scroll to show max possible output
     ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
     comint-input-ignoredups t           ; no duplicates in command history
     comint-completion-addsuffix t       ; insert space/slash after file completion
     comint-buffer-maximum-size 50000    ; max length of the buffer in lines
     comint-prompt-read-only nil         ; if this is t, it breaks shell-command
     comint-get-old-input (lambda () "") ; what to run when i press enter on a
                                        ; line above the current prompt
     comint-input-ring-size 5000         ; max shell history size
     protect-buffer-bury-p nil
     comint-move-point-for-output nil
     completion-at-point-functions
     '(tags-completion-at-point-function bash-completion-dynamic-complete)
     )

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

    (add-hook 'shell-mode-hook 'track-shell-directory/procfs)

    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

    (setenv "EDITOR" "emacsclient") ; no -nw; just open in an emacs buffer
    (setenv "PAGER" "cat") ; let emacs take care of scrolling

    (defun set-scroll-conservatively ()
      "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
      (set (make-local-variable 'scroll-conservatively) 10))

    (add-hook 'shell-mode-hook 'set-scroll-conservatively)
    ;; (add-hook 'shell-mode-hook (lambda ()
    ;;                              (shell-dirtrack-mode 0)
    ;;                               (setq dirtrack-list '("^\\(.*\\)|" 1 nil))
    ;;                              (dirtrack-mode 1)))

    ;; shortcut to select shells
    (defun ido-for-mode(prompt the-mode)
      (switch-to-buffer
       (ido-completing-read prompt
                            (save-excursion
                              (delq
                               nil
                               (mapcar (lambda (buf)
                                         (when (buffer-live-p buf)
                                           (with-current-buffer buf
                                             (and (eq major-mode the-mode)
                                                  (buffer-name buf)))))
                                       (buffer-list)))))))

    (defun ido-shell-buffer()
      (interactive)
      (ido-for-mode "Shell:" 'shell-mode))

    (bind-key "s" 'ido-shell-buffer dmb-jump-keymap)

    (defun rename-shell-buffer (new-name)
      "rename the current buffer with the form shell<foo>"
      (interactive "M*shell*<_>:")
      (rename-buffer (concat "*shell*<" new-name ">") t))

    (bind-key "C-M-n" 'rename-shell-buffer comint-mode-map)

    ) ;; :config
  ) ;; use-package

(provide 'dmb-shell)
