; *** org mode ***
;;(require 'org-install)
(use-package org-clock
  :commands org-clock-in
  :config
  (setq org-clock-idle-time 15)
  ;; Change task state to WIP when clocking in
  (setq org-clock-in-switch-to-state "WIP")
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; wrap clock entries in a drawer if they exceed this many
  (setq org-clock-into-drawer 3)
  (setq org-clock-clocked-in-display 'nil)
  (setq org-log-into-drawer t)
  (setq org-agenda-window-setup 'other-window)
  (setq org-agenda-restore-windows-after-quit t)

  (defun bergey/org-clock-message-clock-string ()
    (interactive)
    (message (substring-no-properties (org-clock-get-clock-string)))
    )

  (bind-keys :map org-agenda-mode-map
             ("M-m" . nil) ;; conflicts with window switching; was org-agenda-bulk-toggle
             )
  :bind ("C-. C-t" . 'bergey/org-clock-message-clock-string)
  )

(use-package org-archive
  :commands org-archive-subtree-default)

(global-set-key "\C-ca" #'org-agenda)
(global-set-key "\C-cl" #'org-store-link)
(bind-key "C-. c" #'org-capture)

;; set this without org-git-store-link, which is nifty but usually not what I want.
(setq org-store-link-functions
      '(org-irc-store-link
        org-info-store-link
        org-docview-store-link
        org-bibtex-store-link
        )
      )

(defun bergey/org-end-of-subtree ()
  (interactive)
  (org-end-of-subtree))

;; TODO move this to :config does not need to run in hook
(add-hook 'org-mode-hook
          #'(lambda ()
             (bind-keys :map org-mode-map
                         ("C-c C-x C-i" . org-clock-in)
                         ("C-c C-x m" . org-mark-ring-goto)
                         ("C-M-n" . bergey/org-end-of-subtree)
                         ("M-S-a" . org-forward-sentence)
                         ("M-n" . org-move-subtree-down)
                         ("M-p" . org-move-subtree-up)
                         ("C-S-n" . org-move-item-down)
                         ("C-S-p" . org-move-item-up)
                         ("C-c C-x r" . org-refile)
                         ("M-h" . nil) ;; conflicts with window switching; was org-mark-element
                         )))

(add-hook 'org-mode-hook #'(lambda () (diminish 'org-indent-mode)))

(setq
 org-agenda-start-on-weekday nil
 org-blank-before-new-entry (quote ((heading) (plain-list-item)))
 org-columns-default-format "%45ITEM %TODO %3PRIORITY %5Effort{:} %CLOCKSUM"
 org-global-properties (quote (("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 5:00 8:00 13:00")))
 org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m))
 org-special-ctrl-a/e t
 org-startup-indented t
 org-agenda-span 'day
 org-enforce-todo-dependencies t
 org-agenda-dim-blocked-tasks t
 org-goto-interface 'outline-path-completion
 org-log-done nil
 org-n-level-faces 6
 )

;; Set to the location of your Org files on your local system

(setq org-directory
      (cl-case system-type
        ('gnu/linux "~/records/org/")
        ('darwin "~/records/org/")
        ('windows-nt (format  "c:/Users/%s/records/org/" (user-login-name)))
        ))

(defun in-org-directory (fn)
  (cond
   ((stringp fn) (concat org-directory fn))
   ((listp fn) (mapcar #'in-org-directory fn))))

(setq org-agenda-files (in-org-directory "org-agenda-files"))

(setq
 bergey/work-agenda-files '("braze.org")
 bergey/home-agenda-files '("house.org")
 bergey/teal-agenda-files '("teal.org")
 bergey/not-work-agenda-files '("house.org" "teal.org" "capture.org")
 )

(setq org-agenda-custom-commands
      '(
        ("w" "TODO at work" tags-todo "-someday-next-administrata-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files bergey/work-agenda-files)))
        ("h" "TODO at home" tags-todo "-someday-next-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files bergey/home-agenda-files)))
        ("b" "TODO for teallabs" tags-todo "-someday-next-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files bergey/teal-agenda-files)))
        ("W" "agenda at work" agenda ""
         ((org-agenda-files bergey/work-agenda-files)))
        ("H" "agenda at home" agenda ""
         ((org-agenda-files bergey/home-agenda-files)))
        ("B" "agenda for teallabs" agenda ""
         ((org-agenda-files bergey/teal-agenda-files)))

        ("p" tags "project+LEVEL=1|contract+LEVEL=1")
        ("P" tags-todo "project")
        ("D" "this month" tags-todo "-someday-next+PRIORITY<\"C\"-PRIORITY=\"\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
        ("d" "this month, not work" tags-todo  "-someday-next+PRIORITY<\"C\"-PRIORITY=\"\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files bergey/not-work-agenda-files)))
        ("X" tags "TODO=\"BLOCKED\"|TODO=\"PR\"")
        ("$" tags "buy")
        ("o" tags "TODO=\"DONE\"|TODO=\"CANCEL\"")))

(use-package org-cliplink
  ;; create bookmarks from OS clipboard
  :ensure t
  :commands org-cliplink-capture)

(setq org-capture-templates
      '(("t" "task" entry (file "capture.org")
         "* TODO %?\n%a")
        ("n" "note" entry (file "notes.org")
         "* %<%Y-%m-%d> %?")
        ("b" "bookmark" entry (file+headline "bookmarks.org" "recent")
         "* %(org-cliplink-capture)\nEntered on %U\n%?")
        ("d" "dinner" entry (file+headline "recipes.org" "2022")
         ;; "* %(format-time-string \"%Y-%m-%d\")\n%?")
         "* %<%Y-%m-%d>\n%?")
        ))
        ;; Unused, need to check if they work / are useful
        ;; ("e" "email needs reply" entry (file+headline "" "Email")
        ;;  "* TODO [#B] reply to %:from re %?\n%a")
        ;; ("r" "reading" entry (file+headline "read.org" "triage")
        ;;  "* %?")
        ;; ("f" "fitness log" entry (file+headline "misc.org" "2017")
        ;;  "* %<%Y-%m-%d>\n%?")
        ;; ;; templates used by the Chrome plugin to save current page / selection
        ;; ("p" "chrome bookmarks" entry (file+headline "read.org" "Chrome")
        ;;  "* %a\nEntered on %U\n%?")
        ;; ("L" "chrome selected content" entry (file+headline "read.org" "Chrome")
        ;;  "* %a\nEntered on %U\n \%i\n%?")

;; TODO is the normal state for an actionable task
;; WIP and DONE are self-explanatory
;; BLOCKED - awaiting some other event, typically action by another person
;; DELAY - awaiting action by me, not currently actionable
;; QA - like BLOCKED, but more specific
;; CANCEL - won't do, but want to keep the record that I've decided not to
;; WISH - review these periodically, might go back on the TODO list
(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(p)" "|" "DONE(d!)")
        (type "DELAY(l)" "BLOCKED(b)" "REVIEW(q)" "ASSIGNED(a)" "|"  "WISH(w)" "CANCEL(k!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "light gray" :weight bold))
        ("WISH" . (:foreground "deep sky blue" :weight bold))
        ("DELAY" . (:foreground "chocolate" :weight bold))
        ("BLOCKED" . (:foreground "chocolate" :weight bold))
        ("QA" . (:foreground "chocolate" :weight bold))
        ("REVIEW" . (:foreground "chocolate" :weight bold))
        ("WIP" . (:foreground "magenta" :weight bold))
        ("ASSIGNED" . (:foreground "chocolate" :weight bold))
        ))

(eval-after-load 'org
  '(add-to-list 'org-emphasis-alist
               '("*" (:foreground "red")
                 )))

(setq org-lowest-priority ?D)
(setq org-default-priority ?C)
(setq org-use-fast-todo-selection t)

(setq org-deadline-warning-days 0)
(setq org-imenu-depth 4)

(setq org-default-notes-file (in-org-directory "capture.org"))

;; don't show scheduled items in the agenda list of all TODO items
(setq org-agenda-todo-ignore-scheduled "all")

(setq org-outline-path-complete-in-steps nil)

;; idle in X11, not just in emacs
(setq org-clock-x11idle-program-name "xprintidle")

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path t)

;; collapse headers when entering new file
(setq
 org-agenda-inhibit-startup nil
 org-startup-folded 'overview)

;; for xmobar org-clock
(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt) txt)

;; ox = org-export
(use-package ox-jira :ensure t :defer 5)

;; github formatted markdown
(use-package ox-gfm :ensure t :defer 5)

(use-package ox-reveal :ensure t
  :custom
  (org-reveal-root "/Users/bergey/records/teallabs/slides/js/reveal.js")
  )

(provide 'bergey-org)
