;; *** org mode ***

;; Set to the location of your Org files on your local system
(setq org-directory
      (cl-case system-type
        (gnu/linux "~/records/org/")
        (darwin "~/dotfiles/private/")
        (windows-nt (format  "c:/Users/%s/records/org/" (user-login-name)))
        ))

(defun in-org-directory (fn)
  (cond
   ((stringp fn) (concat org-directory fn))
   ((listp fn) (mapcar #'in-org-directory fn))))

(use-package org
  :commands org-mode
  ;; :hook (org-mode . #'(lambda () (diminish 'org-indent-mode)))
  :bind
  (
   :map outline-minor-mode-map
   ("M-h" . nil) ;; was outline-promote
   :map org-mode-map
   ("M-h" . nil) ;; conflicts with window switching; was org-mark-element
   ("M-n" . nil) ;; was org-move-subtree-down
   ("C-c C-x C-i" . org-clock-in)
   ("C-c C-x m" . org-mark-ring-goto)
   ("C-M-n" . bergey/org-end-of-subtree)
   ("M-S-a" . org-forward-sentence)
   ("C-S-n" . org-move-item-down) ;; only working on - not * trees?
   ("C-S-p" . org-move-item-up)
   ("C-S-k" . org-move-subtree-up)
   ("C-S-j" . org-move-subtree-down)
   ("C-S-RET" . bergey/org-insert-todo-heading)
   ("M-RET" . bergey/org-meta-return)
   )

  :custom
  ;; TODO is the normal state for an actionable task
  ;; WIP and DONE are self-explanatory
  ;; BLOCKED - awaiting some other event, typically action by another person
  ;; DELAY - awaiting action by me, not currently actionable
  ;; QA - like BLOCKED, but more specific
  ;; CANCEL - won't do, but want to keep the record that I've decided not to
  ;; WISH - review these periodically, might go back on the TODO list
  (org-todo-keywords
   '((sequence "TODO(t)" "WIP(p)" "|" "DONE(d!)")
     (type "DELAY(l)" "BLOCKED(b)" "REVIEW(q)" "ASSIGNED(a)" "|"  "WISH(w)" "CANCEL(k!)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "light gray" :weight bold))
     ("WIP" . (:foreground "#ea84fe" :weight bold))
     ("WISH" . (:foreground "#5e84fe" :weight bold))
     ("DELAY" . (:foreground "#8f4a31" :weight bold))
     ("BLOCKED" . (:foreground "#8f4a31" :weight bold))
     ("QA" . (:foreground "#8f4a31" :weight bold))
     ("REVIEW" . (:foreground "#8f4a31" :weight bold))
     ("ASSIGNED" . (:foreground "#8f4a31" :weight bold))
     ))
  (org-agenda-inhibit-startup nil)
  (org-startup-folded 'overview)
  (org-priority-lowest ?D)
  (org-priority-default ?C)
  (org-use-fast-todo-selection t)
  (org-startup-indented t)
  (org-enforce-todo-dependencies t)
  (org-log-done nil)
  (org-n-level-faces 6)
  (org-blank-before-new-entry (quote ((heading) (plain-list-item))))
  (org-columns-default-format "%45ITEM %TODO %3PRIORITY %5Effort{:} %CLOCKSUM")
  (org-global-properties (quote (("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 5:00 8:00 13:00"))))
  (org-modules '(org-habit))
  (org-special-ctrl-a/e t)
  (org-deadline-warning-days 0)
  (org-imenu-depth 4)

  :config
  (add-to-list 'org-emphasis-alist '("*" (:foreground "red")))

  (defun bergey/org-end-of-subtree ()
    (interactive)
    (org-end-of-subtree))

  (defun bergey/org-insert-todo-heading ()
    "always insert TODO, not the state of current heading"
    (interactive)
    (let ((org-insert-heading-respect-content t))
      (org-insert-todo-heading '(4)))
    )

  (defun bergey/org-meta-return (&optional arg)
    "like org-meta-return, but call bergey/org-insert-todo-heading rather than org-insert-heading"
    (interactive)
    (or (run-hook-with-args-until-success 'org-metareturn-hook)
        (call-interactively (cond (arg #'bergey/org-insert-todo-heading)
                                  ((org-at-table-p) #'org-table-wrap-region)
                                  ((org-in-item-p) #'org-insert-item)
                                  (t #'bergey/org-insert-todo-heading)))))
  (evil-define-key 'normal org-mode-map (kbd "M-h") nil)
  (evil-define-key 'normal outline-mode-map (kbd "M-h") nil)
  ;; parameterize in case I find other links to handle with xdg-open
  (defun org-link-set-xdg-open (scheme)
    (org-link-set-parameters
     scheme
     :follow `(lambda (path) (browse-url-xdg-open (format "%s:%s" ,scheme path)))))
  ;; zotero links to open an item in the app
  (org-link-set-xdg-open "zotero")
  (org-link-set-xdg-open "calibre")
  )

(use-package org-agenda
  :bind
  (("C-c a" . org-agenda)
   :map org-agenda-mode-map
   ("M-m" . nil) ;; conflicts with window switching; was org-agenda-bulk-toggle
   ("j" . evil-next-line) ;; was org-agenda-goto-date
   ("k" . evil-previous-line) ;; was org-agenda-capture
   )

  :config
  (setq bergey/work-agenda-files '("braze.org"))
  (setq bergey/home-agenda-files '("house.org"))
  (setq bergey/teal-agenda-files '("teal.org"))
  (setq bergey/not-work-agenda-files '("house.org" "teal.org" "capture.org"))

  :custom
  (org-agenda-window-setup 'other-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-files (in-org-directory "org-agenda-files"))
  ;; don't show scheduled items in the agenda list of all TODO items
  (org-agenda-todo-ignore-scheduled "all")

  (org-agenda-custom-commands
   '(
     ("w" "TODO at work" tags-todo "-someday-next-administrata-TODO=\"DELAY\""
      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
       (org-agenda-sorting-strategy '(priority-down))
       (org-agenda-files bergey/work-agenda-files)))
     ("h" "TODO at home" tags-todo "-someday-next-TODO=\"DELAY\""
      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
       (org-agenda-sorting-strategy '(priority-down))
       (org-agenda-files bergey/home-agenda-files)))
     ("b" "TODO for teallabs" tags-todo "-someday-next-TODO=\"DELAY\""
      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
       (org-agenda-sorting-strategy '(priority-down))
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
  )

(use-package org-capture
  :bind
  ( :map bergey/global-keymap
    ("c" . org-capture)
    )
  :custom
  (org-capture-templates
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
  (org-default-notes-file (in-org-directory "capture.org"))
  )

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

  (defun bergey/org-clock-message-clock-string ()
    (interactive)
    (message (substring-no-properties (org-clock-get-clock-string)))
    )
  :bind
  ( :map bergey/global-keymap
    ("C-t" . 'bergey/org-clock-message-clock-string)
    )
  )

(use-package org-archive
  :commands org-archive-subtree-default)

(use-package ol
  :bind ("C-c l" . org-store-link)
  :custom
  ;; set this without org-git-store-link, which is nifty but usually not what I want.
  (org-store-link-functions
   '(org-irc-store-link
     org-info-store-link
     org-docview-store-link
     org-bibtex-store-link
     )
   )
  )

(use-package org-cliplink
  ;; create bookmarks from OS clipboard

  :commands org-cliplink-capture
  )

(use-package ox
  :commands org-export-dispatch
  :custom
  (org-export-backends '(ascii html icalendar latex md odt))
  :config
  (use-package ox-jira)

  ;; github formatted markdown
  (use-package ox-gfm)

  (use-package ox-reveal
    :custom
    (org-reveal-root "/Users/bergey/records/teallabs/slides/js/reveal.js")
    )
  )


(use-package org-refile
  :bind
  (:map org-mode-map
        ("C-c C-x r" . org-refile)
        )
  :custom
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path t)
  )

(provide 'bergey-org)
