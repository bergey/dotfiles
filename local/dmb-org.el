; *** org mode ***
;;(require 'org-install)
(use-package org-clock
  :commands org-clock-in
  :config (progn
            (setq org-clock-idle-time 15)
            ;; Change task state to WIP when clocking in
            (setq org-clock-in-switch-to-state "WIP")
            ;; Removes clocked tasks with 0:00 duration
            (setq org-clock-out-remove-zero-time-clocks t)
            ;; wrap clock entries in a drawer if they exceed this many
            (setq org-clock-into-drawer 5)
            (setq org-clock-clocked-in-display 'nil)
            (setq org-log-into-drawer t)
            (setq org-agenda-window-setup 'other-window)
            (setq org-agenda-restore-windows-after-quit t)

            (defun bergey/org-clock-message-clock-string ()
              (interactive)
              (message (substring-no-properties (org-clock-get-clock-string)))
              ;; The above works, but maybe I would like to format better
              ;; (org-clock-get-clocked-time)
              ;; (substring-no-properties org-clock-current-task)
              )
            (bind-key "C-. C-t" 'bergey/org-clock-message-clock-string)
            ))

(use-package org-archive
  :commands org-archive-subtree-default)

(use-package org-pomodoro
  :commands org-pomodoro
  )

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
;; set this without org-git-store-link, which is nifty but usually not what I want.
(setq org-store-link-functions
      (append
       '(org-irc-store-link
        org-info-store-link
        org-gnus-store-link
        org-docview-store-link
        org-bibtex-store-link
        org-bbdb-store-link)
       (if (not (eq system-type 'windows-nt))
           '(org-notmuch-search-store-link org-notmuch-store-link)
         '())
       ))

(setq org-log-done nil)

(defun dmb-org-end-of-subtree ()
  (interactive)
  (org-end-of-subtree))

(add-hook 'org-mode-hook
          '(lambda ()
             (bind-keys :map org-mode-map
                         ;; ("C-c C-m" . helm-imenu)
                         ("C-c C-x C-i" . org-clock-in)
                         ("C-c C-x m" . org-mark-ring-goto)
                         ("C-M-n" . dmb-org-end-of-subtree)
                         ("M-S-a" . org-forward-sentence)
                         ("M-n" . org-move-subtree-down)
                         ("M-p" . org-move-subtree-up)
                         ("C-S-n" . org-move-item-down)
                         ("C-S-p" . org-move-item-up)
                         ("C-c C-x r" . org-refile)
                         )))

(add-hook 'org-mode-hook '(lambda () (diminish 'org-indent-mode)))

(setq
 org-agenda-start-on-weekday nil
 org-blank-before-new-entry (quote ((heading) (plain-list-item)))
 org-columns-default-format "%45ITEM %TODO %3PRIORITY %5Effort{:} %CLOCKSUM"
 org-global-properties (quote (("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 5:00 8:00 13:00")))
 org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m org-notmuch))
 org-special-ctrl-a/e t
 org-startup-indented t
 org-agenda-span 'day
 org-enforce-todo-dependencies t
 org-agenda-dim-blocked-tasks 'invisible
 org-goto-interface 'outline-path-completion
 )

;; Set to the location of your Org files on your local system

(setq org-directory
      (case system-type
        ('gnu/linux "~/Dropbox/org-mode/")
       ('windows-nt
        (format  "c:/Users/%s/records/org-mode/" (user-login-name)))
       ('darwin "~/Dropbox/org-mode/")))

(defun in-org-directory (fn) (concat org-directory fn))

(setq org-agenda-files (in-org-directory "org-agenda-files"))

(setq dmb-work-agenda-files '("teal.org" ))

(setq dmb-home-agenda-files
      (-remove
       (lambda (filename) (-any?
                           (lambda (work) (s-ends-with? work filename))
                           (cons "cb.trello"dmb-work-agenda-files)) )
       (file-expand-wildcards (concat org-directory "[a-z]*.org"))
       ))

;; TODO add remaining files to task lists
;; Not everything household related is in house.org now
;; There's at least one other category - code / study / personal
(setq org-agenda-custom-commands
      '(("p" tags "project+LEVEL=1|contract+LEVEL=1")
        ("P" tags-todo "project")
        ("W" "agenda at work" agenda ""
         ((org-agenda-files
           (mapcar 'in-org-directory dmb-work-agenda-files))))
        ("H" "agenda at home" agenda ""
         ((org-agenda-files (list (in-org-directory "house.org")))))
        ("D" tags-todo "-someday-next+PRIORITY<\"C\"-PRIORITY=\"\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
        ("d" tags-todo  "-someday-next-TODO=\"DELAY\"-CATEGORY=\"cb\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
        ("w" tags-todo "-someday-next-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files (mapcar 'in-org-directory dmb-work-agenda-files))))
        ("h" tags-todo "-someday-next-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files (list (in-org-directory "house.org")))))
        ("n" tags "next+LEVEL=1")
        ("s" tags "someday+LEVEL=1|TODO=\"WISH\"+LEVEL=1|next+LEVEL=1")
        ("q" tags-todo "quick-someday")
        ("B" tags "TODO=\"BLOCKED\"|TODO=\"PR\"")
        ("b" tags "buy")
        ("o" tags "TODO=\"DONE\"|TODO=\"CANCEL\"")))

(setq org-capture-templates
      '(("t" "task" entry (file "capture.org")
         "* TODO %?\n%a")
        ("e" "email needs reply" entry (file+headline "" "Email")
         "* TODO [#B] reply to %:from re %?\n%a")
        ("r" "reading" entry (file+headline "read.org" "triage")
         "* %?")
        ("d" "dinner" entry (file+headline "recipes.org" "2017")
         ;; "* %(format-time-string \"%Y-%m-%d\")\n%?")
         "* %<%Y-%m-%d>\n%?")
        ("f" "fitness log" entry (file+headline "misc.org" "2017")
         "* %<%Y-%m-%d>\n%?")
        ("n" "note" entry (file "notes.org")
         "* %<%Y-%m-%d> %?")
        ;; templates used by the Chrome plugin to save current page / selection
        ("p" "chrome bookmarks" entry (file+headline "read.org" "Chrome")
         "* %a\nEntered on %U\n%?")
        ("L" "chrome selected content" entry (file+headline "read.org" "Chrome")
         "* %a\nEntered on %U\n \%i\n%?")
        ))

(require 'org-protocol)

(use-package org-notmuch
  :commands org-notmuch-store-link
  :commands org-notmuch-search-store-link)


;; TODO is the normal state for an actionable task
;; WIP and DONE are self-explanatory
;; BLOCKED - awaiting some other event, typically action by another person
;; DELAY - awaiting action by me, not currently actionable
;; PR - like BLOCKED, but more specific
;; CANCEL - won't do, but want to keep the record that I've decided not to
;; WISH - review these periodically, might go back on the TODO list
(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(p)" "|" "DONE(d!)")
        (type "DELAY(l)" "BLOCKED(b)" "QA(q)" "ASSIGNED(a)" "|"  "WISH(w)" "CANCEL(k!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "light gray" :weight bold))
        ("WISH" . (:foreground "deep sky blue" :weight bold))
        ("DELAY" . (:foreground "chocolate" :weight bold))
        ("BLOCKED" . (:foreground "chocolate" :weight bold))
        ("QA" . (:foreground "chocolate" :weight bold))
        ("WIP" . (:foreground "magenta" :weight bold))
        ("ASSIGNED" . (:foreground "chocolate" :weight bold))
        ))

(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))
;; (setq org-tags-exclude-from-inheritance '("project"))
;; (setq org-tags-exclude-from-inheritance nil)

(setq org-lowest-priority ?D)
(setq org-default-priority ?C)
(setq org-use-fast-todo-selection t)

(setq org-deadline-warning-days 0)
(setq org-imenu-depth 4)

(setq org-default-notes-file (in-org-directory "capture.org"))
(bind-key "C-. c" 'org-capture)

;; don't show scheduled items in the agenda list of all TODO items
(setq org-agenda-todo-ignore-scheduled "all")

(setq org-outline-path-complete-in-steps nil)

;; idle in X11, not just in emacs
(setq org-clock-x11idle-program-name "xprintidle")

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path t)

(use-package org-trello :ensure t
  :commands org-trello org-trello-mode
  :config
  ;; (setq org-trello-files '("~/records/org-mode/cb.org"))
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
              (org-trello-mode)))))
)

;; collapse headers when entering new file
(setq org-agenda-inhibit-startup nil)
;; use default value of org-startup-folded

;; run every time emacs has been idle for 15 minutes
;;(run-with-idle-timer 900 t 'org-mobile-push)

;; for xmobar org-clock
(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt) txt)

(defun dmb-pick-n (n input)
  "Pick n random elements from lst.  If there are not n items, return lst entire.  If the input list contains no duplicates, neither will the output."
  (cl-labels
      ((worker (n l acc lst)
               (if (= 0 n) acc
                 (let ((i (random l)))
                   (worker (1- n) (1- l) (cons (nth i lst) acc) (-remove-at i lst))))))
    (let ((l (length input)))
      (if (>= n l) lst (worker n l '() input))
      )))

(defun dmb-pick-n-stable (n input)
  "Like `dmb-pick-n', but the elements will appear in the output
  in the same order as in the input"
  (let ((range (-iterate '1+ 0 (length input))))
    (-select-by-indices (sort (dmb-pick-n n range) '<) input)))

  (defun dmb-schedule-n-tasks (n tag)
    (interactive "nSchedule how many tasks? \nMPick tasks with tag: ")
    (let (()))(org-map-entries (lambda () (cons (current-buffer) (point))) (format "+%s" tag) 'agenda)
    )

(provide 'dmb-org)
