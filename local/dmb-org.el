; *** org mode ***
;;(require 'org-install)
(setq
 org-mobile-directory "~/records/org-mode/MobileOrg"
 org-mobile-inbox-for-pull "~/records/org-mode/MobileOrg/from-mobile.org")
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
            (setq org-log-into-drawer t)
            (setq org-agenda-window-setup 'other-window)
            (setq org-agenda-restore-windows-after-quit t)
            ))

(use-package org-archive
  :commands org-archive-subtree-default)

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
         '())))

(setq org-log-done nil)

(defun dmb-org-end-of-subtree ()
  (interactive)
  (org-end-of-subtree))

(add-hook 'org-mode-hook
          '(lambda ()
             (bind-keys :map org-mode-map
                         ("C-c C-m" . helm-imenu)
                         ("C-c C-x C-i" . org-clock-in)
                         ("C-c C-x m" . org-mark-ring-goto)
                         ("C-M-n" . dmb-org-end-of-subtree)
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
 )

;; Set to the location of your Org files on your local system

(setq org-directory
      (cond
       ((eq system-type 'gnu/linux) "~/records/org-mode/")
       ((eq system-type 'windows-nt)
        (format  "c:/Users/%s/records/org-mode/" (user-login-name)))))

(defun in-org-directory (fn) (concat org-directory fn))

(setq org-agenda-files (in-org-directory "org-agenda-files"))

(setq org-agenda-custom-commands
      '(("p" tags "project+LEVEL=1")
        ("P" tags-todo "project")
        ("W" "agenda at work" agenda ""
         ((org-agenda-files
           (mapcar 'in-org-directory '("jet.org" "research.org")))))
        ("H" "agenda at home" agenda "" ((org-agenda-files
                 (-remove
                  (lambda (fn) (or (s-contains? "jet.org" fn) (s-contains? "cb.org" fn)))
                  (file-expand-wildcards (concat org-directory "[a-z]*.org"))
                    ))))
        ("D" tags-todo "-someday-next+PRIORITY<\"C\"-PRIORITY=\"\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
        ("d" tags-todo  "-someday-next-TODO=\"DELAY\"-CATEGORY=\"cb\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
        ("w" tags-todo "-someday-next-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files (mapcar 'in-org-directory '("jet.org" "research.org")))))
        ("h" tags-todo "-someday-next-TODO=\"DELAY\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
          (org-agenda-files
           (-remove (lambda (fn) (or
                                  (s-contains? "jet.org" fn)
                                  (s-contains? "cb.org" fn)
                                  ))
                    (file-expand-wildcards (concat org-directory "[a-z]*.org"))
                    ))))
        ("n" tags "next+LEVEL=1")
        ("s" tags "someday+LEVEL=1|TODO=\"WISH\"+LEVEL=1|next+LEVEL=1")
        ("q" tags-todo "quick-someday")
        ("B" tags "TODO=\"BLOCKED\"|TODO=\"PR\"")
        ("b" tags "buy")
        ("o" tags "TODO=\"DONE\"|TODO=\"CANCEL\"")
        ))

(setq org-capture-templates
      '(("t" "task" entry (file+headline "" "Tasks")
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
        ("n" "note" entry (file+headline "" "Notes")
         "* %<%Y-%m-%d> %?")
        ))

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
        (type "DELAY(l)" "BLOCKED(b)" "PR(g)" "ASSIGNED(a)" "|"  "WISH(w)" "CANCEL(k!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "light gray" :weight bold))
        ("WISH" . (:foreground "deep sky blue" :weight bold))
        ("DELAY" . (:foreground "chocolate" :weight bold))
        ("BLOCKED" . (:foreground "chocolate" :weight bold))
        ("PR" . (:foreground "chocolate" :weight bold))
        ("WIP" . (:foreground "magenta" :weight bold))
        ("ASSIGNED" . (:foreground "chocolate" :weight bold))
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

;; (use-package org-trello
;;   :ensure org-trello
;;   :config
;;   (progn
;;     (setq org-trello-files '("~/records/org-mode/cb.org"))
;; ))

;; collapse headers when entering new file
(setq org-agenda-inhibit-startup nil)
;; use default value of org-startup-folded

;; run every time emacs has been idle for 15 minutes
;;(run-with-idle-timer 900 t 'org-mobile-push)

;; for xmobar org-clock
(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt) txt)

(provide 'dmb-org)
