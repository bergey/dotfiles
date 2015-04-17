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
            ))

(use-package org-archive
  :commands org-archive-subtree-default)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
;; set this without org-git-store-link, which is nifty but usually not what I want.
(setq org-store-link-functions
      '(org-irc-store-link
        org-info-store-link
        org-gnus-store-link
        org-docview-store-link
        org-bibtex-store-link
        org-bbdb-store-link
        org-notmuch-search-store-link
        org-notmuch-store-link))

(setq org-log-done nil)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in)))

(add-hook 'org-mode-hook '(lambda () (diminish 'org-indent-mode)))

(setq
 org-agenda-start-on-weekday nil
 org-blank-before-new-entry (quote ((heading) (plain-list-item)))
 org-columns-default-format "%45ITEM %TODO %3PRIORITY %5Effort{:} %CLOCKSUM"
 org-global-properties (quote (("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 4:00 8:00")))
 org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m org-git-link org-notmuch))
 org-special-ctrl-a/e t
 org-startup-indented t
 org-agenda-span 'day
 )

(setq org-agenda-custom-commands
      '(("p" tags "project+LEVEL=1")
        ("P" tags-todo "project")
        ("D" tags-todo "-someday-next-SCHEDULED>=2014-01-01+PRIORITY<\"C\"-PRIORITY=\"\"")
        ("d" tags-todo  "-someday-next-SCHEDULED>=2014-01-01")
        ("n" tags "next+LEVEL=1")
        ("w" tags "someday+LEVEL=1|TODO=\"WISH\"+LEVEL=1|next+LEVEL=1")
        ("q" tags-todo "quick-someday")
        ("B" tags "TODO=\"BLOCKED\"|TODO=\"PR\"")
        ("b" tags "buy")))

(use-package org-notmuch
  :commands org-notmuch-store-link
  :commands org-notmuch-search-store-link)

;; Set to the location of your Org files on your local system

(setq org-directory "~/records/org-mode")

;; TODO is the normal state for an actionable task
;; WIP and DONE are self-explanatory
;; BLOCKED - awaiting some other event, typically action by another person
;; DELAY - awaiting action by me, not currently actionable
;; PR - like BLOCKED, but more specific
;; CANCEL - won't do, but want to keep the record that I've decided not to
;; WISH - review these periodically, might go back on the TODO list
(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(p)" "|" "DONE(d!)")
        (type "BLOCKED(b)" "|" "PR(g)"  "DELAY(l)" "WISH(w)" "CANCEL(k!)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "light gray" :weight bold))
        ("WISH" . (:foreground "deep sky blue" :weight bold))
        ("DELAY" . (:foreground "chocolate" :weight bold))
        ))

;; (setq org-tags-exclude-from-inheritance '("project"))
;; (setq org-tags-exclude-from-inheritance nil)

(setq org-lowest-priority ?D)
(setq org-default-priority ?C)
(setq org-use-fast-todo-selection t)

(setq org-deadline-warning-days 0)
(setq org-imenu-depth 4)

(setq org-default-notes-file (concat org-directory "/capture.org"))
(bind-key "C-. c" 'org-capture)

(setq org-agenda-files "/home/bergey/records/org-mode/org-agenda-files")

;; don't show scheduled items in the agenda list of all TODO items
(setq org-agenda-todo-ignore-scheduled "all")

(setq org-outline-path-complete-in-steps nil
      org-completion-use-ido t)

;; idle in X11, not just in emacs
;; (setq org-clock-x11idle-program-name "xprintidle")

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path t)

;; (use-package org-trello
;;   :ensure org-trello
;;   )

;; collapse headers when entering new file
(setq org-agenda-inhibit-startup nil)
;; use default value of org-startup-folded

(provide 'dmb-org)
