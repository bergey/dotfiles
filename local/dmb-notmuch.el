;; *** mail ***
(use-package notmuch
  :commands notmuch-hello-query-counts
  :init (progn
          ;; for example:
          ;; (setq notmuch-saved-searches (quote (
          ;;                                      ("inbox" . "tag:inbox and not tag:list and not tag:deleted and not SPAM")
          ;;                                      ("diagrams" . "tag:diagrams and tag:inbox")
          ;;                                      ("org-mode" . "tag:inbox and to:emacs-orgmode@gnu.org"))))

          (use-package dash)

          ;; "list-orphan" is a saved search which captures any new mail tagged
          ;; list but not part of a more-specific saved-search

          (if (boundp 'notmuch-saved-searches)
              (setq notmuch-saved-searches
                    (-snoc
                     notmuch-saved-searches
                     (cons
                      "list-orphan"
                      (concat
                       "tag:list and tag:inbox and not ("
                       (-reduce
                        'concat
                        (-interpose
                         " or "
                         (--map
                          (replace-regexp-in-string
                           " and tag:inbox" ""
                           (replace-regexp-in-string
                            "tag:inbox and " ""
                            ;; old format (key . query)
                            ;; new plist format (:name name :query query)
                            (if (listp  (cdr it))
                                (plist-get it :query)
                              (cdr it))))
                          (cdr notmuch-saved-searches))))
                       ")"
                       )))))

            ;; colorization by indent
            (defface message-double-quoted-text
              '((default (:foreground "deep sky blue")))
              "Current email is a reply to a reply to the quoted text"
              :group 'message)
            (defface message-triple-quoted-text
              '((t (:foreground "violet red")))
              "Current email includes replies three levels deep"
              :group 'message)
            (defface message-multiply-quoted-text
              '((default (:foreground "SpringGreen2")))
              "Current email includes replies four levels deep"
              :group 'message)

            (defun quoted-regex (n)
              "Return a string which matches email quoted n levels deep"
              (eval (append '(concat "^") (make-list 3 "[ \\t]*>") (list ".*$"))))

            (defface notmuch-show-known-addr
              '(
                (((class color) (background dark)) :foreground "spring green")
                (((class color) (background light)) :background "spring green" :foreground "black"))
              "Face for sender or recipient already listed in bbdb"
              :group 'notmuch-show
              :group 'notmuch-faces)

            (defface notmuch-show-unknown-addr
              '(
                (((class color) (background dark)) :foreground "dark orange")
                (((class color) (background light)) :background "gold" :foreground "black"))
              "Face for sender or recipient not listed in bbdb"
              :group 'notmuch-show
              :group 'notmuch-faces)

          (defun helm-notmuch-count-searches ()
            (let ((searches (notmuch-hello-query-counts
                             (if (and
                                  (boundp 'notmuch-saved-search-sort-function)
                                  notmuch-saved-search-sort-function)
                                 (funcall notmuch-saved-search-sort-function
                                          notmuch-saved-searches)
                               notmuch-saved-searches)
                             :show-empty-searches
                             (if (boundp 'notmuch-show-empty-saved-searches)
                                 notmuch-show-empty-saved-searches)
                             nil)))
              (mapcar (lambda (s) (let
                                      ((name (plist-get s :name))
                                       (query (plist-get s :query))
                                       (msg-count (plist-get s :count)))
                                    (cons (format "%8s %s" (notmuch-hello-nice-number msg-count) name) query)))
                      searches)))

          (defvar helm-notmuch-saved-searches
            '((name . "Notmuch Mail Searches")
              (candidates . helm-notmuch-count-searches)
              (action . (lambda (query) (notmuch-search query notmuch-search-oldest-first)))))

          (defun helm-notmuch-saved-searches ()
            (interactive)
            (helm-other-buffer 'helm-notmuch-saved-searches "*Helm Notmuch*")))

  :bind (("C-. m" . helm-notmuch-saved-searches)
        ("C-. n" . notmuch-mua-new-mail)
        ("C-. s" . notmuch-search))

  :config (progn

            (defun bergey-notmuch-toggle-bindings (key taga)
              (lexical-let ((lkey key) (ltag taga))
                (define-key notmuch-show-mode-map lkey
                  (lambda ()
                    ;;  (concat "toggle " taga " tag for message")
                    (interactive)
                    (bergey-notmuch-show-toggle-tag-archive ltag)))
                (define-key notmuch-search-mode-map lkey
                  (lambda ()
                    ;;  (concat "toggle " taga " tag for message or region")
                    (interactive)
                    (bergey-notmuch-search-toggle-tag-archive ltag)))))

            (bergey-notmuch-toggle-bindings "d" "deleted")
            (bergey-notmuch-toggle-bindings "s" "sched")
            (bergey-notmuch-toggle-bindings "D" "todo")
            (bergey-notmuch-toggle-bindings "F" "filter")
            (bergey-notmuch-toggle-bindings "M" "muted")

          (defun bergey-notmuch-show-toggle-tag-archive (tag)
            "toggle specified tag; if adding tag, also remove inbox tag"
            (if (member tag (notmuch-show-get-tags))
                (notmuch-show-tag-message (concat "-" tag))
              (notmuch-show-tag-message (concat "+" tag)))
            (notmuch-show-archive-message-then-next-or-next-thread))

          (defun bergey-notmuch-search-tag (tagc)
            "Change tags for the currently selected thread or region.  Like
notmuch-search-tag, but always using the region if active"
            (let* ((beg (if (region-active-p) (region-beginning) (point)))
                   (end (if (region-active-p) (region-end) (point)))
                   (search-string (notmuch-search-find-stable-query-region beg end t)))
              (funcall 'notmuch-search-tag tagc beg end)))

          (defun bergey-notmuch-search-toggle-tag-archive (tagb)
            "toggle specified tag; if adding tag, also remove inbox tag"
            (if (member tagb (notmuch-search-get-tags))
                (bergey-notmuch-search-tag (list (concat "-" tagb)))
              (bergey-notmuch-search-tag (list "-inbox" (concat "+" tagb))))
            (notmuch-search-next-thread))

          (defun bergey-notmuch-search-save-search ()
            "save the current dislpayed search"
            (interactive)
            (let ( (name (read-from-minibuffer "Save this search as: ")))
              (push (cons name notmuch-search-query-string) notmuch-saved-searches)))

          (defun rss-load-url () "Load the source page for this email in an external browser"
            (interactive)
            (save-excursion
              ;; move to url
              (goto-char (point-max))
              (search-backward "URL:")
              (forward-word 2)
              ;; read url even if split over multiple lines
              (let ((url (buffer-substring (point) (point-max))))
                ;; load url
                ;; (command-execute 'browse-url))))
                (browse-url url))))

          ;; bbdb
          ;; interactive versions of these functions for testing
          (defun bbdb/header-test ()
            (interactive)
            (message (notmuch-show-get-from)))

          (defun bbdb/prop-test ()
            (interactive)
            (message (notmuch-show-get-message-properties)))

          (defun bbdb/move-test ()
            (interactive)
            (notmuch-show-move-to-message-top))

          (defun bbdb/extent-test ()
            (interactive)
            (message (notmuch-show-message-extent)))

          ;; functions to add sender / recipients to BBDB

          (defun notmuch-bbdb/snarf-headers (headers)
            ;; Helper function to avoid code duplication in the two below
            ;; headers should have the same format as bbdb-get-addresses-headers

            ;; bbdb-get-addresses reads these
            ;; Ugh, pass-by-global
            (let* ((all-addrs (bbdb-get-addresses nil nil 'notmuch-bbdb/get-header-content))
                   (bbdb-gag-messages t) ; suppress m/n processed message
                   (addrs (assoc headers all-addrs))
                   )
              ;;(message "%s" addrs)
              (bbdb-update-records
               (list addrs)
               t
               t)))

          (defun notmuch-bbdb/snarf-from ()
            "Import the sender of the current message into BBDB"
            (interactive)
            (notmuch-bbdb/snarf-headers 'authors))

          (defun notmuch-bbdb/snarf-to ()
            "Import all recipients of the current message into BBDB"
            (interactive)
            (notmuch-bbdb/snarf-headers 'recipients))

          (defvar notmuch-bbdb/header-by-name
            ;; both are case sensitive
            '( ("From" . :From)
               ("To" . :To)
               ("CC" . :Cc)
               ("BCC" . :Bcc)
               ("Resent-From" . nil)
               ("Reply-To" . nil)
               ("Resent-To" . nil)
               ("Resent-CC" . nil))
            "Alist for dispatching header symbols as used by notmuch-show-get-header
from strings as used by bbdb-get-addresses")

          (defun notmuch-bbdb/get-header-content (name)
            (notmuch-show-get-header (cdr (assoc name notmuch-bbdb/header-by-name))))

;;; color coding addresses by bbdb status

          ;; code taken from bbdb-gnus.el
          (defun bbdb/notmuch-known-sender ()
                                        ; plist-get because we cannot use notmuch-show-get-header before formatting
            (let* ((from (plist-get headers :From))
                   (splits (mail-extract-address-components from))
                   (name (car splits))
                   (net (cadr splits))
                   (record (and splits
                                (bbdb-search-simple
                                 name
                                 (if (and net bbdb-canonicalize-net-hook)
                                     (bbdb-canonicalize-address net)
                                   net)))))
              (and record net (member (downcase net) (bbdb-record-net record)))))

          (defun bbdb/check-known-sender ()
            (interactive)
            (if (bbdb/notmuch-known-sender) (message "Sender is known") (message "Sender is not known")))

          (defun dmb-delete-email-name-part ()
            "Delete the name part of an email from or to header on the
  current line, leaving only the address part (and removing the
  <>)"
            (interactive)
            (let ((ln (bounds-of-thing-at-point 'line)))
              ;; (message ln)
              (replace-regexp ".*<\\(.*\\)>" "\\1" nil (car ln) (cdr ln))))

          (defun dmb-notmuch-show-stash-email-addr ()
            "Copy the email part (between <>) of From line."
            (interactive)
            ;; (notmuch-show-stash-from)
            ;; (setq kill-ring (cons (replace-regexp-in-string ".*<\\(.*\\)>" "\\1" (car kill-ring)) (cdr kill-ring)))
            (notmuch-common-do-stash (replace-regexp-in-string ".*<\\(.*\\)>" "\\1" (notmuch-show-get-from)))
            )

            (setq notmuch-hello-sections (quote (notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags)))

            (setq message-dont-reply-to-names '("bergey@mit.edu" "bergey@alum" "bergey@fastmail" "dmbergey@gmail" "bergey@thehacktory.org bergey@teallabs.org"))
            ;; for msmtp
            (setq message-send-mail-function 'message-send-mail-with-sendmail)
            (setq sendmail-program "msmtp")
            (setq mail-specify-envelope-from t)
            (setq message-sendmail-envelope-from 'header)
            (setq mail-envelope-from 'header)

            (setq message-kill-buffer-on-exit t) ; kill sent mail buffer on send

            (bind-key "g" 'notmuch-hello-update notmuch-hello-mode-map)

            (bind-key "C-c C-o" 'browse-url notmuch-show-mode-map)
            (bind-key "c e" 'dmb-notmuch-show-stash-email-addr notmuch-show-mode-map)
            (bind-key "F" 'notmuch-show-forward-message notmuch-show-mode-map)


            ;; t is easier to type than +
            (bind-key "t" 'notmuch-show-add-tag notmuch-show-mode-map)
            (bind-key "t" 'notmuch-search-add-tag notmuch-search-mode-map)
            (bind-key "T" 'notmuch-search-filter-by-tag notmuch-search-mode-map)
            (bind-key "$" '(lambda () (interactive (switch-to-buffer "*Notmuch errors*"))) notmuch-search-mode-map)

            (bind-key "g" 'notmuch-poll-and-refresh-this-buffer notmuch-search-mode-map)
            (bind-key "S" 'bergey-notmuch-search-save-search notmuch-search-mode-map)

            (define-prefix-command 'notmuch-bbdb-keymap)
            (bind-key "B" 'notmuch-bbdb-keymap notmuch-show-mode-map)
            (bind-key "f" 'notmuch-bbdb/snarf-from notmuch-bbdb-keymap)
            (bind-key "t" 'notmuch-bbdb/snarf-to notmuch-bbdb-keymap)

            ;;; bbdb

            (autoload 'footnote-mode "footnote" nil t)

            (add-hook 'message-mode-hook
                      (lambda ()
                        (font-lock-add-keywords
                         nil
                         '(("[ \t]*>[ \t]*>[ \t]*>[ \t]*>.*$"
                            (0 'message-multiply-quoted-text))
                           ("[ \t]*>[ \t]*>[ \t]*>.*$"
                            (0 'message-triple-quoted-text))
                           ("^[ \t]*>[ \t]*>.*$"
                            (0 'message-double-quoted-text))
                           ("^\+"
                            (0 'magit-diff-add))
                           ("^-"
                            (0 'magit-diff-del))
                           ))
                        (footnote-mode 1)))


            (require 'gnus-art)

            (setq message-citation-line-function 'message-insert-formatted-citation-line)
            (setq message-citation-line-format "On %Y-%m-%d at %R, %f wrote:")

            ;; crypto
            ;; deactivate until I have a teallabs.org key!
            ;;(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

            ;; from http://notmuchmail.org/emacstips
            (defun  notmuch-show-bounce-message (&optional address)
              "Bounce the current message."
              (interactive "sBounce To: ")
              (notmuch-show-view-raw-message)
              (message-resend address))
            (bind-key "b" 'notmuch-show-bounce-message notmuch-show-mode-map)

            (setq mm-default-directory "~/tmp/")

            ;; TODO try using notmuch-user-name & notmuch-user-primary-email as in notmuch-mua-prompt-for-sender
            (defvar notmuch-default-sender "Daniel Bergey <bergey@teallabs.org>"
              "Default email account to use in notmuch-mua-new-reply")

            ;; Default to sending from alum address
            ;; I'd prefer to only rewrite fastmail addr to alum, but that would require
            ;; patching notmuch-mua-reply, which is huge, and more likely to change upstream
            (defun notmuch-mua-new-reply (query-string &optional prompt-for-sender reply-all)
              "Invoke the notmuch reply window."
              (interactive "P")
              (let ((sender
                     (if prompt-for-sender
                         (notmuch-mua-prompt-for-sender)
                       notmuch-default-sender)))
                (notmuch-mua-reply query-string sender reply-all)))

            (let ((ln (bounds-of-thing-at-point 'line))) (message "%s" (cdr ln)) ())

            (setq notmuch-tree-show-out t)

            ;; w3m at least results in legible text on black background
            (setq mm-text-html-renderer 'w3m)
            (setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
            (bind-key "g" 'w3m-lnum-follow notmuch-show-mode-map)
            (bind-key "f" 'w3m-lnum-external-view-this-url notmuch-show-mode-map)

            ;; override function from notmuch-show.el
            (defun notmuch-show-fontify-header ()
              (let ((face (cond
                           ((looking-at "[Tt]o:")
                            'message-header-to)
                           ((looking-at "[Bb]?[Cc][Cc]:")
                            'message-header-cc)
                           ((looking-at "[Ss]ubject:")
                            'message-header-subject)
                           ((looking-at "[Ff]rom:")
                            'message-header-from)
                           (t
                            'message-header-other))))

                (overlay-put (make-overlay (point) (re-search-forward ":"))
                             'face 'message-header-name)
                (overlay-put (make-overlay (point) (re-search-forward ".*$"))
                             'face face)))

            ;; override function from notmuch-show
;;             (defun notmuch-show-insert-headerline (headers date tags depth)
;;               "Insert a notmuch style headerline based on HEADERS for a
;; message at DEPTH in the current thread."
;;               (let ((start (point))
;;                     (face (if (bbdb/notmuch-known-sender) 'notmuch-show-known-addr 'notmuch-show-unknown-addr))
;;                     (end-from))
;;                 (insert (notmuch-show-spaces-n (* notmuch-show-indent-messages-width depth))
;;                         (notmuch-show-clean-address (plist-get headers :From)))
;;                 (setq end-from (point))
;;                 (insert
;;                  " ("
;;                  date
;;                  ") ("
;;                  (propertize (mapconcat 'identity tags " ")
;;                              'face 'notmuch-tag-face)
;;                  ")\n")
;;                 (overlay-put (make-overlay start (point)) 'face 'notmuch-message-summary-face)
;;                 (save-excursion
;;                   (goto-char start)
;;                   (overlay-put (make-overlay start end-from) 'face face))))

            ))

(provide 'dmb-notmuch)
