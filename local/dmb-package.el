(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(measure-time "package-initialize" (package-initialize))

;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t))))
  (require package))

(require-package 'use-package)

(setq use-package-verbose t)

(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(provide 'dmb-package)
