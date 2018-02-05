(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;              '("stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(measure-time "package-initialize" (package-initialize))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-verbose t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(provide 'dmb-package)
