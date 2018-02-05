(require 'package)


(setq package-enable-at-startup nil)
(measure-time "package-initialize" (package-initialize))

(eval-when-compile (require 'use-package))

(setq use-package-verbose t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(provide 'dmb-package)
