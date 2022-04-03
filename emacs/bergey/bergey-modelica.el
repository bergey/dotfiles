;; Modelica mode
(setq load-path (cons "~/elisp" load-path))
(autoload 'modelica-mode "modelica-mode" "Modelica Editing Mode" t)
(setq auto-mode-alist (cons '("\.mo$" . modelica-mode) auto-mode-alist))

;; Enable Modelica browsing
(autoload 'mdc-browse "mdc-browse" "Modelica Class Browsing" t)
(autoload 'br-mdc "br-mdc" "Modelica Class Browsing" t)

(defvar br-env-lang-avector
  '[
    ("C++/C"   . "c++-")
    ("Eiffel"  . "eif-")
    ("Info"    . "info-")
    ("Java"    . "java-")
    ("Lisp"    . "clos-")
    ("Modelica" . "mdc-")
    ("Obj-C"   . "objc-")
    ("Python"  . "python-")
    ]
  "Association vector of elements of OO-Browser languages.")

;; Autostart OO-Browser (the installation is assumed under ~/oo-browser)
(setq load-path (append
                 '("~/oo-browser/"
                   "~/oo-browser/hypb/")
                 load-path))
;;(load "br-start")
;;(global-set-key "\C-c\C-o" 'oo-browser)

(require 'inf-modelica)
(add-hook 'modelica-mode-hook (lambda () (define-key mdc-mode-map (kbd "C-c C-l") 'inferior-modelica-load-file)))

(provide 'bergey-modelica)
