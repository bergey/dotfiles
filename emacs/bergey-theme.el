(deftheme bergey
  "Created 2013-01-18.")

;; TODO remove unused faces
;; TODO consolidate colors again

;; must match rainbow-r-colors-alist at bottom of file or these names will not get highlighted
(setq
 bergey-colors
 '(
   ("b-red" . "#fe0b54")
   ("b-orange" . "#f6bb2b")
   ("b-green" . "#0be37a")
   ("b-cyan" . "#13f2f8")
   ("b-indigo" . "#5e84fe")
   ("b-violet" . "#ea84fe")
   ("dark-red" . "#8c0437")
   ("dark-orange" . "#8f4a31")
   ("dark-blue" . "#086783")
   ))

(defmacro bergey/theme (name &rest body)
  `(custom-theme-set-faces
    ,name
    ,@(-map
       (lambda (face)
         (let* (
                (face-name (car face))
                (definition (cadr face))
                (attrs (-map
                        ;; replace my local color aliases; emacs doesn't actually know them
                        (lambda (a) (or (cdr (assoc a bergey-colors)) a))
                        (if (stringp definition) `(:foreground ,definition) definition))))
           `(list (quote ,face-name) (list (list 't (list ,@attrs))))))
       body)))

(bergey/theme
 'bergey
 (fixed-pitch (:family "Inconsolata"))
 (monospace  (:family "Inconsolata"))
 (italic (:underline nil :slant 'italic))
 (cursor (:background "thistle"))

 (mode-line (:foreground "b-violet" :background "gray10"))
 (mode-line-inactive (:background "gray20"))
 (telephone-line-projectile "b-indigo")

 ;; programming language syntax, general
 (font-lock-builtin-face "b-indigo")
 (font-lock-comment-face "dark-red")
 (font-lock-constant-face "b-cyan")
 (font-lock-keyword-face "b-indigo")
 (font-lock-preprocessor-face (:inherit 'font-lock-builtin-face :foreground "b-cyan"))
 (font-lock-string-face "b-orange")
 (font-lock-type-face "b-violet")
 (font-lock-function-name-face "white")
 (font-lock-variable-name-face "white")
 (font-lock-doc-face "b-green")
 (font-lock-regexp-grouping-backslash "turquoise")
 (font-lock-regexp-grouping-construct "turquoise")

 (fic-face (:background "chocolate1" :foreground "black"))
 (mmm-default-submode-face (:background "gray1"))
 (show-paren-match (:background "steelblue3" :foreground "black"))
 (show-paren-mismatch (:background "purple" :foreground "black"))
 (whitespace-line (:underline "DeepSkyBlue" :foreground nil :background nil))

 ;; syntax for particular languages
 (web-mode-function-call-face "b-green")
 (font-mediawiki-sedate-face "gold")
 (highlight-quoted-quote "white")
 (highlight-quoted-symbol "#95f")
 ;; (haskell-constructor-face (:foreground ,b-red))
 (ledger-font-xact-highlight-face (:background "black"))
 (markdown-bold-face (:foreground "red" :inherit 'bold))
 (markdown-code-face (:inherit 'org-verbatim))
 (markdown-table-face (:inherit 'org-table))
 (proof-locked-face (:background "#333"))
 (shm-quarantine-face (:background "saddle brown")) ; structured haskell mode
 (web-mode-html-attr-name-face "white")
 (web-mode-html-tag-face "red")
 ;; other, non-programming modes
 (company-tooltip (:background "grey40" :foreground "white"))
 (company-tooltip-selection (:background "grey20" :foreground "white"))
 (dired-directory "gold")
 (git-annex-dired-annexed-available "spring green")
 (git-annex-dired-annexed-unavailable "chocolate")
 (magit-item-highlight (:background "#101010" :foreground "cyan"))

 ;; org-mode
 (org-verbatim "white")
 (org-code "dark-orange")
 (org-column (:family "Inconsolata" :height 90))
 (org-habit-alert-face (:foreground "black" :background "darkgoldenrod"))
 (org-tag (:background "#1f004d"))
 (org-table (:foreground "gray70" :inherit 'fixed-pitch))

 ;; outlines / headings
 (outline-1 "b-red") ;; lch 55 87 21
 (outline-2 "b-orange") ;; lch 80 75 80
 (outline-3 "b-green") ;; lch 80 75 150
 (outline-4 "b-cyan") ;; lch 87 50 200
 (outline-5 "b-indigo") ;; lch 57 64 282
 (outline-6 "b-violet") ;; lch 70 70 320

 ;; nested parens
 (rainbow-delimiters-depth-1-face "b-red") ;; lch 55 87 21
 (rainbow-delimiters-depth-2-face "b-orange") ;; lch 80 75 80
 (rainbow-delimiters-depth-3-face "b-green") ;; lch 80 75 150
 (rainbow-delimiters-depth-4-face "b-cyan") ;; lch 87 50 200
 (rainbow-delimiters-depth-5-face "b-indigo") ;; lch 57 64 282
 (rainbow-delimiters-depth-6-face "b-violet") ;; lch 70 70 320

 (error "salmon1")
 (warning "orange")
 (success "yellow green")
 (highlight (:foreground "white" :background "dark green"))
 (region (:foreground "black" :background "chartreuse1"))
 (isearch (:background "palevioletred2" :foreground "black"))
 (lazy-highlight (:background "paleturquoise4" :foreground "black"))
 (lazy-highlight (:background "gray25"))
 (link (:foreground "cyan" :background "#134"))
 (link-visited (:foreground "#59c" :background "#134"))
 (secondary-selection (:background "paleturquoise" :foreground "black"))
 (diff-added (:inherit 'diff-changed :foreground "green"))
 (diff-removed (:inherit 'diff-changed :foreground "red"))
 (ediff-current-diff-A (:foreground"light gray" :background "#553333"))
 (ediff-current-diff-B (:foreground"light gray" :background "#335533"))
 (smerge-refined-added (:foreground"#005000"))
 (eglot-highlight-symbol-face (:inherit nil) (:underline t))
 (erc-notice-face "LightSteelBlue4")

 (default (:inherit nil :stipple nil :background "black" :foreground "#1a7"
                    :inverse-video nil :box nil :strike-through
                    nil :overline nil
                    :underline nil :slant 'normal :weight 'normal
                    :width 'normal))
 )

;; https://colorjs.io/apps/picker/
(defun rgb (r g b) (format "#%02x%02x%02x" (* 2.55 r) (* 2.55 g) (* 2.55 b)))
;; (rgb 55.2 1.67 21.6) "dark-red" ;; lch 30 54 13
;; (rgb 56.2 29.1 19.4) "dark-orange" ;; lch 40 40 45
;; (rgb 14.3 42.5 0.57) "#246c01" ;; lch 40 57 130
;; (rgb 3.5 40.5 51.6) "#086783" ;; lch 40 29 233
;; (rgb 28 35.3 67.6) "#475aac"
;; (rgb 54.6 25.7 55.4) "#8b418d"

;; (set-face-foreground 'org-headline-done nil) ;; lch 70 70 320

;; (set-face-attribute 'powerline-active1 nil :inherit nil)

;; Local Variables:
;; eval: (rainbow-mode)
;; rainbow-r-colors: t
;; rainbow-r-colors-alist: (("b-red" . "#fe0b54") ("b-orange" . "#f6bb2b") ("b-green" . "#0be37a") ("b-cyan" . "#13f2f8") ("b-indigo" . "#5e84fe") ("b-violet" . "#ea84fe") ("dark-red" . "#8c0437") ("dark-orange" . "#8f4a31") ("dark-blue" . "#086783"))
;; End:
