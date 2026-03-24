(deftheme bergey
  "Created 2013-01-18.")

;; TODO rainbow-mode highlighting for my color names
;; TODO macro wrap custom-theme-set-faces - display spec t, string implies :foreground
;; TODO remove unused faces
;; TODO consolidate colors again

;; (setq
;;  b-red "#fe0b54" ;; lch 55 87 21
;;  b-orange "#f6bb2b" ;; lch 80 75 80
;;  b-green "#0be37a" ;; lch 80 75 150
;;  b-cyan "#13f2f8" ;; lch 87 50 200
;;  b-indigo "#5e84fe" ;; lch 57 64 282
;;  b-violet "#ea84fe" ;; lch 70 70 320
;;  dark-red "#8c0437"  ;; lch 30 54 13
;;  dark-orange "#8f4a31" ;; lch 40 40 45
;;  dark-blue "#086783" ;; lch 40 29 233
;;  )

(defmacro bergey/theme (name &rest body)
  `(custom-theme-set-faces
    ,name
    ,@(--map
       (let* (
              (face-name (car it))
              (definition (cadr it))
              (attrs (if (stringp definition) `(:foreground ,definition) definition)))
         `(list (quote ,face-name) (list (list 't (list ,@attrs)))))
       body)))

(bergey/theme 'bergey
              (fixed-pitch (:family "Inconsolata"))
              (monospace  (:family "Inconsolata"))
              (italic (:underline nil :slant 'italic))
              (cursor (:background "thistle"))

              (mode-line (:foreground "#ea84fe" :background "gray10"))
              (mode-line-inactive (:background "gray20"))
              (telephone-line-projectile "#5e84fe")

              ;; programming language syntax, general
              (font-lock-builtin-face "#5e84fe")
              (font-lock-comment-face "#8c0437")
              (font-lock-constant-face "#13f2f8")
              (font-lock-keyword-face "#5e84fe")
              (font-lock-preprocessor-face (:inherit 'font-lock-builtin-face :foreground "#13f2f8"))
              (font-lock-string-face "#f6bb2b")
              (font-lock-type-face "#ea84fe")
              (font-lock-function-name-face "white")
              (font-lock-variable-name-face "white")
              (font-lock-doc-face "#0be37a")
              (font-lock-regexp-grouping-backslash "turquoise")
              (font-lock-regexp-grouping-construct "turquoise")

              (fic-face (:background "chocolate1" :foreground "black"))
              (mmm-default-submode-face (:background "gray1"))
              (show-paren-match (:background "steelblue3" :foreground "black"))
              (show-paren-mismatch (:background "purple" :foreground "black"))
              (whitespace-line (:underline "DeepSkyBlue" :foreground nil :background nil))

              ;; syntax for particular languages
              (web-mode-function-call-face "#0be37a")
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
              (org-code "#8f4a31")
              (org-column (:family "Inconsolata" :height 90))
              (org-habit-alert-face (:foreground "black" :background "darkgoldenrod"))
              (org-tag (:background "#1f004d"))
              (org-table (:foreground "gray70" :inherit 'fixed-pitch))

              ;; outlines / headings
              (outline-1 "#fe0b54") ;; lch 55 87 21
              (outline-2 "#f6bb2b") ;; lch 80 75 80
              (outline-3 "#0be37a") ;; lch 80 75 150
              (outline-4 "#13f2f8") ;; lch 87 50 200
              (outline-5 "#5e84fe") ;; lch 57 64 282
              (outline-6 "#ea84fe") ;; lch 70 70 320

              ;; nested parens
              (rainbow-delimiters-depth-1-face "#fe0b54") ;; lch 55 87 21
              (rainbow-delimiters-depth-2-face "#f6bb2b") ;; lch 80 75 80
              (rainbow-delimiters-depth-3-face "#0be37a") ;; lch 80 75 150
              (rainbow-delimiters-depth-4-face "#13f2f8") ;; lch 87 50 200
              (rainbow-delimiters-depth-5-face "#5e84fe") ;; lch 57 64 282
              (rainbow-delimiters-depth-6-face "#ea84fe") ;; lch 70 70 320

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
;; (rgb 55.2 1.67 21.6) "#8c0437" ;; lch 30 54 13
;; (rgb 56.2 29.1 19.4) "#8f4a31" ;; lch 40 40 45
;; (rgb 14.3 42.5 0.57) "#246c01" ;; lch 40 57 130
;; (rgb 3.5 40.5 51.6) "#086783" ;; lch 40 29 233
;; (rgb 28 35.3 67.6) "#475aac"
;; (rgb 54.6 25.7 55.4) "#8b418d"

;; (set-face-foreground 'org-headline-done nil) ;; lch 70 70 320

;; (set-face-attribute 'powerline-active1 nil :inherit nil)

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
