(deftheme bergey
  "Created 2013-01-18.")

(setq
 b-red "#fe0b54" ;; lch 55 87 21
 b-orange "#f6bb2b" ;; lch 80 75 80
 b-green "#0be37a" ;; lch 80 75 150
 b-cyan "#13f2f8" ;; lch 87 50 200
 b-indigo "#5e84fe" ;; lch 57 64 282
 b-violet "#ea84fe" ;; lch 70 70 320
 dark-red "#8c0437"  ;; lch 30 54 13
 dark-orange "#8f4a31" ;; lch 40 40 45
 dark-blue "#086783" ;; lch 40 29 233
 )

(custom-theme-set-faces 'bergey
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(italic ((t (:underline nil :slant italic))))
 '(monospace ((t (:family "Inconsolata"))))
 '(cursor ((t (:background "thistle"))))

 '(error ((t (:foreground "salmon1"))))
 '(warning ((t (:foreground "orange"))))
 '(success ((t (:foreground "yellow green"))))
 '(highlight ((t (:foreground "white" :background "dark green"))))
 '(region ((t (:foreground "black" :background "chartreuse1"))))
 '(isearch ((t (:background "palevioletred2" :foreground "black"))))
 '(lazy-highlight ((t (:background "paleturquoise4" :foreground "black"))))
 '(lazy-highlight ((t (:background "gray25"))))
 '(link ((t (:foreground "cyan" :background "#134"))))
 '(link-visited ((t (:foreground "#59c" :background "#134"))))
 '(secondary-selection ((t (:background "paleturquoise" :foreground "black"))))

 ;; programming language syntax, general
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(ediff-current-diff-A ((t (:foreground"light gray" :background "#553333"))))
 '(ediff-current-diff-B ((t (:foreground"light gray" :background "#335533"))))
 '(smerge-refined-added ((t (:foreground"#005000"))))
 '(eglot-highlight-symbol-face ((t (:inherit nil) (:underline t))))

 `(font-lock-builtin-face ((t (:foreground ,b-indigo))))
 `(font-lock-comment-face ((t (:foreground ,dark-red))))
 `(font-lock-constant-face ((t (:foreground ,b-cyan))))
 `(font-lock-keyword-face ((t (:foreground ,b-indigo))))
 `(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground ,b-cyan))))
 `(font-lock-string-face ((t (:foreground ,b-orange))))
 `(font-lock-type-face ((t (:foreground ,b-violet))))
 '(font-lock-function-name-face ((t (:foreground "white"))))
 '(font-lock-variable-name-face ((t (:foreground "white"))))
 `(font-lock-doc-face ((t (:foreground ,b-green))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "turquoise"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "turquoise"))))

 '(fic-face ((t (:background "chocolate1" :foreground "black"))))
 '(mmm-default-submode-face ((t (:background "gray1"))))
 '(show-paren-match ((t (:background "steelblue3" :foreground "black"))))
 '(show-paren-mismatch ((t (:background "purple" :foreground "black"))))
 '(whitespace-line ((t (:underline "DeepSkyBlue" :foreground nil :background nil))))

 ;; syntax for particular languages
 `(web-mode-function-call-face ((t (:foreground ,b-green))))
 '(font-mediawiki-sedate-face ((t (:foreground "gold"))) t)
 '(highlight-quoted-quote ((t (:foreground "white"))))
 '(highlight-quoted-symbol ((t (:foreground "#95f"))))
;; `(haskell-constructor-face ((t (:foreground ,b-red))))
 '(ledger-font-xact-highlight-face ((t (:background "black"))))
 '(markdown-bold-face ((t (:foreground "red" :inherit bold))))
 '(markdown-code-face ((t (:inherit org-verbatim))))
 '(markdown-table-face ((t (:inherit org-table))))
 '(proof-locked-face ((t (:background "#333"))))
 '(shm-quarantine-face ((t (:background "saddle brown")))) ; structured haskell mode
 '(web-mode-html-attr-name-face ((t (:foreground "white"))))
 '(web-mode-html-tag-face ((t (:foreground "red"))))

 ;; outlines / headings
 '(outline-1 ((t (:foreground "#fe0b54")))) ;; lch 55 87 21
 '(outline-2 ((t (:foreground "#f6bb2b")))) ;; lch 80 75 80
 '(outline-3 ((t (:foreground "#0be37a")))) ;; lch 80 75 150
 '(outline-4 ((t (:foreground "#13f2f8")))) ;; lch 87 50 200
 '(outline-5 ((t (:foreground "#5e84fe")))) ;; lch 57 64 282
 '(outline-6 ((t (:foreground "#ea84fe")))) ;; lch 70 70 320

 ;; nested parens
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#fe0b54")))) ;; lch 55 87 21
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#f6bb2b")))) ;; lch 80 75 80
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#0be37a")))) ;; lch 80 75 150
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#13f2f8")))) ;; lch 87 50 200
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#5e84fe")))) ;; lch 57 64 282
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#ea84fe")))) ;; lch 70 70 320

 ;;; other, non-programming modes
 '(company-tooltip ((t (:background "grey40" :foreground "white"))))
 '(company-tooltip-selection ((t (:background "grey20" :foreground "white"))))
 '(dired-directory ((t (:foreground "gold"))))
 '(git-annex-dired-annexed-available ((t (:foreground "spring green"))))
 '(git-annex-dired-annexed-unavailable ((t (:foreground "chocolate"))))
 '(magit-item-highlight ((t (:background "#101010" :foreground "cyan"))))

 ;; org-mode
 '(org-verbatim ((t (:foreground "white"))))
 `(org-code ((t (:foreground ,dark-orange))))
 '(org-column ((t (:family "Inconsolata" :height 90))))
 '(org-habit-alert-face ((t (:foreground "black" :background "darkgoldenrod"))))
 '(org-tag ((t (:background "#1f004d"))))
 '(org-table ((t (:foreground "gray70" :inherit fixed-pitch))))

 '(erc-notice-face ((t (:foreground "LightSteelBlue4"))))

 '(mode-line ((t (:foreground "#ea84fe" :background "gray10"))))
 '(mode-line-inactive ((t (:background "gray20"))))
 '(telephone-line-projectile ((t (:foreground "#5e84fe"))))

 '(default
    ((t (:inherit nil :stipple nil :background "black" :foreground "#1a7"
                  :inverse-video nil :box nil :strike-through
                  nil :overline nil
                  :underline nil :slant normal :weight normal
                  :width normal)))))

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
