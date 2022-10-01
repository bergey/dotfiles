(deftheme bergey
  "Created 2013-01-18.")

(custom-theme-set-faces 'bergey
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(italic ((t (:underline nil :slant italic))))
 '(monospace ((t (:family "Inconsolata"))))
 '(cursor ((((class color) (min-colors 89)) (:background "thistle"))))

 '(error ((((class color) (min-colors 89)) (:foreground "salmon1"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "orange"))))
 '(success ((((class color) (min-colors 89)) (:foreground "yellow green"))))
 '(highlight ((((class color) (min-colors 89)) (:foreground "white" :background "dark green"))))
 '(region ((((class color) (min-colors 89)) (:foreground "black" :background "chartreuse1"))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "gray25"))))
 '(link ((((class color) (min-colors 89)) (:foreground "cyan" :background "#134"))))
 '(link-visited ((((class color) (min-colors 89)) (:foreground "#59c" :background "#134"))))
 '(secondary-selection ((t (:background "paleturquoise" :foreground "black"))))

 ;; programming language syntax, general
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(ediff-current-diff-A ((t (:foreground"light gray" :background "#553333"))))
 '(ediff-current-diff-B ((t (:foreground"light gray" :background "#335533"))))
 '(fic-face ((t (:background "chocolate1" :foreground "black"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "LightSteelBlue"))))
 '(font-lock-builtin-face ((t (:foreground "gold"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-doc-face ((t (:foreground "#afa"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "turquoise"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "red"))))
 '(font-lock-keyword-face ((t (:foreground "deep sky blue"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "cyan"))))
 '(font-lock-regexp-grouping-backslash ((((class color) (min-colors 89)) (:foreground "turquoise"))))
 '(font-lock-regexp-grouping-construct ((((class color) (min-colors 89)) (:foreground "turquoise"))))
 '(font-lock-string-face ((t (:foreground "#bea"))))
 '(font-lock-type-face ((t (:foreground "#d4e"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(mmm-default-submode-face ((t (:background "gray1"))))
 '(show-paren-match ((((class color) (background dark)) (:background "steelblue3" :foreground "black"))))
 '(show-paren-mismatch ((((class color)) (:background "purple" :foreground "black"))))
 '(whitespace-line ((((class color) (background dark)) (:underline "DeepSkyBlue" :foreground nil :background nil))))
 '(highlight-indent-guides-even-face ((t (:family "Inconsolata" :background "#103"))))
 '(highlight-indent-guides-odd-face ((t (:family "Inconsolata" :background "gray10"))))
 '(highlight-indent-guides-character-face ((t (:foreground "#d70"))))

 ;; syntax for particular languages
 '(font-mediawiki-sedate-face ((t (:foreground "gold"))) t)
 '(highlight-quoted-quote ((((class color) (min-colors 89)) (:foreground "white"))))
 '(highlight-quoted-symbol ((t (:foreground "#95f"))))
 '(haskell-constructor-face ((t (:foreground "#95f"))))
 '(ledger-font-xact-highlight-face ((((class color) (background dark)) (:background "black"))))
 '(markdown-bold-face ((((class color) (background dark)) (:foreground "red" :inherit bold))))
 '(markdown-code-face ((t (:inherit org-verbatim))))
 '(markdown-table-face ((t (:inherit org-table))))
 '(proof-locked-face ((((class color) (background dark)) (:background "#333"))))
 '(shm-quarantine-face ((((class color) (background dark)) (:background "saddle brown")))) ; structured haskell mode
 '(web-mode-html-attr-name-face ((((class color) (background dark)) (:foreground "white"))))
 '(web-mode-html-tag-face ((((class color) (background dark)) (:foreground "red"))))

 ;; outlines / headings
 '(org-level-2 ((((class color) (min-colors 89)) (:foreground "white"))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :foreground "deep sky blue"))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(outline-6 ((t (:inherit font-lock-constant-face :foreground "burlywood"))))

 ;; nested parens
'(rainbow-delimiters-depth-1-face ((t (:foreground "#fe0b54")))) ;; lch 55 87 21
'(rainbow-delimiters-depth-2-face ((t (:foreground "#f6bb2b")))) ;; lch 80 75 80
'(rainbow-delimiters-depth-3-face ((t (:foreground "#0be37a")))) ;; lch 80 75 150
'(rainbow-delimiters-depth-4-face ((t (:foreground "#13f2f8")))) ;; lch 87 50 200
'(rainbow-delimiters-depth-5-face ((t (:foreground "#5e84fe")))) ;; lch 57 64 282
'(rainbow-delimiters-depth-6-face ((t (:foreground "#ea84fe")))) ;; lch 70 70 320

 ;;; other, non-programming modes
 '(company-tooltip ((((class color) (background dark)) (:background "grey40" :foreground "white"))))
 '(company-tooltip-selection ((((class color) (background dark)) (:background "grey20" :foreground "white"))))
 '(dired-directory ((((class color) (background dark)) (:foreground "gold"))))
 '(git-annex-dired-annexed-available ((((class color) (background dark)) (:foreground "spring green"))))
 '(git-annex-dired-annexed-unavailable ((((class color) (background dark)) (:foreground "chocolate"))))
 '(magit-item-highlight ((t (:background "#101010" :foreground "cyan"))))

 ;; org-mode
 '(org-code
   ((((class color grayscale) (min-colors 88) (background light))
     (:foreground "grey50"))
    (((class color grayscale) (min-colors 88) (background dark))
     (:foreground "grey70"))
    (((class color) (min-colors 8) (background light))
     (:foreground "green"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow"))))
 '(org-column ((t (:family "Inconsolata" :height 90))))
 '(org-habit-alert-face ((((class color) (background dark)) (:foreground "black" :background "darkgoldenrod"))))
 '(org-tag ((((class color) (background dark)) (:background "#1f004d"))))
 '(org-verbatim ((t (:foreground "gold"))))
 '(org-table ((t (:foreground "gray70" :inherit fixed-pitch))))


 ;; (smart-)mode-line
 '(mode-line-inactive ((t (:background "#222"))))
 '(sml/global ((t (:background "#005"))))
 '(mode-line ((t ("#005"))))

 ;; email messages (2019-06-18 not currently using)
 '(message-cited-text ((((class color) (min-colors 89)) (:foreground "SpringGreen3"))))
 '(message-header-cc ((((class color) (min-colors 89)) (:foreground "yellow green"))))
 '(message-header-name ((((class color) (min-colors 89)) (:foreground "dark turquoise"))))
 '(message-header-other ((t (:foreground "pale turquoise"))))
 '(message-header-subject ((((class color) (min-colors 89)) (:foreground "pale turquoise"))))
 '(message-header-to ((((class color) (min-colors 89)) (:foreground "pale green"))))
 '(message-separator ((((class color) (min-colors 89)) (:foreground "deep sky blue"))))

 ;; do I even use these?
 '(gnus-header-content ((((class color) (min-colors 89)) (:weight normal :foreground "yellow green"))))
 '(gnus-header-from ((((class color) (min-colors 89)) (:foreground "pale green"))))
 '(gnus-header-name ((((class color) (min-colors 89)) (:foreground "dark sea green"))))
 '(gnus-header-newsgroups ((((class color) (min-colors 89)) (:foreground "dark khaki"))))
 '(gnus-header-subject ((((class color) (min-colors 89)) (:foreground "pale turquoise"))))

 '(erc-notice-face ((((class color) (background dark)) (:foreground "LightSteelBlue4"))))

 '(default
    ((t (:inherit nil :stipple nil :background "black" :foreground "#1a7"
                  :inverse-video nil :box nil :strike-through
                  nil :overline nil
                  :underline nil :slant normal :weight normal
                  :width normal)))))

;; (set-face-attribute 'comint-highlight-prompt nil :inherit nil)

;; (set-face-foreground 'font-lock-string-face "#bea")
;; (set-face-background 'sml/pre-id-separator "#700")
;; (set-face-background 'powerline-active1 "#222")
;; (set-face-background 'powerline-active2 "#599")

;; https://colorjs.io/apps/picker/
(defun hex (r g b) (format "#%02x%02x%02x" r g b))
;; (defun rgb (r g b) (format "#%02x%02x%02x" (* 2.55 r) (* 2.55 g) (* 2.55 b)))
;; (format "%x" (* 2.55 100)) "fe" "ff"
;; (rgb 100 50 0) "#fe7f00"
;; (rgb 52.6 85.7 87.3) "#86dade" ;; lch 90 28 197
;; (rgb 50.6 83.6 83.3) "#81d5d4" ;; lch 80 28 197
;; (rgb 48.8 84.1 81.1) "#7cd6ce" ;; lch 80 30 190
;; (rgb 47.8 83.8 86.6) "#79d5dc" ;; lch 80 30 205
;; (rgb 38.3 3.28 14.7) "#610825" ;; lch 20 40 13
;; (rgb 69.5 12.3 29.8) "#b11f4b" ;; lch 40 60 13
;; (rgb 45.4 19.7 10.9) "#73321b" ;; lch 30 40 45
;; (rgb 14.1 31.5 6.37) "#235010" ;; lch 30 40 130
;; (rgb 23.8 41.1 15.7) "#3c6828" ;; lch 40 40 130
;; (rgb 5 42.4 28.5) "#0c6c48" ;; lch 40 36 160
;; (rgb 4 41.6 41.1) "#0a6a68" ;; lch 40 27 195

;; (rgb 55.2 1.67 21.6) "#8c0437" ;; lch 30 54 13
;; (rgb 56.2 29.1 19.4) "#8f4a31" ;; lch 40 40 45
;; (rgb 14.3 42.5 0.57) "#246c01" ;; lch 40 57 130
;; (rgb 3.5 40.5 51.6) "#086783" ;; lch 40 29 233
;; (rgb 28 35.3 67.6) "#475aac"
;; (rgb 54.6 25.7 55.4) "#8b418d"

;; (set-face-foreground 'indent-guides-depth-1 "#8c0437" )
;; (set-face-foreground 'indent-guides-depth-2  "#8f4a31" )
;; (set-face-foreground 'indent-guides-depth-3  "#246c01" )
;; (set-face-foreground 'indent-guides-depth-4  "#086783" )

;; (rgb 98.4 30.9 46) "#fa4e75" ;; lch 60 70 15
;; (rgb 91.3 40.4 23.2) "#e8673b" ;; lch 60 70 45
;; (rgb 28.8 64.2 13) "#49a321" ;; lch 60 70 130
;; (rgb 2.67 62 78.5) "#069ec8" ;; lch 60 40 233
;; (rgb 46.2 54.4 97.2) "#758af7" ;; lch 60 60 285
;; (rgb 80.3 42.5 81) "#cc6cce" ;; lch 60 60 325
;; (rgb 83.8 38.9 85.1) "#d563d9" ;; lch 60 70 325

;; (((((((((((())))))))))))
;; (set-face-foreground 'rainbow-delimiters-depth-1-face (rgb 99.8 4.49 33.1)) ;; lch 55 87 21
;; (set-face-foreground 'rainbow-delimiters-depth-2-face (rgb 96.6 73.6 17.2)) ;; lch 80 75 80
;; (set-face-foreground 'rainbow-delimiters-depth-3-face (rgb 4.4 89.3 48.2) ) ;; lch 80 75 150
;; (set-face-foreground 'rainbow-delimiters-depth-4-face (rgb 7.8 95.2 97.5)) ;; lch 87 50 200
;; (set-face-foreground 'rainbow-delimiters-depth-5-face (rgb 37 52 100)) ;; lch 57 64 282
;; (set-face-foreground 'rainbow-delimiters-depth-6-face (rgb 92 52 100)) ;; lch 70 70 320

;; (set-face-foreground 'font-lock-doc-face "#afa")
;; (set-face-attribute 'powerline-active1 nil :inherit nil)
;; (set-face-foreground 'highlight-indent-guides-character-face "#d70")

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
