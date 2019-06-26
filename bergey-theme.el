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
 '(font-lock-string-face ((t (:foreground "#6b3"))))
 '(font-lock-type-face ((t (:foreground "#d4e"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(mmm-default-submode-face ((t (:background "gray1"))))
 '(show-paren-match ((((class color) (background dark)) (:background "steelblue3" :foreground "black"))))
 '(show-paren-mismatch ((((class color)) (:background "purple" :foreground "black"))))
 '(whitespace-line ((((class color) (background dark)) (:underline "DeepSkyBlue" :foreground nil :background nil))))
 '(highlight-indent-guides-even-face ((t (:family "Inconsolata" :background "#103"))))
 '(highlight-indent-guides-odd-face ((t (:family "Inconsolata" :background "gray10"))))
 '(highlight-indent-guides-character-face ((t (:foreground "#530"))))

 ;; syntax for particular languages
 '(font-mediawiki-sedate-face ((t (:foreground "gold"))) t)
 '(highlight-quoted-quote ((((class color) (min-colors 89)) (:foreground "white"))))
 '(highlight-quoted-symbol ((t (:foreground "#95f"))))
 '(haskell-constructor-face ((t (:foreground "#95f"))))
 '(ledger-font-xact-highlight-face ((((class color) (background dark)) (:background "black"))))
 '(markdown-bold-face ((((class color) (background dark)) (:foreground "red" :inherit bold))))
 '(markdown-code-face ((t (:inherit org-verbatim))))
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
 '(rainbow-delimiters-depth-1-face ((((class color) (background dark)) (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((((class color) (background dark)) (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-3-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((((class color) (background dark)) (:foreground "magenta"))))
 '(rainbow-delimiters-depth-5-face ((((class color) (background dark)) (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((((class color) (background dark)) (:foreground "chocolate"))))
 '(rainbow-delimiters-depth-7-face ((((class color) (background dark)) (:foreground "medium purple"))))
 '(rainbow-delimiters-depth-8-face ((((class color) (background dark)) (:foreground "green"))))
 '(rainbow-delimiters-depth-9-face ((((class color) (background dark)) (:foreground "gray"))))

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

;; (set-face-foreground 'font-lock-string-face "#6b3")
;; (set-face-background 'font-lock-string-face nil)
;; (set-face-foreground 'font-lock-doc-face "#afa")
;; (set-face-attribute 'markdown-code-face :inherit nil)

(provide-theme 'bergey)

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
