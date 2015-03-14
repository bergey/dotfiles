(deftheme bergey
  "Created 2013-01-18.")

(defface monospace
  '((t (:family "Inconsolata" :height 90)))
  "Face for fixed-width text")

(custom-theme-set-faces
 'bergey
 '(cursor ((((class color) (min-colors 89)) (:background "thistle"))))
 '(error ((((class color) (min-colors 89)) (:foreground "salmon1"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "orange"))))
 '(shm-quarantine-face ((((class color) (background dark)) (:background "saddle brown"))))
 '(success ((((class color) (min-colors 89)) (:foreground "yellow green"))))
 '(highlight ((((class color) (min-colors 89)) (:foreground "white" :background "dark green"))))
 '(region ((((class color) (min-colors 89)) (:foreground "black" :background "Chartreuse1"))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "palevioletred2" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "paleturquoise4" :foreground "black"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:background "gray25"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "Chocolate1"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "turquoise"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "Red"))))
 '(font-lock-keyword-face ((t (:foreground "deep sky blue"))))
 '(font-lock-string-face ((t (:foreground "gold"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "aquamarine"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(link ((((class color) (min-colors 89)) (:underline t :foreground "cyan"))))
 '(link-visited ((((class color) (min-colors 89)) (:underline t :foreground "dark cyan"))))
 '(gnus-header-content ((((class color) (min-colors 89)) (:weight normal :foreground "yellow green"))))
 '(gnus-header-from ((((class color) (min-colors 89)) (:foreground "pale green"))))
 '(gnus-header-subject ((((class color) (min-colors 89)) (:foreground "pale turquoise"))))
 '(gnus-header-name ((((class color) (min-colors 89)) (:foreground "dark sea green"))))
 '(gnus-header-newsgroups ((((class color) (min-colors 89)) (:foreground "dark khaki"))))
 '(message-header-name ((((class color) (min-colors 89)) (:foreground "dark turquoise"))))
 '(message-header-cc ((((class color) (min-colors 89)) (:foreground "yellow green"))))
 '(message-header-other ((t (:foreground "pale turquoise"))))
 '(message-header-subject ((((class color) (min-colors 89)) (:foreground "pale turquoise"))))
 '(message-header-to ((((class color) (min-colors 89)) (:foreground "pale green"))))
 '(message-cited-text ((((class color) (min-colors 89)) (:foreground "SpringGreen3"))))
 '(message-separator ((((class color) (min-colors 89)) (:foreground "deep sky blue"))))
 '(org-level-2 ((((class color) (min-colors 89)) (:foreground "white"))))
 '(org-level-5 ((((class color) (min-colors 89)) (:foreground "magenta"))))
 '(font-lock-type-face ((t (:foreground "magenta"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(font-lock-builtin-face ((t (:foreground "gold"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "cyan"))))
 '(font-mediawiki-sedate-face ((t (:foreground "gold"))) t)
 '(magit-item-highlight ((t (:background "#101010" :foreground "cyan"))))
 '(mmm-default-submode-face ((t (:background "gray1"))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :foreground "deep sky blue"))))
 '(outline-6 ((t (:inherit font-lock-constant-face :foreground "burlywood"))))
 '(secondary-selection ((t (:background "paleturquoise" :foreground "black"))))
 '(show-paren-match ((((class color) (background dark)) (:background "steelblue3" :foreground "black"))))
 '(show-paren-mismatch ((((class color)) (:background "purple" :foreground "black"))))
 '(whitespace-line ((((class color) (background dark)) (:underline "DeepSkyBlue" :foreground nil :background nil))))
 '(git-annex-dired-annexed-unavailable ((((class color) (background dark)) (:foreground "dark orange"))))
 '(git-annex-dired-annexed-available ((((class color) (background dark)) (:foreground "spring green"))))
 '(dired-directory ((((class color) (background dark)) (:foreground "gold"))))
 '(company-tooltip ((((class color) (background dark)) (:background "grey40" :foreground "white"))))
 '(company-tooltip-selection ((((class color) (background dark)) (:background "grey20" :foreground "white"))))
 '(org-habit-alert-face ((((class color) (background dark)) (:foreground "black" :background "darkgoldenrod"))))
 '(org-verbatim
   ((((class color grayscale) (min-colors 88) (background light))
     (:foreground "grey50" :underline t))
    (((class color grayscale) (min-colors 88) (background dark))
     (:foreground "grey70" :underline t))
    (((class color) (min-colors 8) (background light))
     (:foreground "green" :underline t))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow" :underline t))))
 '(org-code
   ((((class color grayscale) (min-colors 88) (background light))
     (:foreground "grey50"))
    (((class color grayscale) (min-colors 88) (background dark))
     (:foreground "grey70"))
    (((class color) (min-colors 8) (background light))
     (:foreground "green"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow"))))
 '(rainbow-delimiters-depth-1-face ((((class color) (background dark)) (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((((class color) (background dark)) (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-3-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((((class color) (background dark)) (:foreground "magenta"))))
 '(rainbow-delimiters-depth-5-face ((((class color) (background dark)) (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((((class color) (background dark)) (:foreground "chocolate"))))
 '(rainbow-delimiters-depth-7-face ((((class color) (background dark)) (:foreground "medium purple"))))
 '(rainbow-delimiters-depth-8-face ((((class color) (background dark)) (:foreground "green"))))
 '(rainbow-delimiters-depth-9-face ((((class color) (background dark)) (:foreground "gray"))))
 '(ledger-font-xact-highlight-face ((((class color) (background dark)) (:background "black"))))
 '(erc-notice-face ((((class color) (background darc)) (:foreground "LightSteelBlue4"))))
 '(italic ((t (:underline nil :slant italic))))
 '(monospace ((t (:family "Inconsolata"))))
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "Chartreuse" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal)))))

(provide-theme 'bergey)
