;; useful bits of regex / elisp that I haven't used enough to justify
;; making into functions

;; convert type-name style record field names to underscore-style (Haskell)
(replace-regexp "createAlert\\([A-Z]\\)" "_\\,(downcase \\1)" )
