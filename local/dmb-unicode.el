(define-key 'iso-transl-ctl-x-8-map "d" [?δ])
(define-key 'iso-transl-ctl-x-8-map "D" [?Δ])
(define-key 'iso-transl-ctl-x-8-map "R" [?ℝ])

(define-key 'iso-transl-ctl-x-8-map "t" [?θ])
(define-key 'iso-transl-ctl-x-8-map "h" [?φ])
(define-key 'iso-transl-ctl-x-8-map "p" [?π])
(define-key 'iso-transl-ctl-x-8-map "T" [?§])
(define-key 'iso-transl-ctl-x-8-map "s" [?σ])
(define-key 'iso-transl-ctl-x-8-map "S" [?Σ])
(define-key 'iso-transl-ctl-x-8-map "r" [?ρ])
(define-key 'iso-transl-ctl-x-8-map "G" [?Γ])

(define-key 'iso-transl-ctl-x-8-map "M" [?—])
(define-key 'iso-transl-ctl-x-8-map "#" [?♯])

;; greek input (for math & science, not language)
(require 'greek-unicode-insert)
(define-key 'iso-transl-ctl-x-8-map "g" 'greek-unicode-insert-map)
(define-key 'greek-unicode-insert-map "g" [?γ])

(provide 'dmb-unicode)
