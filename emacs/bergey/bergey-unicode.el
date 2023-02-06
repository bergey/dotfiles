(bind-key "C-*" iso-transl-ctl-x-8-map)
(bind-key "C-* RET" 'insert-char)

(mapcar
 (lambda (s) (define-key 'iso-transl-ctl-x-8-map (string (elt s 0)) (vector(elt s 1))))
 '("dδ" "DΔ" "GΓ" "hφ" "pπ" "rρ" "sσ" "SΣ" "tθ"
   "Rℝ"
   "T§" "M—" "N–" "#♯"
   "|‖"
   ))

;; TODO bind-key
(define-key 'iso-transl-ctl-x-8-map (kbd "<right>") "→")
(define-key 'iso-transl-ctl-x-8-map (kbd "<left>") "←")
(define-key 'iso-transl-ctl-x-8-map (kbd "<up>") "↑")
(define-key 'iso-transl-ctl-x-8-map (kbd "<down>") "↓")

;; greek input (for math & science, not language)
(require 'greek-unicode-insert)
(define-key 'iso-transl-ctl-x-8-map "g" 'greek-unicode-insert-map)
(define-key 'greek-unicode-insert-map "g" [?γ])

(provide 'bergey-unicode)
