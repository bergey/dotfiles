;; This is inteded to be used together with
;;
;;   setxkbmap -layout "us(dvp)"
;;

(eval-after-load 'quail
  '(progn
     (add-to-list 'quail-keyboard-layout-alist
       '("dvp" . "\
                              \
  %&7[5{3}1(9=0*2)4+6]8!`#    \
  ;:,<.>pPyYfFgGcCrRlL/?@^    \
  aAoOeEuUiIdDhHtTnNsS-_\\|    \
  '\"qQjJkKxXbBmMwWvVzZ        \
                              "))
     (add-to-list 'quail-keyboard-layout-alist
       '("dvorak" . "\
                              \
  1!2@3#4$5%6^7&8*9(0)[{]}`~  \
  '\",<.>pPyYfFgGcCrRlL/?=+    \
  aAoOeEuUiIdDhHtTnNsS-_\\|    \
  ;:qQjJkKxXbBmMwWvVzZ         \
                             "))

     (add-to-list 'quail-keyboard-layout-alist
                  '("dvorak-swap" . "\
                              \
  !1@2#3$4%5^6&7*8(9)0[{]}`~  \
  '\",<.>pPyYfFgGcCrRlL/?=+    \
  aAoOeEuUiIdDhHtTnNsS-_\\|    \
  ;:qQjJkKxXbBmMwWvVzZ         \
                             "))
     (quail-set-keyboard-layout "dvorak-swap")))

(provide 'dmb-keyboard)
