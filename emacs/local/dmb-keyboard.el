(eval-after-load 'quail
  '(progn
     (add-to-list 'quail-keyboard-layout-alist
                  '("dvorak" . "\
                              \
  1!2@3#4$5%6^7&8*9(0)[{]}`~  \
  '\",<.>pPyYfFgGcCrRlL/?=+    \
  aAoOeEuUiIdDhHtTnNsS-_\\|    \
  ;:qQjJkKxXbBmMwWvVzZ         \
                             "))
     (quail-set-keyboard-layout "dvorak")))

(provide 'dmb-keyboard)
