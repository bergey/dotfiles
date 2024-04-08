;; interactions with the OS at large

;; Use system default web browser
(if (memq system-type '(gnu gnu/linux))
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "chromium")
  ;; browse-url-default-browser calls lynx even if not installed, on Debian, -- 2017-02-23
  (setq browse-url-browser-function 'browse-url-default-browser)
  )

(provide 'bergey-os)
