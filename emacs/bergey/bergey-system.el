;; TODO use `nmcli con up' if connection already exists
;; maybe add a separate function to pick among known networks
(defun wifi-connect ()
  "Prompt for a choice among available 802.11 SSIDS, and connect."
  (interactive)
  (let* (
         (ssid-options (with-temp-buffer
                         (call-process "nmcli" nil t nil "-t" "--fields" "SSID" "dev" "wifi" "list")
                         (s-lines (buffer-string))))
         (ssid (completing-read "SSID >" ssid-options))
         (password (read-passwd (format "password for '%s' > " ssid))))
    ;; nmcli dev wifi connect %s password %s
    (call-process "nmcli" nil nil nil "dev" "wifi" "connect" ssid "password" password)
    )
  )

(provide 'bergey-system)
