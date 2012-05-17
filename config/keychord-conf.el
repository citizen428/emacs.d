(require 'key-chord)
(key-chord-mode 1)
(require 'iy-go-to-char)

;; Move to char (f+g forward, d+f backward)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)
(key-chord-define-global "ee" 'yas/expand)
