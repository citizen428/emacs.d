(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(require 'buffer-move)

(require 'iy-go-to-char)
(require 'key-chord)
(key-chord-mode 1)
;; Move to char (f+g forward, d+f backward)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)
(key-chord-define-global "ee" 'yas/expand)
