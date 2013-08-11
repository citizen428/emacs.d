;; Use command as meta on OSX
(when (eq system-type 'darwin)
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta))

;; a popup menu for the kill ring
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

;; buffer move keys
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; buffer-menu is almost like list-buffer,
;; but moves point to buffer
(global-set-key "\C-x\C-b" 'buffer-menu)

;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; key-chord-mode
(key-chord-mode 1)
;; Move to char (f+g forward, d+f backward)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)
(key-chord-define-global "ww" 'yas/expand)
