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

