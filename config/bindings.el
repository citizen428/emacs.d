;;; bindings.el --- Global keybindings

;; Copyright (C) 2013  Michael Kohl

;; Author: Michael Kohl <citizen428@gmail.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Global keybindings.

;;; Code:

;; popup menu for the kill ring
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

;; Like list-buffer but moves point to buffer
(global-set-key "\C-x\C-b" 'buffer-menu)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(key-chord-mode 1)
(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)
(key-chord-define-global "ww" 'yas/expand)

(provide 'bindings)
;;; bindings.el ends here
