;;; misc-conf.el --- A catch-all config file

;; Copyright (C) 2013  Michael Kohl

;; Author: Michael Kohl(require 'dircolors) <citizen428@gmail.com>
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

;; A catch-all file where configutation that fits no other file goes.

;;; Code:

(require 'nyan-mode)
(nyan-mode)
(global-undo-tree-mode)
(require 'visible-mark)

(global-set-key (kbd "C-=") 'er/expand-region)

(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

(setq minimap-window-location (quote right))

(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.mdown.*\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Automatically make script files executable
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; Some global settings
(setq-default ispell-program-name "aspell")
(setq global-visible-mark-mode t)
(setq global-auto-revert-mode t)

(put 'ido-exit-minibuffer 'disabled nil)

(provide 'misc-conf)
;;; misc-conf.el ends here
