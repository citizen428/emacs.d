;;; programming.el --- Misc programming related settings

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

;; Misc programming related settings that don't fit a more specific file.

;;; Code:

(add-hook 'prog-mode-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

(global-set-key (kbd "<f7>") 'quickrun)
(global-set-key (kbd "<f8>") 'quickrun-compile-only)

(add-hook 'css-mode-hook 'rainbow-mode)

(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "P") 'magit-push)))

(load "git-wip")

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'go-mode-hook
          (lambda ()
            (define-key go-mode-map (kbd "C-x C-s")
              (lambda () (interactive) (gofmt) (save-buffer)))
            (setq tab-width 4)))

;; nmap scripting engine uses Lua
(add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))

(require 'yasnippet)
(yas-global-mode 1)

(provide 'programming)
;;; programming.el ends here
