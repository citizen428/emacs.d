;;; auto-complete-conf.el --- Configuration related to auto-completion

;; Copyright (C) 2013  Michael Kohl

;; Author: Michael Kohl;;auto-complete mode <citizen428@gmail.com>
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

;; Configuration file for auto-completion.

;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(add-to-list 'ac-dictionary-directories (concat config-lib-dir "auto-complete/dict"))
(global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
;; (setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(ac-set-trigger-key "TAB")

(define-key ac-completing-map (kbd "C-M-n") 'ac-next)
(define-key ac-completing-map (kbd "C-M-p") 'ac-previous)

;; tab and enter complete
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer
               ac-source-yasnippet))

(dolist (mode '(magit-log-edit-mode haml-mode
                sass-mode yaml-mode haskell-mode
                html-mode nxml-mode sh-mode clojure-mode
                lisp-mode textile-mode markdown-mode))
  (add-to-list 'ac-modes mode))

;;ac-slime auto-complete plugin
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(provide 'auto-complete-conf)
;;; auto-complete-conf.el ends here
