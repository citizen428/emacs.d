;;; lisps-conf.el --- Configuration for Lisp programming

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

;; Configuration for various Lisp implementations (CL, EmacsLisp,
;; Clojure, Scheme).

;;; Code:

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; slime lets you connect to a swank server
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(require 'slime)
(slime-setup)

(defun turn-on-paredit ()
  (paredit-mode t))

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(defun turn-on-rainbows ()
  (rainbow-paren-mode t)
  (rainbow-delimiters-mode t))

(require 'rainbow-parens)

(dolist (x '(scheme emacs-lisp lisp clojure))
  (when window-system
    (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(\\|)" . 'esk-paren-face))))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-rainbows)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'run-coding-hook))

;; highlight expression on eval
(require 'highlight)
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.5)

(provide 'lisps-conf)
;;; lisps-conf.el ends here
