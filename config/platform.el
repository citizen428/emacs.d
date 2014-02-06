;;; platform.el --- Platform (OS, windowing system) specific configuration

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

;; This file contains OS or windowing system specifc configurations.

;;; Code:

;; From https://github.com/purcell/emacs.d/blob/master/init-exec-path.el
(defun set-exec-path-from-shell-PATH ()
  "Ensure $PATH is the same as in a terminal."
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (eq window-system 'ns)
  ;; Use command as meta
  (setq ns-alternate-modifier 'none)
  (setq ns-command-modifier 'meta)
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))
  (set-exec-path-from-shell-PATH))

(provide 'platform)
;;; platform.el ends here
