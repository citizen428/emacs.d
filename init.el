;;; init.el --- Main configuraton file

;; Copyright (C) 2013 Michael Kohl

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

;; Main entry point of the configuration.  Performs and version check,
;; does some basic setup and then proceeds to load the more specific
;; configuration files.

;;; Code:

(when (version< emacs-version "24.1")
  (error "This configuration was made for Emacs 24.1+"))

(defconst config-main-dir
  (if (boundp 'user-emacs-directory) user-emacs-directory "~/.emacs.d/")
  "Main configuration directory.")

(defconst config-lib-dir (expand-file-name "lib/" config-main-dir)
  "Directory for non-ELPA packages.")

(defconst config-extra-dir (expand-file-name "config/" config-main-dir )
  "Directory for additional config files.")

(defun load-config (file-name)
  "Load FILE-NAME (without extension) from CONFIG-EXTRA-DIR."
  (load-file (concat config-extra-dir file-name ".el")))

(setq user-full-name "Michael Kohl"
      user-mail-address "citizen428@gmail.com")

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'load-path config-lib-dir)
(let ((default-directory config-lib-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; general handling
(load-config "basic")
(load-config "bindings")
(load-config "cosmetic")
(load-config "platform")

;; programming
(load-config "programming")
(load-config "ruby-conf")
(load-config "lisps-conf")
(load-config "clojure-conf")
(load-config "auto-complete-conf")

;; misc modes
(load-config "misc-conf")
(load-config "org-mode-conf")
(load-config "twittering-conf")

(provide 'init)
;;; init.el ends here
