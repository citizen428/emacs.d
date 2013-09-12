;;; cosmetic.el --- Configuration of visual aspects

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

;; Used for configuration the visual aspects of Emacs.

;;; Code:

(load-file (concat config-lib-dir "tomorrow-theme/tomorrow-night-theme.el"))

;; highlight current line
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#333333")

;; set cursor colour
(set-cursor-color "#777777")

;; make sure ansi colour character escapes are honoured
(ansi-color-for-comint-mode-on)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


(if (eq window-system 'x)
    (set-frame-font "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso8859-15")
  (set-frame-font "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso8859-15"))

(provide 'cosmetic)
;;; cosmetic.el ends here
