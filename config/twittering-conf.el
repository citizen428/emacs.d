;;; twittering-conf.el --- Twitter related configuration

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

;; Configuration for twittering-mode.

;;; Code:

(setq twittering-icon-mode t)
(setq twittering-timer-interval 600)
(setq twittering-url-show-status nil)
(setq twittering-use-show-minibuffer-length t)
(setq twittering-tinyurl-service 'is.gd)
(setq twittering-use-master-password t)

(setq twittering-initial-timeline-spec-string
      '(":friends"
        ":replies"
        ":direct_messages"
        ":search/happynerds/"
        ":search/citizen428.net/"))

(add-hook 'twittering-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(("R" . twittering-native-retweet)
                    ("l" . twittering-goto-next-thing)))))

(provide 'twittering-conf)
;;; twittering-conf.el ends here
