(require 'twittering-mode)
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
