;; slime lets you connect to a swank server
(require 'slime)

(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(require 'slime)
(slime-setup)



