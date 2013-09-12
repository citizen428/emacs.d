(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory (concat config-main-dir "etc/snippets"))

;; Load the snippets
(yas/load-directory yas/root-directory)
