;;auto-complete mode
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "lib/auto-complete/dict"))
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
