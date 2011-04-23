;; ;; Scheme
;; (require 'quack)

;; ;; Erlang

;; (setq erlang-path "/usr/local/Cellar/erlang/R14B02")
;; (setq load-path (cons (concat erlang-path "/lib/erlang/lib/tools-2.6.6.3/emacs") load-path))
;; (setq erlang-root-dir erlang-path)
;; (setq exec-path (cons (concat erlang-path "/bin") exec-path))
;; (require 'erlang-start)

;; ;; Factor
;; (load-file "/Applications/factor/misc/fuel/fu.el")

;; ;; GNU Smalltalk
;; (setq gst-path "/usr/local/Cellar/gnu-smalltalk/3.2.2/share/emacs/site-lisp")

;; (setq auto-mode-alist
;;       (append  '(("\\.st\\'" . smalltalk-mode))
;;                auto-mode-alist))

;; (autoload 'smalltalk-mode (concat gst-path "/smalltalk-mode.elc") "" t)

;; Haskell
(load (concat dotfiles-dir "lib/haskell-mode/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ;; Pure
;; (require 'pure-mode)
;; (setq auto-mode-alist
;;       (cons '("\\.pure\\(rc\\)?$" . pure-mode) auto-mode-alist))
