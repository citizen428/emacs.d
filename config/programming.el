(require 'quickrun)
(global-set-key (kbd "<f7>") 'quickrun)
(global-set-key (kbd "<f8>") 'quickrun-compile-only)

;; CSS
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Erlang
(setq erlang-path "/usr/local/Cellar/erlang")
(setq load-path (cons (concat erlang-path "/R15B01/lib/erlang/lib/tools-2.6.7/emacs") load-path))
(setq erlang-root-dir erlang-path)
(setq exec-path (cons (concat erlang-path "/bin") exec-path))
(require 'erlang-start)

;; Git
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "P") 'magit-push)))

(load "git-wip")

;; Haskell
(load (concat dotfiles-dir "lib/haskellmode-emacs/haskell-site-file"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
;; nmap scripting engine
(add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))

;; ;; GNU Smalltalk
;; (setq gst-path "/usr/local/Cellar/gnu-smalltalk/3.2.2/share/emacs/site-lisp")

;; (setq auto-mode-alist
;;       (append  '(("\\.st\\'" . smalltalk-mode))
;;                auto-mode-alist))

;; (autoload 'smalltalk-mode (concat gst-path "/smalltalk-mode.elc") "" t)

;; Scheme
;; (require 'quack)

;; YAML
(require 'yaml-mode)
