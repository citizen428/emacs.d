;; Scheme
(require 'quack)

;; Erlang

(setq erlang-path "/usr/local/Cellar/erlang/R14B02")
(setq load-path (cons (concat erlang-path "/lib/erlang/lib/tools-2.6.6.3/emacs") load-path))
(setq erlang-root-dir erlang-path)
(setq exec-path (cons (concat erlang-path "/bin") exec-path))
(require 'erlang-start)

;; Factor
(load-file "/Applications/factor/misc/fuel/fu.el")

;; GNU Smalltalk
(setq gst-path "/usr/local/Cellar/gnu-smalltalk/3.2.2/share/emacs/site-lisp")

(setq auto-mode-alist
      (append  '(("\\.st\\'" . smalltalk-mode))
               auto-mode-alist))

(autoload 'smalltalk-mode (concat gst-path "/smalltalk-mode.elc") "" t)

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

;; GForth
(autoload 'forth-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
     			    auto-mode-alist))
(autoload 'forth-block-mode "gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
     			    auto-mode-alist))
(add-hook 'forth-mode-hook
          (function (lambda ()
                      ;; customize variables here:
                      (setq forth-indent-level 4)
                      (setq forth-minor-indent-level 2)
                      (setq forth-hilight-level 3))))
