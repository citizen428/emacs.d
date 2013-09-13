;; enable Flycheck and electric-pair-mode for all buffers
(add-hook 'prog-mode-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Quickrun
(global-set-key (kbd "<f7>") 'quickrun)
(global-set-key (kbd "<f8>") 'quickrun-compile-only)

;; CSS
(add-hook 'css-mode-hook 'rainbow-mode)

;; Git
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd "P") 'magit-push)))

(load "git-wip")

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; nmap scripting engine uses Lua
(add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))

(require 'yasnippet)
(yas-global-mode 1)
