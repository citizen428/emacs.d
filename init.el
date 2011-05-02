;; start package.el before everything else
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))

(defun add-local-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun load-local-file (p)
  (load-file (concat dotfiles-dir p)))

(add-local-path "lib")

(require 'dircolors)
(require 'smooth-scrolling)
(require 'rainbow-parens)
(require 'rainbow-delimiters)
(require 'buffer-move)
(load "git-wip")

;; The amazing undo tree
(add-local-path "lib/undo-tree/")
(require 'undo-tree)
(global-undo-tree-mode)

;; slime lets you connect to a swank server
(add-local-path "lib/slime")
(require 'slime)

(load-local-file "config/built-in.el")
(load-local-file "config/paredit-conf.el")
(load-local-file "config/lisps-conf.el")
(load-local-file "config/cosmetic.el")
(load-local-file "config/bindings.el")
(load-local-file "config/highlight-flash-conf.el")
(load-local-file "config/ido-conf.el")
(load-local-file "config/clojure-conf.el")
(load-local-file "config/slime-conf.el")
(load-local-file "config/auto-complete-conf.el")
(load-local-file "config/durendal-conf.el")
(load-local-file "config/smex-conf.el")
(load-local-file "config/yasnippet-conf.el")

(load-local-file "config/org-mode-conf.el")
;; (load-local-file "config/twittering-conf.el")
(load-local-file "config/ruby-conf.el")
(load-local-file "config/misc-lang-conf.el")
(load-local-file "config/misc-conf.el")
