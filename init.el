;; start package.el before everything else
(package-initialize)

;; When started from Emacs.app or similar, ensure $PATH
;; is the same the user would see in Terminal.app
;; https://github.com/purcell/emacs.d/blob/master/init-exec-path.el
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (or (eq 'ns window-system) (eq window-system 'x))
  (if window-system (set-exec-path-from-shell-PATH)))

(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))

(defun add-local-path (p)
  (add-to-list 'load-path (concat dotfiles-dir p)))

(defun load-local-file (p)
  (load-file (concat dotfiles-dir p)))

(defun add-subdirs-to-load-path (dir)
  (let ((default-directory  (concat dotfiles-dir dir)))
    (normal-top-level-add-subdirs-to-load-path)))

(add-local-path "lib")
(add-subdirs-to-load-path "lib")

;; general handling
(load-local-file "config/bindings.el")
(load-local-file "config/built-in.el")
(load-local-file "config/cosmetic.el")
(load-local-file "config/ido-conf.el")
(load-local-file "config/smex-conf.el")

;; programming
(load-local-file "config/ruby-conf.el")
(load-local-file "config/slime-conf.el")
(load-local-file "config/lisps-conf.el")
(load-local-file "config/clojure-conf.el")
(load-local-file "config/programming.el")
(load-local-file "config/auto-complete-conf.el")
(load-local-file "config/yasnippet-conf.el")

;; misc modes
(load-local-file "config/misc-conf.el")
(load-local-file "config/org-mode-conf.el")
(load-local-file "config/twittering-conf.el")


