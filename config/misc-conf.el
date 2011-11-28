(require 'buffer-move)
(require 'csv-mode)
(require 'dircolors)
(require 'highlight-indentation)
(require 'minimap)
(setq minimap-window-location (quote right))
(require 'smooth-scrolling)
(require 'visible-mark)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

(require 'nyan-mode)
(nyan-mode)

(load "git-wip")

(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . shell-script-mode))

;; Automatically make script files executable
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; Flymake errors at point from Dave Love on gnu.emacs.help:
(defun my-flymake-show-help ()
   (when (get-char-property (point) 'flymake-overlay)
     (let ((help (get-char-property (point) 'help-echo)))
       (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)
(setq-default ispell-program-name "aspell")

(require 'undo-tree)
(global-undo-tree-mode)

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(setq global-visible-mark-mode t)
