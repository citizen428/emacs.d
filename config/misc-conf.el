(require 'csv-mode)
(require 'dircolors)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(require 'markdown-mode)
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(require 'minimap)
(setq minimap-window-location (quote right))
(require 'nyan-mode)
(nyan-mode)
(require 'smooth-scrolling)
(require 'undo-tree)
(global-undo-tree-mode)
(require 'visible-mark)

(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.mdown.*\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

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

;; Some global settings
(setq-default ispell-program-name "aspell")
(setq global-visible-mark-mode t)
(setq global-auto-revert-mode t)
