(require 'dircolors)
(require 'nyan-mode)
(nyan-mode)
(global-undo-tree-mode)
(require 'visible-mark)

(global-set-key (kbd "C-=") 'er/expand-region)

(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

(setq minimap-window-location (quote right))

(add-to-list 'auto-mode-alist '("\\.zsh.*\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.mdown.*\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Automatically make script files executable
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;; Some global settings
(setq-default ispell-program-name "aspell")
(setq global-visible-mark-mode t)
(setq global-auto-revert-mode t)
