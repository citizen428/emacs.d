(require 'visible-mark)
(require 'csv-mode)
(require 'markdown-mode)
(add-local-path "lib/nyan-mode/")
(require 'nyan-mode)
(nyan-mode)

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

;; backups and auto-saves go to the temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'post-command-hook 'my-flymake-show-help)

(setq-default ispell-program-name "aspell")
