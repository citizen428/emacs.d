(require 'org-mac-link-grabber)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c g") 'omlg-grab-link)))

(setq org-todo-keywords '((sequence "TODO" "WIP" "DELEGATED" "|" "DONE")))
