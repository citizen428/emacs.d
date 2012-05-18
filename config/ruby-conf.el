(require 'starter-kit-ruby)

(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(require 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)

(add-hook 'ruby-mode-hook 'whitespace-mode)

(require 'rspec-mode)

;; cucumber
(add-local-path "lib/cucumber.el")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(require 'haml-mode)

(add-to-list 'inf-ruby-implementations '("pry" . "pry -f"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")

;;; fron emacs-starter-kit
;;; Flymake

(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    ;; Invoke ruby with '-c' to get syntax checking
    (list "ruby" (list "-c" local-file))))

(defun flymake-ruby-enable ()
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name)
             (if (fboundp 'tramp-list-remote-buffers)
                 (not (subsetp
                       (list (current-buffer))
                       (tramp-list-remote-buffers)))
               t))
    (local-set-key (kbd "C-c d")
                   'flymake-display-err-menu-for-current-line)
    (flymake-mode t)))

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)
     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)
     (add-hook 'ruby-mode-hook 'flymake-ruby-enable)))
