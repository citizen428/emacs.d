(add-local-path "lib/clojure-mode")
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-paren-mode)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\>\\)"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

;; command to align let statements
(add-local-path "lib/align-cljlet")
(require 'align-cljlet)

;; http://jakemccrary.com/blog/2010/12/07/quickily-starting-a-powerful-clojure-repl.html
(defun clojure-swank ()
  "Launch swank-clojure from ~/.lein/bin"
  (interactive)
  (let ((buffer (get-buffer-create "*clojure-swank*")))
    (flet ((display-buffer (buffer-or-name &optional not-this-window frame) nil))
      (bury-buffer buffer)
      (shell-command "~/.lein/bin/swank-clojure &" buffer))
    (set-process-filter (get-buffer-process buffer)
                        (lambda (process output)
                          (with-current-buffer "*clojure-swank*" (insert output))
                          (when (string-match "Connection opened on local port +\\([0-9]+\\)" output)
                            (slime-connect "localhost" (match-string 1 output))
                            (set-process-filter process nil)))) (message "Starting swank.. ")))

(defun clojure-kill-swank ()
  "Kill swank process started by lein swank."
  (interactive)
  (let ((process (get-buffer-process "*clojure-swank*")))
    (when process (ignore-errors (slime-quit-lisp))
          (let ((timeout 10))
            (while (and (> timeout 0) (eql 'run (process-status process)))
              (sit-for 1)
              (decf timeout)))
          (ignore-errors (kill-buffer "*clojure-swank*")))))
