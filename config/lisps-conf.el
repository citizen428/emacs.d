(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(require 'paredit)
(defun turn-on-paredit ()
  (paredit-mode t))

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(defun turn-on-rainbows ()
  (rainbow-paren-mode t)
  (rainbow-delimiters-mode t))

(require 'rainbow-parens)
(require 'rainbow-delimiters)

(dolist (x '(scheme emacs-lisp lisp clojure))
  (when window-system
    (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(\\|)" . 'esk-paren-face))))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-rainbows)
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'run-coding-hook))

;; highlight expression on eval
(require 'highlight)
(require 'eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 0.5)
