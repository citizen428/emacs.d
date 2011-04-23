(require 'starter-kit-ruby)

;; RVM support
(require 'rvm)
(rvm-use-default)

(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(require 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)

;; whitespace-mode for Ruby code
(add-hook 'ruby-mode-hook 'whitespace-mode)

;; rspec
(require 'rspec-mode)

;; cucumber
(add-local-path "lib/cucumber.el")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(require 'haml-mode)

(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; minor fix for ruby-compilation
(define-key ruby-compilation-minor-mode-map [return] 'comint-send-input)
