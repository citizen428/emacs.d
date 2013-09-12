(add-hook 'ruby-mode-hook 'ruby-end-mode)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'haml-mode-hook 'rinari-minor-mode)

;; Use Pry as inferior Ruby implementation
(add-to-list 'inf-ruby-implementations '("pry" . "pry -f"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")
