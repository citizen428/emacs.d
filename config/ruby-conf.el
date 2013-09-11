(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'haml-mode-hook 'rinari-minor-mode)

(add-to-list 'inf-ruby-implementations '("pry" . "pry -f"))
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
(setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")
