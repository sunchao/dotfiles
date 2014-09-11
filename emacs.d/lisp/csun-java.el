;; java-mode hook
(add-hook 'java-mode-hook '(lambda () (setq c-basic-offset 2)))
(add-hook 'java-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'java-mode-hook 'turn-on-auto-fill)

(provide 'csun-java)
