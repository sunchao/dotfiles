;; ruby quick run
(defun my-ruby-compile()
  "Compile Ruby program"
  (interactive)
  (compile (concat "ruby " (buffer-name))))

(defun my-compile-ruby-keys ()
  (local-set-key (kbd "C-c C-c") 'my-ruby-compile))
(add-hook 'ruby-mode-hook 'my-compile-ruby-keys)

(add-hook 'ruby-mode-hook '(lambda () (setq fill-column 80)))

(provide 'csun-ruby)
