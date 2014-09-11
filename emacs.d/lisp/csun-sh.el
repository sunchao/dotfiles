;; tweaking sh-mode
(defun my-sh-mode-hook ()
  "Change indentation space"
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

(provide 'csun-sh)
