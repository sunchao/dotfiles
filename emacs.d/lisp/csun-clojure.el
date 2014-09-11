(defun turn-on-paredit (paredit-mode t))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

(provide 'csun-clojure)
