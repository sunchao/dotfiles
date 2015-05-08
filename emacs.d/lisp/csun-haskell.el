(require 'inf-haskell) ;; inferior Haskell mode
(require 'haskell-mode)

;; (add-hook
;;  'haskell-mode-hook
;;  '(lambda ()
;;     (turn-on-haskell-doc-mode)
;;     (turn-on-haskell-indent)
;;     ;; (turn-on-haskell-indentation)
;;     (local-set-key (kbd "C-c C-f") 'haskell-hoogle)
;;     (haskell-indent-mode)
;;     (interactive-haskell-mode)))

;; (add-hook
;;  'haskell-mode-hook
;;  '(lambda ()
;;     (hl-line-mode -1)
;;     (global-hl-line-mode -1))
;;  't)


(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; haskell-mode-save-buffer is not bound
;; (define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)

;; it's tedius that, after typed type signature for a function,
;; you have to type the function name again to start the implementation.

(defun csun/haskell-gen-funname ()
  "Generate function name from last type signature, starting
  from the cursor position"
  (interactive)
  ())

(setq haskell-font-lock-symbols t) ;; enable Unicode display
(provide 'csun-haskell)
