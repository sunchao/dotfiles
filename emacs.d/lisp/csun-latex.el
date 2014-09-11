;; load AUCTEX mode
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
(add-hook
 'LaTeX-mode-hook 
 '(lambda ()
    (TeX-fold-mode t)
    (outline-minor-mode t)))

(add-hook 'LaTeX-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)	;; use pdflatex as default
(setq TeX-view-program-list '(("Preview" "open -a Preview %o")))
(setq TeX-view-program-selection '((output-pdf "Preview")))
(setq-default TeX-master "main")	;; make AUCTEX aware of the default main

(provide 'csun-latex)
