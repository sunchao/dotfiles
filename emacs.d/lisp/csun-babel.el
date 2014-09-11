(require 'ob-tangle)

(org-babel-do-load-languages ;; toggle language support
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (ruby . t)
   (python . t)
   (clojure . t)
   (sh . t)
   (latex . t)))

(setq org-src-fontify-natively t) ;; fontify code block automatically

(provide 'csun-babel)
