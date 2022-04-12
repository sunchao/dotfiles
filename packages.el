(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(defvar packages-to-install
  '(
	  ;; general
	  gruvbox-theme projectile rg swiper uniquify magit find-file-in-repository find-file-in-project
    restart-emacs cl-lib exec-path-from-shell use-package helm recentf counsel winum
    ;; general editor utils
	  company fill-column-indicator paredit flymake flycheck yasnippet
    ;; Other language modes
    yaml yaml-mode markdown-mode
	  ;; C++
	  cc-mode
	  ;; Org
	  org-install ob-tangle
	  ;; Golang
	  go-guru go-autocomplete auto-complete-config go-eldoc
	  ;; Rust
	  lsp-mode rustic flycheck-rust
    ))

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install packages from the list that are not yet installed
(dolist (p packages-to-install)
  (when (and (not (package-installed-p p)) (assoc p package-archive-contents))
    (package-install p)))
