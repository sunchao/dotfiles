(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/my-themes")

(require 'package)
(require 'cl)

;;; personal information
(setq user-full-name "Chao Sun")
(setq user-mail-address "sunchao.chris@gmail.com")

(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; If for some reason, you got this error:
;; Error during download request: Not Found
;; Just do M-x package-refresh-contents

(defvar my-packages
  '(paredit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; for mac
;(defvar macosx-p  (string-match "darwin" (symbol-name system-type)))

(setq mac-option-modifier 'super) ;; mac-specific key binding
(setq mac-command-modifier 'meta)  ;;
(set-default-font "Liberation Mono 12")


;; (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; fix path issues when launching emacs GUI under Mac
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))

;; configuration on faces.
(when (not package-archive-contents)
  (package-refresh-contents))

;; change meta key. For Ubuntu only!
(setq x-super-keysym 'meta)

;; add to PATH and exec path
(setq shell-file-name "/bin/bash")
(setenv "PATH" (concat "/usr/local/bin:" "/usr/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin" "/usr/texbin") exec-path))

;; set tab width
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;;; don't use TAB for indentation
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq tab-stop-list ())
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x 2) tab-stop-list)))

;; starting up options
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 100000)
(setq default-major-mode 'text-mode) ;; set default mode to be text
(show-paren-mode t) ;; show parenthesis matchup
(line-number-mode t) ;; show line number
(column-number-mode t) ;; show column number
(tool-bar-mode -1) ;; don't show tool bar
(menu-bar-mode -1) ;; don't show menu bar

;; starting up options
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 100000)
(setq efault-major-mode 'text-mode) ;; set default mode to be text
(show-paren-mode t) ;; show parenthesis matchup
(line-number-mode t) ;; show line number
(column-number-mode t) ;; show column number
(tool-bar-mode -1) ;; don't show tool bar
(menu-bar-mode t) ;; show menu bar

(setq x-select-enable-clipboard t) ;; don't know what's this
(auto-fill-mode t) ;; set auto fill
(setq visible-bell t) ;; turn on visible bell instead of audible one
(setq display-time-day-and-date t) ;; display stuff
(setq global-font-lock-mode t) ;; enable font lock mode on all
(setq inhibit-startup-msg t) ;; disable startup message
(setq make-backup-files -1)
(setq-default show-trailing-whitespace -1)
(setq-default fill-column 80) ;; 70 -> 80
(scroll-bar-mode -1) ;; don't need scroll bar

;; (remove-hook 'coding-hook 'turn-on-hl-line-mode) ;; turn off hl-line-mode

(load-theme 'Darkula t)
;; (load-theme 'solarized-dark t)

;; give duplicated buffer name more information
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-seperator ":")

;; aliases
(defalias 'qrr 'query-and-replace)

;; key bindings
(global-set-key (kbd "C-c C-v") 'compile)
(global-set-key (kbd "C-c C-r") 'query-replace)
(global-set-key (kbd "C-x a c") 'comment-region)
(global-set-key (kbd "C-x a u") 'uncomment-region)
(global-set-key (kbd "C-x a d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x a r") 'align-regexp)
;; (setq whitespace-style '(trailing lines space-before-tab
;;                                   indentation space-after-tab))
;; (setq whitespace-line-column 80)
;; (global-whitespace-mode 1)

(autoload 'gtags-mode "gtags" "" t)


(setq display-time-string-forms
      '((format-time-string "%m/%d:%H:%M" now)
        (if mail " Mail" "") " ")
      display-time-mail-file "/var/mail/csun"
      display-time-mail-face 'display-time-mail-face)

;; (display-time-mode nil)

(setq-default mode-line-format
              (list
               " -- "
               'mode-line-buffer-identification ;; filename
               " (%l,%c) " ;; line and col number
               " "
               `(vc-mode vc-mode)
               " "
               'mode-line-misc-info))

(defun init-file ()
  (interactive)
  (find-file "/Users/chao/.emacs.d/lisp/init.el"))

(require 'csun-utils)
(require 'csun-cpp)
; (require 'csun-elisp)
(require 'csun-ido)
(require 'csun-org)
(require 'csun-babel)
(require 'csun-sml)
(require 'csun-twelf)
(require 'csun-haskell)
; (require 'csun-clisp)
; (require 'csun-clojure)
(require 'csun-ruby)
(require 'csun-sh)
(require 'csun-yasnippet)


