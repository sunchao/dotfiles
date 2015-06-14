(require 'package)

;;; 'lisp' contains a set of language-specific elisp files, besides
;;; the init.el.
;;; 'lisp/my-themes' contains themes that cannot be installed via the
;;; the built-in theme installer.
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/my-themes")

;;; A short-cut to the init file
(defun init-file ()
  (interactive)
  (find-file "~/.emacs.d/lisp/init.el"))

;;; personal information
(setq user-full-name "Chao Sun")
(setq user-mail-address "sunchao.chris@gmail.com")

;;; A list of packages required.
;;; All the required packages should be in here.
(setq package-list
      '(paredit cl popup ggtags ack flymake
      magit find-file-in-repository google-c-style
    ;; modes for C++ dev
      cc-mode cmake-mode cmake-project irony flymake-google-cpplint))

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
       ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (p package-list)
  (when (not (package-installed-p p))
    (package-install p)))

;;; Required packages
(require 'cl)
(require 'uniquify)
(require 'cc-mode) ;; C++ mode
(require 'flymake) ;; Syntax checking on the fly
(require 'popup)
(require 'irony)
(require 'find-file-in-repository)
(require 'csun-utils) ;; Utility functions
(require 'csun-org) ;; Org mode

;;; Set font and keybindings specific to OS
(let ((sys (symbol-name system-type)))
  (cond ((string-match sys "darwin")
         (progn
           (setq mac-option-modifier 'super) ;; mac-specific key binding
           (setq mac-command-modifier 'meta)  ;;
           (set-default-font "Liberation Mono 10")
           (load-theme 'Darkula t)))
        ((string-match sys "gnu/linux")
         (progn
           (setq x-super-keysym 'meta)
           (set-default-font "Source Code Pro 10")))))

;; add to PATH and exec path
(setq shell-file-name "/bin/bash")
(setenv "PATH" (concat "/usr/local/bin:" "/usr/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin" "/usr/texbin") exec-path))

;; set tab width
;; use space instead of tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq indent-line-function 'insert-tab)
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
;; (setq whitespace-style '(trailing lines space-before-tab
;;                                   indentation space-after-tab))
;; (setq whitespace-line-column 80)
;; (global-whitespace-mode 1)

;; Wind Move
(windmove-default-keybindings 'meta)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; give duplicated buffer name more information
(setq uniquify-buffer-name-style 'post-forward
      uniquify-seperator ":")

;;; Set default mode line format
;;; TODO: this needs improvement

(setq display-time-string-forms
      '((format-time-string "%m/%d:%H:%M" now)
        (if mail " Mail" "") " ")
      display-time-mail-file "/var/mail/csun"
      display-time-mail-face 'display-time-mail-face)

(setq-default
  mode-line-format
  (list
   " -- "
   'mode-line-buffer-identification ;; filename
   " (%l,%c) " ;; line and col number
   " "
   `(vc-mode vc-mode)
   " "
   'mode-line-misc-info))

;;; Gtags
(autoload 'gtags-mode "gtags" "" t)

;;; Magit mode
;;; To prevent Magit from reverting all unmodified buffer
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")


;;; Paredit mode
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;;; IDO mode
;;; TODO: consider Helm mode as a replacement
(ido-mode t)
(setq ido-enable-flex-matching nil) ;; enable fuzzy matching

;;; SML mode
(defun my-sml-mode-hook () "Local defaults for SML mode"
  (setq sml-indent-level 2)        ; conserve on horizontal space
  (setq words-include-escape t)    ; \ loses word break status
  (setq indent-tabs-mode nil))     ; never ever indent with tabs
(add-hook 'sml-mode-hook 'my-sml-mode-hook)

;;; C++ mode
(setq-default c-basic-offset 2 c-default-style "linux")
(setq-default tab-width 2 indent-tabs-mode nil)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(defun my:flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load))
;; (add-hook 'c-mode-hook 'my:flymake-google-init)
;; (add-hook 'c++-mode-hook 'my:flymake-google-init)

;;; Use Google C++ style
(require 'google-c-style)
(add-hook 'c++-mode-hook 'google-set-c-style)
(add-hook 'c++-mode-hook 'google-make-newline-indent)

(require 'ggtags) ;; See https://github.com/leoliu/ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)


;;; Twelf mode
(cond
 ((file-exists-p "/usr/local/twelf/emacs/twelf-init.el")
 (progn
   (setq twelf-root "/usr/local/twelf/")
   ;; make the variable more recognizable
   (setq twelf-font-dark-background t)
   (load (concat twelf-root "emacs/twelf-init.el"))
   (load (concat twelf-root "emacs/twelf-font.el"))
   (add-hook
    'twelf-mode-hook
    '(lambda ()
       (setq twelf-chatter "1")
       (setq twelf-unsafe "true")
       (setq twelf-indent 2) ;; set indentation to 2
       (set-fill-column 100) ;; break line at 78
       (auto-fill-mode))) ;; auto wrap lines
   )))

;; key bindings
(global-set-key (kbd "C-c C-v") 'compile)
(global-set-key (kbd "C-c C-r") 'query-replace-regexp)
(global-set-key (kbd "C-c C-f") 'find-file-in-repository)
(global-set-key (kbd "C-c C-g") 'ack)
(global-set-key (kbd "C-c C-e") 'magit-status)
(global-set-key (kbd "C-x a c") 'comment-region)
(global-set-key (kbd "C-x a u") 'uncomment-region)
(global-set-key (kbd "C-x a d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x a r") 'align-regexp)


