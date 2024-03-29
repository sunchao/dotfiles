;;; Emacs Configuration for Chao Sun
;;; Last Modified: Wed Apr 20 14:47:56 2022.

;;; 'lisp' contains a set of language-specific elisp files, besides
;;; the init.el.

;;; 'lisp/my-themes' contains themes that cannot be installed via the
;;; the built-in theme installer.

;; Default conf dir
(defun emacs-conf-dir () (concat (getenv "HOME") "/git/dotfiles"))

;;; A short-cut to the init file
(defun init-file ()
  (interactive)
  (find-file (concat (emacs-conf-dir) "/chao-init.el")))

;;; ---------------------------------------------------------------------------
;;; A bunch of config setups

(global-flycheck-mode)
(setq flycheck-pos-tip-timeout 9999) ;; set timeout to a large enough value.

;;; Set $PATH and exec-path from shell
;; (exec-path-from-shell-initialize)

;;; Set font and keybindings specific to OSs
(let ((sys (symbol-name system-type)))
  (cond ((string-match sys "darwin")
         (progn
           (setq mac-option-modifier 'super) ;; mac-specific key binding
           (setq mac-command-modifier 'meta)  ;;
           ))
        ((string-match sys "gnu/linux")
         (progn
           (setq x-super-keysym 'meta)))))

;; Set tab width, and use space instead of tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list ())
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x 2) tab-stop-list)))

;; Start up options
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 100000)
(setq default-major-mode 'text-mode) ;; set default mode to be text
(show-paren-mode t) ;; show parenthesis matchup
(line-number-mode t) ;; show line number
(column-number-mode t) ;; show column number
(auto-fill-mode t) ;; set auto fill
(setq x-select-enable-clipboard t) ;; don't know what's this
(setq display-time-day-and-date t) ;; display stuff
(setq global-font-lock-mode t) ;; enable font lock mode on all
(setq inhibit-startup-msg t) ;; disable startup message
(setq-default show-trailing-whitespace -1)

;; Full screen mode at startup
(tool-bar-mode -1) ;; don't show tool bar
(menu-bar-mode -1) ;; don't show menu bar
(scroll-bar-mode -1) ;; don't need scroll bar
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(setq whitespace-style
      '(trailing lines space-before-tab
                 indentation space-after-tab))
;; Backup files in a separate dir
(setq backup-directory-alist `(("." . "~/.emacs_backup_files")))

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Wind Move
(windmove-default-keybindings 'meta)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Add column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "gray")

;; Give duplicated buffer name more information
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-seperator ":")

(setq left-fringe-width 0)
(setq right-fringe-width 0)

;; Use ripgrep for searching
(require 'rg)
(require 'swiper)


;;; ---------------------------------------------------------------------------
;;; Utility Functions

(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun replace-tabs (begin end)
  "Replace all tabs in the selected region with 2 whitespaces."
  (interactive "r")
  (save-excursion
    (replace-regexp "\t" " " nil begin end)))

(defun delete-n-chars (n)
  (interactive "n")
  (if (> n 0)
    (save-excursion
    (progn
      (delete-char 1)
      (delete-n-chars (-1 n))))))

(defun delete-first-n (begin end &optional n)
  "For all lines in the region, delete first n characters"
  (interactive "r\nNHow many to delete: ")
  (or n (setq n 2))
  (let ((number-of-lines
         (save-excursion
           (goto-char begin)
           (let ((count 0))
             (while (< (point) end)
               (forward-line)
               (setq count (1+ count)))
             count))))
    (save-excursion
      (goto-char begin)
      (while (> number-of-lines 0)
        (beginning-of-line)
        (let ((line-length
             (save-excursion
               (end-of-line)
               (current-column))))
          (if (> line-length n)
              (delete-char n)))
        (setq number-of-lines (1- number-of-lines))
        (forward-line)))))

(defun insert-first-n (begin end &optional n)
  "For all lines in the region, insert n spaces before each line.\n
   In default, add 2 spaces"
  (interactive "r\nNHow many spaces to add: ")
  (let ((nlines
         (save-excursion
           (goto-char begin)
           (let ((count 0))
             (while (< (point) end)
               (forward-line)
             (setq count (1+ count)))
             count))))
    (save-excursion
      (goto-char begin)
      (while (> nlines 0)
        (insert-char ?\s n t)
        (setq nlines (1- nlines))
        (forward-line)))))

(defun print-point ()
  "Print value of point - for debugging purpose"
  (interactive)
  (message "point is %d" (point)))

(defun query-replace-word-under-cursor (replace-word)
  "Query-replace the world under the current cursor position."
  (interactive "sReplace current word with: ")
  (forward-word)
  (let ((end (point)))
    (progn
      (backward-word)
      (kill-ring-save (point) end)
      (message "Replacing %s with %s" (current-kill 0) replace-word)
      (query-replace (current-kill 0) replace-word))))

(defun grep-word-under-cusor ()
  "Grep the word under current directory"
  (interactive)
  (forward-word)
  (let ((end (point)))
    (progn
      (backward-word
       (kill-ring-save (point) end)
       (message "Greping word %s" (current-kill 0))
       (grep (current-kill 0))))))

(defun set-last-modified-tag ()
  "Insert current date after Last Modified:"
  (interactive)
  (let ((tostr (concat "Last Modified: " (current-time-string) ".")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\Last Modified:\\([A-Za-z0-9: ]*\\)?\\." nil t)
        (replace-match tostr nil t)))))

(defun split-3-windows ()
  "Split the current window into 3 equal-size sub-windows.
   Quite useful for large monitor."
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows))

(defun split-4-windows ()
  "Split the current window into 4 equal-size sub-windows.
   Quite useful for large monitor."
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows))

;; Update last modified tag every time we save this file.
(add-hook 'write-file-hooks
          (lambda ()
            (set-last-modified-tag)))

;;; CTAGS stuff
(defun create-tags (dir-name)
  " Create CTAGS under directory `dir-name`"
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -e -R %s" dir-name)))

(setq tags-table-list '(""))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

(defun insert-cdh-jira (jira-number)
  "Insert a org-mode link at the point for the specified CDH jira"
  (interactive "sJIRA Number: ")
  (insert
   (concat "[[https://jira.cloudera.com/browse/CDH-" jira-number
           "][CDH-" jira-number "]]")))

(defun insert-apache-jira (jira-code)
  "Insert a org-mode link at the point for the specified Apache jira"
  (interactive "sJIRA Code: ")
  (insert
   (concat "[[https://issues.apache.org/jira/browse/" jira-code
           "][" jira-code "]]")))

(defun my-transpose-sexps ()
  "If point is after certain chars transpose chunks around that.
Otherwise transpose sexps."
  (interactive "*")
  (if (not (looking-back "[,]\\s-*" (point-at-bol)))
      (progn (transpose-sexps 1) (forward-sexp -1))
    (let ((beg (point)) end rhs lhs)
      (while (and (not (eobp))
                  (not (looking-at "\\s-*\\([,]\\|\\s)\\)")))
        (forward-sexp 1))
      (setq rhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (re-search-backward "[,]\\s-*" nil t)
      (setq beg (point))
      (while (and (not (bobp))
                  (not (looking-back "\\([,]\\|\\s(\\)\\s-*" (point-at-bol))))
        (forward-sexp -1))
      (setq lhs (buffer-substring beg (point)))
      (delete-region beg (point))
      (insert rhs)
      (re-search-forward "[,]\\s-*" nil t)
      (save-excursion
        (insert lhs)))))

(defun open-or-switch-to (file)
  (let ((existing-buffer (find-buffer-visiting file)))
    (cond (existing-buffer (switch-to-buffer existing-buffer))
          (t (find-file-existing file)))))

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height)))

(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)

;;; Utility keybindings
;; (global-set-key (kbd "C-c t") 'describe-function-in-popup)
(global-set-key (kbd "C-x a t") 'query-replace-word-under-cursor)
(global-set-key (kbd "C-x 4") 'split-3-windows)
(global-set-key (kbd "C-x 5") 'split-4-windows)

;;; Ace-Window
(global-set-key (kbd "C-u") 'ace-window)

;;; ---------------------------------------------------------------------------
;;; Text Mode
(add-hook 'text-mode-hook
          (lambda ()
            (setq fill-column 72)))

;;; ---------------------------------------------------------------------------
;;; Term Mode

;; Don't want trailing whitespace for term
(add-hook 'term-mode-hook
  (lambda ()
    (progn
      (setq show-trailing-whitespace nil))))

;;; ---------------------------------------------------------------------------
;;; Rust Mode

(require 'rustic)
(require 'lsp-mode)
(require 'company)
(require 'yasnippet)
(require 'flycheck)

(defun get-rust-indent-offset ()
  "Setting up project specific indentation."
  (let ((path (projectile-project-root)))
    (cond
     ((string-match "leveldb-rs" path) 2)
     ((string-match "parquet-rs" path) 2)
     (t 4))
    )
  )

(defun rk/rustic-mode-hook ()
  "See https://github.com/brotzeit/rustic/issues/253."
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (fci-mode 1)
  (setq fill-column 100)
  (auto-fill-mode))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-command "~/.cargo/bin/rust-analyzer")
  (lsp-eldoc-render-all t)
  (lsp-eldoc-hook nil)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin")))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c p" . rustic-compile)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-e" . xref-find-references))

  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t))

(add-hook 'rust-mode-hook
          '(lambda ()
             (fci-mode 1)
             (setq fill-column 100)
             (setq rust-indent-offset (get-rust-indent-offset))
             (setq cargo-process--enable-rust-backtrace 1)
             (face-spec-set
              'rust-unsafe-face
              '((t :foreground "red"
                   :weight bold))
              'face-defface-spec)))

(defun my-cargo-run-bin (cmd)
  (interactive "MThe binary to run: ")
  (cargo-process-run-bin cmd))

(eval-after-load 'rust-mode
  (lambda ()
    (define-key rust-mode-map (kbd "C-q n") 'flycheck-next-error)
    (define-key rust-mode-map (kbd "C-q p") 'flycheck-previous-error)
    (define-key rust-mode-map (kbd "C-c r b") 'rustic-cargo-build)
    (define-key rust-mode-map (kbd "C-c r t") 'rustic-cargo-test)
    (define-key rust-mode-map (kbd "C-c r g") 'rustic-cargo-current-test)
    (define-key rust-mode-map (kbd "C-c r w") 'my-cargo-run-bin)))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;; ---------------------------------------------------------------------------
;;; Magit Mode

;;; To prevent Magit from reverting all unmodified buffer
;;; TODO: is this still necessary
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Remove diff highlight
(defun disable-magit-highlight-in-buffer ()
  (face-remap-add-relative 'magit-section-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

;; Customize diff color (for dark background)
(custom-set-faces
   '(magit-diff-added ((t (:foreground "lime green"))))
   '(magit-diff-added-highlight ((t (:foreground "lime green"))))
   '(magit-diff-base-highlight ((t (:foreground "#eeeebb"))))
   '(magit-diff-removed ((t (:foreground "red"))))
   '(magit-diff-removed-highlight ((t (:foreground "red"))))
   '(magit-diff-hunk-heading
     ((t (:foreground "#9a9aba" :background nil))))
   '(magit-diff-hunk-heading-highlight
     ((t (:foreground "#9a9aba" :background nil))))
   '(magit-blame-name ((t (:foreground "#b1951d" :background nil))))
   '(magit-blame-summary ((t (:foreground "#b1951d" :background nil))))
   '(magit-blame-hash ((t (:foreground "#bc6ec5" :background nil))))
   '(magit-blame-heading ((t (:foreground "#67b11d" :background nil))))
   '(magit-branch-current
     ((t (:inherit bold :foreground "#4f97d7" :background nil :box 1))))
   '(magit-branch-local
     ((t (:inherit bold :foreground "#4f97d7" :background nil)))))


;;; Keybindings
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g l") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g d") 'magit-diff)
(global-set-key (kbd "C-c g f") 'magit-stage-file)


;;; ---------------------------------------------------------------------------
;;; Paredit Mode

(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


;;; ---------------------------------------------------------------------------
;;; SML Mode

(defun my-sml-mode-hook () "Local defaults for SML mode"
  (setq sml-indent-level 2)        ; conserve on horizontal space
  (setq words-include-escape t)    ; \ loses word break status
  (setq indent-tabs-mode nil))     ; never ever indent with tabs
(add-hook 'sml-mode-hook 'my-sml-mode-hook)


;;; ---------------------------------------------------------------------------
;;; GGtags

(autoload 'gtags-mode "gtags" "" t)
(require 'ggtags) ;; See https://github.com/leoliu/ggtags
(add-hook
 'c-mode-common-hook
 (lambda ()
   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
     (ggtags-mode 1))))

;; Keybindings
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;;; ---------------------------------------------------------------------------
;;; Irony Mode

(require 'irony)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)


;;; ---------------------------------------------------------------------------
;;; Company Mode

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function

;; ;; (optional) adds CC special commands to `company-begin-commands' in order to
;; ;; trigger completion at interesting places, such as after scope operator
;; ;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; (add-hook 'c++-mode-hook 'company-mode)
;; (add-hook 'c-mode-hook 'company-mode)

;;; ---------------------------------------------------------------------------
;;; C++ Mode

(require 'cc-mode) ;; C++ mode
(require 'flymake) ;; Syntax checking on the fly

(defun my:flymake-google-init()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint"))
  (flymake-google-cpplint-load))
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'google-make-newline-indent)
(add-hook 'c++-mode-hook '(lambda () (setq fill-column 90)))

;; Impala C/C++ Style - inactive right now.
;; (require 'impala-c-style) ;;

;; slight modifications for Impala
;; (defun impala-c++-style-hook ()
;;   (c-set-offset 'arglist-intro '++)
;;   (c-set-offset 'arglist-cont-nonempty '++)
;;   (c-set-offset 'arglist-close '++))
;; (add-hook 'c++-mode-hook 'impala-c++-style-hook)

;; (add-hook 'c-mode-hook 'impala-set-c-style)
;; (add-hook 'c++-mode-hook 'impala-set-c-style)

(setq-default c-basic-offset 2 c-default-style "linux")
(setq-default tab-width 2 indent-tabs-mode nil)

(defun compile-recordservice ()
  "Compile the RecordService backend."
  (interactive)
  (compile "$IMPALA_HOME/bin/make_debug.sh --notests"))
(global-set-key (kbd "C-C C-V") 'compile-recordservice)

(defun switch-to-header-or-impl ()
  "Switch between .cc file and .h file."
 (interactive)
 (let ((other-file
        (cond
         ((not (buffer-file-name)) (error "Buffer not visiting a file"))
         ((string-match-p "\\.cc$" (buffer-file-name))
          (replace-regexp-in-string "\\.cc$" ".h"  (buffer-file-name)))
         ((string-match-p "\\.cpp$" (buffer-file-name))
          (replace-regexp-in-string "\\.cpp$" ".h"  (buffer-file-name)))
         ((string-match-p "\\.h$" (buffer-file-name))
          (replace-regexp-in-string "\\.h$" ".cc" (buffer-file-name)))
         (t (error "Not a .cc or .h file: %s" (buffer-file-name))))))
        (open-or-switch-to other-file)))


;;; ---------------------------------------------------------------------------
;;; Java Mode

(add-hook 'java-mode-hook '(lambda () (setq c-basic-offset 2)))
(add-hook 'java-mode-hook '(lambda () (setq fill-column 90)))
(add-hook 'java-mode-hook 'turn-on-auto-fill)


;;; ---------------------------------------------------------------------------
;;; Python Mode

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq python-indent 4)))


;;; ---------------------------------------------------------------------------
;;; Bash Mode

(defun my-sh-mode-hook ()
  "Change indentation space"
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)


;;; ---------------------------------------------------------------------------
;;; Latex Mode

;; load AUCTEX mode
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
(add-hook
 'LaTeX-mode-hook
 '(lambda ()
    (TeX-fold-mode t)
    (outline-minor-mode t)))

(add-hook 'LaTeX-mode-hook '(lambda () (setq fill-column 90)))
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)	;; use pdflatex as default
(add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd "C-c C-t") 'fill-region)))
(setq TeX-view-program-list '(("Preview" "open -a Preview %o")))
(setq TeX-view-program-selection '((output-pdf "Preview")))
(setq-default TeX-master "main")	;; make AUCTEX aware of the default


;;; ---------------------------------------------------------------------------
;;; Twelf Mode

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
       (set-fill-column 100) ;; break line at 100
       (auto-fill-mode))) ;; auto wrap lines
   )))


;;; ---------------------------------------------------------------------------
;;; Org Mode

(require 'org-install)
(require 'ob-tangle)

;;; Some tips:
;;; - to see a break-up of time spent for each day, go to calendar C-ca
;;; and press l.
;;; - to add a note to a calendar item, press C-c C-z
;;; - to attach a file to a org item, press C-c C-a
;;; - to search all topics under a particular tag, use C-c \
;;; - to insert a source code block, press <s

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

;; Set idle time
(setq org-clock-idle-time 10)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Enable fontifying code blocks
(setq org-src-fontify-natively t)

;; todo keywords
;; (setq org-todo-keywords
;;       (quote ((sequence "TODO(t)" "STARTED(s)" "WAITING(w)"
;;                         "DONE(d)" "CANCELLED(c)" "DEFERRED(f)"))))

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/notes.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t))))

(add-hook
 'org-mode-hook
 (lambda ()
   (setq-default fill-column 80)
   (turn-on-font-lock)
   (turn-on-auto-fill)
   (fci-mode 0) ;; turn-off fci mode
   (local-set-key (kbd "C-c w") 'delete-first-n)
   (local-set-key (kbd "C-c e") 'delete-trailing-whitespace)))

(setq org-agenda-files (list "~/Dropbox/org/gtd.org"))
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-clock-into-drawer t)

(let ((backcolor (assq :background face-attribute-name-alist)))
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("DONE" . (:foreground "orange"))
          ("STARTED" . (:foreground "darkgreen"))
          ("WAITING" . (:foreground "yellow"))
          ("CANCELLED" . (:foreground "blue" :weight bold)))))

;;; This code clocks in whenever a task is started, and clocks out whenever
;;; the task is waiting. Also automatically start task if clocks in.
(eval-after-load 'org
  '(progn
     (defun wicked/org-clock-in-if-starting ()
       "Clock in when the task is marked STARTED."
       (when (and (string= org-state "STARTED")
                  (not (string= org-last-state org-state)))
         (org-clock-in)))
     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-in-if-starting)
     (defadvice org-clock-in (after wicked activate)
       "Set this task's status to 'STARTED'."
       (org-todo "STARTED"))
     (defun wicked/org-clock-out-if-waiting ()
       "Clock out when the task is marked WAITING."
       (when (and (string= org-state "WAITING")
                  (equal (marker-buffer org-clock-marker) (current-buffer))
                  (< (point) org-clock-marker)
                  (> (save-excursion (outline-next-heading) (point))
                     org-clock-marker)
                  (not (string= org-last-state org-state)))
         (org-clock-out)))
     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-out-if-waiting)))

(defun my/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (my/quantified-get-hours x time-summary)) category))))

(setq org-agenda-custom-commands
      (quote
       (("w" "Work related items"
         (
          (tags "@WORK"
                ((org-agenda-overriding-header "Next Work Items")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done)
                  )
                 (org-agenda-sorting-strategy '(priority-down))))
          (tags "@WORK+TODO=\"POSTPONED\""
                ((org-agenda-overriding-header "Deferred Work Items")
                 (org-agenda-sorting-strategy '(priority-down)))))
         )
        )
       ))

;;; Key Bindings
(global-set-key (kbd "C-c o c") 'org-clock-goto) ;; go to currently clock item
(global-set-key (kbd "C-c o p") 'org-capture) ;; add a task for future


;;; ---------------------------------------------------------------------------
;;; Go Mode

(defun switch-test-file ()
  "Switch between xx.go file and xx_test.go file."
  (interactive)
  (let ((other-file
         (cond
          ((not (buffer-file-name)) (error "Buffer not visiting a file"))
          ((string-match-p "\\(\\.*\\)_test.go$" (buffer-file-name))
           (replace-regexp-in-string "\\(\\.*\\)_test.go$" "\\1.go"  (buffer-file-name)))
          ((string-match-p "\\(\\.*\\).go$" (buffer-file-name))
           (replace-regexp-in-string "\\(\\.*\\).go$" "\\1_test.go" (buffer-file-name)))
          (t (error "Not a xx.go or xx_test.go file: %s" (buffer-file-name))))))
    (open-or-switch-to other-file)))

;;; Copy GOPATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))
;; TODO: parse GOPATH and populate PATH with entries
;; (setenv "PATH" (concat (getenv "PATH") ":/Users/chao/go/bin"))
;; (setq exec-path (cons "/Users/chao/go/bin" exec-path))

(require 'go-guru)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;; Helpful shortcuts
;;; 1) C-M-a: beginning of defun
;;; 2) C-M-e: end of defun
;;; 3) M-.: jump to definition
;;; 4) C-c C-d: describe definition


(defun my-go-mode-hook ()
  (whitespace-mode -1) ; don't highlight hard tabs
  (local-set-key (kbd "M-.") 'godef-jump)
  (auto-complete-mode 1)
  (flycheck-mode 1)
  (fci-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
    (set (make-local-variable 'compile-command)
      "go generate && go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-p") 'switch-test-file)
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-k") 'godoc)
  (local-set-key (kbd "C-q n") 'flycheck-next-error)
  (local-set-key (kbd "C-q p") 'flycheck-previous-error)
  (local-set-key (kbd "C-x t") 'go-test-current-test)
  (local-set-key (kbd "C-x f") 'go-test-current-file)
  (local-set-key (kbd "C-c C-f") 'find-file-in-repository)

  (setq
   gofmt-command "goimports"
   tab-width 2         ; display tabs as two-spaces
   indent-tabs-mode 1  ; use hard tabs to indent
   fill-column 100)    ; set a reasonable fill width
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;; ---------------------------------------------------------------------------
;;; Haskell Mode
(add-hook 'haskell-mode-hook #'hindent-mode)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "M-8") 'haskell-navigate-imports))


;;; ---------------------------------------------------------------------------
;;; XML Mode

(setq
  nxml-child-indent 4
  nxml-attribute-indent 4
  nxml-slash-auto-complete-flag t)


;;; ---------------------------------------------------------------------------
;;; Markdown Mode

(add-hook 'markdown-mode-hook
         '(lambda ()
            (setq-default fill-column 80)
              (auto-fill-mode t)))


;;; ---------------------------------------------------------------------------
;;; Customized Functions

;;; Quickly find my GTD file
(defun gtd ()
  (interactive)
  (find-file (concat (getenv "HOME") "/Dropbox/org/gtd.org")))

(defun create-jira-link ()
  ;;; TODO: implement this!
  (interactive "r\JIRA #: "))

;; If we want to create internal links in a org file, and show the
;; links on github, we need to create properties with
;; "CUSTOM_ID" of a particular format. For a header like "Foo Bar", we
;; need a "CUSTOM_ID" of "foo-bar". Here we define a command to
;; automatically create such property for us.
(defun org-process-link ()
  "Generate CUSTOM_IDs for all level-2 headings (which, in my case, are
the name of all individual problems). For a heading like 'Foo Bar',
generate a id 'foo-bar'. Also, generate a list of links AFTER CURRENT POINT."
  (interactive)
  (let ((pairs nil))
    (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\*[ \t]+" nil t)
      (let ((old)
            (new)
            (end-of-line
             (save-excursion
               (end-of-line)
               (point))))
        (save-excursion
          (while (< (point) end-of-line)
            (forward-word)
            (let ((oldc (if old " " ""))
                  (newc (if new "-" "")))
              (setq old (concat old oldc (word-at-point))
                    new (concat new newc (downcase (word-at-point)))))))
        (org-set-property "CUSTOM_ID" new)
        (setq pairs (cons (list new old) pairs)))))
    (mapc (lambda (p) (princ (concat "\t- [[#" (car p) "][" (cadr p) "]]\n")
                        (current-buffer))) pairs)))

(setq org-src-fontify-natively t) ;; fontify code block automatically

;;; Key bindings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)


;;; ---------------------------------------------------------------------------
;;; Key Bindings

(require 'find-file-in-repository)

(global-set-key (kbd "C-c C-v") 'compile)
(global-set-key (kbd "C-c C-r") 'query-replace-regexp)
(global-set-key (kbd "C-c C-f") 'counsel-git)
(global-set-key (kbd "C-c C-o") 'counsel-locate)
(global-set-key (kbd "C-c C-k") 'my-transpose-sexps)
(global-set-key (kbd "C-c C-g") 'counsel-rg)
(global-set-key (kbd "C-x a c") 'comment-region)
(global-set-key (kbd "C-x a u") 'uncomment-region)
(global-set-key (kbd "C-x a d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a f") 'recentf-open-files)


;;; --------------------------------- THE END ---------------------------------
