(require 'org-install)
(require 'ob-tangle)
(require 'csun-utils)

;;; Some tips:
;;; - to see a break-up of time spent for each day, go to calendar C-ca
;;; and press l.
;;; - to add a note to a calendar item, press C-c C-z
;;; - to attach a file to a org item, press C-c C-a
;;; - to search all topics under a particular tag, use C-c \


;;; ========================= My settings ============================ ;;

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

;; Set idle time
(setq org-clock-idle-time 10)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Set fill-column to 80
(setq fill-column 80)

;; Enable fontifying code blocks
(setq org-src-fontify-natively t)

(add-hook
 'org-mode-hook
 (lambda ()
   (turn-on-font-lock)
   (turn-on-auto-fill)
   (local-set-key (kbd "C-c w") 'delete-first-n)
   (local-set-key (kbd "C-c e") 'delete-trailing-whitespace)))

(setq org-agenda-files (list "~/Dropbox/org/gtd.org"))
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-clock-into-drawer t)

(let ((backcolor (assq :background face-attribute-name-alist)))
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("DONE" . (:foreground "orange"))
          ("STARTED" . (:foreground "gold"))
          ("WAITING" . (:foreground "yellow"))
          ("CANCELLED" . (:foreground "blue" :weight bold)))))

;;; =================== My customized functions ==================== ;;;

;;; Quickly find my GTD file
(defun gtd ()
  (interactive)
  (find-file (concat (getenv "HOME") "/Dropbox/org/gtd.org")))

;;; I found that, often given a Hive JIRA number, I need to create a corresponding URL
;;; link to it.

(defun create-jira-link ()
  ;;; TODO: implement this!
  (interactive "r\JIRA #: ")
  )

;; Load SQL source code blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

;; If we want to create internal links in a org file, and show the
;; links on github, we need to create properties with
;; "CUSTOM_ID" of a particular format. For a header like "Foo Bar", we
;; need a "CUSTOM_ID" of "foo-bar". Here we define a command to
;; automatically create such property for us.
(defun csun/org-process-link ()
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


;;; Babel mode

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

(provide 'csun-org)
