(require 'org-install)
(require 'csun-utils)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-c w") 'delete-first-n)))
(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "C-c e") 'delete-trailing-whitespace)))

(setq org-agenda-files (list "~/Dropbox/org/gtd.org"))
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-clock-into-drawer t)
(setq org-agenda-custom-commands
  '(("H" "Office and Home Worklist"
    ((agenda "")
       (tags-todo "OFFICE")
       (tags-todo "HOME")
       (tags-todo "JOB")
       (tags-todo "WORK")
       (tags-todo "PROJECT")
       (tags-todo "READING")))))

(let ((backcolor (assq :background face-attribute-name-alist)))
  (setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("STARTED" . (:foreground "gold" :background backcolor))
        ("WAITING" . (:foreground "red" :background backcolor))
        ("CANCELLED" . (:foreground "blue" :weight bold)))))


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

(provide 'csun-org)
