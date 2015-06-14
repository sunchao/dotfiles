;; Last Modified: Thu May 14 09:48:15 2015.
;; utility functions

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
          (princ (buffer-substring-no-properties (line-beginning-position)
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
  (interactive "r\nNHow many: ")
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

(defun make-spaces (begin end &optional n)
  "For all lines in the region, add n spaces before each line.\n
  In default, add 2 spaces"
  (interactive "r\nNHow many spaces to add ")
  (or n (setq n 2))
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
        (beginning-of-line)
        (insert-char `?\ ' n t)
        (setq nlines (1- nlines))
        (forward-line)))))

(defun print-point ()
  "Print value of point - for debugging purpose"
  (interactive)
  (message "point is %d" (point)))

(defun describe-function-in-popup ()
  "Describe Elisp function in a popup window."
  (interactive)
  (let ((desc (save-window-excursion
                (describe-function (symbol-at-point))
                (switch-to-buffer "*Help*")
                (buffer-string))))
    (popup-tip desc
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

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
      (while (re-search-forward "\\Last Modified:\\([A-Za-z0-9: ]*\\)?\\." nil t)
        (replace-match tostr nil t)))))

(defun split-3-windows ()
  "Split the current window into 3 equal-size sub-windows.
   Useful for large monitor"
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows))


;; keybindings
(global-set-key (kbd "C-c t") 'describe-function-in-popup)
(global-set-key (kbd "C-x a t") 'query-replace-word-under-cursor)
(global-set-key (kbd "C-x 4") 'split-3-windows)


;; Some settings

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

(provide 'csun-utils)
