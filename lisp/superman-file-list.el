;;; superman-file-list.el --- List files in projects

;; Copyright (C) 2014  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Author: Klaus Kähler Holst <kkho@biostat.ku.dk>
;;
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(defun superman-view-file-list (&optional arg)
  (interactive "p") 
  (let ((pro (superman-view-current-project)))
    (if (and arg (> arg 1))
	;; old-style file-list display 
	(progn
	  (split-window-vertically)
	  (other-window 1)
	  (superman-file-list pro))
      (superman-display-file-list pro))))

(defun superman-file-list (project &optional ext)
  "List files in project's location that match extension EXT"
  (if (featurep 'file-list)
      (let ((loc (concat (superman-get-location project) (car project))))
	(cond ((file-list-select-internal nil (or ext ".")
					  nil nil loc (concat "*File-list-" (car project) "*")))
	      (t
	       (switch-to-buffer (concat "*File-list-" (car project) "*"))
	       (toggle-read-only -1)
	       (erase-buffer)
	       (insert "FILE-LIST: No files in project"))))
    (error "file-list.el not loaded.")))

(defun superman-write-file-list (list buf)
  "Write file-list LIST to temporary buffer BUF."
  (get-buffer-create buf)
  (switch-to-buffer buf)
  (erase-buffer)
  (while list
    (insert "* " (caar list) "\n"
	    ":PROPERTIES:\n:PATH: "
	    (cadar list) "\n:END:\n")
    (setq list (cdr list))))

(defvar superman-file-list-balls
  '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" font-lock-variable-name-face))
    ("Path" ("fun" superman-dont-trim) ("face" font-lock-keyword-face)))
  "balls used to display file-lists.")

(defun superman-display-file-list (project &optional list balls ext view-buffer refresh)
  "Format file-list displays."
  (let* ((nick (if project (car project) (get-text-property (point-min) 'nickname)))
	 (loc (or (get-text-property (point-min) 'loc)
		  (when project
		    (concat (superman-get-location project) (car project)))
		  (error "Missing location")))
	 (balls (or balls superman-default-balls))
	 (view-buf (or view-buffer (get-buffer-create (concat "*FileList: " nick "*"))))
	 (list (if refresh
		   (file-list-select-internal
		    nil (or ext ".")
		    nil nil loc (concat "*File-list-" (or nick "nix") "*"))
		 (or list
		     (with-current-buffer view-buf
		       file-list-current-file-list)
		     (file-list-select-internal
		      nil (or ext ".")
		      nil nil loc (concat "*File-list-" (or nick "nix") "*")))))
	 (balls superman-file-list-balls)
	 (count 0)
	 ;; (cycle file-list-display-level)
	 (maxfile (apply 'max (mapcar #'(lambda (x) (length (car x))) list)))
	 cycle)
    (setcdr (assoc "width" (assoc "FileName" superman-file-list-balls)) `(,maxfile))
    (switch-to-buffer view-buf)
    (org-mode)
    (font-lock-mode -1)
    (setq file-list-current-file-list list)
    (setq display-level (get-text-property (point-min) 'file-list-display))
    (if (and (not refresh) display-level)
	nil ;; buffer exists and needs no refreshment
      ;; (unless display-level
      ;; (put-text-property (point-min) (1+ (point-min)) 'file-list-display cycle))
      ;; insert the file-list 
      (goto-char (point-min))
      (erase-buffer)
      ;; prepare buffer if necessary
      (run-hooks 'superman-file-list-pre-display-hook)
      (goto-char (point-min))
      (insert (superman-make-button
	       (concat "SuperMan:FileList"
		       (when nick (concat ": " nick)))
	       'superman-redo
	       'superman-project-button-face
	       "Refresh project view"))
      (put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
      (put-text-property (point-at-bol) (point-at-eol) 'loc loc)
      (insert "\n")
      (superman-view-insert-action-buttons
       '(("/name" file-list-by-name)
	 ("/path" file-list-by-path)
	 ("/time" file-list-by-time)
	 ("/size" file-list-by-size)
	 ("widen" (lambda () (interactive)
		    (superman-display-file-list
		     nil nil nil nil nil 'refresh))))
       nil "Filter:")
      (superman-view-insert-action-buttons
       '(("by name" file-list-sort-by-name)
	 ("by path" file-list-sort-by-path)
	 ("by time" file-list-sort-by-time)
	 ("by size" file-list-sort-by-size)) nil "  Sort:")
      (superman-view-insert-action-buttons
       '(("grep" file-list-grep)
	 ("kill" file-list-remove)
	 ("copy" file-list-copy)
	 ("query-replace" file-list-query-replace)
	 ("clear display" file-list-clear-display)) nil "Action:")
      (insert "\n\n* File-list:")
      (put-text-property (point-at-bol) (point-at-eol) 'file-list-start t)
      (insert "\n")
      (while list
	(let* ((el (car list))
	       (file (car el))
	       (path (cadr el)))
	  ;; (insert " ")
	  ;; (put-text-property (point-at-bol) (1+ (point-at-bol)) 'filename file)
	  ;; (put-text-property (point-at-bol) (1+ (point-at-bol)) 'path path)
	  (insert (superman-format-thing
		   `(list (("FileName" . ,file)
			   ("Path" . ,path)))
		   balls) "\n")
	  (setq list (cdr list))))
      (goto-char (next-single-property-change (point-min) 'file-list-start))
      (end-of-line)
      (insert "\n")
      ;; insert the column names
      (when superman-empty-line-after-cat (insert "\n"))
      (insert (superman-column-names balls))
      (superman-file-list-mode)
      (run-hooks 'superman-file-list-pre-display-hook))))

(defun superman-file-list-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map file-list-mode-map)
  (setq major-mode 'superman-file-list-mode)
  (setq mode-name "file-list")
  (make-local-variable 'file-list-current-file-list)
  (make-local-variable 'file-list-match-history)
  (run-hooks 'file-list-completion-mode-hook))

(provide 'superman-file-list)
;;; superman-file-list.el ends here
