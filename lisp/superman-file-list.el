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
    (if arg
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
  '(("FileName" ("fun" superman-dont-trim))
    ("Path" ("fun" superman-dont-trim)))
  "balls used to display file-lists.")

(defun superman-display-file-list (project &optional balls ext refresh)
  "Format file-list displays."
  (let* ((nick (car project))
	 (loc (concat (superman-get-location project) (car project)))
	 (balls (or balls superman-default-balls))
	 (list (file-list-select-internal
		nil (or ext ".")
		nil nil loc (concat "*File-list-" (car project) "*")))
	 (balls superman-file-list-balls)
	 (count 0)
	 ;; (cycle file-list-display-level)
	 (view-buf (get-buffer-create (concat "*FileList: " nick "*")))
	 cycle)
    (switch-to-buffer view-buf)
    (setq display-level (get-text-property (point-min) 'file-list-display))
    (if (and (not refresh) display-level)
	nil ;; buffer exists and needs no refreshment
      (unless display-level
	(put-text-property (point-min) (1+ (point-min)) 'file-list-display cycle))
      ;; insert the file-list 
      (goto-char (point-min))
      (erase-buffer)
      ;; prepare buffer if necessary
      (run-hooks 'superman-file-list-pre-display-hook)
      (goto-char (point-min))
      (insert "\nFile-list:\n")
      (while list
	(let* ((el (car list))
	       (file (car el))
	       (path (cadr el)))
	  (insert " ")
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'file file)
	  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'path path)
	  (setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
	  (setq list (cdr list))))
      (goto-char (point-min))
      ;; insert the column names
      (when superman-empty-line-after-cat (insert "\n"))
      (insert (superman-column-names balls))
      (run-hooks 'superman-file-list-pre-display-hook))))
	    
(provide 'superman-file-list)
;;; superman-file-list.el ends here
