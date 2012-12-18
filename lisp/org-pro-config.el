;;; org-pro-config.el --- Project specific window configurations

;; Copyright (C) 2012  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: tools

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
;; It may be counterintuitive, but saving a window configuration is not
;; so easy. One reason are windows showing unsaved, temporary stuff such
;; as file-unrelated buffers. Also, the dimensions of
;; frames and windows depend on the current screen and most people will
;; at least occasionally work on different screens.

;; What we could do is restore from saved files and certain
;; file-unrelated buffers, as for example a buffer showing a shell. We
;; can also save the number of windows and the horizontal and vertical
;; splits in the current frame. 

;;; Code:

(setq org-pro-default-config "INDEX")
(setq org-pro-sticky-config nil)

(defvar org-pro-file-manager "file-list")

;; could be 
;; (setq org-pro-sticky-config "recent.org / *R* | TODO")
(setq org-pro-config-action-alist '(("INDEX" . org-pro-find-index)
				    ("TODO" . org-pro-todo)
				    ("TIMELINE" . org-pro-timeline)
				    ("LOCATION" . org-pro-location)
				    ("DOCUMENTS" . org-pro-view-documents)
				    ("FILELIST" . org-pro-file-list)
				    ("magit" . org-pro-magit)
				    ("recent.org" . org-pro-recent-org)
				    ("*shell*" . (lambda (project) (if (get-buffer "*shell*") (switch-to-buffer "*shell*") (shell))))
                                    ("*ielm*" . (lambda (project) (if (get-buffer "*ielm*") (switch-to-buffer "*ielm*") (ielm))))
				    ("*R*" . org-pro-find-R-function)))

(defvar org-pro-find-R-function
  "Function used to find *R*"
  (lambda (project) (if (get-buffer "*R*") (switch-to-buffer "*R*") (R))))

(defun org-pro-find-index (project)
  (let* ((index (org-pro-get-index project)))
    (unless (file-exists-p index)
      (unless (file-exists-p (file-name-directory index))
	(make-directory (file-name-directory index) 'with-parents))
      (make-directory (file-name-directory index) 'with-parents))
    (find-file index)))



(defun org-pro-file-list (project)
  (if (featurep 'file-list)
      (let ((loc (concat (org-pro-get-location project) (car project))))
	(cond ((file-list-select-internal nil "." nil nil loc (concat "*File-list-" (car project) "*")))
	      (t
	       (switch-to-buffer (concat "*File-list-" (car project) "*"))
	       (toggle-read-only -1)
	       (erase-buffer)
	       (insert "FILE-LIST: No files in project"))))
    (error "file-list.el not loaded.")))

(defun org-pro-find-thing (thing project)
  (let* ((case-fold-search t)
	 (action (cdr (assoc (replace-regexp-in-string "^[ \t\n]+\\|[ \t\n]+$" ""  (car thing))
			     org-pro-config-action-alist))))
    (cond ((functionp action) (funcall action project))
	  ((and (car thing) (file-name-directory (car thing)))
	   (find-file (expand-file-name
		       (car thing) (concat (org-pro-get-location project) (car project)))))
	  (t (switch-to-buffer (car thing))))))

(defun org-pro-read-config-list (string)
  ;; return a list of lists with vertical splits 
  ;; where each element can have horizontal splits
  (split-string string "[ \t]+:[ \t]+"))

(defun org-pro-read-config (config &optional pos)
  ;; return a list with horizontal splits 
  ;; where each element can have vertical splits
  (let* ((vlist (split-string config "[ \t]+|[ \t]+"))
	 (hlist (mapcar '(lambda (x) (split-string x "[ \t]+/[ \t]+")) vlist)))
    hlist))


(defun org-pro-save-config (&optional config project)
  (interactive)
  (let ((conf (or config (org-pro-current-config)))
	(pro (or project org-pro-current-project (org-pro-select-project))))
    (find-file-other-window (concat (org-pro-get-location pro) (car pro) "/.org-pro-window-config"))
    (goto-char (point-max))
    (unless (looking-at "^$") (insert "\n"))
    (insert conf)
    (save-buffer)))

(defun org-pro-current-config ()
  (let* ((windata (winner-win-data))
	 config
	 prev-row)
    (while windata
      (let* ((buf (cdr (car windata)))
	     (pos (car (car windata)))
	     ;;	     (col (nth 0 pos))
	     (row (nth 1 pos))
	     (thing
	      (cond 
	       ((buffer-file-name buf)
		(replace-regexp-in-string (getenv "HOME") "~"  (buffer-file-name buf)))
	       ;; (get-buffer-process buf)
	       (t (buffer-name buf)))))
	(setq config (concat config (when prev-row (if (< prev-row row) " / " " | ")) thing))
	(setq windata (cdr windata))
	(setq prev-row row)))
    config))


(defvar org-pro-config-cycle-pos 0 "Position in the current window configuration cycle. Starts at 0.")


(defun org-pro-get-config (project)
  (let* ((config (or org-pro-sticky-config org-pro-default-config "INDEX"))
	 (config-file  (concat (org-pro-get-location project) (car project) "/.org-pro-window-config"))
	 (filed-config (when (file-exists-p config-file)
			 (save-window-excursion
			   (find-file config-file)
			   (replace-regexp-in-string "\n" " : "  (replace-regexp-in-string "[\n\t ]+$" "" (buffer-string))))))
	 (prop-config (cdr (assoc "config" (cadr project)))))
    (if (not config)
	(setq config prop-config)
      (when filed-config
	(setq config (concat config " : " filed-config)))
      (when prop-config
	(setq config (concat config " : " prop-config)))
      config)))


(defun org-pro-set-config (&optional project config pos)
  (interactive)
  (let* ((pro (or project org-pro-current-project (org-pro-select-project)))
	 (conf (or config (org-pro-get-config pro)))
	 (pos (or pos org-pro-config-cycle-pos 0))
	 (window-config (org-pro-read-config (nth pos (org-pro-read-config-list conf))))
	 (ncolumns (length window-config))
	 top-windows)
    ;;(message conf)
    (delete-other-windows)
    (setq top-windows (list (selected-window)))
    (loop for n from 1 to (- ncolumns 1) do
	  (split-window-horizontally)
	  (other-window 1)
	  (setq top-windows (append top-windows (list (selected-window)))))
    (loop for n from 0 to (- ncolumns 1) do 
	  (select-window (nth n top-windows))
	  (let ((el (nth n window-config)))
	    (while el
	      (org-pro-find-thing el pro)
	      (setq el (cdr el))
	      (when el (split-window-vertically) (other-window 1)))))
    (select-window (nth 0 top-windows))))




(provide 'org-pro-config)
;;; org-pro-config.el ends here
