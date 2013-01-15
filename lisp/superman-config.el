;;; superman-config.el --- Project specific window configurations

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

(defun superman-find-index (project)
  (let* ((index (superman-get-index project)))
    (unless (file-exists-p index)
      (unless (file-exists-p (file-name-directory index))
	(make-directory (file-name-directory index) 'with-parents))
      (make-directory (file-name-directory index) 'with-parents))
    (find-file index)))

(defun superman-file-list (project)
  (if (featurep 'file-list)
      (let ((loc (concat (superman-get-location project) (car project))))
	(cond ((file-list-select-internal nil "." nil nil loc (concat "*File-list-" (car project) "*")))
	      (t
	       (switch-to-buffer (concat "*File-list-" (car project) "*"))
	       (toggle-read-only -1)
	       (erase-buffer)
	       (insert "FILE-LIST: No files in project"))))
    (error "file-list.el not loaded.")))


(defun superman-find-thing (thing project)
  (save-window-excursion
    (let* ((case-fold-search t)
	   (action (cdr (assoc (replace-regexp-in-string "^[ \t\n]+\\|[ \t\n]+$" ""  thing)
			       superman-config-action-alist))))
      (cond ((functionp action) (funcall action project))
	    ((and thing (file-name-directory thing))
	     (find-file (expand-file-name
			 thing (concat (superman-get-location project) (car project)))))
	    (t (switch-to-buffer thing))))
    (current-buffer)))

(defun superman-read-config-list (string)
  ;; return a list of lists with vertical splits 
  ;; where each element can have horizontal splits
  (split-string string "[ \t]+:[ \t]+"))

(defun superman-read-config (config &optional pos)
  ;; return a list with horizontal splits 
  ;; where each element can have vertical splits
  (let* ((vlist (split-string config "[ \t]+|[ \t]+"))
	 (hlist (mapcar '(lambda (x) (split-string x "[ \t]+/[ \t]+")) vlist)))
    hlist))



(defun superman-save-config (&optional config project)
  (interactive)
  (let ((conf (or config (superman-current-config)))
	(pro (or project superman-current-project (superman-select-project))))
    (find-file-other-window (concat (superman-get-location pro) (car pro) "/.superman-window-config"))
    (goto-char (point-max))
    (unless (looking-at "^$") (insert "\n"))
    (insert conf)
    (save-buffer)))

(defun superman-current-config ()
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

(defun superman-get-config (project)
  (let* ((filed-config (when (file-exists-p config-file)
			 (save-window-excursion
			   (with-temp-buffer (find-file (concat
							 (superman-get-location project)
							 (car project) "/.superman-window-config"))
					     (setq str (replace-regexp-in-string
							"\n" " : "
							(replace-regexp-in-string "[\n\t ]+$" ""
										  (buffer-string))))))))
	 config
 	 (prop-config (cdr (assoc "config" (cadr project)))))
    (when filed-config
 	(setq config (concat (if superman-sticky-config (concat superman-sticky-config " : ")) filed-config)))
    (when prop-config
 	(setq config (concat (if config (concat config " : ")) prop-config)))
    (when (not config) (setq config superman-default-config))
    config))


(defun superman-split-windows (window-config project)
  (let ((ncolumns (length window-config))
	(nrows (mapcar 'length window-config))
	top-windows)
    (delete-other-windows)
    (setq top-windows (list (selected-window)))
    ;; first create the windows 
    (loop for c from 1 to (- ncolumns 1) do
	  (split-window-horizontally)
	  (other-window 1)
	  (setq top-windows (append top-windows (list (selected-window)))))
    (loop for c from 0 to (- ncolumns 1) do
	  (select-window (nth c top-windows)) ;; switch to the top-window in this column
	  (let ((nrow (nth c nrows)));; number of rows in this column
	    (loop for r from 0 to (- nrow 1) do
		  (switch-to-buffer (superman-find-thing (nth r (nth c window-config)) project))
		  (when (and (< r (- nrow 1)) (> nrow 1 ))
		    (split-window-vertically)
		    (other-window 1)))))
    (select-window (nth 0 top-windows))))
    
(defun superman-set-config (&optional project config pos)
  "Set a user defined window configuration."
  (interactive)
  (let* ((pro (or project superman-current-project (superman-select-project)))
	 (conf (or config (superman-get-config pro)))
	 (pos (or pos superman-config-cycle-pos 0))
	 (window-config (superman-read-config (nth pos (superman-read-config-list conf)))))
    ;;(message conf)
    (superman-split-windows window-config pro)))


(defun superman-magit (project)
  (magit-status (concat (superman-get-location project) (car project))))

(defun superman-location (project)
  (let ((loc (concat (superman-get-location project) (car project))))
    (find-file loc)))

(defun superman-timeline (project)
  (let (tbuf)
    (save-window-excursion
      (let ((index (superman-get-index project))
            (org-agenda-sticky nil)
            (bufname (concat "*" (car project) "-timeline*")))
        (if (not (file-exists-p index))
            (progn
              (switch-to-buffer bufname)
              (setq tbuf (get-buffer bufname))
              (toggle-read-only -1)
	      (erase-buffer)
              (insert "TIMELINE: Project index file does not exist yet"))
          (when (get-buffer bufname)
            (kill-buffer bufname))
          (find-file index)
          (org-timeline 'yeah)
	  (rename-buffer bufname)
	  (local-set-key [(return)] 'org-return)
	  (setq tbuf (current-buffer)))))
    (switch-to-buffer tbuf)))

(defun superman-recent-org (project)
  (car (superman-list-files
	(concat (superman-get-location project) (car project)) "^[^\\.].*\\.org$" "time")))


(defun superman-todo (project)
  (let (tbuf)
    (save-window-excursion
      (let* ((location (concat (superman-get-location project) (car project)))
	     (org-files (superman-list-files location "^[^\\.].*\\.org$" nil))
	     (org-agenda-sticky t) ;; to enable multiple agenda buffers
	     (org-agenda-files org-files)
	     (bufname (concat "*" (car project) "-todo*")))
	(when (get-buffer bufname)
	  (kill-buffer bufname))
	(org-todo-list org-match)
	(rename-buffer bufname)
	(setq tbuf (current-buffer))))
    (switch-to-buffer tbuf)))

(defun superman-find-project (project pos)
  (superman-set-config project nil (or pos superman-config-cycle-pos 0)))


(defun superman-get (project el)
  (cdr (assoc el (cadr project))))

(defun superman-get-index (project)
"Extract the index file of PROJECT."
  (cdr (assoc "index" (cadr project))))


(defun superman-get-git (project)
  (or (cdr (assoc "git" (cadr project))) ""))

(defun superman-get-git-location (project)
  (or (cdr (assoc "git-location" (cadr project)))
      (concat (superman-get-location project) (car project))))

(defun superman-get-location (project)
  "Get the directory associated with PROJECT."
  (file-name-as-directory (cdr (assoc "location" (cadr project)))))
;;  (let ((loc (cdr (assoc "location" (cadr project)))))
;;                (if loc 
;;                                (concat (file-name-as-directory loc)
;;                                        (car project)))))

(defun superman-get-publish-directory (project)
  (cdr (assoc "publish-directory" (cadr project))))

(defun superman-get-category (project)
  (cdr (assoc "category" (cadr project))))

(defun superman-get-state (project)
  (cdr (assoc "state" (cadr project))))


(defun superman-switch-config (&optional project)
  "Switch to the next window configuration (if any)."
  (interactive)
  (let* ((pro (or project superman-current-project))
	 (curpos (or superman-config-cycle-pos 0))
	 (config-list (superman-read-config-list
		       (superman-get-config pro))))
    (if (> (length config-list) (1+ superman-config-cycle-pos));; cycle-pos starts at 0
	(setq superman-config-cycle-pos (1+ superman-config-cycle-pos))
      (setq superman-config-cycle-pos 0))
    (superman-find-project pro superman-config-cycle-pos)))


(provide 'superman-config)
;;; superman-config.el ends here
