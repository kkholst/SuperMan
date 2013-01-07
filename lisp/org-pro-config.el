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


(defun org-pro-magit (project)
  (magit-status (concat (org-pro-get-location project) (car project))))

(defun org-pro-location (project)
  (let ((loc (concat (org-pro-get-location project) (car project))))
    (find-file loc)))

(defun org-pro-timeline (project)
  (let (tbuf)
    (save-window-excursion
      (let ((index (org-pro-get-index project))
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

(defun org-pro-recent-org (project)
  (car (org-pro-list-files
	(concat (org-pro-get-location project) (car project)) "^[^\\.].*\\.org$" "time")))


(defun org-pro-todo (project)
  (let (tbuf)
    (save-window-excursion
      (let* ((location (concat (org-pro-get-location project) (car project)))
	     (org-files (org-pro-list-files location "^[^\\.].*\\.org$" nil))
	     (org-agenda-sticky t) ;; to enable multiple agenda buffers
	     (org-agenda-files org-files)
	     (bufname (concat "*" (car project) "-todo*")))
	(when (get-buffer bufname)
	  (kill-buffer bufname))
	(org-todo-list org-match)
	(rename-buffer bufname)
	(setq tbuf (current-buffer))))
    (switch-to-buffer tbuf)))

(defun org-pro-find-project (project pos)
  (org-pro-set-config project nil (or pos org-pro-config-cycle-pos 0)))


(defun org-pro-get (project el)
  (cdr (assoc el (cadr project))))

(defun org-pro-get-index (project)
"Extract the index file of PROJECT."
  (cdr (assoc "index" (cadr project))))


(defun org-pro-get-git (project)
  (or (cdr (assoc "git" (cadr project))) ""))

(defun org-pro-get-git-location (project)
  (or (cdr (assoc "git-location" (cadr project)))
      (concat (org-pro-get-location project) (car project))))

(defun org-pro-get-location (project)
  "Get the directory associated with PROJECT."
  (file-name-as-directory (cdr (assoc "location" (cadr project)))))
;;  (let ((loc (cdr (assoc "location" (cadr project)))))
;;                (if loc 
;;                                (concat (file-name-as-directory loc)
;;                                        (car project)))))

(defun org-pro-get-publish-directory (project)
  (cdr (assoc "publish-directory" (cadr project))))

(defun org-pro-get-category (project)
  (cdr (assoc "category" (cadr project))))

(defun org-pro-get-state (project)
  (cdr (assoc "state" (cadr project))))


(defun org-pro-switch-config (&optional project)
  "Switch to the next window configuration (if any)."
  (interactive)
  (let* ((pro (or project org-pro-current-project))
	 (curpos (or org-pro-config-cycle-pos 0))
	 (config-list (org-pro-read-config-list
		       (org-pro-get-config pro))))
    (if (> (length config-list) (1+ org-pro-config-cycle-pos));; cycle-pos starts at 0
	(setq org-pro-config-cycle-pos (1+ org-pro-config-cycle-pos))
      (setq org-pro-config-cycle-pos 0))
    (org-pro-find-project pro org-pro-config-cycle-pos)))


(provide 'org-pro-config)
;;; org-pro-config.el ends here
