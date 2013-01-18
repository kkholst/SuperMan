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
      (message thing)
      (cond ((functionp action) (funcall action project))
	    ((and thing (string= (substring thing 0 1) "!"))
	     (superman-start-shell (substring thing 1 (length thing))))
	    ((and thing (file-name-directory thing))
	     (find-file (expand-file-name
			 thing (concat (superman-get-location project) (car project)))))
	    (t (switch-to-buffer thing))))
    (current-buffer)))

(defun superman-distangle-config-list (string)
  ;; return a list of lists with vertical splits 
  ;; where each element can have horizontal splits
  (split-string string "[ \t]+:[ \t]+"))

(defun superman-distangle-config (config)
  ;; return a list with horizontal splits 
  ;; where each element can have vertical splits
  (let* ((vlist (split-string config "[ \t]+|[ \t]+"))
	 (hlist (mapcar '(lambda (x) (split-string x "[ \t]+/[ \t]+")) vlist)))
    hlist))



(defun superman-save-config (&optional config project)
  (interactive)
  (let* ((conf (or config (superman-current-config)))
	(pro (or project superman-current-project (superman-select-project)))
	(org-capture-mode-hook 'org-narrow-to-subtree)
	(org-capture-templates `(("s" "save" plain
				  (file+headline (superman-get-index pro) "Configuration")
				  ,(concat "windows:" conf "%?") :unnarrowed t))))
    (org-capture nil "s")))
    ;; (superman-goto-project-config)
    ;; (find-file-other-window (concat (superman-get-location pro) (car pro) "/.superman-window-config"))
    ;; (goto-char (point-max))
    ;; (unless (looking-at "^$") (insert "\n"))
    ;; (insert conf)
    ;; (save-buffer)))

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


(defun superman-read-config (project)
  (let* (config)
    (save-window-excursion
      (superman-goto-project project "Configuration" 'create nil)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*windows:[ \t]*" (point-max) t)
	(if config
	    (setq config (concat config " : "
				 (replace-regexp-in-string
				  "[ \t]*$" ""
				  (buffer-substring-no-properties (point) (point-at-eol)))))
	  (setq config
		(replace-regexp-in-string
		 "[ \t]*$" ""
		 (buffer-substring-no-properties (point) (point-at-eol)))))))
    (when (not config) (setq config superman-default-config))
    config))
;; (when filed-config
;; (setq config (concat (if superman-sticky-config (concat superman-sticky-config " : ")) filed-config)))
;; (when prop-config
;; (setq config (concat (if config (concat config " : ")) prop-config)))
;; (when (not config) (setq config superman-default-config))
;; config))


(defun superman-smash-windows (window-config project)
  "Smash windows according to the WINDOW-CONFIG and
then fill relative to project."
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
		  (switch-to-buffer
		   (superman-find-thing (nth r (nth c window-config)) project))
		  (when (and (< r (- nrow 1)) (> nrow 1 ))
		    (split-window-vertically)
		    (other-window 1)))))
    (select-window (nth 0 top-windows))))
    
(defun superman-switch-config (&optional project)
  "Switch to the next user defined window configuration. If
none exist switch to `superman-default-config' instead."
  (interactive)
  (let* ((pro (or project superman-current-project ((lambda () (interactive) (superman-switch-to-project) superman-current-project))))
	 (curpos (or superman-config-cycle-pos 0))
	 (config-list (superman-distangle-config-list
		       (superman-read-config pro)))
	 window-config)
    (if (> (length config-list) (1+ superman-config-cycle-pos));; cycle-pos starts at 0
	(setq superman-config-cycle-pos (1+ superman-config-cycle-pos))
      (setq superman-config-cycle-pos 0))
    ;; 
    (setq window-config (superman-distangle-config (nth superman-config-cycle-pos config-list)))
    (superman-smash-windows window-config pro)))



(defun superman-magit (project)
  (magit-status (concat (superman-get-location project) (car project))))

(defun superman-location (project)
  (let ((loc (concat (superman-get-location project) (car project))))
    (find-file loc)))

(defun superman-timeline (project)
  "Display a project specific timeline based on the index file."
  (interactive)
  (let* ((index (superman-get-index project))
	 (org-agenda-window-setup 'current-window)
	 (org-agenda-buffer-name (concat "*T*"))
	 (org-agenda-sticky nil)
	 (org-agenda-buffer-name (concat "*Timeline[" (car project) "]*")))
    (find-file index)
     (push ?L unread-command-events)
     (call-interactively 'org-agenda)))
;; (let ((tmp-file-name (concat "/tmp/timeline" index)))
;; (find-file-noselect tmp-file-name)
;; (with-temp-buffer (get-file-buffer tmp-file-name)
;; (erase-buffer)
;; (insert-buffer (get-file-buffer index))
;; (save-buffer)
;; ;; (switch-to-buffer (get-file-buffer index))
;; (message (buffer-name (current-buffer)))
;; (test)
;; (org-timeline 'yeah)
;; (test)))))


;; (defun superman-timeline (project)
  ;; (let (tbuf)
    ;; (save-window-excursion
      ;; (let ((index (superman-get-index project))
            ;; (org-agenda-sticky nil)
            ;; (org-agenda-buffer-name (concat "*" (car project) "-timeline*")))
        ;; (if (not (file-exists-p index))
            ;; (progn
              ;; (switch-to-buffer bufname)
              ;; (setq tbuf (get-buffer bufname))
              ;; (toggle-read-only -1)
	      ;; (erase-buffer)
              ;; (insert "TIMELINE: Project index file does not exist yet"))
          ;; ;; (when (get-buffer bufname)
            ;; ;; (kill-buffer bufname))
          ;; (find-file index)
          ;; (org-timeline 'yeah)
	  ;; ;; (rename-buffer bufname)
	  ;; (local-set-key [(return)] 'org-return)
	  ;; (setq tbuf (current-buffer)))))
    ;; (switch-to-buffer tbuf)))

(defun superman-recent-org (project)
  (car (superman-list-files
	(concat (superman-get-location project) (car project)) "^[^\\.].*\\.org$" "time")))


(defun superman-todo (&optional project)
  "Display a project specific timeline based on the index file."
  (interactive)
  (let* ((index (superman-get-index (or project superman-current-project)))
	 (org-agenda-window-setup 'current-window)
	 (org-agenda-buffer-name (concat "*T*"))
	 (org-agenda-sticky nil)
	 (org-agenda-files (list index))
	 (org-agenda-buffer-name (concat "*Timeline[" (car project) "]*")))
    (find-file index)
    ))
     ;; (push ?T unread-command-events)
     ;; (call-interactively 'org-agenda)))
  ;; (find-file (superman-find-index project))

(defun old-superman-todo (project)
  (let (tbuf)
    (save-window-excursion
      (let* ((location (concat (superman-get-location project) (car project)))
	     (org-files (superman-list-files location "^[^\\.].*\\.org$" nil))
	     (org-agenda-sticky nil) 
	     (org-agenda-files org-files)
	     (bufname (concat "*TODO[" (car project) "]*")))
	(when (get-buffer bufname)
	  (kill-buffer bufname))
	(org-todo-list org-match)
	(rename-buffer bufname)
	(setq tbuf (current-buffer))))
    (switch-to-buffer tbuf)))

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




(provide 'superman-config)
;;; superman-config.el ends here
