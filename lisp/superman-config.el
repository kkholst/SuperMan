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


;;{{{ finding buffers

(defun superman-find-thing (thing project)
  "Extract THING from `superman-config-action-alist' and apply it to PROJECT.
Returns the corresponding buffer."
  (save-window-excursion
    (let* ((case-fold-search t)
	   (action (cdr
		    (assoc
		     (replace-regexp-in-string "^[ \t\n]+\\|[ \t\n]+$" ""  thing)
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
;;}}}
;;{{{ saving window configs

(defun superman-save-config (&optional config project)
  (interactive)
  (let* ((conf (or config (superman-current-config)))
	(pro (or project superman-current-project (superman-select-project)))
	(org-capture-mode-hook 'org-narrow-to-subtree)
	(org-capture-templates `(("s" "save" plain
				  (file+headline (superman-get-index pro) "Configuration")
				  ,(concat "windows:" conf "%?") :unnarrowed t))))
    (org-capture nil "s")))

(defun superman-current-config ()
  (let* ((windata (winner-win-data))
	 config
	 prev-row)
    (while windata
      (let* ((buf (cdr (car windata)))
	     (pos (car (car windata)))
	     (row (nth 1 pos))
	     (bname (buffer-name buf))
	     (thing
	      (cond 
	       ((buffer-file-name buf)
		(replace-regexp-in-string (getenv "HOME") "~"  (buffer-file-name buf)))
	       ((string-match "\\*Documents\\[.*\\]\\*" bname) "DOCUMENTS")
	       ((string-match "\\*Project\\[.*\\]\\*" bname) "PROJECT")
	       (t bname))))
	(setq config (concat config (when prev-row (if (< prev-row row) " / " " | ")) thing))
	(setq windata (cdr windata))
	(setq prev-row row)))
    config))


;;}}}
;;{{{ reading window configs
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

(defun superman-read-config (project)
  (let* (config)
    (save-window-excursion
      (save-restriction
	(superman-goto-project project "Configuration" 'create nil nil nil)
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
      config)))
;;}}}
;;{{{ smashing window configs
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
    
(defun superman-switch-config (&optional project position)
  "Switch to the next user defined window configuration of PROJECT.

If no window configuration exists, as specified in the subtree
Configuration in index file of PROJECT, use
 `superman-default-config' instead.

If POSITION is an integer n then switch to the nth window configuration
of PROJECT and set `superman-default-config',
else cycle the value `superman-config-cycle-pos' and use it to
find the next window configuration."
  (interactive)
  (let* ((pro (or project
		  superman-current-project
		  (superman-switch-to-project 'force nil t)))
	 (position (when (integerp position) position)) 
	 (config-list (superman-distangle-config-list
		       (superman-read-config pro)))
	 window-config)
    (if position
	(setq superman-config-cycle-pos position)
      (if (> (length config-list) (1+ superman-config-cycle-pos));; cycle-pos starts at 0
	  (setq superman-config-cycle-pos (1+ superman-config-cycle-pos))
	(setq superman-config-cycle-pos 0)))
    (setq window-config
	  (superman-distangle-config
	   (nth superman-config-cycle-pos config-list)))
    (superman-smash-windows window-config pro)))
;;}}}
;;{{{ functions that find things

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

;;}}}
;;{{{ superman-shell
(defun superman-view-goto-shell ()
  "Switches to *shell* buffer and. "
  (interactive)
  (let ((sbuf (get-buffer "*shell*"))
	(cmd (concat "cd " default-directory))
	input)
    (if sbuf
	(switch-to-buffer-other-window sbuf)
      (split-window-vertically)
      (shell))
    (goto-char (point-max))
    (comint-bol)
    (when (looking-at ".*")
      (setq input (match-string 0))
      (replace-match ""))
    (insert cmd)
    (comint-send-input)
    (if input (insert input))))
;;}}}
;;{{{ synchronization

(defun superman-read-rsync (project)
  (let* (rsync)
    (save-window-excursion
      (superman-goto-project project "Configuration" 'create)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*rsync:[ \t]*" (point-max) t)
	(if rsync
	    (setq rsync (concat rsync " ; "
				(replace-regexp-in-string
				 "[ \t]*$" ""
				 (buffer-substring-no-properties (point) (point-at-eol)))))
	  (setq rsync
		(replace-regexp-in-string
		 "[ \t]*$" ""
		 (buffer-substring-no-properties (point) (point-at-eol)))))))
    rsync))
(defun superman-save-rsync (&optional config project)
  (interactive)
  (let* ((pro (or project superman-current-project (superman-select-project)))
	 (org-capture-mode-hook 'org-narrow-to-subtree)
	 (org-capture-templates `(("s" "save" plain
				   (file+headline (superman-get-index pro) "Configuration")
				   ,(concat "rsync:" "%(read-directory-name \"Rsync from: \") "
					    "%(read-directory-name \"Rsync to: \")") :unnarrowed t))))
    (org-capture nil "s")))

(defun superman-synchronize-project (&optional project)
  (interactive)
  (let* ((pro (or project
		  superman-current-project
		  (superman-switch-to-project 'force nil t)))
	 (rsync-list (superman-distangle-config
		      (superman-read-rsync pro)))
	 (rsync-cmd (when rsync-list
		      (if (eq 1 (length rsync-list))
			  (caar rsync-list)
			(caar (completing-read "Choose rsync: " rsync-list)))))
	 (cmd (concat "rsync -e ssh -avzAHX --delete-after " rsync-cmd)))
    (when (yes-or-no-p (concat "Do this? " cmd))
      (shell-command-to-string cmd))))


;;}}}
(provide 'superman-config)
;;; superman-config.el ends here
