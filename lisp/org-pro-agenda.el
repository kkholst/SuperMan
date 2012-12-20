;;; org-pro-agenda.el --- Agenda views for managing documents, notes and similar things

;; Copyright (C) 2012  Thomas Alexander Gerds, Klaus Kähler Holst

;; Authors: Thomas Alexander Gerds <tag@biostat.ku.dk>, Klaus Kähler Holst <kkho@biostat.ku.dk>
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

;; manage documents 

(defun org-pro-view-project (&optional project)
  (interactive)
  (let ((pro (or project org-pro-current-project (org-pro-select-project))))
    (switch-to-buffer (concat "*" (car pro) " view*"))
    (local-set-key "d" 'org-pro-view-documents)))


(defun org-pro-view-documents (&optional project)
"View documents of the current project"
  (interactive)
  (let* ((pro (or project org-pro-current-project (org-pro-select-project)))
	 (index (org-pro-get-index pro))
	 (org-agenda-custom-commands
	 `(("d" "view Project-DOCUMENTS" tags "LastCommit={.+}"
	    ((org-agenda-overriding-columns-format "%20ITEM(Title) %8TODO(ToDo) %GitStatus %50LastCommit(Last Commit)")
	     (org-agenda-view-columns-initially t)
	     (org-agenda-files `(,index)))))))
    (push ?d unread-command-events)
    (call-interactively 'org-agenda)
    (org-pro-view-mode t)))

(add-to-list 'org-agenda-custom-commands
	     `("pd" "view Project-DOCUMENTS" tags "LastCommit={.+}"
	       ((org-agenda-overriding-columns-format "%20ITEM(Title) %8TODO(ToDo) %GitStatus %50LastCommit(Last Commit)")
		(org-agenda-view-columns-initially t)
		(org-agenda-files `(,(org-pro-get-index org-pro-current-project))))))
;; (setq org-agenda-custom-commands
;; '(("d" "view Project-DOCUMENTS"
;; ((tags "LastCommit={.+}"
;; ((org-agenda-overriding-columns-format "%20ITEM(Title) %8TODO(ToDo) %GitStatus %50LastCommit(Last Commit)")
;; (org-agenda-view-columns-initially t)
;; (org-agenda-files '("~/emacs-genome/snps/ProjectManager/org/ProjectManager.org"))))))))

;; (setq org-agenda-custom-commands
      ;; '(("c" agenda nil
	 ;; ((org-agenda-overriding-columns-format "%75ITEM %7Effort{:} %7CLOCKSUM{Total} %15TAGS %SCHEDULED")
	  ;; (org-agenda-view-columns-initially t)
	  ;; (org-agenda-start-with-log-mode t)
	  ;; (org-agenda-ndays 1)
	  ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\* TODO"))))))


;; hack of org-agenda-todo
(defun org-pro-treat-document (&optional arg)
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (prop (get-char-property (point) 'org-columns-key))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (org-get-at-bol 'org-hd-marker))
	 (todayp (org-agenda-todayp (org-get-at-bol 'day)))
	 (inhibit-read-only t)
	 org-agenda-headline-snapshot-before-repeat newhead just-one)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (cond ((string= prop "ITEM")
		 (org-narrow-to-element)
		 (if (re-search-forward ":Hash:" nil t)
		     (progn 
		       (widen)
		       (org-pro-git-revision-at-point)
		       (let ((buffer-file-name (expand-file-name (buffer-name)))) (normal-mode))))
		 (if (re-search-forward ":filename:" nil t)
		     (progn 
		       (widen)
		       (org-open-at-point-global)
		       (widen))))
		((string= prop "GitStatus")
		 (nth 1 (org-pro-git-status-file-at-point))
		 (org-columns-redo))
		((string= prop "Decoration")
		 (org-pro-git-tag-at-point)
		 (org-columns-redo))
		((string= prop "filename")    
		 (org-columns-open-link))
		((string= prop "Other")
		 (org-columns-open-link))
		((string= prop "Hash")
		 (org-pro-git-revision-at-revision))
		((string= prop "LastCommit")
		 (org-pro-git-commit-file-at-point)
		 (org-columns-redo))
		(t (org-columns-edit-value)))))
	(save-excursion
	  (org-agenda-change-all-lines newhead hdmarker 'fixface just-one))
	(org-move-to-column col)))
  org-columns-redo)

(provide 'org-pro-agenda)
;;; org-pro-agenda.el ends here
