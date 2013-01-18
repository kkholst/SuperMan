;;; superman-capture.el --- superman captures stuff

;; Copyright (C) 2013  Klaus Kähler Holst, Thomas Alexander Gerds

;; Authors: Klaus Kähler Holst <kkho@biostat.ku.dk>
;;          Thomas Alexander Gerds <tag@biostat.ku.dk>
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

;;; Code:

;;{{{ capture documents, notes, etc.

(defun superman-goto-project (&optional project heading create prop-alist)
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (index (superman-get-index pro))
	 hiddenp
	 (head (or heading (read-string "Goto heading: "))))
    (if index
	(find-file index)
      (error (concat "Project " pro " does not have an index.")))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cond ((re-search-forward
	      (format org-complex-heading-regexp-format (regexp-quote head))
	      nil t))
	    (create
	     (insert "* " head "\n")
	     (org-set-property "Project" pro)
	     (forward-line -1))
	    (t (error (concat "Heading " head " not found in index file of " (car pro)))))
      (org-narrow-to-subtree)
      (if prop-alist (mapcar (lambda (p)
			       (unless (org-entry-get nil (car p))
				 (org-set-property (car p) (car (cdr p))))) prop-alist))
      (goto-char (point-max))
      )))
      ;; (forward-line)
      ;; (unless (looking-at "^[ \t]*$") (progn (insert "\n") (forward-line -1))))))


(defun superman-goto-project-notes ()
  (interactive)
  (or (superman-goto-project nil "Notes" 'create)))

(defun superman-goto-project-bookmarks ()
  (interactive)
  (or (superman-goto-project nil "Bookmarks" 'create)))

(defun superman-goto-project-documents ()
  (interactive)
  (or (superman-goto-project nil "Documents" 'create)))

(defun superman-goto-project-tasks ()
  (interactive)
  (or (superman-goto-project nil "Tasks" 'create)))

(defun superman-goto-project-mailbox ()
  (interactive)
  (or (superman-goto-project nil "Mailbox" 'create)))

(defun superman-goto-project-config ()
  (interactive)
  (or (superman-goto-project nil "Configuration" 'create)))

(defun superman-goto-project-calendar ()
  (interactive)
  (or (superman-goto-project nil "Calendar" 'create)))

;; Choose a prefix
(setq superman-capture-prefix "P")
(add-to-list 'org-capture-templates `(,superman-capture-prefix "Project management") 'append)
(defun superman-capture() 
  (interactive)
  (push (string-to-char superman-capture-prefix) unread-command-events)
  (call-interactively 'org-capture))
;; Capturing links 
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "l") "Add link" plain 
				      (function superman-goto-project-bookmarks) "\n *** %a%?") 'append)
;; Capturing tasks
(add-to-list 'org-capture-templates
	     `(,(concat superman-capture-prefix "t") "Add task" plain
	       (function superman-goto-project-tasks) "\n*** TODO %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:")
	     'append)
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "c") "Add checklist item" plain
				      (function superman-goto-project-tasks) "\n- [ ] %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing notes
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "n") "Add note" plain
				      (function superman-goto-project-notes) "\n*** %? \n:PROPERTIES:\n:NoteDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing documents
(add-to-list 'org-capture-templates
	     `(,(concat superman-capture-prefix "d") "Add document" plain
	       (function superman-goto-project-documents) "\n*** %? \n:PROPERTIES:\n:filename: [[%(read-file-name \"Document file: \")]]\n:CaptureDate: %T\n:END:") 'append)

;; Capturing meetings
;; Note: inactive time stamp for CaptureDate
(add-to-list 'org-capture-templates
 	     `(,(concat superman-capture-prefix "m") "Arrange a meeting" plain
 	       (function superman-goto-project-calendar)
 	       (concat "\n*** %? \n:PROPERTIES:\n:Date: %^T"
 		       "\n:Participants:"
 		       "\n:Location:"
 		       "\n:CaptureDate: %U"
 		       "\n:END:"
 		       "\n**** Agenda\n"
 		       "\n**** TODO Minutes\n") 'append))

;;}}}


(provide 'superman-capture)
;;; superman-capture.el ends here
