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

(defun superman-goto-project (&optional project heading create)
  "Goto project index file call `widen' and then search for HEADING
and narrow the buffer to this subtree.

If HEADING is not found and CREATE is non-nil create the HEADING.
Leaves point at the end of the section."
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (index (superman-get-index pro))
	 hiddenp
	 (head (or heading (read-string "Goto heading: "))))
    (if index
      	(progn
	  (find-file index)
	  (unless (file-exists-p (file-name-directory index))
	    (make-directory  (file-name-directory index))))
      (error (concat "Project " pro " does not have an index.")))
    (widen)
    (goto-char (point-min))
    (cond ((re-search-forward
	    (format org-complex-heading-regexp-format (regexp-quote head))
	    nil t))
	  (create
	   (goto-char (point-max))
	   (insert "\n* " head "\n")
	   ;; (org-set-property "Project" pro)
	   (forward-line -1))
	  (t (message (concat "Heading " head " not found in index file of " (car pro)))))
    (org-narrow-to-subtree)
    (goto-char (point-max))))


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

;;(setq org-capture-templates nil)
;; Capturing links 
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "l") "Add link" plain 
				      (function superman-goto-project-bookmarks) "\n*** %?\n:PROPERTIES:\n:Link: %a\n:BookmarkDate: %T\n\:END:") 'append)

(defun superman-capture-bookmark (&optional project)
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (org-capture-templates
	  `(("n" "Add a bookmark/link" plain
	     (function
	      (lambda () (interactive)
		(superman-goto-project pro "Bookmarks" 'create)))
	     ,(concat
	       "\n*** %?\n:PROPERTIES:\n:Link: %a\n:BookmarkDate: %T\n\:END:")))))
    (org-capture nil "n")))

;; Capturing tasks
(defun superman-capture-task (&optional project)
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (org-capture-templates
	  `(("t" "Add a task" plain
	     (function
	      (lambda () (interactive)
		(superman-goto-project pro "Tasks" 'create)))
	     ,(concat "\n*** TODO %? \n:PROPERTIES:\n:TaskDate: <%<%Y-%m-%d %a>>\n:END:")))))
    (org-capture nil "t")))
(add-to-list 'org-capture-templates
	     `(,(concat superman-capture-prefix "t") "Add task" plain
	       (function superman-goto-project-tasks) "\n*** TODO %? \n:PROPERTIES:\n:TaskDate: <%<%Y-%m-%d %a>>\n:END:")
	     'append)
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "c") "Add checklist item" plain
				      (function superman-goto-project-tasks) "\n- [ ] %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing notes
(defun superman-capture-note (&optional project)
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (org-capture-templates
	  `(("n" "Add a note" plain
	     (function
	      (lambda () (interactive)
		(superman-goto-project pro "Notes" 'create)))
	     ,(concat "\n*** %? \n:PROPERTIES:\n:NoteDate: <%<%Y-%m-%d %a>>\n:END:")))))
    (org-capture nil "n")))
  
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
 	       ,(concat "\n*** %? \n:PROPERTIES:\n:MeetingDate: %^T"
			"\n:Participants:"
			"\n:Location:"
			"\n:CaptureDate: %U"
			"\n:END:"
			"\n**** Agenda\n"
			"\n**** TODO Minutes\n")))

(defun superman-capture-meeting (&optional project)
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (org-capture-templates
	  `(("m" "Arrange a meeting" plain
	     (function
	      (lambda () (interactive)
		(superman-goto-project pro "Calendar" 'create)))
	     ,(concat "\n*** %? \n:PROPERTIES:\n:MeetingDate: %^T"
		      "\n:Participants:"
		      "\n:Location:"
		      "\n:CaptureDate: %U"
		      "\n:END:"
		      "\n**** Agenda\n"
		      "\n**** TODO Minutes\n"))))
	 ;; FIXME the hook does not seem to get activated
	 (org-capture-before-finalize-hook
	  'superman-google-export-appointment))
    (org-capture nil "m")))
	 
;;}}}
;;{{{ capture synchronization commands
(setq superman-unison-switches "-ignore 'Regex .*(~|te?mp|rda)$' -ignore 'Regex ^(\\.|#).*'")
      ;; "-ignore 'Regex .*' -ignorenot 'Regexp *.(org|R|tex|Rd)$'")
(defun superman-capture-unison (&optional config project)
  (interactive)
  (let* ((pro (or project superman-current-project (superman-select-project)))
	 (org-capture-mode-hook 'org-narrow-to-subtree)
	 (org-capture-templates
	  `(("u" "unison" plain
	     (file+headline (superman-get-index pro) "Configuration")
	     ,(concat "*** unison%?\n:PROPERTIES:"
		      "\n:UNISON:"
		      "unison-gtk"
		      "\n:ROOT-1:"
		      "%(read-directory-name \"Unison root directory 1: \") "
		      "\n:ROOT-2:"
		      "%(read-directory-name \"Unison root directory 2: \") "
		      "\n:SWITCHES:default"
		      "\n:END:\n") :unnarrowed t))))
    (org-capture nil "u")))

(defun superman-unison (&optional project)
  (interactive)
  (save-excursion
    (save-restriction
      (let ((pro (or project
		     superman-current-project
		     (superman-switch-to-project 'force nil t)))
	    cmd)
	(superman-goto-project pro "Configuration")
	(org-narrow-to-subtree)
	(goto-char (point-min))
	(if (re-search-forward ":UNISON:" nil t)
	    (progn
	      (setq cmd
		    (concat
		     (superman-get-property (point) "UNISON")
		     " "
		     (superman-get-property (point) "ROOT-1")
		     " "
		     (superman-get-property (point) "ROOT-2")
		     " "
		     (if (string= (superman-get-property (point) "SWITCHES") "default")
			 superman-unison-switches
		       (superman-get-property (point) "SWITCHES"))))
	      (superman-goto-shell)
	      (insert cmd)
	      (insert "&")
	      (comint-send-input))
	  (when (y-or-n-p (concat "No unison configuration found for project "
				(car pro)
				". Create one? "))
	      (superman-capture-unison pro)))))))

;;}}}
;;{{{ capture mails

(defun superman-capture-mail ()
  (interactive)
  (let ((org-capture-templates
    '(("E"  "Store email (and attachments) in project"
       plain (function superman-gnus-project-mailbox)
       "\n*** MAIL from %:fromname: %:subject %?\n:PROPERTIES:\n:CaptureDate: %T\n:LINK: %a\n:EmailDate: %:date\n:END:\n\n%i"))
    ))
    (push ?E unread-command-events)
    (call-interactively 'org-capture)))

(defun superman-gnus-project-mailbox (&optional arg)
  (interactive)
  (unless (or  (eq major-mode 'gnus-article-mode)
               (eq major-mode 'gnus-summary-mode))
    (error "Can only capture mails from gnus-article-buffers"))
  (if arg (org-store-link))
  (let* ((buf (current-buffer))
         ;; (pro (completing-read "Select project: " superman-project-alist))
         (entry (superman-select-project))
         (pro (car entry))
         (loc (superman-get-location entry))
         (org (superman-get-index entry))
	 (region (buffer-substring (region-beginning) (region-end)))
         (mailbox (file-name-as-directory
                   (concat (file-name-as-directory loc) pro "/" "Mailbox"))))
    (gnus-summary-select-article-buffer)
    (if region
	(plist-put org-store-link-plist :initial
		   (concat (plist-get org-store-link-plist :initial)
			   (concat "----\n" region "\n----\n"))))
    (superman-save-attachments pro mailbox buf)
    (if org
	(progn
	  (find-file org)
	  (unless (file-exists-p (file-name-directory org))
	    (make-directory  (file-name-directory org))))
      (error "Project " pro " does not have an org-file."))
    (goto-char (point-min))
    (if (re-search-forward "^[*]+ Mailbox" nil t)
	(progn
	  (end-of-line)
	  (insert "\n"))
      ;; (goto-char (point-max))
      (insert "\n\n* Mailbox\n"))))

(defun superman-save-attachments (project dir buf)
  "Interactively save mail contents in project org file
and MIME parts in sub-directory 'mailAttachments' of the project."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((mime-line ""))
    (unless (file-exists-p dir)
      (if (featurep 'xemacs)
          (make-directory-path dir)
        (make-directory dir t)))
    (save-excursion
      (switch-to-buffer buf)
      (gnus-summary-display-buttonized 't))
    (goto-char (point-min))
    (while (re-search-forward "\\[[0-9]+\\." nil t)
      ;; modified code from `mm-save-data'
      (save-excursion
        (let* ((data (get-text-property (point) 'gnus-data))
               (filename (or (mail-content-type-get
                              (mm-handle-disposition data) 'filename)
                             (mail-content-type-get
                              (mm-handle-type data) 'name)))
               file)
          (when filename
            (setq filename (gnus-map-function
                            mm-file-name-rewrite-functions
                            (file-name-nondirectory filename))))
          (when (and data filename)
            (setq file (read-file-name
                        "Save attached file to: "
                        dir nil nil (replace-regexp-in-string "[ ]+" "" filename)))
            (if (file-directory-p file)
                (message "file not saved")
              (when (or (not (file-exists-p file))
                        (y-or-n-p (concat "File " file " exists, overwrite?" )))
                (mm-save-part-to-file data file))
              (setq mime-line (concat "\n**** Attac: " (file-name-nondirectory file)
				      "\n:PROPERTIES:\n:CaptureDate: " (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:EmailDate:%:date" ;; (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:Link:" "[[file:" file "][" (file-name-nondirectory file) "]]"
                                      "\n:END:\n"
                                      mime-line)))))))
    ;; information about the saved attachments is
    ;; saved such that capture can put it via %i
    (plist-put org-store-link-plist :initial
               (concat (plist-get org-store-link-plist :initial) mime-line))))
;;}}}
(provide 'superman-capture)
;;; superman-capture.el ends here
