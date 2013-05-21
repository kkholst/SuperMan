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

;;{{{ superman capture

(defun superman-goto-project (&optional project heading create end-of leave-narrowed jabber)
  "Goto project index file call `widen' and then search for HEADING
and narrow the buffer to this subtree.

If HEADING is not found and CREATE is non-nil create the HEADING.

If END-OF is non-nil leaves point at end of the section,
otherwise at the beginning.

If JABBER is non-nil message about non-existing headings.
"
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (index (superman-get-index pro))
	 (head (or heading (read-string "Goto heading: ")))
	 value)
    (if index
      	(progn
	  (find-file index)
	  (unless (file-exists-p (file-name-directory index))
	    (make-directory  (file-name-directory index))))
      (error (concat "Project " pro " does not have an index.")))
    (widen)
    (show-all)
    (goto-char (point-min))
    (setq value
	  (cond ((re-search-forward (format org-complex-heading-regexp-format (regexp-quote head)) nil t)
		 'found)
		(create
		 (goto-char (point-max))
		 (insert "\n* " head "\n")
		 ;; (org-set-property "Project" pro)
		 (forward-line -1)
		 'create)
		(t (when jabber (message (concat "Heading " head " not found in index file of " (car pro))))
		   nil)))
    (when value 
      (org-narrow-to-subtree)
      (if end-of (goto-char (point-max))
	;; leave point at the first entry or at the end of this section
	(end-of-line)
	(if (outline-next-heading)
	    (beginning-of-line)
	(goto-char (point-max)))))
    (unless leave-narrowed
      (widen)
      (show-all))
  value))

(defun superman-capture (project heading plist &optional level)
  (let* ((what (car plist))
	 (level (or level 3))
	 (props (cadr plist))
	 (scene (current-window-configuration))
	 head-point
	 (body "")
	 (title (concat "### Captured " what " for project " (car project)))
	 (S-buf (generate-new-buffer-name "*Capture of SuperMan*")))
    (if heading
	(cond ((stringp heading)
	       (superman-goto-project project heading 'create nil nil nil))
	      ((markerp heading)
	       (progn (switch-to-buffer (marker-buffer heading))
		      (goto-char heading))))
      (find-file (superman-get-index project))
      (widen)
      (show-all)
      (goto-char (point-max)))
    (switch-to-buffer
     (make-indirect-buffer (current-buffer) S-buf))
    (delete-other-windows)
    (insert "\n"
	    (make-string level (string-to-char "*"))
	    " NIX \n")
    (forward-line -1)
    (org-narrow-to-subtree)
    (skip-chars-forward "[* ]")
    (kill-line)
    (goto-char (point-min))
    (insert title)
    (put-text-property (point-at-bol) (point-at-eol) 'scene scene)
    (insert "\n#"
	    (make-string (length title) (string-to-char "-"))
	    "\n# C-c C-c to save "
	    "\n# C-c C-q to quit without saving"
	    "\n# ---yeah #%*^#@!--------------")
    (insert "\n\n")
    (put-text-property (point-min) (point) 'face font-lock-string-face)
    (org-mode)
    (show-all)
    (end-of-line)
    (setq head-point (point))
    (insert "\n:PROPERTIES:")
    ;; (put-text-property (point-at-bol) (point-at-eol) 'read-only t)
    (while props
      (let* ((el (car props))
	     (key (car el))
	     (val (ignore-errors (nth 1 el)))
	     (req (nth 2 el)))
	(cond ((stringp key)
	       (ignore-errors
		 (insert "\n:" key ": ")
		 ;; (put-text-property (point-at-bol) (- (point) 1) 'read-only t)
		 (put-text-property (- (point) 1) (point) 'prop-marker (point))
		 (if req 
		     (put-text-property (- (point) 1) (point) 'required "required-field"))
		 (when val (insert (superman-make-value val)))))
	      ((eq key 'fun) (ignore-errors (funcall (cdr el))))
	      ((eq key 'hdr) (ignore-errors
			       (save-excursion
				 (org-back-to-heading)
				 (end-of-line)
				 (insert (superman-make-value val)))))
	      ((eq key 'body) (setq body (concat body (superman-make-value val)))))
	(setq props (cdr props))))
    (insert "\n:END:\n")
    (insert body)
    ;; (put-text-property (point-at-bol) (point-at-eol) 'read-only t)
    (insert "\n")
    (goto-char head-point)
    (superman-capture-mode)))


(defvar superman-capture-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")

(define-minor-mode superman-capture-mode
"Toggle superman capture mode.
With argument ARG turn superman-doccapture-mode on if ARG is positive, otherwise
turn it off."
     :lighter " *S*-Capture"
     :group 'org
     :keymap 'superman-capture-mode-map)

(defun superman-capture-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-capture-mode t))


(define-key superman-capture-mode-map  "\C-c\C-c" 'superman-clean-scene)
(define-key superman-capture-mode-map  "\C-c\C-q" 'superman-quit-scene)

(defun superman-make-value (val)
  (cond ((stringp val) val)
	((functionp val) (funcall val))
	((listp val)
	 (let ((thing (car val)))
	   (cond ((stringp thing)
		  (add-text-properties 0 (length thing) (cdr val) thing))
		 ((functionp thing)
		  (funcall thing (cdr val))))))))

(defvar superman-capture-before-clean-scene-hook nil)
(defun superman-clean-scene ()
  (interactive)
  (let ((scene (get-text-property (point-min) 'scene))
	req
	next)
    (goto-char (point-min))
    (while (setq next (next-single-property-change (point-at-eol) 'prop-marker))
      (goto-char next)
      (if (looking-at "[ \t]*\n")
	  (if (setq req (get-text-property (point) 'required))
	      (progn
		(put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-warning-face)
		(error (concat (or req "This is a required field"))))
	    (beginning-of-line)
	    (kill-line)
	    (forward-line -1))
	(end-of-line)))
    (save-buffer)
    (run-hooks superman-capture-before-clean-scene-hook)
    (goto-char (point-min))
    (outline-next-heading)
    (delete-region (point-min) (point))
    (kill-buffer (current-buffer))
    (set-window-configuration scene)
    (when superman-view-mode (superman-redo))))

(defun superman-quit-scene ()
  (interactive)
  (let ((scene (get-text-property (point-min) 'scene)))
    (delete-region (point-min) (point-max))
    (save-buffer)
    (kill-buffer (current-buffer))
    (set-window-configuration scene)))

;; (superman-capture superman-current-project "Notes" '("Note" (("a" "b"))))

;;}}}
;;{{{ capture documents, notes, etc.

;; Choose a prefix
;; (setq superman-capture-prefix "P")
;; (add-to-list 'org-capture-templates
	     ;; `(,superman-capture-prefix "Project management") 'append)

(defun superman-capture-document (&optional project)
  (interactive)
  (let* ((pro (or project
		  superman-view-current-project
		  (superman-select-project)))
	 (marker (get-text-property (point-at-bol) 'org-hd-marker))
	 (heading (if (and marker (superman-current-cat))
		      marker
		    "Documents"))
	 (dir (expand-file-name (concat (superman-get-location pro) (car pro))))
	 (file (read-file-name (concat "Add document to " (car pro) ": ") (file-name-as-directory dir))))
    (superman-capture
     pro
     heading
     `("Document" (("FileName" ,(concat "[[" file "]]"))
		   (hdr ,(file-name-nondirectory file))
		   ("GitStatus" ,(nth 1 (superman-git-get-status file nil))))))))

(defun superman-capture-project (&optional nickname)
  "Create a new project. Prompt for CATEGORY and NICKNAME if necessary.
This function modifies the 'superman' and creates and visits the index file of the new project.
To undo all this you can try to call 'superman-delete-project'. "
  (interactive)
  (superman-refresh)
  (let* ((nickname (or (and (not (string= nickname "")) nickname) (read-string "Project name (short) ")))
	 category)
    ;; check if nickname exists 
    (while (assoc nickname superman-project-alist)
      (setq nickname
	    (read-string (concat "Project " nickname " exists. Please choose a different name (C-g to exit): "))))
    (setq category (or category
		       (completing-read "Category: "
					(mapcar (lambda (x) (list x)) (superman-parse-project-categories)) nil nil)))
    (superman-capture
     `("*S*" (("index" . ,superman-home)))
     category
     `("Project" (("Nickname" ,nickname)
		  ("InitialVisit" ,(format-time-string "<%Y-%m-%d %a>"))
		  ("Others" "")
		  ;; ("Location" ("" (required t)))
		  ("Location" "")
		  (hdr ,(concat "ACTIVE " nickname))
		  ("Index" "")
		  ("Category" ,category)))
     superman-project-level)))
    
(defun superman-capture-note (&optional project)
  (interactive)
  (let ((pro (or project
		 superman-view-current-project
		 (superman-select-project))))
    (superman-capture pro
		      "Notes"
		      `("Note" (("NoteDate" ,(format-time-string "<%Y-%m-%d %a>")))))))

(defun superman-capture-bookmark (&optional project)
  (interactive)
    (let ((pro (or project
		   superman-view-current-project
		   (superman-select-project))))
    (superman-capture pro
		      "Bookmarks"
		      `("Bookmark" (("BookmarkDate"  ,(format-time-string "<%Y-%m-%d %a>"))
				    ("Link" nil))))))

(defun superman-capture-task (&optional project)
  (interactive)
  (let ((pro (or project
		 superman-view-current-project
		 (superman-select-project))))
    (superman-capture
     pro
     "Tasks"
     `("Task" (("TaskDate"  ,(format-time-string "<%Y-%m-%d %a>"))
	       (fun 'org-todo))))))

;; Capturing meetings
;; Note: inactive time stamp for CaptureDate

(defun superman-capture-meeting (&optional project)
  (interactive)
  (let ((pro (or project
		 superman-view-current-project
		 (superman-select-project)))
	(date (format-time-string  "<%Y-%m-%d %a %H:%M>" (org-read-date t t))))
    (superman-capture
     pro
     "Calendar"
     `("Meeting" (("MeetingDate" ,date)
		  ("Participants" nil)
		  ("Location" nil)
		  ("GoogleCalendar" ,superman-google-default-calendar)
		  ("CaptureDate" ,(format-time-string "<%Y-%m-%d %a>"))
		  ('fun 'org-todo)))))
  (setq superman-capture-before-clean-scene-hook
	'superman-google-export-appointment))

;;}}}
;;{{{ capture synchronization commands
(setq superman-unison-switches "-ignore 'Regex .*(~|te?mp|rda)$' -ignore 'Regex ^(\\.|#).*'")
      ;; "-ignore 'Regex .*' -ignorenot 'Regexp *.(org|R|tex|Rd)$'")

(defun superman-capture-unison (&optional project)
  (interactive)
  (let ((pro (or project
		 superman-view-current-project
		 (superman-select-project)))
	(root-1 (read-directory-name "Unison root directory 1: "))
	(root-2 (read-directory-name "Unison root directory 2: ")))
    (superman-capture
     pro
     "Configuration"
     `("Unison" (("UNISON" "unison-gtk")
		 ("ROOT-1" ,root-1)
		 ("ROOT-2" ,root-2)
		 ("CaptureDate" ,(format-time-string "<%Y-%m-%d %a>")))))))

(defun superman-unison (&optional project)
  (interactive)
  (save-excursion
    (save-restriction
      (let ((pro (or project
		     superman-current-project
		     (superman-switch-to-project nil t)))
	    cmd)
	(superman-goto-project pro "Configuration" 'create nil nil nil)
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

;; (defun superman-capture-mail ()
  ;; (interactive)
  ;; (let ((org-capture-templates
    ;; '(("E"  "Store email (and attachments) in project"
       ;; plain (function superman-gnus-goto-project-mailbox)
       ;; "\n*** MAIL from %:fromname: %:subject %?\n:PROPERTIES:\n:CaptureDate: %T\n:LINK: %a\n:EmailDate: %:date\n:END:\n\n%i"))
    ;; ))
    ;; (push ?E unread-command-events)
    ;; (call-interactively 'org-capture)))

(defun superman-capture-mail (&optional project)
  ;; (defun superman-gnus-goto-project-mailbox (project &optional arg)
  (interactive)
  (unless (or (eq major-mode 'gnus-article-mode)
	      (eq major-mode 'gnus-summary-mode))
    (error "Can only capture mails from either gnus-article or gnus-summary buffers"))
  ;; activate the connection with summary
  (when (eq major-mode 'gnus-article-mode)
      (gnus-article-show-summary))
  (gnus-summary-select-article-buffer)
  (let* ((buf (current-buffer))
	 (link (org-store-link 1))
	 (entry (or project
		   superman-view-current-project
		   (superman-select-project)))
	 (pro (car entry))
	 (loc (superman-get-location entry))
	 (index (superman-get-index entry))
	 (region (buffer-substring (region-beginning) (region-end)))
	 (mailbox (file-name-as-directory (concat (file-name-as-directory loc) pro "/" "Mailbox")))
	 (from (message-fetch-field "from"))
	 (subject (message-fetch-field "subject"))
	 (date (message-fetch-field "date"))
	 (attachments (superman-save-attachments pro mailbox (current-buffer) date)))
    (superman-capture
     entry
     "Mail"
     `("Mail" (("CaptureDate"  ,(format-time-string "<%Y-%m-%d %a>"))
	       (body ,(concat "----\n" region "\n----\n"))
	       (body ,attachments)
	       ("EmailDate" ,date)
	       (hdr ,(concat "Mail from " from " " subject ))
	       ("Link" ,link))))))

(defun superman-save-attachments (project dir buf date)
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
				      "\n:EmailDate:" date 
				      ;; (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:Link:" "[[file:" file "][" (file-name-nondirectory file) "]]"
                                      "\n:END:\n"
                                      mime-line)))))))
    mime-line))
;;}}}
;;{{{ capture cats
(defun superman-capture-cat (&optional project)
  (interactive)
  (let ((pro (or project
		 superman-view-current-project
		 (superman-select-project))))
    (superman-capture
     pro
     nil
     `("Section" (("Ball1" "todo :face superman-get-todo-face")
		  ("Ball2" "hdr :name Title :width 13 :face font-lock-function-name-face")
		  ("Ball3" "")))
     1)))

;;}}}
(provide 'superman-capture)
;;; superman-capture.el ends here
