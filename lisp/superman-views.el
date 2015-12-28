;;; superman-views.el --- Superman views of project contents 

;; Copyright (C) 2012-2016 Thomas Alexander Gerds, Klaus Kaehler Holst

;; Authors: Thomas Alexander Gerds <tag@biostat.ku.dk>
;;          Klaus Kaehler Holst <kkho@biostat.ku.dk>
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

;; Code:

;;{{{ Variables

(defvar superman-column-separator 2 "Number of characters between columns")

(defvar superman-hl-line nil "Set to non-nil in order to
highlight the current line in superman views.")

(defvar superman-view-current-project nil "Buffer local project variable" )
(make-variable-buffer-local 'superman-view-current-project)

;; (defvar superman-view-marks nil "Marks for items in agenda.")
;; (make-variable-buffer-local 'superman-view-marks)

;; (defvar superman-mark-face 'bold  "Face name for marked entries in the view buffers.")

(defvar superman-cats '(("Meetings" . "Date")
			("Calendar" . "Date")
			("Documents" . "FileName")
			("Data" . "DataFileName")
			("Notes" . "NoteDate")
			("Tasks" . "TaskDate")
			("Mail" . "EmailDate")
			("Bookmarks" . "BookmarkDate"))
  "Alist of the form ((cat.1 . term.1)(cat.2 . term.2) ...)  where cat.i
refers to the ith bloke in the project view and term.i identifies
headlines in the project index file to be shown in that bloke.")

(setq superman-views-delete-empty-cats t)
(setq superman-views-permanent-cats '("Documents"))
(setq superman-cat-headers
      '(("Documents" . superman-documents-view-header)))


(defvar superman-document-category-separator '32 "Symbol for separating categories in document views.
See `org-agenda-block-separator'. Set to '0 to get a funny line.
Can also be set to (string-to-char \"~\") with any string in place of ~.")

(defvar superman-finalize-cat-alist nil

  "List of functions and variables used to finalize superman-views.

Elements are of the form '(cat fun balls) where cat is the name
of the heading in which the function fun is applied with arguments given by
balls.

A ball is a special list:

 (key (\"width\" number) (\"fun\" trim-function) (\"args\" trim-argument-list) (\"face\" face-or-fun) (\"name\" string) (required t-or-nil))

Examples:

Column showing property \"Foo\" 

 (\"Foo\" (\"fun\" superman-trim-string) (\"width\" 17) (\"face\" font-lock-function-name-face) (\"name\" \"Foo\") (required nil))

Column showing the header

 (hdr (\"fun\" superman-trim-string) (\"name\" \"Description\"))

Column showing the todo-state 

 (todo (\"face\" superman-get-todo-face))

")

(setq superman-finalize-cat-alist
      '(("documents" superman-document-balls)
	("data" superman-data-balls)
	("notes" superman-note-balls)
	("mail" superman-mail-balls)
	("tasks" superman-task-balls)
	("bookmarks" superman-bookmark-balls)
	("meetings" superman-meeting-balls)
	("calendar" superman-meeting-balls)
	("gitFiles" superman-document-balls)))

(defun superman-dont-trim (x len) x)
(setq superman-document-balls
      '((hdr ("width" 23) ("face" font-lock-function-name-face) ("name" "Description"))
	;; ("GitStatus" ("width" 10) ("face" superman-get-git-status-face))
	;; ("LastCommit" ("fun" superman-trim-date) ("face" font-lock-string-face))
	("FileName" ("fun" superman-dont-trim))))

(setq superman-default-balls
      '((todo ("width" 6) ("face" superman-get-todo-face))	
	(".*Date" ("fun" superman-trim-date) ("regexp" t) ("face" font-lock-string-face))
	(hdr ("width" full) ("face" font-lock-function-name-face))))

(setq superman-meeting-balls
      '((hdr ("width" 23) ("face" font-lock-function-name-face))
	;; ("Date" ("fun" superman-trim-date) ("face" font-lock-string-face))
	(".*Date" ("fun" superman-trim-date) ("regexp" t) ("face" font-lock-string-face))
	("Participants" ("width" 23))))

(setq superman-note-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	("NoteDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" full) ("face" font-lock-function-name-face))))
(setq superman-data-balls
      '(("CaptureDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" 23) ("face" font-lock-function-name-face))
	("DataFileName" ("fun" superman-dont-trim))))
(setq superman-task-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	(priority ("width" 8) ("face" superman-get-priority-face))
	("TaskDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" full) ("face" font-lock-function-name-face))))
(setq superman-bookmark-balls
      '(("BookmarkDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("face" font-lock-function-name-face) ("name" "Description") ("width" 45))
	("Link" ("fun" superman-trim-link) ("width" full) ("name" "Bookmark"))))


(setq superman-mail-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	(priority ("width" 8) ("face" superman-get-priority-face))
	("EmailDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" 23) ("face" font-lock-function-name-face))
	("Link" ("fun" superman-trim-bracketed-filename) ("width" 48))))
	;; (attac ("Link" ("fun" superman-trim-bracketed-filename) ("width" full)))))

;;}}}
;;{{{ faces
;; FIXME
;;}}}
;;{{{ Trim stuff and frequently used funs

(defun superman-trim-string (str &rest args)
  "Trim string STR to a given length by either calling substring
or by adding whitespace characters. The length is stored in the first
element of the list ARGS. If length is a string which cannot be converted
to an integer then do not trim the string STR."
  (let* ((slen (length str))
	 (len (car args))
	 (numlen (cond
		  ((integerp len) len)
		  ((eq len 'full) "full")
		  ((stringp len)
		   (if (string= len "full") slen
		       (setq len (string-to-number len))
		     (if (< len 1) 13 len)))
		  (t 13)))
	 (diff (unless (eq len 'full) (- numlen slen))))
    (if diff
	(if (> diff 0)
	    (concat str (make-string diff (string-to-char " ")))
	  (substring str 0 numlen))
      str)))

(defun superman-trim-link (link &rest args)
  ;;  Bracket links
  (if (string-match org-bracket-link-regexp link)
      (let* ((rawlink (org-match-string-no-properties 1 link))
	     (len (car args))
	     tlink)
	(if (match-end 3)
	    (setq tlink
		  (replace-match
		   (superman-trim-string
		    (org-match-string-no-properties 3 link) len)
		   t t link 3))
	  (setq tlink (org-make-link-string
		       rawlink
		       (superman-trim-string "link" len))))
	tlink)
    ;; plainlinks
    (let* ((len (car args)))
      (if (string-match org-link-re-with-space link)
	  (concat "[[" link "]["
		  (superman-trim-string link len) "]]")
	link))))

(defun superman-trim-bracketed-filename (file &rest args)
  ;;  Links to files
  (if (string-match org-bracket-link-regexp file)
      (let ((filename (org-match-string-no-properties 1 file))
	    (len (car args))
	    (match (match-end 3))
	    (match-string (org-match-string-no-properties 3 file))
	    trimmed-file-name)
	(if match
	    (setq trimmed-file-name
		  (replace-match
		   (superman-trim-string
		    match-string len)
		   t t file 3))
	  (setq trimmed-file-name
		(org-make-link-string
		 filename
		 (superman-trim-string
		  (file-name-nondirectory filename) len))))
	trimmed-file-name)
    (superman-trim-string file (car args))))

(defun superman-trim-filename (filename &rest args)
  (if (string-match org-bracket-link-regexp filename)
      (setq filename (org-match-string-no-properties 1 filename)))
  ;;  raw filenames
  (let ((linkname (file-name-nondirectory filename))
	(len (car args)))
    (when (string= linkname "") ;; for directories show the mother
      (setq linkname (file-name-nondirectory
		      (directory-file-name filename))))
    (org-make-link-string
     filename
     (superman-trim-string linkname len))))

(defun superman-trim-date (date &optional len)
  (let ((len (if (stringp len) (length len) len))
	date-string)
    (if (< len 1) (setq len 13))
    (if (string-match org-ts-regexp0 date)
	(let* ((days (org-time-stamp-to-now date)))
	  (cond ((= days 0)
		 (setq date "today"))
		((= days 1)
		 (setq date "yesterday"))
		((> days 0)
		 (setq date (concat "in " (int-to-string days) " days")))
		(t (setq date (concat (int-to-string (abs days)) " days ago"))))
	  (setq date-string (superman-trim-string date len))
	  (put-text-property 0 (length date-string) 'sort-key (abs days) date-string))
      (setq date-string (superman-trim-string date len))
      (put-text-property 0 (length date-string) 'sort-key 0 date-string))
    date-string))

(defun superman-view-current-project (&optional no-error)
  "Identifies the project associated with the current view buffer
and sets the variable superman-view-current-project."
  (let* ((nick (get-text-property (point-min) 'nickname))
	 (pro (when nick (assoc nick superman-project-alist))))
    (if pro
	(setq superman-view-current-project pro)
      (unless no-error
	(error "Malformed header of project view buffer: cannot identify project")))))

(defun superman-view-control (project &optional git-dir)
  "Insert the git repository if project is git controlled
and the keybinding to initialize git control otherwise. If GIT-DIR is
a non-nil then it is assumed to be the path to a git directory,
otherwise it is tested if the path provided by a text-property 'git-dir
at (point-min) is git controlled."
  (let* ((loc (or git-dir (get-text-property (point-min) 'git-dir)))
	 (button (superman-make-button "Contrl:"
				       'superman-git-display
				       'superman-header-button-face
				       "Show git status"))
	 (control (if (or git-dir (superman-git-p loc))
		      (superman-make-button
		       (concat "Git repository at " "[[" (abbreviate-file-name (superman-git-toplevel loc)) "]]")
		       'superman-git-display
		       nil
		       "Control git repository")
		    (superman-make-button
		     "not set. <> press button or `GI' to initialize git control"
		     'superman-git-init
		     nil
		     (concat
		      "Initialize git repository at " git-dir)))))
    (put-text-property 0 1 'superman-header-marker t button)
    (put-text-property 0 1 'superman-header-marker t control)
    (concat button " " control)))

(defun superman-view-others (project)
  "Insert the names and emails of the others (if any)." 
  (let* ((pro (or project (superman-view-current-project)))
	(others (superman-get-others pro)))
    (if others
	(let ((key
	       (superman-make-button
		"Others:"
		`(lambda ()
		   (interactive)
		   (superman-capture-others
		    ,(car pro)))
		'superman-header-button-face
		"Set names of collaborators")))
	  ;; (put-text-property 0 (length key) 'face 'org-level-2 key)
	  (put-text-property 0 1 'superman-header-marker t key)
	  (concat key " " others))
      "")))



(defun superman-current-cat ()
  (let ((cat-point (superman-cat-point)))
    (when cat-point
      (get-text-property cat-point 'cat))))

(defun superman-current-subcat ()
  (let* ((cat-pos (superman-cat-point))
	 (subcat-pos (superman-subcat-point))
	 (subp (and subcat-pos (> subcat-pos cat-pos))))
    (when cat-pos
      (get-text-property
       (if subp subcat-pos cat-pos)
       (if subp 'subcat 'cat)))))

(defun superman-current-subcat-pos ()
  "Compare the positions of the current category and
the current sub-category and return the minimum."
  (let* ((cat-pos (superman-cat-point))
	 (subcat-pos (superman-subcat-point))
	 (subp (and subcat-pos (> subcat-pos cat-pos))))
    (when cat-pos
      (if subp subcat-pos cat-pos))))

;;}}}
;;{{{ window configuration

(defun superman-view-read-config (project)
  (let (configs
	(case-fold-search t))
    (save-window-excursion
      (when (superman-goto-project project "Configuration" nil nil 'narrow nil)
	  (goto-char (point-min))
	  (while (outline-next-heading)
	    (let ((config (or (superman-get-property (point) "Config")
			      (cdr (assoc
				    (downcase
				     (or (org-get-heading t t) "Untitled"))
				     superman-config-alist))))
		  (hdr (or (org-get-heading t t) "NoHeading")))
	      (when config 
		(setq
		 configs
		 (append
		  configs
		  (list (cons hdr config)))))))
	  (widen)))
      configs))

(defun superman-ual (&optional project)
  (interactive)
  (find-file superman-ual))

(defun superman-view-insert-project-buttons ()
  "Insert project buttons"
  (if (> (length superman-project-history) 1)
      (let* ((prev (car (reverse superman-project-history)))
	     (next (let ((n (cadr superman-project-history)))
		     (unless (string-equal prev n) n)))
	     (all-button (superman-make-button
			  "Projects" 'superman
			  'superman-next-project-button-face "List of projects"))
	     (next-button (when next (superman-make-button
				      next
				      `(lambda () (interactive)
					 (superman-switch-to-project ,next))
				      'superman-next-project-button-face
				      (concat "Switch to project " next) 17)))
	     (prev-button (when prev
			    (superman-make-button
			     prev
			     `(lambda () (interactive) (superman-switch-to-project ,prev))
			     'superman-next-project-button-face
			     (concat "Switch to project " prev)))))
	(when prev
	  (put-text-property 0 1 'superman-header-marker t prev-button))
	(when next
	  (put-text-property 0 1 'superman-header-marker t next-button))
	(put-text-property 0 1 'superman-header-marker t all-button)
	(insert (concat " " prev-button " " next-button " " all-button)))
    (insert "  " (superman-make-button "Projects" 'superman 'superman-next-project-button-face "List of projects"))))

(defvar superman-default-action-buttons
'(("Documents" (lambda () (interactive) (superman-capture-document nil nil nil)))
  ("Notes" (lambda () (interactive) (superman-capture-note nil nil nil)))
	("Tasks" (lambda () (interactive) (superman-capture-task nil nil nil)))
	("Text" (lambda () (interactive) (superman-capture-text nil nil nil)))
	("Meetings" (lambda () (interactive) (superman-capture-meeting nil nil nil)))
	("Calendar" (lambda () (interactive) (superman-capture-meeting nil nil nil)))
	("Bookmarks" (lambda () (interactive) (superman-capture-bookmark nil nil nil))))
  "Default action buttons as used by `superman-view-insert-action-buttons'
for project views.")

(defvar superman-default-action-buttons-outside-project
  '(("Meeting (Calendar)" (lambda () (interactive) (superman-capture-calendar nil nil t)) superman-capture-button-face "Capture a meeting for calendar" nil 43)
    ("Meeting (Project)" (lambda () (interactive) (superman-capture-meeting nil nil t)) superman-capture-button-face "Capture a meeting" nil 43)
    ("Task" (lambda () (interactive) (superman-capture-task nil nil t)) superman-capture-button-face "Capture a task" nil 43)
    ("Document" (lambda () (interactive) (superman-capture-document nil nil t)) superman-capture-button-face "Capture a document" nil 43)
    ("Note" (lambda () (interactive) (superman-capture-note nil nil t)) superman-capture-button-face "Capture a note" nil 43)
    ("Text" (lambda () (interactive) (superman-capture-text nil nil t)) superman-capture-button-face "Capture text" nil 43)
    ("Bookmark" (lambda () (interactive) (superman-capture-bookmark nil nil t)) superman-capture-button-face "Capture a bookmark" nil 43))
  "Default action buttons as used by `superman-view-insert-action-buttons'
for project views.")


(defun superman-view-pop-actions ()
  (interactive)
  (superman-capture-item 'pop))

(defvar superman-action-button-width 11 "Width of action buttons")

(defun superman-view-insert-action-buttons (&optional button-list no-newline title-string column)
  "Insert capture buttons. BUTTON-LIST is an alist providing button labels, functions and help strings.
 If omitted, it is set to
  '((\"Document\" 'superman-capture-document)
    (\"Task\" 'superman-capture-task)
    (\"Note\" 'superman-capture-note)
    (\"Bookmark\" 'superman-capture-bookmark)
    (\"Meeting\" 'superman-capture-meeting))
If NO-NEWLINE is non-nil no new line is inserted.
TITLE-STRING is the label of the first button and defaults to \"Action\".
If COLUMN is non-nil arrange buttons in one column, otherwise in one row.
"
  (let* (
	 ;; (title
	  ;; (if (and title-string (get-text-property 0 'button title-string)) title-string
	    ;; (superman-make-button
	     ;; (or title-string "Action:")
	     ;; 'superman-view-pop-actions
	     ;; 'superman-header-button-face
	     ;; "Edit action buttons")))
	 (b-list
	  (or button-list superman-default-action-buttons))
	 (i 1))
    (while b-list
      (let* ((b (car b-list))
	     (b-name (nth 0 b))
	     ;; (b-tail (nth 1 b))
	     (b-fun (nth 1 b))
	     (b-fun (if (and (listp b-fun)
			     (symbolp (car b-fun))
			     (string= (symbol-name (car b-fun)) "quote"))
			(nth 1 b-fun) b-fun))
	     (cmd (cond ((functionp b-fun) b-fun)
			((stringp b-fun) (intern b-fun))
			(t `(lambda () (interactive)
			      (message
			       (concat "Function "
				       ,(if (stringp b-fun) b-fun
					  (if (symbolp b-fun)
					      (symbol-name b-fun) ""))
				       " is not defined"))))))
	     (b-face (or (nth 2 b) 'superman-capture-button-face))
	     (b-help (or (nth 3 b)  (concat "capture " (downcase b-name))))
	     (b-width (or (nth 5 b) superman-action-button-width)))
	(when (= i 1)
	  (unless no-newline
	    (insert "\n"))
					;	  (insert title " ")
	  )
	(setq b-name
	      (superman-make-button
	       b-name cmd b-face b-help nil b-width))
	(put-text-property
	 0 1
	 'superman-header-marker t b-name)
	(when column (insert "\n"))
	(insert "" b-name " "))
      (setq i (+ i 1) b-list (cdr b-list)))))

(defun superman-view-insert-config-buttons (project)
  "Insert window configuration buttons"
  (let* ((pro (or project superman-current-project))
	 (config-list (superman-view-read-config pro)))
    ;; (title-marker 
    ;; (save-excursion
    ;; (superman-goto-project project "Configuration" nil nil 'narrow nil)
    ;; (goto-char (point-min))
    ;; (point-marker))))
    ;; (put-text-property 0 (length title) 'superman-e-marker title-marker title)
    ;; (when (or superman-sticky-displays config-list) (insert title " "))
    (when superman-sticky-displays
      (let ((sd superman-sticky-displays))
	(while sd 
	  (insert (apply 'superman-make-button (car sd)) " ")
	  (setq sd (cdr sd)))))
    (while config-list
      (let* ((current-config (car config-list))
	     (config-name (car current-config))
	     (config-cmd (cdr current-config)))
	(insert "[" (superman-make-button
		     config-name
		     `(lambda () (interactive)
			(superman-switch-config nil nil ,config-cmd))
		     'superman-warning-face
		     config-cmd)
		"]  "))
      (setq config-list (cdr config-list)))))

(defvar superman-sticky-displays
  '(("Timeline" superman-project-timeline superman-capture-button-face "Show project time line" nil 11)
    ("Todo" superman-project-todo superman-capture-button-face "Show project time line" nil 11)
    ("Git (g)" superman-git-display superman-capture-button-face "Show project git repository" nil 11)
    ("Files (f)" superman-view-file-list superman-capture-button-face "Show project files" nil 11)
    ("Help (h)" superman-ual superman-capture-button-face "Open the superman-ual" nil 11 ))
  "Alist of displays that are shown as action buttons for all projects.")

;;}}}
;;{{{ unison

(defun superman-view-read-unison (project)
  (let (unisons)
    (save-window-excursion
      (when (superman-goto-project project "Configuration" nil nil t nil)
	;; (org-narrow-to-subtree)
	(goto-char (point-min))
	(while (re-search-forward ":UNISON:" nil t)
	  (org-back-to-heading t)
	  (let ((hdr (progn 
		       (looking-at org-complex-heading-regexp)
		       (or (match-string-no-properties 4) "Untitled")))
		(unison-cmd (superman-get-property (point) "UNISON")))
	    (when (string= unison-cmd "superman-unison-cmd")
	      (setq unison-cmd superman-unison-cmd))
	    (setq
	     unisons
	     (append
	      unisons
	      (list (cons hdr
			  (concat
			   unison-cmd
			   " "
			   (superman-get-property (point) "ROOT-1")
			   " "
			   (superman-get-property (point) "ROOT-2")
			   " "
			   (if (string= (superman-get-property (point) "SWITCHES") "default")
			       superman-unison-switches
			     (superman-get-property (point) "SWITCHES"))))))))
	  (outline-next-heading))
	(widen)))
    unisons))

(defun superman-view-insert-unison-buttons (project)
  "Insert unison buttons"
  (let* ((pro (or project superman-current-project))
	 (title
	  (superman-make-button
	   "Unison:"
	   'superman-capture-unison
	   'superman-header-button-face
	   "Capture unison"
	   ))
	 (title-marker 
	  (save-excursion
	    (superman-goto-project project "Configuration" nil nil 'narrow nil)
	    (goto-char (point-min))
	    (point-marker)))
	 (unison-list (superman-view-read-unison pro))
	 (i 1))
    (put-text-property 0 (length title) 'superman-e-marker title-marker title)
    (put-text-property 0 1 'superman-header-marker t title)
    (while unison-list
      (let* ((current-unison (car unison-list))
	     (unison-name (car current-unison))
	     (unison-cmd (cdr current-unison)))
	(when (= i 1)
	  (insert "\n")
	  (insert title " "))
	(put-text-property
	 0 1
	 'superman-header-marker t unison-name)
	(insert "["
		(superman-make-button
		 unison-name 
		 `(lambda () (interactive)
		    ;; prevents from synchronizing
		    ;; unsaved buffers
		    (save-some-buffers nil t)
		    (async-shell-command ,unison-cmd))
		 'superman-warning-face
		 unison-name
		 'superman-view-delete-entry) "] "))
      (setq i (+ i 1) unison-list (cdr unison-list)))))

;;}}}
;;{{{ superman-buttons

(defun superman-make-button (string &optional fun face help fun-3 width)
  "Create a button with label STRING and FACE.
 If FUN is a function then it is bound to mouse-2 and RETURN events.  
 HELP is a string which is shown when the mouse over the button.
 If FUN-3 is a command it gets bound to mouse-3.
Width determines the amount of white-space around string to achieve
a fixed button width. This is useful to align a series of buttons.
"
  (let ((map (make-sparse-keymap))
	(help (or help "Superman-button"))
	(string (if (not width) string
		  (let* ((len (length string))
			 (diff (- width len))
			 (rest (/ diff 2)))
		    (if (< diff 0)
			(substring string 0 width)
		      (concat (make-string rest (string-to-char " "))
			      string (make-string (- width (+ len rest)) (string-to-char " "))))))))
    (unless (functionp fun)
      (setq fun #'(lambda () (interactive)
		    (message
		     "Not bound to a command"))))
    (define-key map [return] fun)
    (when fun-3
      (define-key map [mouse-3]
	`(lambda ()
	   (interactive)
	   ;; switch to the current window/buffer
	   (let* ((pos last-command-event)
		  (posn (event-start pos)))
	     (with-current-buffer (window-buffer (posn-window posn))
	       (goto-char (posn-point posn))
	       ;; to see where we are:
	       ;; (message (concat (buffer-name) (int-to-string (point))))
	       (funcall ',fun-3))))))
    (define-key map [mouse-2]
      `(lambda ()
	 (interactive)
	 ;; switch to the current window/buffer
	 (let* ((pos last-command-event)
		(posn (event-start pos)))
	   (with-current-buffer (window-buffer (posn-window posn))
	     (goto-char (posn-point posn))
	     ;; to see where we are:
	     ;; (message (concat (buffer-name) (int-to-string (point))))
	     (funcall ',fun)))))
    (add-text-properties
     0 (length string) 
     (list
      'button (list t)
      'category 'default-button
      'face (or face 'superman-default-button-face)
      'keymap map
      'superman-header-marker t
      'mouse-face 'highlight
      'follow-link t
      'help-echo help)
     string)
    string))

  
;;}}}
;;{{{ git branches and remote
(defun superman-view-insert-git-buttons ()
  "Insert the git buttons
Translate the branch names into buttons."
  (superman-view-insert-action-buttons
   '(("[Diff project]" superman-git-diff superman-default-button-face "Git diff")
     ("[Commit project]" superman-git-commit-project superman-default-button-face "Git all project")
     ("[Commit marked]" superman-git-commit-marked superman-default-button-face "Git commit marked files")
     ("[Status]" superman-git-status superman-default-button-face "Git status")
     ("[Delete marked]" superman-view-delete-marked superman-default-button-face "Delete marked files")))
  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'git-buttons t))

(defun superman-view-insert-git-branches (&optional dir)
  "Insert the git branch(es) if project is git controlled.
Translate the branch names into buttons."
  (let ((loc (or dir
		 (get-text-property (point-min) 'git-dir)))
	(view-buf (current-buffer)))
    (let* ((branch-list (delq nil (superman-git-branches loc)))
	   (current-branch (car branch-list))
	   (remote (member-if
		    (lambda (x)
		      (string-match "^remotes/" x)) branch-list))
	   (other-branches (cdr branch-list))
	   (title "Branch:"))
      (when remote 
	;; (setq other-branches (delete remote other-branches)))
	(setq other-branches
	      (delete-if
	       (lambda (x) (string-match "remotes/" x)) other-branches)))
      (put-text-property 0 (length title) 'face 'org-level-2 title)
      (put-text-property 0 (length title) 'superman-header-marker t title)
      (put-text-property 0 (length title) 'git-branches t title)
      (insert
       (superman-make-button
	title
	'superman-git-new-branch
	'superman-header-button-face
	"Create new git branch")
       " ")
      (put-text-property
       0 1
       'superman-header-marker t current-branch)
      (superman-make-button
       current-branch
       'superman-git-status
       'superman-warning-face
       "View status of current branch")
      (insert "[" current-branch "]  ")
      (while other-branches
	(let* ((b (car other-branches))
	       (fun `(lambda ()
		       (interactive)
		       (superman-run-cmd
			,(concat "cd " loc "; "
				 superman-cmd-git
				 " checkout " b "\n")
			"*Superman-returns*"
			nil
			,(buffer-name view-buf))))
	       (button (superman-make-button
			b fun 'font-lock-comment-face
			"Checkout branch")))
	  (setq other-branches (cdr other-branches))
	  (put-text-property 0 1 'superman-header-marker t button)
	  (insert "[" button "]  ")))
      (when (> (length other-branches) 0)
	(let ((merge-string "-> merge"))
	  (put-text-property 0 1 'superman-header-marker t merge-string)	    
	  (insert (superman-make-button
		   merge-string
		   `(lambda () (interactive)
		      (superman-git-merge-branches ,loc))
		   'font-lock-type-face
		   "Merge two branches"))))
      (when remote
	(let* ((title "Remote:")
	       (svn-p (member-if
		       (lambda (x)
			 (string-match "remotes/git-svn" x)) remote))
	       (git-p (member-if
		       (lambda (x)
			 (string-match "remotes/origin/master" x)) remote))
	       ;; (fetch-cmd (if svn-p "svn fetch" "fetch origin"))
	       ;; (merge-cmd "merge origin/master")
	       ;; (pull-cmd (if svn-p "svn rebase" "pull"))
	       ;; (push-cmd (if svn-p "svn dcommit" "push"))
	       (remote-cmd (if git-p
			       (if svn-p
				   (concat
				    "remote show origin;" superman-cmd-git " svn info")
				 "remote show origin")
			     (if svn-p "svn info" ""))))
	  (put-text-property 0 (length title) 'git-remote t title)
	  ;; git diff --name-status remotes/git-svn
	  (insert "\n"
		  (superman-make-button
		   title
		   `(lambda ()
		      (interactive)
		      (superman-run-cmd
		       (concat "cd " ,loc ";"
			       ,superman-cmd-git " " ,remote-cmd "\n")
		       "*Superman-returns*"
		       (concat "`" ,superman-cmd-git " " ,remote-cmd
			       " run below \n" ,loc "' returns:\n\n")))
		   'superman-header-button-face
		   "Show origin of remote repository"))
	  ;; fetch git
	  (when git-p
	    (insert
	     " "
	     (superman-make-button
	      " fetch origin "
	      `(lambda () (interactive)
		 (superman-run-cmd (concat "cd " ,loc  ";" ,superman-cmd-git " fetch origin\n")
				   "*Superman-returns*"
				   (concat "`" ,superman-cmd-git " fetch origin' run below \n" ,loc "' returns:\n\n")))
	      'file-list-action-button-face
	      "Fetch changes from remote git repository")))
	  ;; fetch svn
	  (when svn-p
	    (insert
	     " "
	     (superman-make-button
	      "svn fetch"
	      `(lambda () (interactive)
		 (superman-run-cmd (concat "cd " ,loc  ";" ,superman-cmd-git " svn fetch\n")
				   "*Superman-returns*"
				   (concat "`" ,superman-cmd-git " svn fetch' run below \n" ,loc "' returns:\n\n")))
	      'file-list-info-button-face
	      "Fetch changes from remote svn repository")))
	  ;; merge
	  (when git-p
	    (insert
	     " "
	     (superman-make-button
	      " merge "
	      `(lambda () (interactive)
		 (superman-run-cmd (concat "cd " ,loc  ";" ,superman-cmd-git " merge origin/master\n")
				   "*Superman-returns*"
				   (concat "`" ,superman-cmd-git " merge origin/master' run below \n" ,loc "' returns:\n\n")))
	      'file-list-inverse-filter-button-face
	      "Merge origin/master and master repository")))
	  ;; pull
	  (when git-p
	    (insert
	     " "
	     (superman-make-button
	      "  pull  "
	      `(lambda () (interactive)
		 (superman-run-cmd
		  (concat "cd " ,loc  ";" ,superman-cmd-git " pull\n")
		  "*Superman-returns*"
		  (concat "`" ,superman-cmd-git " pull' run below \n" ,loc "' returns:\n\n")))
	      'file-list-filter-button-face
	      "Pull changes from remote git repository")))
	  ;; push
	  (when git-p
	    (insert
	     " "
	     (superman-make-button
	      "  push  "
	      `(lambda () (interactive)
		 (superman-run-cmd (concat "cd " ,loc  ";" ,superman-cmd-git " push\n")
				   "*Superman-returns*"
				   (concat "`" ,superman-cmd-git " ' run below \n" ,loc "' returns:\n\n")))
	      'file-list-action-button-face
	      "Push changes to remote git repository")))
	  ;; rebase
	  (when svn-p
	    (insert
	     " "
	     (superman-make-button
	      " rebase "
	      `(lambda () (interactive)
		 (superman-run-cmd
		  (concat "cd " ,loc  ";" ,superman-cmd-git " svn rebase\n")
		  "*Superman-returns*"
		  (concat "`" ,superman-cmd-git " svn rebase' run below \n" ,loc "' returns:\n\n")))
	      'file-list-info-button-face
	      "Rebase with remote svn repository")))
	  ;; dcommit
	  (when svn-p
	    (insert
	     " "
	     (superman-make-button
	      " dcommit "
	      `(lambda () (interactive)
		 (superman-run-cmd (concat "cd " ,loc  ";" ,superman-cmd-git " svn dcommit\n")
				   "*Superman-returns*"
				   (concat "`" ,superman-cmd-git " svn dcommit' run below \n" ,loc "' returns:\n\n")))
	      'file-list-action-button-face
	      "Push changes to remote svn repository"))))))))


;;}}}
;;{{{ Marking elements

(defun superman-toggle-mark (&optional on)
  "Toggle mark for item at point in project view.
If ON is non-nil keep mark for already marked items.
If DONT-MOVE is non-nil stay at item."
  (interactive)
  (when (get-text-property (point-at-bol) 'superman-item-marker)
    (if (org-agenda-bulk-marked-p)
	(unless on (org-agenda-bulk-unmark))
      (org-agenda-bulk-mark))))

(defun superman-view-mark-all (&optional arg)
  (interactive "P")
  arg
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (superman-loop 'superman-toggle-mark
		     (list (if arg nil 'on))))))

(defun superman-view-invert-marks (&optional arg)
  (interactive "P")
  arg
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (superman-loop 'superman-toggle-mark
		     (list arg)))))



(defun superman-marked-p ()
  (org-agenda-bulk-marked-p))

;;}}}
;;{{{ Loops

(defun superman-loop (fun args &optional begin end marked)
  "Call function FUN on all items in the range BEGIN to END and return
the list of results.

If MARKED is non-nil run only on marked items."

  (let (loop-out
	(begin (or begin (point-min)))
	(end (or end (point-max)))
	next)
    (save-restriction
      (narrow-to-region begin end)
      (save-excursion
	(goto-char (point-min))
	(while (setq next (next-single-property-change
			   (point-at-eol) 'superman-item-marker))
	  (goto-char next)
	  (when (or (not marked)
		    (superman-marked-p))
	    (setq loop-out
		  (append (list (apply fun args)) loop-out)))
	  (goto-char next)
	  (end-of-line))
	loop-out))))

(defun superman-view-marked-files (&optional beg end)
  (delq nil (superman-loop
	     #'(lambda ()
		 (or (and (superman-marked-p)
			  (superman-filename-at-point
			   'no-error)))) nil beg end)))

(defun superman-count-items (&optional begin end)
  (let ((count 0) 
	(begin (or begin (point-min)))
	(end (or end (point-max))))
    (save-restriction
      (narrow-to-region begin end)
      (save-excursion
	(goto-char (point-min))
	(while (next-single-property-change
		(point-at-eol) 'superman-item-marker)
	  (goto-char (next-single-property-change (point-at-eol) 'superman-item-marker))
	  ;; (when (or (not marked)
	  ;; (eq (get-text-property (point) (car marked)) (cadr marked)))
	  (setq count (+ 1 count)))
	count))))


(defun superman-structure-loop (fun args)
  "Loop over headings in a superman-views buffer."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (while (outline-next-heading)
      (org-narrow-to-subtree)
      (apply fun args)
      (widen))))

;;}}}
;;{{{ Columns and balls

(defun superman-column-names (balls)
  "Insert column names that are extracted from BALLS via `superman-format-column names'."
  (let ((cols (superman-format-column-names balls)))
    (put-text-property 0 (length cols) 'face 'superman-column-name-face cols)
    ;; (put-text-property 0 (length cols) 'column-names t cols)
    (put-text-property 0 (length cols) 'names (length cols) cols)
    cols))

(defun superman-format-column-names (balls)
  "Format column names NAMES according to balls, similar to
`superman-format-thing'.
Return the formatted string with text-properties."
  (let ((column-names "")
	names
	(column-widths (list 0))
	(copy-balls balls)
	(map (make-sparse-keymap)))
    (define-key map [mouse-2] 'superman-sort-section)
    (define-key map [return] 'superman-sort-section)
    ;; loop columns
    (while copy-balls
      (let* ((b (car copy-balls))
	     (col-name (cond ((cadr (assoc "name" b)))
			     ((stringp (car b)) (car b))
			     ((eq (car b) 'hdr) "Title")
			     ((eq (car b) 'todo) "Status")
			     ((eq (car b) 'priority) "Priority")
			     ((eq (car b) 'attac) " ")
			     ((eq (car b) 'org-hd-marker) " ")
			     (t (symbol-name (cadr (assoc "fun" (cdr b)))))))
	     name
	     (sort-cmd  (concat "sort-by \"" col-name "\"")))
	;; add sorting symbol
	;; (char-to-string #x25b3)
	;; make this name a button
	(setq name
	      (superman-make-button
	       col-name
	       'superman-sort-section
	       'superman-next-project-button-face
	       sort-cmd))
	;; replace trim function
	;; (setcdr (assoc "fun" b) 'superman-trim-string)
	(setq b (remove-if
		 #'(lambda(x)
		     (cond ((not (listp x)) nil)
			   ((string-match (car x) "fun") t)
			   (t nil))) b))
	(setq name
	      (superman-play-ball
	       name
	       b
	       'no-face))
	(setq column-widths (append column-widths (list (length name))))
	(setq column-names (concat column-names name))
	(set-text-properties 0 (length col-name) nil col-name)
	(setq names (append names (list col-name)))
	;; (superman-make-button
	;; (char-to-string #x25b3)
	;; 'superman-sort-section
	;; nil
	;; "Sort section")
	;; name))
	(setq copy-balls (cdr copy-balls))))
    ;; add text properties: columns and column-names
    (put-text-property 0 (length column-names) 'columns column-widths column-names)
    (put-text-property 0 (length column-names) 'column-names names column-names)
    column-names))


(defun superman-parse-props (&optional pom add-point with-heading)
  "Read properties at point or marker POM and return
them in an alist where the key is the name of the property
in lower case.

If ADD-POINT augment the list by an element
which holds the point of the heading."
  (org-with-point-at pom
    (save-excursion
      (let ((case-fold-search t)
	    (pblock (org-get-property-block))
	    props
	    (next 0))
	(when add-point
	  (setq props `(("point" ,(point)))))
	(when pblock
	  (narrow-to-region (car pblock) (cdr pblock))
	  (goto-char (point-min))
	  (while (= next 0)
	    (when (looking-at "^[ \t]*:\\([^:]+\\):[ \t]*\\(.*\\)[ \t]*$")
	      (setq props
		    (append
		     props
		     `((,(downcase (match-string-no-properties 1)) ,(match-string-no-properties 2))))
		    ))
	    (setq next (forward-line 1)))
	  (widen))
	(if (not with-heading)
	    props
	  (let ((header (ignore-errors (org-get-heading t t))))
	    (if (not header)
		`("Untitled section" ,props)
	      `(,header ,props))))))))

(defun superman-delete-balls (&optional pom)
  "Delete balls, i.e. column properties, at point or marker POM."
  (org-with-point-at pom
    (save-excursion
      (let ((case-fold-search t)
	    (beg (point))
	    (end (if (boundp 'org-end-of-meta-data)
		     (org-end-of-meta-data)
		   (org-end-of-meta-data 't)
		   (point)))
	    (kill-whole-line t)
	    balls)
	(save-excursion
	  (goto-char beg)
	  (when (re-search-forward "PROPERTIES" end 't)
	    (while (re-search-forward "^[\t ]+:Ball[0-9]+:" end 't)
	      (beginning-of-line)
	      (kill-line)
	      (goto-char (point-at-bol)))
	    balls))))))


(defun superman-string-to-thing (string &optional prefer-string prefer-symbol)
  "Convert STRING to either integer, string or symbol-name. If not an integer
and PREFER-SYMBOL is non-nil return symbol unless PREFER-STRING."
  (let (thing)
    (if (> (setq thing (string-to-number string)) 0)
	thing
      ;; either string or function
      (if prefer-string
	  (if (or (string= string "nil")
		  (string= string ""))
	      nil
	    string)
	(if (or (functionp (setq thing (intern string)))
		prefer-symbol)
	    thing
	  (if (or (string= string "nil")
		  (string= string ""))
	      nil
	    string))))))

(defvar superman-special-balls-regexp
   "^todo\\|hdr\\|priority\\|index\\|org-hd-marker"
   "Regular expression to identify ball-names as symbolic
instead of string. Symbolic ball-names are treated
in a special way by `superman-play-ball'")

(defun superman-distangle-ball (ball)
  (let* ((plist (split-string ball "[ \t]+:"))
         (prop (car plist))
	 (args
	  (mapcar
	   #'(lambda (x)
	       (let* ((pos (string-match "[ \t]" x))
		      (key (downcase (substring x 0 pos)))
		      (value (substring x (+ pos 1) (length x))))
		 (cond ((string= (downcase key) "args")
			`(,key ,(mapcar 'superman-string-to-thing (split-string value ","))))
		       ((string-match "^fun\\|face$" (downcase key))
			;; prefer symbol
			`(,key ,(superman-string-to-thing value nil t)))
		       (t
			;; prefer string
			`(,key ,(superman-string-to-thing value t nil))))))
	   (cdr plist))))
    (when (string-match superman-special-balls-regexp prop)
      (setq prop (intern prop)))
    (append (list prop) args)))

(defun superman-save-balls ()
  "Save the columns (balls) in current category for future sessions."
  (interactive)
  (if (not superman-view-mode)
      (message "Can only save balls in superman-view-mode.")
    (let ((cat-point (superman-cat-point)))
      (if (not cat-point)
	  (message "Point is not inside a category.")
	(let* ((pom (get-text-property cat-point 'org-hd-marker))
	       (balls (get-text-property cat-point 'balls))
	       (i 1))
	  (superman-delete-balls pom)
	  (while balls
	    (org-entry-put
	     pom
	     (concat "Ball" (int-to-string i))
	     (superman-ball-to-string (car balls)))
	    (setq balls (cdr balls)
		  i (+ i 1))))
	(superman-view-save-index-buffer)
	(superman-redo)))))

(defun superman-thing-to-string (thing)
  (cond ((stringp thing) thing)
	((integerp thing) (int-to-string thing))
	((symbolp thing) (symbol-name thing))))

(defun superman-ball-to-string (ball)
  (let ((key (car ball))
	(args (cdr ball))
	bstring)
    (setq bstring (concat
		   (if (stringp key) 
		       key
		     (symbol-name key)) " "))
    (while args
      (setq bstring 
	    (concat bstring " :"
		    (nth 0 (car args))
		    " "
		    (superman-thing-to-string (nth 1 (car args)))))
	    (setq args (cdr args)))
      bstring))


(defun superman-ball-dimensions ()
  "Return column start, width and nth at (point)."
  (let* ((cols (cdr (get-text-property
		     (point-at-bol) 'columns)))
	 (start 0)
	 width
	 (n 0)
	 (cc (current-column)))
    (when cols
      (while (> cc (+ start (car cols)))
	(setq n (+ 1 n))
	(setq start (+ start (car cols)))
	(setq cols (cdr cols)))
      (setq width (- (car cols) 1))
      (list start width n))))



(defun superman-sort-section-reverse ()
  (interactive)
  (superman-sort-section 1))

(defun superman-sort-section (&optional reverse)
  (interactive "P")
  (let* ((buffer-read-only nil)
	 (cc (current-column))
	 (pp (point-at-bol))
	 (sort-fold-case t) 
	 (dim (superman-ball-dimensions))
	 ;; the first x characters of each column are blank, 
	 ;; where x is defined by superman-column-separator,
	 ;; thus, we shift by superman-column-separator
	 (col-start (+ superman-column-separator (nth 0 dim)))
	 (col-width (nth 1 dim))
	 (current-col (nth 2 dim))
	 (n (get-text-property (superman-cat-point) 'n-items))
	 (pos (superman-current-subcat-pos))
	 (reverse (get-text-property (previous-single-property-change (point) 'button) 'reverse))
	 irregular
	 next
	 beg
	 end)
    (when (get-text-property (point-at-bol) 'column-names)
      (put-text-property
       (previous-single-property-change (point) 'button)
       (next-single-property-change (point) 'button)
       'reverse (not reverse)))
    (when (and pos dim)
      (goto-char pos)
      (goto-char (next-single-property-change
		  (point-at-eol)
		  'superman-item-marker))
      (setq beg (point))
      ;; test if column has fixed width
      (unless (eq col-width
		  (1- (nth (1+ current-col)
			   (get-text-property (point-at-bol) 'columns))))
	(setq irregular t)
	(setq col-width
	      (1- (nth (1+ current-col)
		       (get-text-property (point-at-bol) 'columns))))
	(while (setq next (next-single-property-change (point-at-eol) 'superman-item-marker))
	  (goto-char next)
	  (setq col-width
		(max col-width
		     (nth (1+ current-col)
			  (get-text-property (point-at-bol) 'columns))))))
      ;; move to end of section
      (or (outline-next-heading)
	  (goto-char (point-max)))
      (goto-char (previous-single-property-change
		  (point-at-eol) 'superman-item-marker))
      (end-of-line)
      (setq end (point))
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; sort by sort-key if any
      (if (get-text-property (+ (point) col-start) 'sort-key)
	  (let (key)
	    (if reverse 
		(let ((maxkey (apply 'max (superman-loop
					   #'(lambda (&rest args)
					       (get-text-property (+ (point) col-start) 'sort-key)) '(nil)))))
		  (goto-char (point-min))
		  (setq key (get-text-property (+ (point) col-start) 'sort-key))
		  (insert (int-to-string (+ (- maxkey key) 1)))
		  (while (re-search-forward "^" nil t)
		    (setq key (get-text-property (+ (point) col-start) 'sort-key))
		    (insert (int-to-string (+ (- maxkey key) 1)))))
	      (goto-char (point-min))
	      (setq key (get-text-property (+ (point) col-start) 'sort-key))
	      (insert (int-to-string key))
	      (while (re-search-forward "^" nil t)
		(setq key (get-text-property (+ (point) col-start) 'sort-key))
		(insert (int-to-string key))))
	    (sort-numeric-fields 0 (point-min) (point-max))
	    (goto-char (point-min))
	    (while (re-search-forward "^" nil t)
	      (delete-region (point)
			     (+ (point)
				(skip-chars-forward "[0-9]")))
	      (end-of-line)))
	;; sort using sort-subr
	(if irregular
	    (sort-subr reverse 'forward-line 'end-of-line
		       `(lambda () (forward-char ,col-start))
		       `(lambda ()
			  (let ((here (point))
				(there (point-at-eol)))
			    (forward-char (min (- there here)
					       ,col-width)))))
	  (sort-subr reverse 'forward-line 'end-of-line
		     `(lambda () (forward-char ,col-start))
		     `(lambda () (forward-char ,col-width)))))
      (widen)
      (goto-char pp)
      (forward-char (+ superman-column-separator col-start)))))
      

(defun superman-sort-by-status (a b)
  (let ((A  (substring-no-properties a 0 12))
	(B  (substring-no-properties b 0 12)))
    (if (string= A B) nil 
      (if (string-lessp A B)
	  1 -1))))

;;}}}
;;{{{ Project views

(defun superman-parse-cats (buffer level)
  "Parse headings with level LEVEL in buffer BUFFER. Return a list
with elements (heading-name props) where props is a list
with the heading's properties augmented by an element called \"point\"
which locates the heading in the buffer."
  (ignore-errors
    (save-excursion
      (save-restriction
	(set-buffer buffer)
	(when (eq major-mode 'org-mode)
	  (widen)
	  (show-all)
	  (goto-char (point-min))
	  ;; move to first heading
	  ;; with the correct level
	  ;; (let ((org-outline-regexp "\\*+ "))
	  (while (and (not (and
			    (looking-at org-complex-heading-regexp)
			    (org-current-level)
			    (= (org-current-level) level)))
		      (outline-next-heading)))
	  (when (and (org-current-level) (= (org-current-level) level))
	    (let ((cats `(,(superman-parse-props (point) 'p 'h)))
		  (cat-point (point)))
	      (while (progn (org-forward-heading-same-level 1)
			    (> (point) cat-point))
		;; (looking-at org-complex-heading-regexp)
		(setq cat-point (point)
		      cats (append cats `(,(superman-parse-props cat-point 'p 'h)))))
	      cats)))))))

(defun superman-get-project (object &optional ask)
  "Identify project based on OBJECT and current buffer's text-property nickname at point-min.
Unless optional argument ASK is non-nil use `superman-current-project' if
neither object nor the current buffer identify a project."
  (let (nick)
    (cond
     ((stringp object)
      (assoc object superman-project-alist))
     ((assoc "index" (cadr object))
      object) ;; assume object is a project
     ((and superman-mode ;; in superman-buffer
	   (superman-project-at-point)))
     (ask (superman-select-project))
     ((setq nick (get-text-property (point-min) 'nickname))
      (assoc nick superman-project-alist))
     ((not ask) superman-current-project)
     (t (superman-select-project)))))

(defun superman-view-project (&optional project refresh redo) 
  "Display project in a special view buffer. Optional
argument PROJECT can be the nickname of a project or the
project element of `superman-project-alist'. If PROJECT is nil
use `superman-current-project' and prompt if this is not set.

Return an existing buffer and do not update, unless REFRESH is non-nil.

If REDO is given, it must be the name of a command which should be used
to refresh the view.
"
  (interactive)
  (let* ((pro (superman-get-project project))
	 (vbuf (concat "*Project[" (car pro) "]*")))
    (if (and (not refresh) (get-buffer vbuf))
	;; find existing buffer
	(progn
	  (switch-to-buffer vbuf)
	  (let ((buffer-read-only nil))
	    (widen)
	    (goto-char (next-single-property-change
			(point-min) 'nickname))
	    (when (looking-at ".*$")
	      (replace-match "")
	      (superman-view-insert-project-buttons))))
      ;; refresh
      (let* ((nick (car pro))
	     (loc (superman-project-home pro))
	     (gitp (superman-git-p loc))
	     (index (superman-get-index pro))
	     (ibuf (or (get-buffer index) ;; new since 11.jan.2014
		       (get-file-buffer index)
		       (find-file index)))
	     (cats (delete-if
		    #'(lambda (cat)
			(string= "Configuration" (car cat)))
		    (superman-parse-cats ibuf 1)))
	     ;; identify appropriate buttons
	     (buttons
	      (with-current-buffer ibuf
		(goto-char (point-min))
		(let ((b-string))
		  (when (re-search-forward ":CaptureButtons:" nil t)
		    (setq b-string (superman-get-property (point) "CaptureButtons" nil))
		    (if b-string
			(mapcar #'(lambda (x)
				    (split-string x "|"))
				(split-string (replace-regexp-in-string
					       "^[ \t]*\\|[ \t]$" "" b-string) "," t)) "nil")))))
	     ;; maybe disable config buttons
	     (config-buttons nil)
	     ;; (with-current-buffer
	     ;; (goto-char (point-min))
	     ;; (when (re-search-forward ":ConfigButtons:" nil t))))
	     ;; (superman-get-property (point) "ConfigButtons" nil))))
	     (font-lock-global-modes nil)
	     (org-startup-folded nil))
	(set-text-properties 0 (length nick) nil nick)
	(switch-to-buffer vbuf)
	(setq buffer-read-only nil)
	(erase-buffer)
	;; major-mode
	(org-mode)
	(font-lock-mode -1)
	;; minor-mode
	(superman-view-mode-on)
	;; (font-lock-default-function nil)
	;; insert header, set text-properties and highlight
	(let* ((others (superman-get-others pro))
	       (button-text (concat
			     (if (or (not others) (string= others "")) "Project with me-myself-and-I"
			       (concat "Project with " others))
			     "\nlast update at: " (format-time-string "%r")
			     "\nPress key 'R' or this button to refresh the view")))
	  (insert (superman-make-button
		   ;; (concat "Project: " nick)
		   (concat " Project: " nick " ")
		   'superman-redo
		   'superman-face
		   button-text)))
	;; (put-text-property (point-at-bol) (point-at-eol) 'face 'superman-project-button-face)
	(put-text-property (point-at-bol) (point-at-eol) 'redo-cmd
			   (or redo
			       `(superman-view-project ,nick t)))
	(put-text-property (point-at-bol) (point-at-eol) 'git-dir
			   (superman-git-toplevel loc))
	(put-text-property (point-at-bol) (point-at-eol) 'dir loc)
	(put-text-property (point-at-bol) (point-at-eol) 'project-view t) ;; to identify project-view buffer, do not copy to other views
	(put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
	(put-text-property (point-at-bol) (point-at-eol) 'index index)
	;; link to previously selected projects
	(insert " ")
	(unless config-buttons
	  (superman-view-insert-config-buttons pro))
	;; (superman-view-insert-project-buttons)
	(insert "\n\n")
	(superman-view-insert-unison-buttons pro)
	;; (when gitp
	;; (insert (superman-view-control pro))
	;; action buttons
	;; (unless (and (stringp buttons) (string= buttons "nil"))
	  ;; (superman-view-insert-action-buttons buttons))
	;; loop over cats
	(goto-char (point-max))
	(insert "\n\n")
	(while cats
	  (superman-format-cat (car cats) ibuf vbuf loc)
	  (setq cats (cdr cats)))
	;; leave index buffer widened
	(set-buffer ibuf)
	(widen)
	(show-all)
	(switch-to-buffer vbuf))
      (goto-char (point-min))
      ;; facings
      (save-excursion
	(while (or (org-activate-bracket-links (point-max))
		   (org-activate-plain-links (point-max)))
	  (add-text-properties
	   (match-beginning 0) (match-end 0)
	   '(face org-link))))
      ;; default-dir
      (setq default-directory
	    (superman-project-home pro))
      (setq buffer-read-only t))))

(defun superman-redo-cat (&optional cat)
  "Redo the current section in a superman view buffer.
Optional argument CAT is an alist providing formatting 
properties such as balls for the section.
"
  (if (not (get-text-property (point-at-bol) 'cat))
      (error "Not at category heading")
    (let* ((cat-point (point-at-bol))
	   (marker (get-text-property
		    (point-at-bol) 'org-hd-marker))
	   (cat (cond (cat)
		      (marker
		       (superman-parse-props
			(get-text-property (point-at-bol) 'org-hd-marker)
			'p 'h))
		      ((string= (get-text-property (point-at-bol) 'cat) "git")
		       `("git"
			 (("git-cycle" ,superman-git-default-displays)
			  ("git-display"  ,(car superman-git-default-displays))
			  ("point" ,(point-at-bol)))))
		      (t (error "Don't know how to do this cat"))))
	   (view-buf (current-buffer))
	   (index-buf (when marker (marker-buffer marker)))
	   (buffer-read-only nil))
      (org-cut-subtree)
      (superman-format-cat 
       cat index-buf
       view-buf
       (get-text-property (point-min) 'git-dir))
      (if cat-point
	  (goto-char cat-point)))))

(defun superman-format-cat (cat index-buf view-buf git-dir)
  "Format category CAT based on information in buffer INDEX-BUF. Display the result
in buffer VIEW-BUF."
  (let* ((case-fold-search t)
	 (name (downcase (car cat)))
	 (props (cadr cat))
	 (git (string= "git" name))
	 (mail (string= "mail" name))
	 ;; prefer columns/balls specified in index buffer
	 (cat-balls (unless git
		      (if props
			  (delete
			   nil
			   (mapcar
			    #'(lambda (x)
				(when (string-match "^Ball[0-9]+$" (car x))
				  (superman-distangle-ball (cadr x)))) props)))))
	 (gear (cdr (assoc name superman-finalize-cat-alist)))
	 (balls (or cat-balls (eval (nth 0 gear)) superman-default-balls))
	 (index-point (cadr (assoc "point" props)))
	 (buttons (cadr (assoc "buttons" props)))
	 ;; (folded (cadr (assoc "startfolded") props))
	 (free (assoc "freetext" props))
	 (hidden (assoc "hidden" props)))
    (cond ((and hidden
		;;hidden sections 
		(let ((hidden-expr (cadr hidden)))
		  (if (string-match "not[ \t]*\\(.*\\)$" hidden-expr)
		      (not (eval (intern (match-string-no-properties 1 hidden-expr))))
		    (eval (intern hidden-expr))))))
	  ;; free text sections are shown as they are
	  (free
	   (superman-format-freetext index-buf index-point view-buf))
	  ;; git control section
	  (git
	   ;; git display git control of directory 
	   (superman-git-format-display
	    view-buf git-dir props
	    index-buf 
	    name))
	  ;; mail section
	  (mail
	   ;; git display git control of directory 
	   (superman-format-mailbox
	    index-buf index-point view-buf balls buttons name))
	  ;; table view for sections with balls 
	  (balls 
	   (superman-format-table-view
	    index-buf index-point view-buf
	    balls buttons name))
	  (goto-char (point-max))
	  (widen))))
;; (when folded (hide-subtree))

(defun superman-format-table-view (index-buffer index-point
						view-buffer balls buttons name)
  ;; create table view in view-buffer with columns characterized by balls
  ;; and (property) values found in index-buffer 
  (let (countsub line (count 0) 
		 index-marker view-point)
    (set-buffer view-buffer)
    (setq view-point (point))
    (set-buffer index-buffer)
    (widen)
    (goto-char index-point)
    (let* ((item-prop (superman-get-property (point) "item-level"))
	   (item-level (if item-prop (string-to-number item-prop)
			 superman-item-level))
	   (sub-level (- item-level 1))
	   (attac-level (+ item-level 1))
	   (attac-balls (cdr (assoc 'attac balls))))
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (setq index-marker (point-marker))
      ;; loop over items in cat
      ;; format elements (if any and if wanted)
      ;; region is narrowed to section
      (while (outline-next-heading)
	(cond ((eq (org-current-level) sub-level)
	       ;; sub-headings
	       (let ((subhdr (progn (looking-at org-complex-heading-regexp) (match-string-no-properties 4))))
		 (setq line (concat "*** " subhdr))
		 (put-text-property 0 (length line) 'subcat subhdr line)
		 (put-text-property 0 (length line) 'org-hd-marker (point-marker) line)
		 (put-text-property 0 (length line) 'face 'org-level-3 line)
		 (put-text-property 0 (length line) 'face 'superman-subheader-face line)
		 (put-text-property 0 (length line) 'display (concat "  ☆ " subhdr) line)
		 (with-current-buffer view-buf (setq countsub (append countsub (list `(0 ,(point))))))
		 ;; help superman-one-up to find the right place
		 (when (get-text-property (point-at-bol) 'point-here)
		   (put-text-property 0 (length line) 'point-here t line)
		   (put-text-property  (point-at-bol)  (point-at-eol) 'point-here nil))
		 (with-current-buffer view-buf (insert line " \n" ))
		 (end-of-line)))
	      ;; items
	      ((eq (org-current-level) item-level)		       
	       (if countsub
		   (setf (car (car (last countsub))) (+ (car (car (last countsub))) 1)))
	       (setq count (+ count 1))
	       (setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
	       ;; help superman-one-up to find the right place
	       (when (get-text-property (point-at-bol) 'point-here)
		 (put-text-property 0 (length line) 'point-here t line)
		 (put-text-property  (point-at-bol)  (point-at-eol) 'point-here nil))
	       (with-current-buffer view-buf
		 (insert line "\n")))
	      ;; attachments
	      ((and (eq (org-current-level) attac-level) attac-balls)
	       (setq line (superman-format-thing (copy-marker (point-at-bol)) attac-balls))
	       (with-current-buffer view-buf (insert line "\n"))))))
    ;; add counts in sub headings
    (set-buffer view-buf)
    (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)
    (save-excursion 
      (while countsub
	(let ((tempsub (car countsub)))
	  (goto-char (nth 1 tempsub))
	  (put-text-property
	   (- (point-at-eol) 1) (point-at-eol) 'display
	   (concat " [" (int-to-string (car tempsub)) "]")))
	(setq countsub (cdr countsub))))
    ;; (widen)
    ;; empty cats are not shown unless explicitly wanted
    (if (or (not (member cat superman-capture-alist))
	    (member name superman-views-permanent-cats)
	    (> count 0))
	(progn 
	  ;; insert the section name
	  (set-buffer view-buf)
	  (goto-char view-point)
	  (when (and
		 superman-empty-line-before-cat
		 (save-excursion (beginning-of-line 0)
				 (not (looking-at "^[ \t]*$"))))
	    (insert "\n"))
	  (superman-view-insert-section-name name count balls index-marker)
	  (insert "\n")
	  ;; insert the column names
	  (when superman-empty-line-after-cat (insert "\n"))
	  (insert (superman-column-names balls))
	  (when buttons
	    (beginning-of-line)
	    (funcall (intern buttons))
	    (insert "\n")))
      (delete-region (point) (point-max)))
    (goto-char view-point)))

(defun superman-format-freetext (index-buffer index-point view-buffer)
  (let (index-marker view-marker)
    ;; set mark at beginning of the category in view-buf
    (set-buffer view-buffer)
    (setq view-marker (point))
    (set-buffer index-buffer)
    (widen)
    (goto-char index-point)
    (setq index-marker (point-marker))
    (save-restriction
      (org-narrow-to-subtree)
      (let ((text
	     (buffer-substring
	      (progn
		(if (boundp 'org-end-of-meta-data)
		    (org-end-of-meta-data)
		  (org-end-of-meta-data 't))
		(point))
	      (point-max))))
	(with-current-buffer view-buffer
	  (when superman-empty-line-before-cat (insert "\n"))
	  (superman-view-insert-section-name
	   (concat (upcase (substring-no-properties  name 0 1)) (substring-no-properties  name 1 (length name) ))
	   0 balls
	   index-marker)
	  (insert "\n")
	  (when superman-empty-line-after-cat (insert "\n"))
	  (put-text-property (- (point-at-eol) 1) (point-at-eol) 'head name)
	  (insert text)
	  (end-of-line)
	  (insert "\n")
	  (forward-line -1)
	  (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)
	  (forward-line 1))))))
  
(defun superman-format-mailbox (index-buffer index-point view-buffer
					     balls buttons name) 
  (superman-format-table-view index-buffer index-point view-buffer
			      balls buttons name))

(defun superman-split-cat (&optional column)
  (interactive)
  (goto-char (next-single-property-change  (superman-cat-point) 'columns))
  (let* ((col-info (get-text-property (point-at-bol) 'columns))
	 (col-start (superman-compute-columns-start col-info))
	 ;; (balls (get-text-property (superman-cat-point) 'balls))
	 (i 0)
	 (colnames (get-text-property (point-at-bol) 'column-names))
	 (col (or column (completing-read "Split column: "
					  (mapcar* 'cons colnames (make-list (length colnames) `())))))
	 (cc (- (length colnames) (length (member col colnames))))
	 (c-start (+ (nth cc col-start) superman-column-separator))
	 (c-end (nth (1+ cc) col-info))
	 (buffer-read-only nil)
	 (cat-head (point))
	 (cat-tail (next-single-property-change (point) 'tail))
	 last-el
	 cur-el
	 next)
    (save-restriction
      (narrow-to-region cat-head cat-tail)
      (goto-char (point-min))
      (while (setq next (next-single-property-change (point-at-eol) 'superman-item-marker))
	(goto-char next)
	(let ((cur-el (buffer-substring-no-properties (+ (point) c-start) (+ (point) c-end c-start))))
	  (delete-region (+ (point) c-start) (+ (point) c-end c-start))
	  (unless (string= last-el cur-el)
	    (insert "** " col ": " cur-el "\n")
	    (forward-line -1)
	    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
	    (forward-line 1)
	    (setq last-el cur-el)))))))
    

(defun superman-view-insert-section-name (name count balls index-marker &optional fun help)
  (let ((fun (or
	      fun
	      'superman-view-edit-item
	      ;; (cadr (assoc name superman-capture-alist))
	      'superman-capture-item)))
    (insert
     (superman-make-button (concat "** " name) fun
			   'superman-capture-button-face
			   (or help "Add new item"))
     "\n"))
  (forward-line -1)
  (beginning-of-line)
  (put-text-property (point-at-bol) (point-at-eol) 'cat name)
  (put-text-property (point-at-bol) (point-at-eol) 'n-items count)
  (put-text-property (point-at-bol) (point-at-eol) 'balls balls)
  (put-text-property (point-at-bol) (point-at-eol) 'org-hd-marker index-marker)
  (put-text-property (point-at-bol) (point-at-eol) 'display (concat "★ " name))
  (end-of-line)
  (insert " [" (int-to-string count) "]")
  (unless (or (not superman-view-mode) superman-git-mode)
    (insert (superman-make-button
	     "column+"
	     'superman-new-ball
	     'superman-git-keyboard-face-i
	     "Add a column")
    " " (superman-make-button
	     "item+"
	     'superman-capture-thing
	     'superman-git-keyboard-face-i
	     "Add an item"))))

;;}}}
;;{{{ Formatting items and column names

(defun superman-play-ball (thing ball &optional no-face)
  "Play BALL at THING which is a marker or an alist and return
a formatted string with faces."
  (let* ((object (car ball))
	 (raw-string
	  (cond ((markerp thing)
		 ;; thing is marker
		 (cond ((stringp object) ;; properties
			(if (assoc "regexp" ball)
			    (superman-get-matching-property thing object t)
			  (superman-get-property thing object t)))
		       ;; important:
		       ;; when introducing new special things 
		       ;; also adapt superman-distangle-ball
		       ((eq object 'org-hd-marker) ;; special: marker
			thing)
		       ((eq object 'hdr) ;; special: header
			(org-with-point-at thing 
			  (org-back-to-heading t)
			  (looking-at org-complex-heading-regexp)
			  (match-string 4)))
		       ((eq object 'todo) ;; special: todo state
			(org-with-point-at thing 
			  (org-back-to-heading t)
			  (and (looking-at org-todo-line-regexp)
			       (match-end 2) (match-string 2))))
		       ((eq object 'priority) ;; special: priority
			(org-with-point-at thing 
			  (org-back-to-heading t)
			  (looking-at org-complex-heading-regexp)
			  (match-string 3)))
		       ((eq object 'attac) ;; special: attachment
			nil)
		       ((eq object 'index) ;; special: index filename
			(file-name-sans-extension
			 (file-name-nondirectory
			  (buffer-file-name (current-buffer)))))
		       (t "--")))
		((stringp thing) thing)
		;; thing is alist
		((listp thing) (cdr (assoc object (cadr thing))))
		(t "--")))
	 (raw-string (if (or (not raw-string) (eq raw-string "")) "--" raw-string))
	 (fun (or (cadr (assoc "fun" (cdr ball))) 'superman-trim-string))
	 (width (or (cdr (assoc "width" (cdr ball))) (list 23)))
	 (args (cdr (assoc "args" (cdr ball))))
	 ;; (preserve-props (assoc "preserve" (cdr ball)))
	 (trim-args (nconc width args))
	 (trimmed-string
	  (concat
	   ;; (make-string superman-column-separator (string-to-char " "))
	   "  " ;; column sep
	   (apply fun raw-string trim-args)))
	 (len (length trimmed-string))
	 (face (or (cadr (assoc "face" ball))
		   (unless (markerp raw-string)
		     (get-text-property 0 'face raw-string))))
	 (sort-key (get-text-property superman-column-separator 'sort-key trimmed-string)))
    ;; remove all existing text-properties
    ;; (unless preserve-props
    ;; (set-text-properties 0 len nil trimmed-string))
    (when sort-key (put-text-property 0 len 'sort-key sort-key trimmed-string))
    ;; FIXME: this needs documentation, i.e. that a ball ("face" "no-face") will avoid the face
    (unless (or no-face (stringp face)) 
      (when (and (not (facep face)) (functionp face)) ;; apply function to get face
	(setq face (funcall
		    face
		    (replace-regexp-in-string
		     "^[ \t\n]+\\|[ \t\n]+$" ""
		     raw-string))))
      (when (or (facep face) (listp face))
	(put-text-property 0 len 'face face trimmed-string)))
    ;; (put-text-property 0 (length trimmed-string) 'face face trimmed-string)))
    trimmed-string))

(defun superman-format-thing (thing balls &optional no-face)
  "Format THING according to balls. THING is either
a marker which points to a header in a buffer 
or an association list. 

Returns the formatted string with text-properties."
  (let ((item "")
	ilen
	(column-widths (list 0))
	(marker (cond ((markerp thing) thing)
		      ((cdr (assoc "marker" (cadr thing))))))
	(copy-balls balls))
    ;; loop columns
    (while copy-balls
      (let* ((b (car copy-balls))
	     (bstring (superman-play-ball thing b no-face)))
	(setq column-widths (append column-widths (list (length bstring))))
	(setq item (concat item bstring)))
      (setq copy-balls (cdr copy-balls)))
    (setq ilen (length item))
    ;; marker in index file
    (when marker
      (put-text-property 0 ilen 'org-hd-marker marker item)
      (put-text-property 0 ilen 'superman-item-marker marker item))
    ;; add balls for redo
    ;; not done (balls are saved in category instead)
    ;; (put-text-property 0 ilen 'balls balls item)
    ;; text property: columns
    (put-text-property 0 ilen 'columns column-widths item)
    item))


;;}}}
;;{{{ Moving (items) around

(defun superman-next-cat ()
  (interactive)
  (goto-char (or (next-single-property-change (point-at-eol)
					      'cat)
		 (point-max))))

(defun superman-cat-point (&optional pos)
  "Return point where current category defines text-properties."
  (if (get-text-property (or pos (point)) 'cat)
      (point)
    (let ((cat-head (previous-single-property-change (or pos (point)) 'cat)))
      (when cat-head
	(save-excursion
	  (goto-char cat-head)
	  (beginning-of-line)
	  (point))))))

(defun superman-subcat-point (&optional pos)
  "Return point where current subcategory defines text-properties."
  (let ((subcat-head (previous-single-property-change (or pos (point)) 'subcat)))
    (when subcat-head
      (save-excursion
	(goto-char subcat-head)
	(beginning-of-line)
	(point)))))

(defun superman-previous-cat ()
  "Move point to start of category"
  (interactive)
  (goto-char (or (superman-cat-point (max 1 (- (point-at-bol) 1))) (point-min))))

(defun superman-swap-balls (list pos)
  "Exchange list element at pos with that at pos + 1.
Starts counting at 0, thus

 (superman-swap-balls (list 1 2 3 4 5) 0)

yields

 (2 1 3 4 5).
If pos is negative place the first element at the
end of the list.

If pos exceeds the length of the list place last element at the
beginning of the list."
  (let ((len (length list))
	(newlist (copy-sequence list)))
    (cond ((< pos 0)
	   (nconc (cdr newlist) (list (car newlist))))
	  ((> pos (- len 2))
	   (nconc (list (nth (- len 1) newlist)) (butlast newlist 1)))
	  (t
	   (nconc (butlast newlist (- len pos))
	     (list (nth (+ pos 1) newlist))
	     (list (nth pos newlist))
	     (nthcdr (+ pos 2) newlist))))))
	     

(defun superman-change-balls (new-balls)
  "Exchange balls (column definitions) in this section."
  (let ((buffer-read-only nil)
	(cat-point (superman-cat-point)))
    (if cat-point
	(save-excursion
	  (goto-char cat-point)
	  (put-text-property (point-at-bol) (point-at-eol) 'balls new-balls))
      (message "Point is not inside a section"))))

(defun superman-compute-columns-start (&optional col-width)
  "Compute column start points from a given list of column widths COL-WIDTH.
If COL-WIDTH is nil use text-property columns at the beginning of the
current line."
  (let* ((cols (get-text-property (point-at-bol) 'columns))
	 (n (- (length cols) 1))
	 (cumcols (list 0))
	 (i 1))
    (while (< i n)
      (setq cumcols (nconc cumcols (list (+ (nth i cols) (nth (- i 1) cumcols)))))
      (setq i (+ i 1)))
    cumcols))

(defun superman-next-ball (&optional arg)
  "Move to ARGth next column."
  (interactive "p")
  (if (get-text-property (point-at-bol) 'columns)
      (let* ((current (nth 2 (superman-ball-dimensions)))
	     (colstart (superman-compute-columns-start))
	     (j (+ current arg)))
	;; (min (- (length colstart) 1)
	;; (+ current arg))))
	(when (> j (- (length colstart) 1)) (setq j 0))
	(when (< j 0) (setq j (- (length colstart) 1)))
	(beginning-of-line)
	(forward-char (+ superman-column-separator ;; offset for whole line
			 (nth j colstart))))
    (forward-char 1)))

(defun superman-previous-ball ()
  "Move to previous column."
  (interactive)
  (if (get-text-property (point-at-bol) 'columns)
      (superman-next-ball -1)
	(backward-char 1)))
    
(defun superman-one-right (&optional left)
  "Move column to the right."
  (interactive "P")
  (if (get-text-property (point-at-bol) 'column-names)
      ;; swap columns
      (let* ((curcol (nth 2 (superman-ball-dimensions)))
	     (balls (get-text-property (superman-cat-point) 'balls))
	     (buffer-read-only nil)
	     (new-balls
	      (superman-swap-balls
	       balls (if left (- curcol 1) curcol)))
	     (beg (previous-single-property-change (point-at-bol) 'cat))
	     (end (or (next-single-property-change (point-at-eol) 'cat) (point-max))))
	;; (message (concat "Len!: " (int-to-string (length superman-document-balls))))
	(save-excursion
	  (superman-change-balls new-balls)
	  (superman-refresh-cat new-balls))
	(if left (superman-next-ball (- curcol 1))
	  (superman-next-ball (+ curcol 1))))
    ;; swap projects
    (if left
	(superman-previous-project)
      (superman-next-project))))
    
(defun superman-one-left ()
  "Move column to the left."
  (interactive)
  (superman-one-right 1))

(defun superman-current-column-name ()
  (let* ((dim (superman-ball-dimensions))
	 (names (get-text-property (point-at-bol) 'column-names)))
    (nth (nth 2 dim) names)))

(defun superman-delete-ball (&optional no-confirm)
  "Delete current column. The column will still pop-up when the
view is refreshed, but can be totally removed
by calling `superman-save-balls' subsequently."
  (interactive)
  (let* ((dim (superman-ball-dimensions))
	 (balls (get-text-property (superman-cat-point) 'balls))
	 (buffer-read-only nil)
	 (new-balls (remove-if (lambda (x) t) balls :start (nth 2 dim) :count 1)))
    (save-excursion
      (superman-change-balls new-balls)
      (superman-refresh-cat new-balls))))

(defun superman-new-ball ()
  "Add a new column to show a property of all items in the
current section."
  (interactive)
  (let* ((balls (copy-sequence (get-text-property (superman-cat-point) 'balls)))
	 (buffer-read-only nil)
	 (props (superman-view-property-keys))
	 (common '(("todo" (todo ("width" 6) ("face" superman-get-todo-face)))
		   ("Date" (".*Date" ("fun" superman-trim-date) ("regexp" t) ("face" font-lock-string-face)))
		   ("priority" (priority ("width" 8) ("face" superman-get-priority-face)))
		   ("hdr" (hdr ("width" full) ("face" font-lock-function-name-face)))))
	 (expanded-props (append common (mapcar (lambda (x) (list x)) props) nil nil))
	 (prop-key (completing-read "Property to show in new column (press tab see existing): "
				    expanded-props))
	 (prop (assoc prop-key expanded-props))
	 (types '(("string" . superman-trim-string)
		 ("date" . superman-trim-date)
		 ("filename" . superman-trim-filename)))
	 (new-ball (if (cdr prop)
		       (cadr prop)
		     (let* ((width (max 10 (string-to-number (read-string "Column width: "))))
			    (fun-el (completing-read "Formatting type: " types))
			    (fun (cdr (assoc fun-el types))))
		       `(,(car prop) ("width" ,width) ("fun" ,fun)))))
	 (new-balls (add-to-list 'balls new-ball)))
    (superman-change-balls new-balls)
    (superman-save-balls)))


(defun superman-tab (&optional arg)
  "Move to next button in the header and call `org-cycle' in the body of the project view."
  (interactive)
  (if superman-git-mode
      (progn
	(goto-char (next-single-property-change (point-min) 'cat))
	(superman-cycle-git-display))
    (cond ((not (previous-single-property-change (point-at-eol) 'cat))
	   (let ((current (get-text-property (point) 'superman-header-marker))
		 (mark (next-single-property-change (point) 'superman-header-marker)))
	     (if mark (progn (goto-char mark)
			     (when current
			       (goto-char (next-single-property-change (point) 'superman-header-marker))))
	       (goto-char (next-single-property-change (point) 'cat)))))
	  (t (org-cycle arg)))))

(defun superman-shifttab (&optional arg)
  "Move to previous button in the header and call `org-shifttab' in the body of the project view."
  (interactive)
  (cond
   ((eq (point) (point-min))
    (org-shifttab arg))
   ((not (previous-single-property-change (point-at-eol) 'cat))
    (let ((current (get-text-property (point) 'superman-header-marker))
	  (mark (previous-single-property-change
		 (point) 'superman-header-marker)))
      (if mark
	  (progn (goto-char (- mark 1))
		 (when current (goto-char  (previous-single-property-change
					    (point) 'superman-header-marker))))
	(goto-char (point-min)))))
   (t (org-shifttab arg))))


;; (defun superman-goto-outline (&optional backward goto upper lower)
  ;; "Find and move to next (previous if BACKWARD is non-nil) outline level
 ;; which is less than or equal to UPPER and greater than LOWER.
;; UPPER defaults to `superman-item-level'.
;; If GOTO is nil do not move. Value is the point of
;; the matching outline level or nil if there is none."
  ;; (let* ((found nil)
	 ;; (lower-bound (or lower 0))
	 ;; (upper-bound (or upper
			  ;; (nth 0 (org-heading-components))
			  ;; superman-item-level)))
    ;; (save-excursion
      ;; (while (and (not found)
		  ;; (if backward
		      ;; (outline-previous-heading)
		    ;; (outline-next-heading)))
	;; (let ((current-level (nth 0 (org-heading-components))))
	  ;; (setq found (and (<= current-level upper-bound)
			   ;; (> current-level lower-bound))))
	;; (if found (setq found (point-at-bol)))))
    ;; (if (and goto found) 
	;; (goto-char found)
      ;; found)))


(defun superman-one-up (&optional arg down)
  "Move item or section in project view up or down. As a side effect
move the corresonding heading in the index buffer, thus making
movements permant."
  (interactive "p")
  (let ((marker (org-get-at-bol 'org-hd-marker))
	(catp (org-get-at-bol 'cat))
	next-level
	next-point
	current-level
	(f (if down
	       'outline-next-heading
	     'outline-previous-heading))
	(first 1)
	(n (or arg 1)))
    (if (not (or catp marker))
	(error "Point is not on an item  or a category" )
      (with-current-buffer (marker-buffer marker)
	(widen)
	(goto-char marker)
	;; now at heading in index buffer
	(setq current-level (nth 0 (org-heading-components)))
	;; set a mark 
	(put-text-property (point-at-bol) (point-at-eol) 'current-item 1)
	(put-text-property (point-at-bol) (point-at-eol) 'point-here 1)
	;; identify new place
	(while (> n 0)
	  (setq next-point (funcall f))
	  (setq next-level
		(and next-point
		     (nth 0 (org-heading-components))))
	  (cond ((not next-point);; at last heading
		 (if first
		     (error "Cannot move further")
		   (setq n 0)))
		((> next-level current-level) ;; skip lower levels
		 (superman-skip-headings current-level (not down))
		 (if (= (point) next-point) ;; no further appropriate level
		     (error "Cannot move further")
		   (setq n (- n 1))
		   ))
		((= next-level current-level) ;; same level
		 (setq n (- n 1)))
		((< next-level current-level) ;; higher level
		 ;; moving outside section
		 (if (save-excursion 
		       (if down
			   ;; check if current heading is appropriate
			   (superman-skip-headings current-level nil)
			 ;; check if there are further appropriate headings 
			 (and (funcall f)
			      (superman-skip-headings current-level t))))
		     (if (and (not down) first)
			 nil ;; skip first higher level 
		       (setq n (- n 1))) ;; there is another appropriate level
		   (if first
		       (error "Cannot move further")
		     (setq n 0)))));; no further heading
	  (setq first nil))
	;; set another mark and remove the first
	(put-text-property (point-at-bol) (point-at-eol) 'next-item 1)
	(if down
	    (goto-char (previous-single-property-change (point) 'current-item))
	  (goto-char (next-single-property-change (point) 'current-item)))
	(remove-list-of-text-properties (point-at-bol) (point-at-eol) '(current-item))
	;; finally cut current heading 
	(org-cut-subtree)
	;; and yank it at new place
	(if down
	    (goto-char (next-single-property-change (point) 'next-item))
	  (goto-char (previous-single-property-change (point) 'next-item)))
	(remove-list-of-text-properties (point-at-bol) (point-at-eol) '(next-item))
	(if (not down)
	    (beginning-of-line)
	  (if (< next-level current-level)
	      (if (boundp 'org-end-of-meta-data)
		  (org-end-of-meta-data)
		(org-end-of-meta-data 't))
	    (org-end-of-subtree t t))
	  (when (not (eq (point-at-bol) (point))) (insert "\n")))
	(yank)
	(when (and (not down)
		   (not (eq (point-at-bol) (point))))
	  (insert "\n"))
	(delete-blank-lines))
      ;; back in view mode
      (superman-redo)
      (if catp
	  (progn (goto-char (point-min))
		 (re-search-forward (concat "*+[ ]+" catp) nil t)
		 (beginning-of-line))
	(goto-char
	 (or (next-single-property-change (point-min) 'point-here)
	     (point-min)))))))

(defun superman-skip-headings (level backward)
  "Skip forward across headings with higher level then LEVEL and
across headings indicated as either hidden or freetext. 
Leave point at first appropriate heading or return nil if there is none.
If BACKWARD is non-nil move backward."
  (if (not (org-at-heading-p))
      (error "Not at heading"))
  (let (inappropriate
	(here (point)))
    (while (and (setq inappropriate
		      (or
		       (> (nth 0 (org-heading-components)) level)
		       (superman-get-property (point) "freetext")
		       (superman-get-property (point) "hidden")))
		(if backward (outline-previous-heading)
		  (outline-next-heading))))
    (if (or inappropriate
	    (not (org-at-heading-p)))
	;;move back
	(goto-char here)
      (point))))
      


(defun superman-one-down (&optional arg)
  (interactive "P")
  (superman-one-up arg t))

(defun superman-cut ()
  (interactive)
  (if superman-view-mode
      (let ((marker (org-get-at-bol 'org-hd-marker))
	    (buffer-read-only nil)
	    (kill-whole-line t))
	(when marker
	  (beginning-of-line)
	  (kill-line)
	  (with-current-buffer
	      (marker-buffer marker)
	    (widen)
	    (show-all)
	    (goto-char marker)
	    (org-cut-subtree))))
    (message "can only cut in superman-view-mode")))

(defun superman-paste ()
  (interactive)
  (if superman-view-mode
      (let ((marker (org-get-at-bol 'org-hd-marker)))
	(when marker
	  (with-current-buffer
	      (marker-buffer marker)
	    (widen)
	    (show-all)
	    (goto-char marker)
	    (org-paste-subtree)))
	(superman-redo))
  (message "can only paste in superman-view-mode")))

;;}}}
;;{{{ Edit and delete items

(defun superman-view-item ()
  (interactive)
  (superman-view-edit-item t))

(defun superman-view-edit-item (&optional read-only marker)
  "Put the item at point into capture mode. If READ-ONLY is non-nil
disable editing."
  (interactive)
  (let* ((obuf (current-buffer))
	 (superman-setup-scene-hook
	  (if read-only
	      (append '(superman-view-item-mode)
		      'superman-setup-scene-hook)
	    superman-setup-scene-hook))
	 (marker
	  (or marker
	      (get-text-property (point-at-bol) 'org-hd-marker)
	      (get-text-property (point-at-bol) 'superman-e-marker)))
	 (catp (get-text-property (point-at-bol) 'cat))
	 (columnsp (get-text-property (point-at-bol) 'column-names))
	 ;; (E-buf (generate-new-buffer-name "*Edit by SuperMan*"))
	 (scene (current-window-configuration))
	 ;; (all-props
	 ;; (if (or (not marker) catp) nil
	 ;; (ignore-errors (superman-view-property-keys))))
	 ;; range
	 ;; (title)
	 (cat-point (superman-cat-point))
	 (free
	  (when cat-point
	    (superman-get-property
	     (get-text-property cat-point 'org-hd-marker) "freetext")))
	 ;; (balls (when cat-point
	 ;; (get-text-property (superman-cat-point) 'balls)))
	 ;; prop
	 ;; edit-point
	 ;; used-props
	 )
    (when (and free (not marker))
      (setq marker (get-text-property cat-point 'org-hd-marker)))
    (when columnsp
      (setq marker (get-text-property (superman-cat-point) 'org-hd-marker)))
    (if (not marker)
	(message "Nothing editable here")
      ;; add properties defined by balls to all-props
      ;; (unless free
      ;; (while balls
      ;; (when (stringp (setq prop (caar balls)))
      ;; (add-to-list 'all-props prop))
      ;; (setq balls (cdr balls))))
      (set-buffer (marker-buffer marker))
      ;; do not edit dynamic buffers
      (unless (buffer-file-name)
	(setq read-only t))
      (setq title
	    (superman-make-button
	     (concat "Superman " (if read-only "view item (read-only)"  "edit") " mode")
	     nil 'superman-capture-button-face))
      (goto-char marker)
      (widen)
      (show-all)
      (cond
       ((org-at-heading-p)
	(org-narrow-to-subtree)
	(setq item (buffer-substring (point-min) (point-max)))
	(widen))
       (t (error "dont know what to copy")))
      (superman-capture-whatever
       marker
       (superman-make-button
	(concat  "Superman " (if read-only "views" "edits") " item")
	nil 'superman-capture-button-face)
       0 ;; level 0 because we are pasting a heading in
       item
       nil 'edit scene read-only nil nil nil))))

(defun superman-view-delete-entry (&optional dont-prompt dont-kill-line)
  "Delete entry at point. Prompt user unless DONT-PROMT is non-nil.

If the entry is found in a buffer which is not associated with a
file-name, e.g., a temporary *Git control* buffer, then the entry is
removed without prompt, no matter the value of DONT-PROMT.

Kill the line of the view-buffer unless DONT-KILL-LINE is non-nil.

If there is a file-name property at the entry, prompt
the user if this should be removed as well.

The value is non-nil unless the user regretted and the entry is not deleted.
"
  (interactive)
  (cond (;; at category heading
	 (get-text-property (point-at-bol) 'cat)
	 (message "cannot (not yet) delete section in this way"))
	;; at sub-cat heading
	((get-text-property (point-at-bol) 'subcat)
	 (message "cannot (not yet) delete sub-section in this way"))
	;; inside header
	((not (previous-single-property-change (point-at-bol) 'cat))
	 (if (get-text-property (point-at-bol) 'superman-e-marker)
	     (superman-view-edit-item)))
	;; inside column names
	((get-text-property (point-at-bol) 'column-names)
	 (let ((cname (superman-current-column-name)))
	   (superman-delete-ball)
	   (if (yes-or-no-p (concat "Delete column " cname " permanently? "))
	       (superman-save-balls)
	     (message (concat "Deleted " cname "; type 'R' to put it back")))))
	;; inside section
	(t
	 (let* ((marker (org-get-at-bol 'org-hd-marker))
		(scene (current-window-configuration))
		(file (superman-filename-at-point t))
		(git-p (when (and file (file-exists-p file)
				  (superman-git-p (file-name-directory file)))
			 (let* ((status (superman-git-XY-status file))
				(xy (concat (nth 1 status) (nth 2 status))))
			   (if (or (string= xy "!!") (string= xy "??")) nil t))))
		(regret nil))
	   (unless dont-prompt
	     (superman-view-index)
	     (when (buffer-file-name)
	       (org-narrow-to-subtree)
	       (setq regret (not (yes-or-no-p "Delete this entry? ")))
	       (widen)))
	   (set-window-configuration scene)
	   (unless (or regret dont-kill-line)
	     (let ((buffer-read-only nil))
	       (beginning-of-line)
	       (kill-line)))
	   (unless regret
	     (when marker
	       (save-excursion
		 (org-with-point-at marker (org-cut-subtree))))
	     (when (and file (file-exists-p file))
	       (unless (setq regret
			     (not (yes-or-no-p
				   (concat (if git-p "Call git rm " "Delete file ")
					   (file-name-nondirectory file) "? "))))
		 (if git-p
		     (superman-run-cmd (concat
					"cd "
					(file-name-directory file)
					";"
					superman-cmd-git " rm -f "
					(file-name-nondirectory file))
				       "*Superman-returns*")
		   (when (file-exists-p file)
		     (delete-file file))))))
	   ;; (superman-view-redo-line))
	   (not regret)))))


(defun superman-view-delete-marked (&optional dont-prompt)
  (interactive)
  (let ((beg (previous-single-property-change (point) 'cat))
	(buffer-read-only nil)
	(end (or (next-single-property-change (point) 'cat)
		 (point-max))))
    (narrow-to-region beg end)
    (when (yes-or-no-p "Delete all the marked entries in this section? ")
      (superman-loop 'superman-view-delete-entry '(t t) beg end 'marked)
      (widen)
      (superman-redo))))

(defun superman-view-help ()
  "Raise help window"
  (interactive)
  (let ((h-buf (get-buffer "*Superman-view-help*")))
    (if h-buf (pop-to-buffer h-buf)
      (pop-to-buffer "*Superman-view-help*")
      (let ((buffer-read-only nil))
	(insert "Key-bindings: (q: close this help window)\n\n"
		"RETURN: visit file or item\n"
		"e: edit item\n"
		"v: view item\n"
		"N: Capture new item\n"
		"t: toggle todo status"
		"i: visit index file\n\n"
		"f: file-list\n"
		"g: git repository\n"))
      (setq buffer-read-only t))
      (other-window 1)))

(defun superman-view-quit-help ()
  "Close help window."
  (interactive)
  (let ((h-win
	 (get-buffer-window "*Superman-view-help*" (selected-frame))))
    (when h-win (delete-window h-win))))

;;}}}
;;{{{ Switch between projects

(defvar superman-project-history nil
  "List of projects that were previously selected
 in the current emacs session.")

(defun superman-next-project (&optional backwards)
  "Switch to next project in `superman-project-history'"
  (interactive "P")
  (let* ((next (if backwards
		   (car (reverse superman-project-history))
		 (cadr superman-project-history))))
	 ;; (phist
	  ;; (member (car superman-current-project)
		  ;; (if backwards
		      ;; (reverse superman-project-history)
		    ;; superman-project-history)))
	 ;; (next (cadr phist)))
    (when next
    (superman-switch-to-project
     (assoc
      next
      superman-project-alist)))))

(defun superman-previous-project ()
  "Switch to previous project in `superman-project-history'."
  (interactive)
  (superman-next-project t))

;;}}}
;;{{{ Redo and refresh 

(defun superman-redo ()
  "Refresh project view."
  (interactive)
  (let ((curline (progn
		   (beginning-of-line)
		   (count-lines 1 (point))))
	cmd)
    (setq cmd (get-text-property (point-min) 'redo-cmd))
    (cond ((commandp cmd) (funcall cmd))
	  (t (eval cmd)))
    (goto-char (point-min))
    (when (> curline 0)
      (forward-line (+ 1 curline)))
    (let ((buffer-read-only nil))
      (put-text-property (point-min)
			 (1+ (point-min)) 'redo-cmd cmd))))

(defun superman-refresh-cat (new-balls)
  "Refresh view of all lines in current category inclusive column names."
  (interactive)
  (let ((start (superman-cat-point))
	(kill-whole-line t)
	(end (or (next-single-property-change (point) 'cat) (point-max))))
    (if (not start)
	(message "Point is not in category.")
      (superman-loop 'superman-view-redo-line nil start end nil)
      (goto-char (next-single-property-change start 'names))
      (beginning-of-line)
      (kill-line)
      (insert (superman-column-names new-balls) "\n"))))

(defun superman-view-redo-line (&optional marker balls)
  (interactive)
  (let* ((buffer-read-only nil)
	 (marker (or marker (get-text-property (point-at-bol) 'org-hd-marker)))
	 (balls (or balls (get-text-property (superman-cat-point) 'balls)))
	 (props (text-properties-at (point-at-bol))))
    (when (and marker (not (get-text-property (point-at-bol) 'cat))
	       (not (get-text-property (point-at-bol) 'subcat)))
      (beginning-of-line)
      (let ((newline
	     (org-with-point-at marker
		 (superman-format-thing marker balls)))
	    (beg (previous-single-property-change (point-at-eol) 'org-hd-marker))
	    (end (or (next-single-property-change (point) 'org-hd-marker)
		     (next-single-property-change (point) 'tail))))
	(delete-region beg end)
	(insert newline)
	(set-text-properties (point-at-bol) (+ (point-at-bol) 1) props)
	(beginning-of-line)
	(while (or (org-activate-bracket-links (point-at-eol)) (org-activate-plain-links (point-at-eol)))
	  (add-text-properties
	   (match-beginning 0) (match-end 0)
	   '(face org-link)))
	(beginning-of-line)))))

;;}}}
;;{{{ helper functions

(defun superman-view-property-keys ()
  "Get a list of all property keys in current section"
  (let ((cat-point (superman-cat-point))
	keys)
    (when cat-point
      (save-excursion
	(org-with-point-at (get-text-property cat-point 'org-hd-marker)
	  (save-restriction
	    (widen)
	    (show-all)
	    (org-narrow-to-subtree)
	    ;; do not show properties of the section
	    ;; heading
	    (outline-next-heading)
	    (narrow-to-region (point) (point-max))
	    (setq keys (superman-property-keys))
	    (widen)
	    keys))))))

(defun superman-view-toggle-todo ()
  (interactive)
  (let ((marker (get-text-property (point-at-bol) 'superman-item-marker)))
	;;(marker (org-get-at-bol 'org-hd-marker)))
    (when marker
      (save-excursion
	(org-with-point-at marker
	  (org-todo)
	  (save-buffer)))
      (superman-view-redo-line marker))))


(defun superman-view-priority-up ()
  (interactive)
  (let ((marker (get-text-property (point-at-bol) 'superman-item-marker)))
	;; (marker (org-get-at-bol 'org-hd-marker)))
    (when marker
      (save-excursion
	(org-with-point-at marker
	  (org-priority-up)
	  (save-buffer)))
      (superman-view-redo-line marker))))

(defun superman-view-priority-down ()
  (interactive)
  (let ((marker (get-text-property (point-at-bol) 'superman-item-marker)))
	;; (marker (org-get-at-bol 'org-hd-marker)))
    (when marker
      (save-excursion
	(org-with-point-at marker
	  (org-priority-down)
	  (save-buffer)))
      (superman-view-redo-line marker))))

(defun superman-next-entry ()
  (interactive)
  (forward-line 1))
  ;; (superman-choose-entry))

(defun superman-choose-entry ()
  (interactive)
  (let ((choice (get-text-property (point-at-bol) 'superman-choice)))
    (when choice
      (cond ((functionp choice) (funcall choice))))))  

(defun superman-previous-entry ()
  (interactive)
  (forward-line -1))
;; (superman-choose-entry))

(defun superman-view-filter ()
  (interactive)
  (if (fboundp 'helm-occur)
      (helm-occur)
    (occur)))

(defun superman-view-dired ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (dir 
	  (or (if m (file-name-directory (org-link-display-format (superman-get-property m "filename"))))
	      (get-text-property (point-min) 'git-dir)
	      (get-text-property (point-min) 'dir)
	      default-directory)))
    (find-file dir)))

(defun superman-hot-return ()
  "Action to be run when user hits return."
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (choice (org-get-at-bol 'superman-choice))
	 (b (superman-current-cat))
	 f pos)
    (if (and choice (functionp choice))
	(funcall choice)
      (if (not m)
	  (save-excursion
	    (beginning-of-line)
	    (cond ((looking-at "Others")
		   (superman-capture-others
		    (assoc (get-text-property (point-min) 'nickname)
			   superman-project-alist))
		   (superman-redo))
		  (t (error "Nothing to do here"))))
	(when (and (not (marker-buffer m))
		   (setq f (get-text-property (point-at-bol) 'superman-project-file)
			 pos (get-text-property (point-at-bol) 'superman-project-file-marker)))
	  (if (file-directory-p f)
	      (superman-view-directory f)
	    (find-file f))
	  (goto-char pos)
	  (setq m (point-marker)))
	(cond (superman-mode
	       (superman-return))
	      ((org-with-point-at m
		 (if (re-search-forward
		      org-any-link-re
		      (save-excursion
			(widen)
			(outline-end-of-subtree)
			(point))
		      t)
		     (progn
		       (superman-open-at-point)
		       t)
		   nil)))
	      (t
	       (switch-to-buffer (marker-buffer m))
	       (widen)
	       (show-all)
	       ;; (org-narrow-to-subtree)
	       (goto-char (marker-position m))))))))

(defun superman-open-at-point ()
  (interactive)
  "Wrapper for org-open-at-point to fix a (temporary?) org-feature
 which disables opening links in properties."
  (let ((thing
	 (if (save-excursion
	       (beginning-of-line)
	       (looking-at org-property-re))
	     (match-string 3)
	   (error "Not in property block"))))
    (with-temp-buffer
      (insert thing)
      (backward-char 1)
      (org-open-at-point))))
  

(defun superman-view-index ()
  "Switch to index buffer and show the entry at point."
  (interactive)
  (let* ((pom (cond ((org-get-at-bol 'org-hd-marker))
		    ((org-get-at-bol 'column-names)
		     (get-text-property (superman-cat-point)
					'org-hd-marker))
		    ((org-get-at-bol 'superman-e-marker))
		    ((ignore-errors
		       (org-back-to-heading))
		     (org-get-at-bol 'org-hd-marker))
		    ((outline-next-heading)
		     (org-get-at-bol 'org-hd-marker))))
	 (ibuf (or (and pom (marker-buffer pom))
		   (get-file-buffer
		    (get-text-property (point-min) 'index))
		   (find-file
		    (get-text-property (point-min) 'index))))
	 (iwin (when ibuf (get-buffer-window ibuf nil))))
    (when ibuf
      (if iwin (select-window iwin)
	;; FIXME this should be customizable
	(split-window-vertically)
	(other-window 1)
	(switch-to-buffer ibuf))
      (show-all)
      (widen)
      (when pom (goto-char pom)))))

(defun superman-view-save-index-buffer ()
  (save-excursion
    (let ((ibuf (get-file-buffer
		 (get-text-property (point-min) 'index))))
      (when ibuf (set-buffer ibuf)
	    (save-buffer)))))

;;}}}
;;{{{ property and filename at point

(defun superman-property-at-point (prop noerror)
  (interactive)
  (let* ((pom (cond
	       ((org-get-at-bol 'org-hd-marker))
	       ((point))
	       (t (error "Don't know where to look for property."))))
	 (propval
	  (superman-get-property pom prop nil)))
    propval))

(defun superman-filename-at-point (&optional noerror)
  "If property FileName exists at point return its value."
  (let* ((file-or-link
	  (cond ((superman-property-at-point "filename"
					      noerror))
		;; e.g. for superman-git-log-mode
		((get-text-property (point-min) 'filename)))))
    (if (not (stringp file-or-link))
	(unless noerror
	  (error "No proper(ty) FileName at point."))
      (org-link-display-format file-or-link))))

(defun superman-filename-with-pom (&optional noerror)
  "Return property `superman-filename-at-point' at point,
if it exists and add text-property org-hd-marker."
  (let* ((file-or-link
	  (superman-property-at-point "filename" noerror))
	 filename)
    (if (not (stringp file-or-link))
	(unless noerror
	  (error "No proper(ty) FileName at point."))
      (setq filename (org-link-display-format file-or-link))
      (put-text-property 0 (length filename) 'org-hd-marker
			 (org-get-at-bol 'org-hd-marker) filename)
      filename)))
;;}}}
;;{{{ View-mode and hot-keys

(defvar superman-view-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
   
(define-minor-mode superman-view-mode
     "Toggle superman project view mode.
With argument ARG turn superman-view-mode on if ARG is positive, otherwise
turn it off.
                   
Enabling superman-view mode electrifies the column view for documents
for git and other actions like commit, history search and pretty log-view."
     :lighter " *S*-View"
     :group 'org
     :keymap 'superman-view-mode-map)

(defun superman-view-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-view-mode t))


(defun superman-view-second-link ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (b (superman-current-cat))
	 f)
    (if (not m)
	(error "Nothing to do here")
      (org-with-point-at m
	(cond (superman-mode
	       (superman-return))
	      ((re-search-forward org-any-link-re nil t)
	       (re-search-forward org-any-link-re nil t)
	       (superman-open-at-point))
	      (t
	       (widen)
	       (show-all)
	       (org-narrow-to-subtree)
	       (switch-to-buffer (marker-buffer m))))))))
	      ;; ((superman-view-index)
	       ;; (org-narrow-to-subtree)))))))


(define-key superman-view-mode-map [return] 'superman-hot-return)
(define-key superman-view-mode-map [(meta left)] 'superman-one-left)
(define-key superman-view-mode-map [(meta right)] 'superman-one-right)
(define-key superman-view-mode-map [(meta up)] 'superman-one-up)
(define-key superman-view-mode-map [(meta down)] 'superman-one-down)
(define-key superman-view-mode-map [(meta return)] 'superman-view-second-link)

(define-key superman-view-mode-map [(right)] 'superman-next-ball)
(define-key superman-view-mode-map [(left)] 'superman-previous-ball)
(define-key superman-view-mode-map [(control down)] 'superman-next-cat)
(define-key superman-view-mode-map [(control up)] 'superman-previous-cat)
(define-key superman-view-mode-map [(control n)] 'superman-next-cat)
(define-key superman-view-mode-map [(control p)] 'superman-previous-cat)
(define-key superman-view-mode-map "n" 'superman-next-entry)
(define-key superman-view-mode-map "p" 'superman-previous-entry)

(define-key superman-view-mode-map [(tab)] 'superman-tab)
(define-key superman-view-mode-map [(shift tab)] 'superman-shifttab)
(define-key superman-view-mode-map [S-iso-lefttab] 'superman-shifttab)
(define-key superman-view-mode-map [(up)] 'superman-previous-entry)
(define-key superman-view-mode-map [(down)] 'superman-next-entry)
(define-key superman-view-mode-map [(shift up)] 'superman-view-priority-up)
(define-key superman-view-mode-map [(shift down)] 'superman-view-priority-down)
(define-key superman-view-mode-map "i" 'superman-view-index)
(define-key superman-view-mode-map "I" 'superman-view-invert-marks)
(define-key superman-view-mode-map "e" 'superman-view-edit-item)
(define-key superman-view-mode-map "D" 'superman-view-dired)
(define-key superman-view-mode-map "/" 'superman-view-filter)
(define-key superman-view-mode-map "f" 'superman-view-file-list)
(define-key superman-view-mode-map "m" 'superman-toggle-mark)
(define-key superman-view-mode-map "M" 'superman-view-mark-all)
(define-key superman-view-mode-map "r" 'superman-view-redo-line)
(define-key superman-view-mode-map "q" 'superman-view-quit-help)
(define-key superman-view-mode-map "h" 'superman-view-help)
(define-key superman-view-mode-map "H" 'superman-ual)
(define-key superman-view-mode-map "t" 'superman-view-toggle-todo)
(define-key superman-view-mode-map "x" 'superman-view-delete-entry)
(define-key superman-view-mode-map "-" 'superman-view-delete-entry)
(define-key superman-view-mode-map "+" 'superman-capture-item)
(define-key superman-view-mode-map "X" 'superman-view-delete-marked)
(define-key superman-view-mode-map "N" 'superman-new-item)
;; (define-key superman-view-mode-map "N" 'superman-capture-thing)
(define-key superman-view-mode-map "Q" 'superman-unison)
(define-key superman-view-mode-map "R" 'superman-redo)
(define-key superman-view-mode-map "S" 'superman-sort-section)
(define-key superman-view-mode-map "V" 'superman-change-view)
(define-key superman-view-mode-map "!" 'superman-goto-shell)
(define-key superman-view-mode-map "?" 'supermanual)

(define-key superman-view-mode-map "Bn" 'superman-new-ball)
(define-key superman-view-mode-map "Bx" 'superman-delete-ball)
(define-key superman-view-mode-map "Bs" 'superman-save-balls)

;; view context
(define-key superman-view-mode-map "V" 'superman-toggle-context-view)
(define-key superman-view-mode-map "v" 'superman-view-item)
;; Git control
(define-key superman-view-mode-map "g" 'superman-git-display)
(define-key superman-view-mode-map "G " 'superman-git-last-log-file)
(define-key superman-view-mode-map "Ga" 'superman-git-annotate)
(define-key superman-view-mode-map "Gx" 'superman-git-delete-file)
(define-key superman-view-mode-map "Gc" 'superman-git-commit-file)
(define-key superman-view-mode-map "GC" 'superman-git-commit-marked)
(define-key superman-view-mode-map "Gd" 'superman-git-diff-file)
(define-key superman-view-mode-map "Gg" 'superman-git-grep)
(define-key superman-view-mode-map "Gh" 'superman-git-history)
(define-key superman-view-mode-map "GI" 'superman-git-init)
(define-key superman-view-mode-map "Gl" 'superman-git-log-file)
(define-key superman-view-mode-map "GL" 'superman-git-log-decoration-only-file)
(define-key superman-view-mode-map "GP" 'superman-git-push)
(define-key superman-view-mode-map "Gs" 'superman-git-status)
(define-key superman-view-mode-map "GS" 'superman-git-search-log-of-file)
(define-key superman-view-mode-map "GBs" 'superman-git-checkout-branch)
(define-key superman-view-mode-map "GBn" 'superman-git-new-branch)
;; (define-key superman-view-mode-map "G=" 'superman-git-difftool-file)




(defvar superman-capture-alist nil
  "List to find capture function. Elements have the form
 (\"heading\" function) e.g.  (\"Documents\" superman-capture-document).") 
(setq superman-capture-alist
      '(("Documents" (lambda (&optional pro) (interactive) (superman-capture-document pro nil nil)))
	("GitFiles" (lambda (&optional pro) (interactive) (superman-capture-document pro nil nil)))
	("Notes" (lambda (&optional pro) (interactive) (superman-capture-note pro nil nil)))
	("Tasks" (lambda (&optional pro) (interactive) (superman-capture-task pro nil nil)))
	("Text" (lambda (&optional pro) (interactive) (superman-capture-text pro nil nil)))
	("BibTeX" (lambda (&optional pro) (interactive) (superman-capture-bibtex pro nil nil)))
	("Meetings" (lambda (&optional pro) (interactive) (superman-capture-meeting pro nil nil)))
	("Calendar" (lambda (&optional pro) (interactive) (superman-capture-meeting pro nil nil)))
	("Bookmarks" (lambda (&optional pro) (interactive) (superman-capture-bookmark pro nil nil)))))

(fset 'superman-new-item 'superman-capture-item)
(defun superman-capture-item (&optional pop)
  "Add a new document, note, task or other item to a project. If called
from superman project view, assoc a capture function from `superman-capture-alist'.
If non exists create a new item based on balls and properties in current section. If point is
not in a section prompt for section first."
  (interactive "p")
  (if (and (or (not pop) (= pop 1)) superman-view-mode)
      (let* ((pro (when superman-view-mode (superman-view-current-project t)))
	     (marker (get-text-property (point-at-bol) 'org-hd-marker))
	     (index (superman-get-index pro))
	     (index-buf (cond
			 ((stringp index)
			  (get-file-buffer index))
			 ((bufferp index) index)))
	     (cat (cond ((superman-current-cat))
			((get-text-property (point-min) 'bibtex-file)
			 "BibTeX")
			(index-buf
			 (let ((cats (superman-parse-cats index-buf 1)))
			   (cond ((> (length cats) 1)
				  (completing-read
				   (concat "Choose category for new item in project " (car  pro) ": ")
				   (append cats superman-capture-alist)))
				 ((eq (length cats) 1) (caar cats))
				 (t nil))))))
	     (fun (if cat (assoc cat superman-capture-alist))))
	(if fun (funcall (cadr fun) pro)
	  (superman-capture-thing pro)))
    ;; behave similar to org-capture outside superman-view-mode
    (let ((c-buf (get-buffer "*Superman-Capture*")))
      (if c-buf (pop-to-buffer c-buf)
	(pop-to-buffer "*Superman-Capture*")
	(insert "Press one of the buttons below\n")
	(superman-view-insert-action-buttons
	 superman-default-action-buttons-outside-project
	 t "" t)
	(insert "\n" (superman-make-button
		      "Unison"
		      'superman-capture-unison
		      'superman-capture-button-face
		      "Capture unison" nil 43))
	(superman-view-mode)
	(goto-char (point-min))
	(setq buffer-read-only t)))))

;;}}}
;;{{{ View-item-mode

(defvar superman-view-item-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
(define-key superman-view-item-mode-map  "q" 'superman-quit-scene)
(define-key superman-view-item-mode-map  "\C-c\C-q" 'superman-quit-scene)

(define-minor-mode superman-view-item-mode
  "Toggle superman view item mode.
With argument ARG turn superman-view-item-mode on if ARG is positive, otherwise
turn it off."
  :lighter " *S*-View-item"
  :group 'org
  :keymap 'superman-view-item-mode-map
  (setq buffer-read-only t))

(defun superman-view-item-mode-on ()
  (interactive)
  (superman-view-item-mode t))

(defun superman-view-show-item-context (n)
  (let ((context
	 (org-with-point-at (org-get-at-bol 'org-hd-marker)
	   ;; (org-end-of-meta-data 't)
	   (let* ((beg (point))
		  (end (progn (forward-line n)
			      (skip-chars-backward "\n\t ")
			      (point))))
	     (buffer-substring-no-properties beg end)))))
  (when (length context)
    (put-text-property 0 1 'item-head t context)
    (put-text-property (length context) (- (length context) 1) 'item-tail t context)
    (end-of-line)
    (insert "\n" context))))

(defun superman-view-show-context (&optional n-lines)
  "Show the first N-LINES (default is 5) lines of all items in current section."
  (interactive "p")
  (put-text-property (point-min) (1+ (point-min)) 'view-item-heads t)
  (let* ((start (or (superman-cat-point) (error "Not in category")))
	 (stop (next-single-property-change start 'tail))
	 (buffer-read-only nil)
	 (n-lines (or n-lines 5)))
    (superman-loop 'superman-view-show-item-context (list n-lines)
		   start stop nil)))

(defun superman-toggle-context-view (&optional n-lines)
  "Either insert or remove the context lines
for the item at point or for all items in the current category."
  (interactive "p")
  (let ((buffer-read-only nil))
    (if (get-text-property (point-min) 'view-item-heads)
	(superman-view-remove-context)
      (superman-view-show-context n-lines))))

(defun superman-view-remove-context ()
  (interactive)
  "Remove context by refreshing the whole display"
  (goto-char (or (superman-cat-point) (point-min)))
  (superman-redo))
  ;; (goto-char (or (previous-single-property-change (point) 'item-head) (point-min)))
  ;; (let* ((buffer-read-only nil)
	 ;; (current-head (- (next-single-property-change (point) 'item-head) 1))
	 ;; (current-tail (next-single-property-change (point) 'item-tail)))
    ;; (while (and current-head current-tail)
      ;; (delete-region current-head current-tail)
      ;; (setq current-head
	    ;; (when (next-single-property-change current-head 'item-head) 
	      ;; (- (next-single-property-change current-head 'item-head) 1)))
      ;; (setq current-tail
	    ;; (when current-head
	      ;; (next-single-property-change current-head 'item-tail))))))

;;}}}
;;{{{ easy menu

(require 'easymenu)
(easy-menu-define superman-menu superman-view-mode-map "*S*"
  '("Superman"
    ["Refresh view" superman-redo t]
    ["New project" superman-new-project t]
    ["New item" superman-new-item t]
    ["Edit item" superman-view-edit-item t]
    ["Filter view (occur)" superman-view-filter t]
    ["Toggle todo" superman-view-toggle-todo t]
    ["Mark item" superman-toggle-mark t]
    ["Mark all" superman-view-mark-all t]
    ["Invert mark" superman-view-invert-marks t]
    ["Delete item" superman-view-delete-entry t]
    ["Delete marked" superman-view-delete-marked t]
    ["Move item up" superman-one-up t]
    ["Move item down" superman-one-down t]
    ["Visit index buffer" superman-view-index t]
    ["Dired" superman-view-dired t]
    ["File list" superman-view-file-list t]
    ("Git"
     ["Git history" superman-git-history t]
     ["Git commit file" superman-git-commit-file t]
     ["Git commit marked" superman-git-commit-marked t]
     ["Git delete file" superman-git-delete-file t]
     ["Git log" superman-git-log-file t]
     ["Git log (tagged versions)" superman-git-log-decoration-file t]
     ["Git grep" superman-git-grep t]
     ["Git annotate" superman-git-annotate t]
     ["Git search (only in log mode)" superman-git-search-log-of-file t]
     ["Git file-list" superman-capture-git-section t]
     ["Git push" superman-git-push t]
     ["Git pull" superman-git-pull t]
     ["Git checkout branch" superman-git-checkout-branch t]
     ["Git new branch" superman-git-new-branch t]
     ["Git init" superman-git-init t])
    ("Columns (balls)"
     ["New column" superman-new-ball t]
     ["Delete column" superman-delete-ball t]
     ["Move column left" superman-one-left t]
     ["Move column right" superman-one-right t])
    ["Shell" superman-goto-shell t]
    ["Unison" superman-run-unison t]))

;;}}}

(provide 'superman-views)

;;; superman-views.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End: 
