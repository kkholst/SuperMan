;;; superman-views.el --- Superman views of project contents 

;; Copyright (C) 2012  Klaus Kaehler Holst, Thomas Alexander Gerds

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

(defvar superman-hl-line nil "Set to non-nil in order to
highlight the current line in superman views.")

(defvar superman-view-current-project nil "Buffer local project variable" )
(make-variable-buffer-local 'superman-view-current-project)

(defvar superman-view-marks nil "Marks for items in agenda.")
(make-variable-buffer-local 'superman-view-marks)



(defvar superman-mark-face 'bold  "Face name for marked entries in the view buffers.")

(defvar superman-cats '(("Meetings" . "Date")
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

Column showing a property 

 (\"Prop\" (\"fun\" superman-trim-string) (\"width\" 17) (\"face\" font-lock-function-name-face) (\"name\" \"Prop\") (required nil))

Column showing the header

 (hdr (\"fun\" superman-trim-string) (\"name\" Description))

Column showing the todo-state 

 (todo (\"face\" superman-get-todo-face))

")

(setq superman-finalize-cat-alist
      '(("Documents" superman-document-balls)
	("Data" superman-data-balls)
	("Notes" superman-note-balls)
	("Mail" superman-mail-balls)
	("Tasks" superman-task-balls)
	("Bookmarks" superman-bookmark-balls)
	("Meetings" superman-meeting-balls)))

;; (list "Description" "GitStatus" "LastCommit" "FileName"))
(defun superman-dont-trim (x len) x)
(setq superman-document-balls
      '((hdr ("width" 23) ("face" font-lock-function-name-face) ("name" "Description"))
	("GitStatus" ("width" 10) ("face" superman-get-git-status-face))
	("LastCommit" ("fun" superman-trim-date) ("face" font-lock-string-face))
	("FileName" ("fun" superman-dont-trim))))
(setq superman-meeting-balls
      '((hdr ("width" 23) ("face" font-lock-function-name-face))
	("Date" ("fun" superman-trim-date) ("face" font-lock-string-face))
	("Participants" ("width" 23))))
(setq superman-note-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	("NoteDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" 49) ("face" font-lock-function-name-face))))
(setq superman-data-balls
      '(("CaptureDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" 23) ("face" font-lock-function-name-face))
	("DataFileName" ("fun" superman-dont-trim))))
(setq superman-task-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	("TaskDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" 49) ("face" font-lock-function-name-face))))
(setq superman-bookmark-balls
      '(("BookmarkDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("face" font-lock-function-name-face) ("name" "Description"))
	("Link" ("fun" superman-trim-link) ("width" 48) ("name" "Bookmark"))))
(setq superman-mail-balls
      '((todo ("width" 7) ("face" superman-get-todo-face))
	("EmailDate" ("fun" superman-trim-date) ("width" 13) ("face" font-lock-string-face))
	(hdr ("width" 23) ("face" font-lock-function-name-face))
	("Link" ("fun" superman-trim-bracketed-filename) ("width" 48))))

;;}}}
;;{{{ faces
;; FIXME
;;}}}
;;{{{ Trim stuff and frequently used funs

(defun superman-trim-string (str &rest args)
  "Trim string STR to a given length by either calling substring
or by adding whitespace characters."
  (let* ((slen (length str))
	 (len (car args))
	 (numlen (cond ((integerp len) len)
		       ((stringp len)
			(setq len (string-to-int len))
			(if (< len 1) 13 len))
		       (t 13)))
	 (diff (- numlen slen)))
    (if (> diff 0)
	(concat str (make-string diff (string-to-char " ")))
      (substring str 0 numlen))))

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
	    trimmed-file-name)
	(if (match-end 3)
	    (setq trimmed-file-name
		  (replace-match
		   (superman-trim-string
		    (org-match-string-no-properties 3 file) len)
		   t t file 3))
	  (setq trimmed-file-name
		(org-make-link-string
		 filename
		 (superman-trim-string
		  (file-name-nondirectory filename) len))))
	trimmed-file-name)
    (superman-trim-string file (car args))))

(defun superman-trim-filename (filename &rest args)
  ;;  raw filenames
  (let ((linkname (file-name-nondirectory filename))
	(len (car args)))
    (when (string= linkname "") ;; for directories show the mother
      (setq linkname (file-name-nondirectory (directory-file-name filename))))
    (org-make-link-string
     filename
     (superman-trim-string linkname len))))

(defun superman-get-git-status-face (str)
  (cond ((string-match "Committed" str ) 'font-lock-type-face)
	((string-match  "Modified" str) 'font-lock-warning-face)
	(t 'font-lock-comment-face)))

(defun superman-trim-date (date &optional len)
  (let ((len (if (stringp len) (length len) len))
	date-string)
    (if (< len 1) (setq len 13))
    (if (string-match org-ts-regexp0 date)
	(let ((age (abs (org-time-stamp-to-now date))))
	  (cond ((= age 0)
		 (setq date "today"))
		((= age 1)
		 (setq date "yesterday"))
		(t (setq date (concat (int-to-string age) " days ago"))))
	  (setq date-string (superman-trim-string date len))
	  (put-text-property 0 (length date-string) 'sort-key age date-string))
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

(defun superman-view-control (project)
  "Insert the git repository if project is git controlled
and the keybinding to initialize git control otherwise."
  (let* ((loc (get-text-property (point-min) 'git-dir))
	 (control (if (superman-git-p loc)
		      (concat "Version control: Git repository at "
			      ;;FIXME: would like to have current git status
			       "[[" (superman-git-toplevel loc) "]]")
		    "Version control: not set. <> press `GI' to initialize git")))
    (put-text-property 0 (length "Version control: ") 'face 'org-level-2 control)
    control))

(defun superman-view-branch (project)
  "Insert the git branch(es) if project is git controlled"
  (when (superman-git-p loc)
    (let* ((loc (get-text-property (point-min) 'git-dir))
	   (branches (superman-git-branches loc))
	   (master (car branches))
	   (others (mapconcat #'(lambda (x)x) (cdr branches) " "))
	   (branch-string (concat "\nBranch: [" master "] " others)))
      (put-text-property 0 (length "Branch:") 'face 'org-level-2 branch-string)
      (put-text-property (+ 3 (length "Branch:"))
			 (+ (length "Branch:") 3 (length master))
			 'face 'font-lock-warning-face branch-string)
      branch-string)))



(defun superman-view-others (project)
  "Insert the names and emails of the others (if any)." 
  (let ((pro (or project (superman-view-current-project)))
	(others (superman-get-others pro)))
    (if others
	(let ((key "Others: "))
	  (put-text-property 0 (length key) 'face 'org-level-2 key)
	  (concat key others "\n"))
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
  (let* ((cat-pos (superman-cat-point))
	 (subcat-pos (superman-subcat-point))
	 (subp (and subcat-pos (> subcat-pos cat-pos))))
    (when cat-pos
      (if subp subcat-pos cat-pos))))

;;}}}
;;{{{ Marking elements

(defun superman-toggle-mark (&optional on)
  "Toggle mark for item at point in project view.
If ON is non-nil keep mark for already marked items.
If DONT-MOVE is non-nil stay at item."
  (interactive)
  (if (org-agenda-bulk-marked-p)
      (unless on (org-agenda-bulk-unmark))
    (org-agenda-bulk-mark)))

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
			   (point-at-eol) 'org-marker))
	  (goto-char next)
	  (when (or (not marked)
		    (superman-marked-p))
	    (setq loop-out
		  (append (list (apply fun args)) loop-out)))
	  (goto-char next)
	  (end-of-line))
	loop-out))))

(defun superman-count-items (&optional begin end)
  (let ((count 0) 
	(begin (or begin (point-min)))
	(end (or end (point-max))))
    (save-restriction
      (narrow-to-region begin end)
      (save-excursion
	(goto-char (point-min))
	(while (next-single-property-change
		(point-at-eol) 'org-marker)
	  (goto-char (next-single-property-change
		      (point-at-eol) 'org-marker))
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
  (let ((cols
	 (superman-format-thing
	  (list "columns"
		(mapcar #'(lambda (b)
			    (cons (car b)
				  (or (cadr (assoc "name" (cdr b)))
				      (cond ((stringp (car b)) (car b))
					    ((eq (car b) 'hdr) "Title")
					    ((eq (car b) 'todo) "Status")))))
			balls)) balls 'no-face)))
    (put-text-property 0 (length cols) 'face 'font-lock-comment-face cols)
    (put-text-property 0 (length cols) 'names (length cols) cols)
    cols))

(defun superman-read-balls (&optional pom)
  (let ((i 1)
	ball
	balls)
    (while
	(setq ball
	      (superman-get-property pom (concat "Ball" (int-to-string i)) nil t))
      (setq balls (append balls (list
				 (superman-distangle-ball ball)))
	    i (+ i 1)))
    balls))


(defun superman-string-to-thing (string &optional prefer-string prefer-symbol)
  "Convert STRING to either integer, string or symbol-name. If not an integer
and PREFER-SYMBOL is non-nil return symbol unless PREFER-STRING."
  (let (thing)
    (if (> (setq thing (string-to-int string)) 0)
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
	 (when (string-match "^todo\\|hdr$" prop)
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
	(let ((pom (get-text-property cat-point 'org-hd-marker))
	      (balls (get-text-property cat-point 'balls))
	      (i 1))
	  (while balls
	    (org-entry-put
	     pom
	     (concat "Ball" (int-to-string i))
	     (superman-ball-to-string (car balls)))
	    (setq balls (cdr balls)
		  i (+ i 1)))
	  (org-entry-put
	   pom
	   (concat "Ball" (int-to-string i)) ""))
	(superman-view-save-index-buffer)))))

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
  (let* ((cols (cdr (get-text-property (point-at-bol) 'columns)))
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

;; (defun superman-get-ball-name ()

(defun superman-sort-section (&optional reverse)
  (interactive "P")
  (let* ((buffer-read-only nil)
	 (cc (current-column))
	 (pp (point-at-bol))
	 (col-start 0)
	 col-width
	 (sort-fold-case t) 
	 (cols (cdr (get-text-property (point-at-bol) 'columns)))
	 (pos (superman-current-subcat-pos))
	 key
	 next
	 beg
	 end)
    (when (and pos cols)
      (while (> cc (+ col-start (car cols)))
	(setq col-start (+ col-start (car cols)))
	(setq cols (cdr cols)))
      ;; the first two characters of each column are blank
      ;; thus, we shift by 2
      (setq col-start (+ 2 col-start))
      (setq col-width (- (car cols) 1))
      (goto-char pos)
      (goto-char (next-single-property-change (point-at-eol) 'org-marker))
      (setq beg (point))
      ;; move to end of section
      (or (outline-next-heading)
	  (goto-char (point-max)))
      (goto-char (previous-single-property-change (point-at-eol) 'org-marker))
      (setq end (point))
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; sort by sort-key if any
      (if (setq key (get-text-property (+ (point) col-start) 'sort-key))
	  (let* ((sort-string (int-to-string key))
		 (slen (length sort-string))
		 pos)
	    (insert sort-string)
	    (put-text-property (point-at-bol) (point) 'delete slen)
	    (while (re-search-forward "^" nil t)
	      (setq key (get-text-property (+ (point) col-start) 'sort-key))
	      ;; (goto-char (+ (point) col-start))
	      (if key (insert (int-to-string key))))
	    (sort-numeric-fields 0 (point-min) (point-max))
	    (goto-char (point-min))
	    (while (re-search-forward "^" nil t)
	      (delete-region (point)
			     (+ (point) (skip-chars-forward "[0-9]")))
	      (end-of-line)))
	(sort-subr reverse 'forward-line 'end-of-line
		   `(lambda () (forward-char ,col-start))
		   `(lambda () (forward-char ,col-width))))
      (widen)
      (goto-char pp)
      (forward-char (+ 2 col-start)))))
      

(defun superman-sort-by-status (a b)
  (let ((A  (substring-no-properties a 0 12))
	(B  (substring-no-properties b 0 12)))
    (if (string= A B) nil 
      (if (string-lessp A B)
	  1 -1))))

;;}}}
;;{{{ Project views

(defun superman-parse-cats (buffer level)
  (save-excursion
    (save-restriction
      (set-buffer buffer)
      (widen)
      (show-all)
      (goto-char (point-min))
      ;; move to first heading
      ;; with the correct level
      (while (and (not (and
			(looking-at org-complex-heading-regexp)
			(org-current-level)
			(= (org-current-level) level)))
		  (outline-next-heading)))
      (when (and (org-current-level)
		 (= (org-current-level) level))
	(looking-at org-complex-heading-regexp)
	(let ((cats `((,(match-string-no-properties 4)
		       ,(superman-read-balls (point)))))
	      (cat-point (point)))
	  (while (progn (org-forward-heading-same-level 1)
			(> (point) cat-point))
	    (looking-at org-complex-heading-regexp)
	    (setq cat-point (point)
		  cats (add-to-list
			'cats
			`(,(match-string-no-properties 4)
			  ,(superman-read-balls (point))))))
	  (reverse cats))))))

(defun superman-view-project (&optional project)
  "Display the current project in a view buffer."
  (interactive)
  (let* ((pro (if (stringp project)
		  (assoc project superman-project-alist)
		(or project
		    superman-current-project
		    (superman-switch-to-project nil t))))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (vbuf (concat "*Project[" (car pro) "]*"))
	 ;; (org-agenda-window-setup 'current-window)
	 (project-header (concat "Project: " (car pro)))
	 (index (superman-get-index pro))
	 (ibuf (or (get-file-buffer index)
		   (find-file index)))
	 ;; (cats superman-cats)
	 (cats (superman-parse-cats ibuf 1))
	 (font-lock-global-modes nil)
	 (org-startup-folded nil))
    (switch-to-buffer vbuf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (font-lock-mode -1)
    (font-lock-default-function nil)
    ;; insert header, set text-properties and highlight
    (insert  (concat "Project: " (car pro)))
    (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd `(superman-view-project ,(car pro)))
    (put-text-property (point-at-bol) (point-at-eol) 'git-dir (superman-git-toplevel loc))
    (put-text-property (point-at-bol) (point-at-eol) 'dir loc)
    (put-text-property (point-at-bol) (point-at-eol) 'nickname (car pro))
    (put-text-property (point-at-bol) (point-at-eol) 'index index)
    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
    (insert (superman-project-view-header pro))
    (goto-char (point-max))
    (insert "\n")
    ;; loop cats
    (while cats
      (let* ((cat (caar cats))
	     (cat-balls (or (cadar cats)))
	     (gear (cdr (assoc cat superman-finalize-cat-alist)))
	     (balls (or cat-balls (eval (nth 0 gear))))
	     cat-head
	     cat-point
	     (count 0)
	     line)
	;; back to vbuf
	(set-buffer vbuf)
	(setq cat-head (point))
	;; move to ibuf
	(when (superman-goto-project pro cat nil nil 'leave-narrowed nil)
	  (let* ((sec-head (save-excursion
			     (outline-previous-heading)
			     (point)))
		 ;; (folded (superman-get-property sec-head  "startFolded"))
		(free (superman-get-property sec-head "freeText")))
	    (goto-char (point-min))
	    (setq cat-point (point-marker))
	    (if free
		(save-restriction
		  (org-narrow-to-subtree)
		   (let ((text (buffer-substring
				(progn (org-end-of-meta-data-and-drawers)
				       (point))
				(point-max))))
		     (with-current-buffer vbuf (insert text))))
	      ;; loop over section cat
	      ;; format elements (if any and if wanted)
	      ;; region is narrowed to section
	      (while (outline-next-heading)
		(if (eq (org-current-level) 2)
		    (let ((subhdr (progn (looking-at org-complex-heading-regexp) (match-string-no-properties 4))))
		      (setq line (concat "*** " subhdr))
		      (put-text-property 0 (length line) 'subcat subhdr line)
		      (put-text-property 0 (length line) 'org-hd-marker (point-marker) line)
		      (put-text-property 0 (length line) 'face 'org-level-3 line)
		      (put-text-property 0 (length line) 'display (concat "  ☆ " subhdr) line)
		      (with-current-buffer vbuf
			(insert
			 line
			 ;;"\n" (superman-column-names balls)
			 "\n"
			 ))
		      (end-of-line))
		  (setq count (+ count 1))
		  (setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
		  (with-current-buffer vbuf (insert line "\n")))))
	    ;; (widen)
	    ;; (show-all)
	    (set-buffer vbuf)
	    (goto-char cat-head)
	    (insert "\n** " cat "\n")
	    (forward-line -1)
	    (beginning-of-line)
	    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
	    (put-text-property (point-at-bol) (point-at-eol) 'cat cat)
	    (put-text-property (point-at-bol) (point-at-eol) 'balls balls)
	    (put-text-property (point-at-bol) (point-at-eol) 'org-hd-marker cat-point)
	    (put-text-property (point-at-bol) (point-at-eol) 'display (concat "★ " cat))
	    (if (or (not (member cat superman-capture-alist))
		    (member cat superman-views-permanent-cats) (> count 0))
		(progn (end-of-line)
		       (insert " [" (int-to-string count) "]")
		       ;; insert hot-keys or blank line
		       (end-of-line)
		       (insert "\n")
		       ;; insert column names 
		       (insert (superman-column-names balls)))
	      (delete-region (point) (point-max)))
	    (goto-char (point-max))
	  (widen)
	  ;; (when folded (hide-subtree))
	  (setq cats (cdr cats))))))
      ;; leave index buffer widened
      (set-buffer ibuf)
      (widen)
      (show-all)
      (switch-to-buffer vbuf))
    (goto-char (point-min))
    ;; facings
    (save-excursion
      (while (or (org-activate-bracket-links (point-max)) (org-activate-plain-links (point-max)))
	(add-text-properties
	 (match-beginning 0) (match-end 0)
	 '(face org-link))))
    ;; default-dir
    (setq default-directory
	  (superman-project-home
	   (superman-view-current-project)))
    ;; minor-mode
    (superman-view-mode-on)
    ;; check modified
    (superman-view-git-update-status nil nil nil 'dont)
    (setq buffer-read-only t))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("URGENT" :foreground "goldenrod1" :weight bold)
	      ("IN PROGRESS" :foreground "blue" :weight bold)
	      ("ACTIVE" :foreground "red" :weight bold)
	      ("WAITING" :foreground "purple" :weight bold)
	      ("PERMANENT" :foreground "SkyBlue3" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      ("CANCELED" :foreground "slate grey" :weight bold))))

;;}}}
;;{{{ Formatting items and column names

(defun superman-play-ball (thing ball &optional no-face)
  "Play BALL at THING which is a marker or an alist and return
a formatted string with faces."
  (let* ((raw-string
	  (if (markerp thing)
	      ;; thing is marker
	      (cond ((stringp (car ball)) ;; properties
		     (superman-get-property thing (car ball) t))
		    ((eq (car ball) 'todo) ;; special: todo state
		     (org-with-point-at thing 
		       (org-back-to-heading t)
		       (and (looking-at org-todo-line-regexp)
			    (match-end 2) (match-string 2))))
		    ((eq (car ball) 'hdr) ;; special: header
		     (org-with-point-at thing 
		       (org-back-to-heading t)
		       (looking-at org-complex-heading-regexp)
		       (match-string 4)))
		    (t "--"))
	    ;; thing is alist
	    (or (cdr (assoc (car ball) (cadr thing))) "--")))
	 (raw-string (if (or (not raw-string) (eq raw-string "")) "--" raw-string))
	 (u (get-text-property 0 'face raw-string))
	 (fun (or (cadr (assoc "fun" (cdr ball)))
		  'superman-trim-string))
	 (width (or (cdr (assoc "width" (cdr ball)))
		    (list 23)))
	 (args (cdr (assoc "args" (cdr ball))))
	 (trim-args (nconc width args))
	 (trimmed-string
	  (concat "  " ;; column sep
		  (apply fun raw-string trim-args)))
	 (face
	  (or (cadr (assoc "face" ball))
	      (get-text-property 0 'face raw-string)))
	 (sort-key (get-text-property 2 'sort-key trimmed-string)))
    ;; remove all existing text-properties
    (set-text-properties 0 (length trimmed-string) nil trimmed-string)
    (when sort-key (put-text-property 0 (length trimmed-string) 'sort-key sort-key trimmed-string))
    (unless no-face
      (when (and (not (facep face)) (functionp face)) ;; apply function to get face
	(setq face (funcall
		    face
		    (replace-regexp-in-string
		     "^[ \t\n]+\\|[ \t\n]+$" ""
		     raw-string))))
      (when (or (facep face) (listp face))
	(setq trimmed-string (org-add-props trimmed-string nil 'face face))))
    ;; (put-text-property 0 (length trimmed-string) 'face face trimmed-string)))
    trimmed-string))

(defun superman-format-thing (thing balls &optional no-face)
  "Format THING according to balls. THING is either
a marker which points to a header in a buffer 
or an association list. A ball has the form

'(key (\"face\" face-or-fun) (\"trim\" fun args))

Value is the formatted string with text-properties (special balls)."
  (let ((item "")
	ilen
	(column-widths (list 0))
	(marker (cond ((markerp thing) thing)
		      ((cdr (assoc "marker" (cadr thing))))))
	(copy-balls balls)
	text-props
	(beg 0))
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
      (put-text-property 0 ilen 'org-marker marker item))
    ;; add balls for redo
    ;; not done (balls are saved in category instead)
    ;; (put-text-property 0 ilen 'balls balls item)
    ;; text property: columns
    (put-text-property 0 ilen 'columns column-widths item)
    item))


(defun superman-project-view-header (pro)
  "Construct extra heading lines for project views."
  (let ((hdr  (concat "\n\n"
		      (superman-view-others pro)
		      (superman-view-control pro)
		      (superman-view-branch pro)
		      ;; "\n"
		      ;; (superman-view-show-hot-keys)
		      )))
    hdr))


;; (defun superman-documents-view-header (pro)
  ;; "Construct extra heading lines for project views."
  ;; (let ((control (superman-view-control pro))
	;; (hotkeys (superman-view-hot-keys superman-view-documents-hot-keys)))
    ;; (concat "\n" control (insert "\n\n" hotkeys "\n\n"))) "\n" )

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
  (let ((len (length list)))
    (cond ((< pos 0)
	   (nconc (cdr list) (list (car list))))
	  ((> pos (- len 2))
	   (nconc (list (nth (- len 1) list)) (butlast list 1)))
	  (t
	   (nconc (butlast list (- len pos))
	     (list (nth (+ pos 1) list))
	     (list (nth pos list))
	     (nthcdr (+ pos 2) list))))))
	     

(defun superman-change-balls (new-balls)
  "Exchange balls (column definitions) in this section."
  (let ((buffer-read-only nil)
	(cat-point (superman-cat-point)))
  (if cat-point
      (save-excursion
	(goto-char cat-point)
	(put-text-property (point-at-bol) (point-at-eol) 'balls new-balls))
    (message "Point is not inside a section"))))

(defun superman-compute-columns-start ()
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
  (let* ((current (nth 2 (superman-ball-dimensions)))
	 (colstart (superman-compute-columns-start))
	 (j (max 0 (min (- (length colstart) 1)
			(+ current arg)))))
    (beginning-of-line)
    (forward-char (+ 2 ;; offset for whole line
		     (nth j colstart)))))

(defun superman-previous-ball ()
  "Move to previous column."
  (interactive)
  (superman-next-ball -1))
    
(defun superman-one-right (&optional left)
  "Move column to the right."
  (interactive "P")
  (let* ((dim (superman-ball-dimensions))
	 (balls (get-text-property (superman-cat-point) 'balls))
	 (buffer-read-only nil)
	 (new-balls (superman-swap-balls balls
					 (if left (- (nth 2 dim) 1)
					   (nth 2 dim))))
	 (beg (previous-single-property-change (point-at-bol) 'cat))
	 (end (or (next-single-property-change (point-at-eol) 'cat) (point-max))))
    (save-excursion
      (superman-change-balls new-balls)
      (superman-refresh-cat))))
    
(defun superman-one-left ()
  "Move column to the left."
  (interactive)
  (superman-one-right 1))

(defun superman-delete-ball ()
  "Delete current column. The column will still pop-up when the
view is refreshed, but can be totally removed
by calling `superman-save-balls' subsequently."
  (interactive)
  (let* ((dim (superman-ball-dimensions))
	 (balls (get-text-property (superman-cat-point) 'balls))
	 (buffer-read-only nil)
	 (new-balls (remove-if (lambda (x) t) balls :start (nth 2 dim) :count 1))
	 (beg (previous-single-property-change (point-at-bol) 'cat))
	 (end (or (next-single-property-change (point-at-eol) 'cat) (point-max))))
    (save-excursion
      (superman-change-balls new-balls)
      (superman-refresh-cat))))

(defun superman-new-ball ()
  "Add a new column to show a property of all items in the
current section."
  (interactive)
  (let* ((balls (copy-sequence (get-text-property (superman-cat-point) 'balls)))
	 (buffer-read-only nil)
	 (props (superman-view-property-keys))
	 (prop (completing-read "Property to show in new column (press tab see existing): "
				(mapcar (lambda (x) (list x)) props) nil nil))
	 (len (string-to-int (read-string "Column width: ")))
	 (new-ball `(,prop ("width" ,len)))
	 (new-balls (add-to-list 'balls new-ball))
	 (beg (previous-single-property-change (point-at-bol) 'cat))
	 (end (or (next-single-property-change (point-at-eol) 'cat) (point-max))))
    (save-excursion
      (superman-change-balls new-balls)
      (superman-save-balls)
      (superman-refresh-cat))))
  
  
(defun superman-one-up (&optional down)
  "Move item in project view up or down."
  (interactive "P")
  (let* ((marker (org-get-at-bol 'org-hd-marker))
	 (catp (org-get-at-bol 'cat))
	 (org-support-shift-select t)
	 (curcat (when catp (superman-current-cat))))
    (when (or catp marker)
      (save-excursion
	(set-buffer (marker-buffer marker))
	(goto-char marker)
	(widen)
	(or (condition-case nil
		(if down
		    (org-move-subtree-down)
		  (org-move-subtree-up))
	      (error nil))
	    (if down
		(progn
		  (put-text-property (point-at-bol) (point-at-eol) 'current-item 1)
		  (outline-next-heading)
		  (if (< (nth 0 (org-heading-components)) 2) (error "Cannot move item outside category"))
		  (put-text-property (point-at-bol) (point-at-eol) 'next-item 1)
		  (org-demote)
		  (goto-char (previous-single-property-change (point) 'current-item))
		  (org-move-subtree-down)
		  (goto-char (previous-single-property-change (point) 'next-item))
		  (org-promote))
	      (put-text-property (point-at-bol) (point-at-eol) 'current-item 1)
	      (outline-previous-heading)
	      (if (< (nth 0 (org-heading-components)) 2) (error "Cannot move item outside category"))
	      (put-text-property (point-at-bol) (point-at-eol) 'next-item 1)
	      (org-demote)
	      (goto-char (next-single-property-change (point) 'current-item))
	      (org-move-subtree-up)
	      (goto-char (next-single-property-change (point) 'next-item))
	      (org-promote))))
      (superman-redo)
      (if catp
	  (progn 
	    (goto-char (point-min))
	    (re-search-forward curcat nil t)
	    (beginning-of-line))
	  ;; (goto-char (if down
	  ;; (next-single-property-change (point-at-eol) 'cat)
	  ;; (previous-single-property-change (point-at-bol) 'cat)))
	  (forward-line (if down 1 -1))))))

(defun superman-one-down ()
  (interactive)
  (superman-one-up 1))

(defun superman-cut ()
  (interactive)
  (if superman-view-mode
      (let ((marker (org-get-at-bol 'org-hd-marker))
	    (buffer-read-only nil))
	(when marker
	  (beginning-of-line)
	  (kill-line)
	  (save-excursion
	    (set-buffer (marker-buffer marker))
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
	  (save-excursion
	    (set-buffer (marker-buffer marker))
	    (widen)
	    (show-all)
	    (goto-char marker)
	    (org-paste-subtree)))
	(superman-redo))
    (message "can only paste in superman-view-mode")))

;;}}}
;;{{{ Edit items

(defun superman-view-property-keys ()
  "Get a list of all property keys in current section"
  (let ((cat-point (superman-cat-point)))
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
	  (superman-property-keys)
	  ))))))

(defun superman-view-edit-item ()
  "Put item at point into capture mode"
  (interactive)
  (let* ((marker (org-get-at-bol 'org-hd-marker))
	 (catp  (org-get-at-bol 'cat))
	 (E-buf (generate-new-buffer-name "*Edit by SuperMan*"))
	 (scene (current-window-configuration))
	 (all-props (if catp nil (superman-view-property-keys)))
	 range
	 (balls (get-text-property (superman-cat-point) 'balls))
	 prop
	 used-props)
    ;; add properties defined by balls to all-props
    (while balls
      (when (stringp (setq prop (caar balls)))
	(add-to-list
	 'all-props
	 prop))
      (setq balls (cdr balls)))
    (set-buffer (marker-buffer marker))
    (widen)
    (show-all)
    (switch-to-buffer
     (make-indirect-buffer (marker-buffer marker) E-buf))
    (goto-char marker)
    (org-narrow-to-subtree)
    (when (outline-next-heading)
      (narrow-to-region (point-min) (point)))
    (org-mode)
    (show-all)
    (delete-other-windows)
    (goto-char (point-min))
    (put-text-property (point) (point-at-eol) 'capture (point))
    (insert "### Superman: edit this item" 
	    "\n# C-c C-c to save "
	    "\n# C-c C-q to quit without saving"
	    "\n### ---yeah #%*^#@!--------------"
	    "\n\n")
    (goto-char (point-min))
    (put-text-property (point) (point-at-eol) 'scene scene)
    (unless catp
      (if (re-search-forward org-property-start-re nil t)
	  (progn
	    (setq range (org-get-property-block))
	    (goto-char (car range))
	    (while (re-search-forward
		    (org-re "^[ \t]*:\\([-[:alnum:]_]+\\):")
		    (cdr range) t)
	      (put-text-property (point) (+ (point) 1) 'prop-marker (point))
	      (add-to-list 'used-props (org-match-string-no-properties 1)))
	    (goto-char (cdr range))
	    (forward-line -1)
	    (end-of-line))
	(outline-next-heading)
	(end-of-line)
	(insert "\n:PROPERTIES:\n:END:\n")
	(forward-line -2)
	(end-of-line))
      (while all-props
	(when (not (member (car all-props) used-props))
	  (insert "\n:" (car all-props) ": ")
	  (put-text-property (- (point) 1) (point) 'prop-marker (point)))
	(setq all-props (cdr all-props)))))
    (goto-char (next-single-property-change (point-min) 'capture))
    (end-of-line)
    (superman-capture-mode))

;;}}}
;;{{{ Switch between projects

(defvar superman-project-history nil
  "List of projects that were previously selected
 in the current emacs session.")

(defun superman-next-project (&optional backwards)
  "Switch to next project in `superman-project-history'"
  (interactive "P")
  (let* ((phist
	  (member (car superman-current-project)
		  (if backwards
		      (reverse superman-project-history)
		    superman-project-history)))
	 (next (cadr phist)))
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
;;{{{ View commands (including redo and git) 

(defun superman-redo ()
  "Refresh project view."
  (interactive)
  (let ((curline (progn
		   (beginning-of-line)
		   (count-lines 1 (point))))
	cmd)
    (setq cmd (get-text-property (point-min) 'redo-cmd))
    (eval cmd)
    (goto-line (+ 1 curline))))

(defun superman-refresh-cat ()
  "Refresh view of all lines in current category inclusive column names."
  (interactive)
  (let ((start (superman-cat-point))
	(end (or (next-single-property-change (point) 'cat) (point-max))))
    (if (not start)
	(message "Point is not in category.")
      (superman-loop 'superman-view-redo-line nil start end nil)
      (goto-char (next-single-property-change start 'names))
      (beginning-of-line)
      (kill-line)
      (insert (superman-column-names new-balls) "\n"))))

(defun superman-view-redo-line ()
  (interactive)
  (let* ((buffer-read-only nil)
	 (marker (org-get-at-bol 'org-hd-marker))
	 (balls (get-text-property (superman-cat-point) 'balls)))
    (when (and marker (not (org-get-at-bol 'subcat)) (not (org-get-at-bol 'subcat)))
      (beginning-of-line)
      (let ((newline (superman-format-thing marker balls)))
	(if (looking-at ".*")
	    (replace-match newline))
	(beginning-of-line)
	(while (or (org-activate-bracket-links (point-at-eol)) (org-activate-plain-links (point-at-eol)))
	  (add-text-properties
	   (match-beginning 0) (match-end 0)
	   '(face org-link)))
	(beginning-of-line)))))


(defun superman-view-toggle-todo ()
  (interactive)
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (when marker
      (save-excursion
	(org-with-point-at marker
	  (org-todo)))
      (superman-view-redo-line))))

(defun superman-next-entry ()
  (interactive)
  ;; check if current section is folded
  (let ((beg (point-at-bol))
	(end (or (next-single-property-change (point-at-eol) 'subcat)
		 (next-single-property-change (point-at-eol) 'cat)
		 (point-max))))
    (if (overlays-in beg end)
	(if (get-text-property (point-at-bol) 'cat)
	    (goto-char (next-single-property-change (point-at-eol) 'cat))
	  (goto-char end))
      (goto-char
       (or (next-single-property-change (point-at-eol) 'org-hd-marker)
	   (point))))))

(defun superman-previous-entry ()
  (interactive)
  ;; check if current section is folded
  (let ((end (point-at-eol))
	(beg (or (previous-single-property-change (point-at-bol) 'subcat)
		 (previous-single-property-change (point-at-bol) 'cat)
		 (point-min))))
    (if (overlays-in beg end)
	(progn 
	  (if (get-text-property (point-at-bol) 'cat)
	      (goto-char (previous-single-property-change (point-at-bol) 'cat))
	    (goto-char beg))
	  (beginning-of-line))
      (let ((pos (previous-single-property-change (point-at-bol) 'org-hd-marker)))
	(when pos
	  (progn (goto-char pos) (beginning-of-line))))))
  (beginning-of-line))

(defun superman-view-delete-entry (&optional dont-prompt dont-redo do-delete-file)
  (interactive)
  (let* ((marker (org-get-at-bol 'org-hd-marker))
	 (scene (current-window-configuration))
	 (file (superman-filename-at-point t)))
    (unless dont-prompt
      (superman-view-index)
      (org-narrow-to-subtree)
      (yes-or-no-p "Delete this entry?"))
    (set-window-configuration scene)
    (when file
      (when (and do-delete-file
		 (yes-or-no-p
		  (concat "Delete file "
			  (file-name-nondirectory file))))
	(if (string-match
	     (superman-get-property marker "GitStatus")
	     "Committed\\|Modified")
	    (shell-command (concat
			    "cd "
			    (file-name-directory file)
			    ";"
			    superman-cmd-git " rm -f "
			    (file-name-nondirectory file)))
	  (when (file-exists-p file)
	    (delete-file file)))))
    (when marker
      (save-excursion
	(org-with-point-at marker (org-cut-subtree)))))
  (unless dont-redo (superman-redo)))

(defun superman-view-delete-all (&optional dont-prompt)
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

(defun superman-view-git-diff ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
  (vc-diff file "HEAD")))


(defun superman-view-git-version-diff ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (file (org-link-display-format (superman-get-property m "filename"))))
    (async-shell-command (concat "cd " (file-name-directory file) "; " superman-cmd-git " difftool " (file-name-nondirectory file)))))
	;; (find-file file)
      ;; (vc-version-diff file "master" nil)))

(defun superman-view-git-ediff ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
    (vc-ediff file "HEAD")))


(defun superman-view-git-annotate (&optional arg)
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (bufn)
    (file (org-link-display-format (superman-get-property m "filename"))))
    (save-window-excursion
      (find-file file)
    (vc-annotate (org-link-display-format file) "HEAD")
    (setq bufn (buffer-name))
    )
    (switch-to-buffer bufn)
  ))

(defun superman-view-git-grep (&optional arg)
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (if arg
	(vc-git-grep (read-string "Grep: "))
	(vc-git-grep (read-string "Grep: ") "*" dir)))))

(defun superman-view-git-history ()
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	(bufn (concat "*history: " dir "*"))
	)
    (when dir
    ;; (vc-print-log-internal
    ;;  'Git
    ;;  (list dir)
      ;;  nil nil 2000)      
      ;; (message dir)
      (save-window-excursion
	(superman-view-index)
	(vc-git-print-log dir bufn t)
	)
      (switch-to-buffer bufn)
      (vc-git-log-view-mode)
)))

(defun superman-view-index ()
  (interactive)
  (let* ((pom (org-get-at-bol 'org-hd-marker))
	 (ibuf (or (and pom (marker-buffer pom))
		   (get-file-buffer
		    (get-text-property (point-min) 'index))
		   (find-file
		    (get-text-property (point-min) 'index))))
	 (iwin (when ibuf (get-buffer-window ibuf nil))))
    (if (and ibuf iwin)
	(select-window (get-buffer-window ibuf nil))
      ;; FIXME this should be customizable
      (split-window-vertically)
      (other-window 1)
      (if ibuf (switch-to-buffer ibuf)
	(find-file index)))
    (show-all)
    (widen)
    (when pom (goto-char pom))
    ;;(org-narrow-to-subtree)
    ))

(defun superman-view-file-list ()
  (interactive)
  (let ((pro (superman-view-current-project)))
    (split-window-vertically)
      (other-window 1)
    (superman-file-list pro)))

(defun superman-view-git-init ()
  (interactive)
  (or (get-text-property (point-min) 'git-dir)
    (let ((pro (superman-view-current-project)))
      (superman-git-init-directory (concat (superman-get-location pro) (car pro)))
      (superman-redo))))


(defun superman-hot-return ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (b (superman-current-cat))
	 f)
    (cond ((string-match "Mail" b)
	   (save-excursion
	     (beginning-of-line)
	     (if (re-search-forward org-bracket-link-regexp nil t)
		 (org-open-at-point))))
	  ;; (message "Open mail"))
	  ((string-match "Bookmarks" b)
	   (save-excursion
	     (beginning-of-line)
	     (if (re-search-forward org-bracket-link-regexp nil t)
		 (org-open-at-point))))
	  ((string-match "Notes" b)
	   (save-excursion
	     (superman-view-index)
	     (org-narrow-to-subtree)))
	  (superman-mode
	   (superman-return))
	  ;; (message "Follow-link"))
	  ((setq f (superman-get-property m "filename"))
	   (org-open-link-from-string f))
	  (t (message "Nothing to do here")))))

(defun superman-view-git-log (&optional arg)
  (interactive "p")
  (superman-git-log-at-point (or arg superman-git-log-limit)))

(defun superman-view-git-log-decorationonly (&optional arg)
  (interactive "p")
  (superman-git-log-decorationonly-at-point (or arg superman-git-search-limit)))

(defun superman-view-git-search (&optional arg)
  (interactive "p")
  (superman-git-search-at-point (or arg superman-git-search-limit)))

(defun superman-view-git-set-status (&optional save redo check)
  (interactive)
  (let ((file (superman-filename-at-point t))
	(pom  (org-get-at-bol 'org-hd-marker)))
    (when
	file
      (superman-git-set-status pom file check)
      (when save (superman-view-save-index-buffer))
      (when redo (superman-redo)))))

(defun superman-view-save-index-buffer ()
  (save-excursion
    (let ((ibuf (get-file-buffer
		 (get-text-property (point-min) 'index))))
      (when ibuf (set-buffer ibuf)
	    (save-buffer)))))

(defun superman-filename-with-pom (&optional noerror)
  "Return property `superman-filename-at-point' at point,
if it exists and add text-property org-hd-marker."
  (let* ((file-or-link
	  (superman-property-at-point
	   (superman-property 'filename) noerror))
	 filename)
    (if (not (stringp file-or-link))
	(unless noerror
	  (error "No proper(ty) FileName at point."))
      (setq filename (org-link-display-format file-or-link))
      (put-text-property 0 (length filename) 'org-hd-marker
			 (org-get-at-bol 'org-hd-marker) filename)
      filename)))


(defun superman-view-git-update-status-forced (&optional beg end dont-redo)
  (interactive)
  (superman-view-git-update-status beg end t dont-redo))

  
(defun superman-view-set-status-at-point ()
  (let ((pom (get-text-property (point-at-bol) 'org-hd-marker))
	(file (superman-filename-at-point t)))
    (if (and pom file)
	;; (ignore-errors
	  (superman-git-set-status pom file nil))))

				     
(defun superman-view-git-update-status (&optional beg end force dont-redo)
  "Update git status for all registered entries within BEG and END.
If FORCE is non-nil run uncondionally through all entries that have a filename.
This comes at the cost of one separate call to git status for each file.

If FORCE is nil run only through those files that have changed status
according to git."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (if (not dir) (message "Project is not git-controlled.")
      (if force
	  (superman-loop 'superman-view-set-status-at-point nil beg end nil)
      (let* ((file-list (delq nil (superman-loop 'superman-filename-with-pom (list t) beg end)))
	     (git-status (shell-command-to-string (concat "cd " dir ";" superman-cmd-git " status --porcelain ")))
	     (status-list
	      (delq nil
		    (mapcar (lambda (x)
			      (if (string= "" x) nil
				(let ((el (split-string x " ")))
				  (cons (caddr el) (cadr el)))))
			    (split-string git-status "\n")))))
	(while file-list
	  (let* ((file (car file-list))
		 (status
		  (superman-status-label
		   (or (cdr (assoc (file-relative-name file dir)
				   status-list)) "C")))
		 (pom (get-text-property 0 'org-hd-marker file))
		 (current-status
		  (superman-get-property pom "GitStatus")))
	    (unless (string= status current-status)
	      (org-entry-put pom (superman-property 'gitstatus) status)
	      (when (or
		     (string= (downcase status) "modified")
		     (and (stringp current-status)
			  (string= (downcase current-status) "modified")))
		(superman-git-set-status pom file nil)))
	    ;; (org-entry-put pom "GitStatus" status)))
	    (setq file-list (cdr file-list))))))
      (unless dont-redo (superman-redo)))))


(defun superman-view-git-push (&optional project)
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 cmd)
    (when dir
      (superman-goto-shell)
      (insert  (concat "cd " dir ";" superman-cmd-git " push")))))

(defun superman-view-git-commit (&optional dont-redo)
  "Add and commit the file given by the filename property
of the item at point.

If dont-redo the agenda is not reversed."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename)))))
    (superman-git-add (list file) dir 'commit nil)
  (superman-view-git-set-status 'save (not dont-redo) nil)))

(defun superman-view-marked-files (&optional beg end)
  (delq nil (superman-loop
	     #'(lambda ()
		 (or (and (superman-marked-p)
			  (superman-filename-at-point 'no-error)))) nil beg end)))

(defun superman-view-git-commit-all (&optional commit dont-redo)
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-add
       (superman-view-marked-files)
       dir
       'commit nil)
      (superman-view-git-update-status nil nil nil)
      (unless dont-redo (superman-redo)))))

;;}}}
;;{{{ View-mode and hot-keys

(defvar superman-view-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
   
(define-minor-mode superman-view-mode
     "Toggle superman project view mode.
With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
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

(define-key superman-view-mode-map [return] 'superman-hot-return)
(define-key superman-view-mode-map [(meta left)] 'superman-one-left)
(define-key superman-view-mode-map [(meta right)] 'superman-one-right)
(define-key superman-view-mode-map [(meta up)] 'superman-one-up)
(define-key superman-view-mode-map [(meta down)] 'superman-one-down)

(define-key superman-view-mode-map [(right)] 'superman-next-ball)
(define-key superman-view-mode-map [(left)] 'superman-previous-ball)
(define-key superman-view-mode-map [(control down)] 'superman-next-cat)
(define-key superman-view-mode-map [(control up)] 'superman-previous-cat)
(define-key superman-view-mode-map [(control n)] 'superman-next-cat)
(define-key superman-view-mode-map [(control p)] 'superman-previous-cat)
(define-key superman-view-mode-map "n" 'superman-next-entry)
(define-key superman-view-mode-map "p" 'superman-previous-entry)

(define-key superman-view-mode-map [(up)] 'superman-previous-entry)
(define-key superman-view-mode-map [(down)] 'superman-next-entry)
(define-key superman-view-mode-map "i" 'superman-view-index)
(define-key superman-view-mode-map "I" 'superman-view-invert-marks)
(define-key superman-view-mode-map "e" 'superman-view-edit-item)
(define-key superman-view-mode-map "F" 'superman-view-file-list)
(define-key superman-view-mode-map "m" 'superman-toggle-mark)
(define-key superman-view-mode-map "M" 'superman-view-mark-all)
(define-key superman-view-mode-map "N" 'superman-new-item)
(define-key superman-view-mode-map "r" 'superman-view-redo-line)
(define-key superman-view-mode-map "t" 'superman-view-toggle-todo)
(define-key superman-view-mode-map "x" 'superman-view-delete-entry)
(define-key superman-view-mode-map "X" 'superman-view-delete-all)
(define-key superman-view-mode-map "N" 'superman-new-item)
(define-key superman-view-mode-map "Q" 'superman-unison)
(define-key superman-view-mode-map "R" 'superman-redo)
(define-key superman-view-mode-map "S" 'superman-sort-section)
(define-key superman-view-mode-map "V" 'superman-switch-config)
(define-key superman-view-mode-map "!" 'superman-goto-shell)
(define-key superman-view-mode-map "?" 'superman-view-help)

(define-key superman-view-mode-map "Bn" 'superman-new-ball)
(define-key superman-view-mode-map "Bx" 'superman-delete-ball)
(define-key superman-view-mode-map "Bs" 'superman-save-balls)

;; Git control
(define-key superman-view-mode-map "Ga" 'superman-view-git-annotate)
(define-key superman-view-mode-map "Gc" 'superman-view-git-commit)
(define-key superman-view-mode-map "GC" 'superman-view-git-commit-all)
(define-key superman-view-mode-map "Gd" 'superman-view-git-diff)
(define-key superman-view-mode-map "Gg" 'superman-view-git-grep)
(define-key superman-view-mode-map "Gh" 'superman-view-git-history)
(define-key superman-view-mode-map "GI" 'superman-view-git-init)
(define-key superman-view-mode-map "Gl" 'superman-view-git-log)
(define-key superman-view-mode-map "GL" 'superman-view-git-log-decorationonly)
(define-key superman-view-mode-map "GP" 'superman-git-push)
(define-key superman-view-mode-map "Gs" 'superman-view-git-search)
(define-key superman-view-mode-map "Gu" 'superman-view-git-update-status)
(define-key superman-view-mode-map "GU" 'superman-view-git-update-status-forced)
(define-key superman-view-mode-map "GBs" 'superman-git-checkout-branch)
(define-key superman-view-mode-map "GBn" 'superman-git-new-branch)
(define-key superman-view-mode-map "G=" 'superman-view-git-version-diff)




(defvar superman-capture-alist nil
  "List to find capture function. Elements have the form
 (\"heading\" function) e.g.  (\"Documents\" superman-capture-document).")

(setq superman-capture-alist
      '(("Documents" superman-capture-document)
	("Notes" superman-capture-note)
	("Tasks" superman-capture-task)
	("Meetings" superman-capture-meeting)
	("Bookmarks" superman-capture-bookmark)))

(defun superman-new-item ()
  "Add a new document, note, task or other item to a project. If called
from superman project view, assoc a capture function from `superman-capture-alist'.
If non exists create a new item based on balls and properties in current section. If point is
not in a section prompt for section first.
"
  (interactive)
  (let* ((pro (or (superman-view-current-project t)
		  (superman-select-project)))
	 (cat (or (superman-current-cat)
		   (completing-read
			(concat "Choose category for new item in project " (car  pro) ": ")
			(append
			 superman-capture-alist
			 (superman-parse-cats
			 (get-file-buffer
			  (superman-get-index pro)) 1)))))
	 (fun (assoc cat superman-capture-alist)))
    (unless superman-view-mode
      (superman-view-project pro)
      (goto-char (point-min))
      (re-search-forward cat nil t))
    (if fun
	(funcall (cadr fun) pro)
      (let* (
	     ;; (cat-point (superman-cat-point))
	     (props (superman-view-property-keys)))
	     ;; (balls (if cat-point (get-text-property cat-point 'balls))))
	(superman-capture
	 pro
	 cat
	 `("Item"
	   ,(mapcar #'(lambda (p) (list p nil)) props)))))))


;;}}}
;;{{{ easy menu

(require 'easymenu)
(easy-menu-define superman-menu superman-view-mode-map "*S*"
  '("Superman"
    ["Refresh view" superman-redo t]
    ["New project" superman-new-project t]
    ["New item" superman-new-item t]
    ["Edit item" superman-view-edit-item t]
    ["Toggle todo" superman-view-toggle-todo t]
    ["Mark item" superman-toggle-mark t]
    ["Mark all" superman-view-mark-all t]
    ["Invert mark" superman-view-invert-marks t]
    ["Delete item" superman-view-delete-entry t]
    ["Delete marked" superman-view-delete-all t]
    ["Move item up" superman-one-up t]
    ["Move item down" superman-one-down t]
    ["Visit index buffer" superman-view-index t]
    ["File list" superman-view-file-list t]
    ("Git"
     ["Git update" superman-view-git-update-status t]
     ["Git commit" superman-view-git-commit t]
     ["Git commit all" superman-view-git-commit-all t]
     ["Git annotate" superman-view-git-annotate t]
     ["Git log" superman-view-git-log t]
     ["Git log (tagged versions)" superman-view-git-log-decorationonly t]
     ["Git search" superman-view-git-search t]
     ["Git push" superman-git-push t]
     ["Git checkout branch" superman-git-checkout-branch t]
     ["Git new branch" superman-git-new-branch t]
     ["Git init" superman-view-git-init t])
    ("Columns (balls)"
     ["New column" superman-new-ball t]
     ["Delete column" superman-delete-ball t]
     ["Move column left" superman-one-left t]
     ["Move column right" superman-one-right t])
    ["Terminal" superman-goto-shell t]
    ["Unison" superman-unison t]
    ))

;;}}}

(provide 'superman-views)

;;; superman-summary.el ends here


