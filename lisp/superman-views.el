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
(defvar superman-view-marks nil "Marks for items in agenda.")
(make-variable-buffer-local 'superman-view-marks)

(defvar superman-view-current-project nil)
(make-variable-buffer-local 'superman-view-current-project)

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



;; (defun superman-view-project-news-line ()
  ;; (concat "New: " (apply 'concat 
			 ;; (mapcar '(lambda (x) (concat "`" superman-view-project-news-prefix
						      ;; (nth 3 x)
						      ;; "': "
						      ;; (nth 1 x) "\t")) superman-view-project-news))))

;; (defun superman-view-project-set-news-keys ()
  ;; (mapcar
   ;; '(lambda (x)
      ;; (define-key superman-view-mode-map
	;; (concat superman-view-project-news-prefix (nth 3 x))
	;; (nth 2 x)))
   ;; superman-view-project-news))

;; (superman-view-project-set-action-keys)	
  

(defvar superman-document-category-separator '32 "Symbol for separating categories in document views.
See `org-agenda-block-separator'. Set to '0 to get a funny line.
Can also be set to (string-to-char \"~\") with any string in place of ~.")

;;}}}
;;{{{ Trim strings and links

(defun superman-trim-string (str &rest args)
  "Trim string STR to a given length by either calling substring
or by adding whitespace characters."
  (let* ((slen (length str))
	 (len (car args))
	 (diff (- len slen)))
    (if (> diff 0)
	(concat str (make-string diff (string-to-char " ")))
      (substring str 0 len))))

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
    (if (string-match org-link-re-with-space link)
	(concat "[[" link "]["
		(superman-trim-string link len) "]]"))))

(defun superman-trim-bracketed-filename (file &rest args)
  ;;  Links to files
  (string-match org-bracket-link-regexp file)
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
    trimmed-file-name))

(defun superman-trim-filename (filename &rest args)
  ;;  raw filenames
  (let ((linkname (file-name-nondirectory filename))
	(len (car args)))
    (when (string= linkname "") ;; for directories show the mother
      (setq linkname (file-name-nondirectory (directory-file-name filename))))
    (org-make-link-string
     filename
     (superman-trim-string linkname len))))



(defvar superman-view-current-project nil "Buffer local project variable" )
(make-variable-buffer-local 'superman-view-current-project)

(defun superman-view-current-project ()
  "Identifies the project associated with the current view buffer
and sets the variable superman-view-current-project."
  (or superman-view-current-project
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "^\\(Project\\|Documents\\):[ \t]*\\(.*\\)[ \t]*$" nil t)
	    (let ((pro (assoc (match-string-no-properties 2)
			      superman-project-alist)))
	      (if pro
		  (setq superman-view-current-project pro)
		(error (concat "Cannot find project " pro "in superman-project-alist."))))
	      (error "Malformed header of project view buffer: cannot identify project")))))

(defun superman-view-control (project)
  "Insert the git repository if project is git controlled
and the keybinding to initialize git control otherwise."
  (let ((pro (or project (superman-view-current-project)))
	(control (if (superman-git-p (concat (superman-get-location pro) (car pro)))
		     (concat "Control: Git repository at "(concat (superman-get-location pro) (car pro)))
		 "Control: not set. <> press `I' to initialize git")))
    (put-text-property 0 (length "Control: ") 'face 'org-level-2 control)
    control))

(defun superman-view-others (project)
  "Insert the names and emails of the others (if any)." 
  (let ((pro (or project (superman-view-current-project)))
	(others (superman-get-others pro)))
    (if others
	(let ((key "Others: "))
	  (put-text-property 0 (length key) 'face 'org-level-2 key)
	  (concat key others "\n"))
      "")))

;;}}}
;;{{{ Marking elements

(defun superman-toggle-mark (&optional on dont-move)
  "Toggle mark for item at point in project view.
If ON is non-nil keep mark for already marked items.
If DONT-MOVE is non-nil stay at item."
  (interactive)
  (if (org-agenda-bulk-marked-p)
      (unless on (org-agenda-bulk-unmark))
    (org-agenda-bulk-mark))
  (when dont-move (forward-line -1)))

(defun superman-mark-all ()
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      ;; (org-agenda-bulk-mark-all))))
      (superman-loop 'superman-toggle-mark (list 'on nil)))))

(defun superman-marked-p ()
  (org-agenda-bulk-marked-p))

;;}}}
;;{{{ Loops

(defun superman-loop (fun args &optional begin end marked)
  "Call function FUN on all items in the range BEGIN to END.
MARKED should be a cons where the car is the name of a text property
and the cdr the value, e.g. (face . 'bold).
The function is only run on items marked in this way."
  (let (loop-out
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
	  (when (or (not marked)
		    (superman-marked-p))
		    ;; (eq (get-text-property (point) (car marked)) (cadr marked)))
	    (setq loop-out
		  (append (list (apply fun args)) loop-out))))
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

(setq superman-views-delete-empty-cats t)
(setq superman-views-permanent-cats '("Documents"))


(defun superman-structure-loop (fun args)
  "Loop over headings in a superman-views buffer."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (while (outline-next-heading)
      (org-narrow-to-subtree)
      (apply fun args)
      (widen))))

(defun superman-finalize-cat (&optional cat)
  (let* ((cat (or cat (org-get-heading t t)))
	 ;; (list superman-finalize-cat-alist)
	 (rest (cdr (assoc cat superman-finalize-cat-alist)))
	 (fun (car rest))
	 (balls (nth 1 rest))
	 cnames)
    ;; treat elements (if any)
    (apply fun (eval balls))
    (goto-char (point-min))
    (if (next-single-property-change
	 (point-at-eol) 'org-marker)
	(progn
	  (end-of-line)
	  ;; insert hot keys for section
	  (let ((hotkeys (superman-view-show-hot-keys
			  superman-view-project-hot-keys cat)))
	    (if (> (length hotkeys) 0)
		(insert "\n\n" hotkeys "\n\n")
	      (insert "\n\n")))
	  ;; insert column names for section
	  (let ((cols (apply 'superman-column-names
			     (list (eval (caddr rest)) (eval balls)))))
	    (insert (car cols))
	    (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-comment-face)
	    (org-back-to-heading)
	    (put-text-property (point-at-bol) (point-at-eol) 'columns (cadr cols)))
	  ;; insert column widths, number of items and highlight 
	  (goto-char (point-min))
	  (end-of-line)
	  (insert " [" (int-to-string (superman-count-items) ) "]")
	  (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2))
      (if (member cat superman-views-permanent-cats)
	  (progn
	    (end-of-line)
	    (insert " [0]")
	    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2))
	(if superman-views-delete-empty-cats
	    (kill-region (point-min) (point-max))))
      (end-of-line 2)
      (kill-region (point) (point-max))
  (goto-char (point-max)))))
  

(defun superman-column-names (names defaults)
  (let ((cnames "")
	col
	(ncols (length defaults))
	(cw 0)
	cwidth
	ball-name
	(c 0))
    (while (< c ncols)
      (setq col (superman-trim-string
		 ;; special or user defined column name
		 ;; given by superman-finalize-cat-alist entry
		 (cond ((nth c names))
		       ((stringp (setq ball-name (nth 0 (nth c defaults))))
			ball-name)
		       ((eq 'hdr ball-name) "Heading")
		       ((eq 'todo ball-name) "Status")
		       (t (symbol-name ball-name)))
		 (or (car (nth 2 (nth c defaults))) 23)))
      ;; width of this column (+ 2 is for "  "
      (setq cw (+ 2 cw (length col)))
      (setq cnames (concat cnames "  " col))
      (setq cwidth (append cwidth (list cw)))
      (setq c (+ 1 c)))
    (list cnames cwidth)))

(defun superman-finalize-documents (&rest balls)
  (superman-loop 'superman-format-item balls))

(defun superman-finalize-notes (&rest balls)
  (superman-loop 'superman-format-item balls))

(defun superman-finalize-data (&rest balls)
  (superman-loop 'superman-format-item balls))

(defun superman-finalize-meetings (&rest balls)
  (superman-loop 'superman-format-item balls))

(defun superman-finalize-tasks (&rest balls)
  (superman-loop 'superman-format-item balls))

(defun superman-finalize-mails (&rest balls)
  (superman-loop 'superman-format-item balls))

(defun superman-finalize-bookmarks (&rest balls)
  (superman-loop 'superman-format-item balls))

;;}}}
;;{{{ Project views

(defvar superman-finalize-cat-alist nil

  "List of functions and variables used to finalize superman-views.

Elements are of the form '(cat fun balls) where cat is the name
of the heading in which the function fun is applied with arguments given by
balls (a list).

A ball can have one of the following alternative forms:

 ('todo len) : the todo-status of the item is trimmed to length len
 ('todo fun args) : function fun is applied to the todo-status of the current item with arguments args 

 ('hdr len) : the heading of the item is trimmed to length len
 ('hdr fun args) : function fun is applied to the heading of the current item with arguments args 

 (prop len) : the property prop (a string) of the current item is trimmed to length len
 (prop fun args) : function fun is applied to the property prop (a string) of the current item with arguments args
")

(defun superman-view-project (&optional project)
  "View documents of the current project."
  (interactive)
  (let* ((pro (or project
		  superman-current-project
		  (superman-switch-to-project 'force nil t)))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (org-agenda-buffer-name (concat "*Project[" (car pro) "]*"))
	 (org-agenda-sticky nil)
	 (org-agenda-window-setup 'current-window)
	 (project-header (concat "Project: " (car pro)))
	 (cats superman-cats)
	 (cat-number-one (car cats))
	 (cmd-block
	  (mapcar '(lambda (cat)
		    (list 'tags (concat (cdr cat) "={.+}")
			  (let ((hdr (if (eq (car cat) (car cat-number-one))
					 (concat project-header "\n\n" "** " (car cat))
				       (concat "** " (car cat)))))
			    `((org-agenda-overriding-header ,hdr)))))
		  cats))
	 (org-agenda-custom-commands
	  `(("p" "view Project"
	     ,cmd-block
	     ((org-agenda-finalize-hook 'superman-finalize-view)
	      (org-agenda-block-separator superman-document-category-separator)
	      (org-agenda-view-columns-initially nil)
	      (org-agenda-buffer-name (concat "*Project[" ,(car pro) "]*"))
	      (org-agenda-files (quote (,(superman-get-index pro)))))))))
    (org-agenda nil "p")))

;;}}}
;;{{{ Finalizing project views

(setq superman-finalize-cat-alist
      '(("Documents" superman-finalize-documents superman-document-balls superman-document-columns)
	("Data" superman-finalize-data superman-data-balls)
	("Notes" superman-finalize-notes superman-note-balls)
	("Mail" superman-finalize-mails superman-mail-balls)
	("Tasks" superman-finalize-tasks superman-task-balls)
	("Bookmarks" superman-finalize-bookmarks superman-bookmark-balls)
	("Meetings" superman-finalize-meetings superman-meeting-balls)))

(setq superman-document-columns
      (list "Description" "GitStatus" "LastCommit" "FileName"))

(defun superman-get-git-status-face (str)
  (cond ((string-match "Committed" str ) 'font-lock-function-name-face)
	((string-match  "Modified" str) 'font-lock-warning-face)
	(t 'font-lock-comment-face)))

(setq superman-document-balls
      '((hdr nil (23))
	("GitStatus" nil (10) superman-get-git-status-face)
	("LastCommit" superman-trim-date (13) font-lock-type-face)
	;; ("FileName" superman-trim-bracketed-filename 23)
	("FileName" (lambda (x len) x) nil)))
(setq superman-meeting-balls
      '((hdr nil (23))
	("Date" superman-trim-date nil font-lock-type-face)
	;; ("Status" 10 nil)
	("Participants" nil (23))))
(setq superman-note-balls
      '((todo nil (7))
	("NoteDate" superman-trim-date (13) font-lock-type-face)
	(hdr nil (49))))
(setq superman-data-balls
      '(("CaptureDate" superman-trim-date (13) font-lock-type-face)
	(hdr nil (23))
	("DataFileName" (lambda (x len) x) nil)))
(setq superman-task-balls
      '((todo nil (7))
	("TaskDate" superman-trim-date (13) font-lock-type-face)
	(hdr nil (49))))
(setq superman-bookmark-balls
      '(("BookmarkDate" superman-trim-date (13) font-lock-type-face)
	(hdr superman-trim-string nil)
	("Link" superman-trim-link (48))))
(setq superman-mail-balls
      '((todo nil (7))
	("EmailDate" superman-trim-date (13) font-lock-type-face)
	(hdr nil (23))
	;; ("Attachment" superman-trim-link nil)
	("Link" superman-trim-link (48))))


(defun superman-trim-date (date &optional len)
  (let ((len (or len 13)))
    (if (string-match org-ts-regexp0 date)
	;; (setq org-display-custom-times t)
	(let ((age (abs (org-time-stamp-to-now date))))
	  (cond ((= age 0)
		 (setq date "today"))
		((= age 1)
		 (setq date "yesterday"))
		(t (setq date (concat (int-to-string age) " days ago"))))
	  (superman-trim-string date len))
      (superman-trim-string date len))))

(defun superman-format-item (&rest balls)
  (let* ((pom (org-get-at-bol 'org-hd-marker))
	 (text-props (text-properties-at (point)))
	 (item "")
	 (cols (list 0))
	 faces
	 beg)
    ;; get values from heading in index buffer
    (org-with-point-at pom
      (let ((hdr-comp (org-with-point-at pom (org-heading-components))))
	(while balls
	  (let* ((b (car balls))
		 type
		 (face-or-fun (nth 3 b))
		 (val (cond ((stringp (car b)) ;; assume b is a property
			     (setq type "prop")
			     (or (superman-get-property (point) (car b) 'inherit) "--"))
			    ((eq (car b) 'todo) 
			     (setq type "todo")
			     (setq face-or-fun 'superman-get-todo-face)
			     (nth 2 hdr-comp))
			    ((eq (car b) 'hdr) 
			     (setq type "hdr")
			     (setq face-or-fun 'font-lock-keyword-face)
			     (nth 4 hdr-comp))))
		 (fun (or (nth 1 b) 'superman-trim-string))
		 (args (if (nth 2 b) (nth 2 b) '(23)))
		 (it (concat "  " (apply fun val args)))
		 (f (cond ((facep face-or-fun)
			   face-or-fun)
			  ((functionp face-or-fun)
			   (funcall face-or-fun
				    (replace-regexp-in-string "^[ \t\n]+\\|[ \t\n]+$" "" it)))
			  (t nil))))
	    (setq cols (append cols (list (length it))))
	    (setq faces (append faces (list f)))
	    (setq item (concat item it)))
	  (setq balls (cdr balls)))))
    (beginning-of-line)
    (looking-at ".*")
    (replace-match item t t)
    (beginning-of-line)
    (add-text-properties (point-at-bol) (point-at-eol) text-props)
    (setq beg (point))
    (while cols
      (let* ((f (car faces)))
	(setq beg (+ beg (car cols)))
	(setq end (if (cadr cols) (+ beg (cadr cols)) (point-at-eol)))
	(if f (put-text-property beg end 'face f)))
      (setq cols (cdr cols))
      (setq faces (cdr faces)))))

(setq superman-cat-heads '(("Documents" "Documents:\n")))

(defun superman-project-view-header (pro)
  "Construct extra heading lines for project views."
  (let ((hdr  (concat "\n\n"
		      (superman-view-others pro)
		      (superman-view-control pro)
		      "\n"
		      (superman-view-show-hot-keys
			    superman-view-project-hot-keys))))
    hdr))


(defun superman-documents-view-header (pro)
  "Construct extra heading lines for project views."
  (let ((control (superman-view-control pro))
	(hotkeys (superman-view-hot-keys superman-view-documents-hot-keys)))
    (concat "\n" control (insert "\n\n" hotkeys "\n\n"))) "\n" )

(setq superman-cat-headers
      '(("Documents" . superman-documents-view-header)))

(defun superman-finalize-view (&optional cat)
  (let* ((org-startup-folded nil)
	 (bufferq-read-only nil)
	 (pro (superman-view-current-project))
	 (header (if cat
		     (apply (cdr (assoc (car cat) superman-cat-headers))
			    (list pro))
		   (superman-project-view-header pro))))
    (org-mode)
    (font-lock-mode -1)
    ;; insert header and highlight
    (goto-char (point-min))
    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
    (end-of-line)
    (when header
      (insert header))
    ;; finalizing cats
    (superman-structure-loop
     'superman-finalize-cat cat)
    ;; facings
    (save-excursion
      (goto-char (point-min))
      (while (or (org-activate-bracket-links (point-max)) (org-activate-plain-links (point-max)))
	(add-text-properties
	 (match-beginning 0) (match-end 0)
	 '(face org-link))))
    ;; default-dir
    (setq default-directory
	  (superman-project-home
	   (superman-view-current-project)))
    ;; minor-mode
    (cond ((not cat) (superman-project-view-mode-on))
	  ((string= (car cat) "Documents")
	   (superman-documents-view-mode-on))
	  (t (superman-view-mode-on)))))

;;}}}
;;{{{ Document views

(defun superman-view-documents (&optional project)
  "View documents of the current project."
  (interactive)
  (let* ((pro (or project
		  superman-current-project
		  (superman-switch-to-project 'force nil t)))
	 (org-agenda-buffer-name (concat "*Documents[" (car pro) "]*"))
	 (org-agenda-sticky nil)
	 (org-agenda-window-setup 'current-window)
	 (cats (progn
		 (superman-goto-project pro "Documents" nil)
		 (superman-property-values "category")))
	 (documents-header (concat "Documents: " (car pro) "\n\n"))
	 (cats-and-one-dog (append `(,(car pro)) cats))
	 (cmd-block
	  (if cats 
	      (mapcar '(lambda (cat)
			 (list 'tags (concat "FileName={.+}" "+" "CATEGORY=\"" cat "\"")
			       (let ((hdr (if (eq cat (car cats-and-one-dog))
					      (concat documents-header (concat "\n** " cat ""))
					    (concat "** " cat ""))))
				 `((org-agenda-overriding-header ,hdr)))))
		      cats-and-one-dog)
	    `((tags "FileName={.+}"
		    ((org-agenda-overriding-header (concat ,documents-header "** Documents")))))))
	 (org-agenda-custom-commands
	  `(("d" "view Project-DOCUMENTS"
	     ,cmd-block
	     ;; ((org-agenda-finalize-hook 'superman-view-finalize-documents)
	     ((org-agenda-finalize-hook '(lambda () (superman-finalize-view '("Documents"))))
	      (org-agenda-block-separator superman-document-category-separator)
	      (org-agenda-view-columns-initially nil)
	      (org-agenda-buffer-name (concat "*Documents[" ,(car pro) "]*"))
	      (org-agenda-files (quote (,(superman-get-index pro))))
	      )))))
    (org-agenda nil "d")))

;;}}}
;;{{{ View commands (including git) 

(defun superman-next-entry ()
  (interactive)
  (goto-char
   (or (next-single-property-change (point-at-eol) 'org-marker)
       (point))))

(defun superman-previous-entry ()
  (interactive)
  (let ((pos (previous-single-property-change (point-at-bol) 'org-marker)))
    (when pos
	(progn (goto-char pos) (beginning-of-line)))))


(defun superman-new-document (&optional file-list)
  (interactive)
  (let* ((pro (superman-view-current-project))
	 (dir (expand-file-name (concat (superman-get-location pro) (car pro))))
	 (fl (or file-list `(,(read-file-name (concat "Add document: ") (file-name-as-directory dir))))))
    ;; FIXME need to write superman-get-documents and filter duplicates
    (save-window-excursion
      (superman-goto-project pro "Documents" 'create)
      (while fl
	(insert "\n*** " (file-name-nondirectory (file-name-sans-extension (car fl)))
		"\n:PROPERTIES:\n:"
		(superman-property 'filename) ": [["(car fl)"]]\n:"
		(superman-property 'gitstatus) ": Unknown\n:"
		(superman-property 'capturedate) ": ")
	(org-insert-time-stamp (current-time) t)
	(insert "\n:END:\n")
	(setq fl (cdr fl)))
      (save-buffer)))
  (org-agenda-redo))
;; (switch-to-buffer (other-buffer)))

(defun superman-new-task ()
  (interactive)
  (superman-capture-task (superman-view-current-project)))

(defun superman-new-meeting ()
  (interactive)
  (superman-capture-meeting (superman-view-current-project)))

(defun superman-new-data (&optional file-list)
  (interactive)
  (let* ((pro (superman-view-current-project))
	 (dir (expand-file-name (concat (superman-get-location pro) (car pro))))
	 (fl (or file-list `(,(read-file-name (concat "Add data: ") (file-name-as-directory dir))))))
    ;; FIXME need to write superman-get-documents and filter duplicates
    (save-window-excursion
      (superman-goto-project pro "Data" 'create)
      (while fl
	(insert "\n*** " (file-name-nondirectory (file-name-sans-extension (car fl)))
		"\n:PROPERTIES:\n:"
		"DataFileName" ": [["(car fl)"]]\n:"
		(superman-property 'gitstatus) ": Unknown\n:"
		(superman-property 'capturedate) ": ")
	(org-insert-time-stamp (current-time) t)
	(insert "\n:END:\n")
	(setq fl (cdr fl)))
      (save-buffer)))
  (org-agenda-redo))

(defun superman-new-note ()
  (interactive)
  (superman-capture-note (superman-view-current-project))
  (superman-view-project))


(defun superman-new-bookmark ()
  (interactive)
  (superman-capture-bookmark (superman-view-current-project))
  (superman-view-project))

(defun superman-view-git-diff ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
  (vc-diff file "HEAD")))

(defun superman-view-git-ediff ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
    (vc-ediff file "HEAD")))


(defun superman-view-git-annotate (&optional arg)
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
    (vc-annotate (org-link-display-format file) "HEAD")
  ))


(defun superman-view-git-grep (&optional arg)
  (interactive)
  (let ((pro (superman-view-current-project))
	(st (read-string "Grep: ")))
    (if arg
	(vc-git-grep st)
	(vc-git-grep st "*" (concat (superman-get-location pro) (car pro))))))

(defun superman-view-git-history ()
  (interactive)
  (let ((pro (superman-view-current-project)))
    (vc-print-log-internal 'Git (list (concat (superman-get-location pro) (car pro))) nil nil 2000)))

(defun superman-view-index ()
  (interactive)
  (let* ((pom (org-get-at-bol 'org-hd-marker))
	 (index (superman-get-index (superman-view-current-project)))
	 (ibuf (if pom (marker-buffer pom)
		 (get-file-buffer index)))
	 (iwin (when ibuf (get-buffer-window ibuf nil))))
    (if (and ibuf iwin)
	(select-window (get-buffer-window ibuf nil))
      ;; FIXME this should be customizable
      (split-window-vertically)
      (other-window 1)
      (if ibuf (switch-to-buffer ibuf)
	(find-file index)))
    (when pom (goto-char pom))))

(defun superman-view-file-list ()
  (interactive)
  (let ((pro (superman-view-current-project)))
    (split-window-vertically)
      (other-window 1)
    (superman-file-list pro)))

(defun superman-view-git-init ()
  (interactive)
  (let ((pro (superman-view-current-project)))
    (superman-git-init-directory (concat (superman-get-location pro) (car pro)))
    (org-agenda-redo)))

;; (defun superman-view-set (&optional dont-redo)
  ;; "Set a property for document at point."
  ;; (interactive)
  ;; (let ((prop "Property"
  ;; (org-entry-put 
  ;; (org-agenda-redo))

;; (defun superman-view-mark-item ()
  ;; (if (org-get-at-bol 'org-hd-marker)
      ;; (let ((buffer-read-only nil))
	;; (add-text-properties (point-at-bol) (point-at-eol) '(:org-view-mark t)))))

;; (defun superman-view-unmark-item ()
  ;; (if (org-get-at-bol 'org-hd-marker)
      ;; (let ((buffer-read-only nil))
	;; (add-text-properties (point-at-bol) (point-at-eol) '(:org-view-mark nil)))))

(defun superman-hot-return ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (b (org-get-heading t t)))
    ;; (save-excursion
    ;; (goto-char (previous-single-property-change (point-at-eol) 'org-agenda-structural-header))
    ;; (beginning-of-line)
    ;; (looking-at "\\[\\([a-zA-Z]+\\)\\]")
    ;; (match-string-no-properties 1))))
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
	  ;; (message "Follow-link"))
	  (t (org-open-link-from-string
	      (superman-get-property m "filename"))))))

(defun superman-view-git-log (arg)
  (interactive "p")
  (superman-git-log-at-point arg))

(defun superman-view-git-log-decorationonly (arg)
  (interactive "p")
  (superman-git-log-decorationonly-at-point arg))

(defun superman-view-git-search (arg)
  (interactive "p")
  (superman-git-search-at-point arg))

(defun superman-view-git-set-status (&optional save redo check)
  (interactive)
  (let ((file (superman-filename-at-point t))
	(pom  (org-get-at-bol 'org-hd-marker)))
    (when
	file
      (superman-git-set-status pom file check)
      (when save (superman-view-save-hd-buffer))
      (when redo (org-agenda-redo)))))

(defun superman-view-save-hd-buffer ()
  (save-excursion
    (goto-char (point-min))
    (org-agenda-next-item 1)
    (set-buffer
     (marker-buffer (org-get-at-bol 'org-hd-marker)))
    (save-buffer)))

(defun superman-view-update-all ()
  "Update git status for all entries (that have a filename)."
  (interactive)
  (superman-loop 'superman-view-git-set-status (list nil nil nil))
  (superman-view-save-hd-buffer)
  (org-agenda-redo))

(defun superman-view-update ()
  (interactive)
  (superman-view-git-set-status 'save 'redo nil))

;; (defun superman-summary-save-and-redo ()
  ;; "Save buffer associated with current item. Then redo agenda view."
  ;; (interactive)
  ;; (superman-view-save-hd-buffer)
  ;; (org-agenda-redo))


;; (defun superman-view-new-document ()
  ;; (unless superman-view-mode (error "Can only be called from document view mode."))
  ;; (let* ((pro (superman-view-current-project))
	 ;; (filename (read-file-name "Document file"
				   ;; (concat (superman-get-location pro) (car pro)))))
    ;; (save-excursion
      ;; (superman-goto-project-documents pro
      ;; (find-file (superman-get-index pro)))))
    
(defun superman-view-git-add (&optional dont-redo)
  "Add but not commit the file given by the filename property
of the item at point.

If dont-redo the agenda is not reversed."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename)))))
    (superman-git-add file dir nil nil)
    (superman-view-git-set-status 'save (not dont-redo) nil)))

(defun superman-view-git-commit (&optional dont-redo)
  "Add and commit the file given by the filename property
of the item at point.

If dont-redo the agenda is not reversed."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename)))))
    (superman-git-add file dir 'commit nil)
  (superman-view-git-set-status 'save (not dont-redo) nil)))

(defun superman-view-git-add-all (&optional dont-redo)
  (interactive)
  (superman-loop 'superman-view-git-add (list 'dont) nil nil 'marked)
  (unless dont-redo (org-agenda-redo)))

(defun superman-view-git-commit-all (&optional commit dont-redo)
  (interactive)
  (let* ((pro (superman-view-current-project))
	 (dir (concat (superman-get-location pro) (car pro))))
    ;; (files (superman-loop 'superman-filename-at-point (list nil))))
    (superman-view-git-add-all 'dont)
    (superman-git-commit dir (concat "Git commit message for selected files in " dir ": "))
    (superman-view-update-all)
    (unless dont-redo (org-agenda-redo))))

;;}}}
;;{{{ view-mode and keybindings

(defvar superman-view-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
   
(define-minor-mode superman-view-mode 
     "Toggle org projectmanager document view mode.
                   With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
                   turn it off.
                   
                   Enabling superman-view mode electrifies the column view for documents
                   for git and other actions like commit, history search and pretty log-view."
     :lighter " S-V"
     :group 'org
     :keymap 'superman-view-mode-map)

(defun superman-view-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-view-mode t))

(defun superman-view-show-hot-keys (keys &optional cat)
  "Show keybindings in project view header or in section CAT."
  (let ((hot-key-string "")
	(hot-keys keys))
    (while hot-keys
      (let* ((x (car hot-keys))
	     (f (intern (concat "superman-" (or cat "project") "-hot-" x))))
	(if (or (not cat) (fboundp f))
	    (setq hot-key-string
		  (concat hot-key-string
			  (concat "" 
				  x
				  ": "
				  (if (boundp f)
				      (eval f)
				    (symbol-name f))
				  "  ")))))
      (setq hot-keys (cdr hot-keys)))
    (unless cat
      (setq hot-key-string (concat "Keys: " hot-key-string))
      (put-text-property 0 (length "Keys: ") 'face 'org-level-2 hot-key-string))
    hot-key-string))

(defun superman-view-hot-keys (keys)
  "Show hot keybindings in header of project view."
  ;; FIXME: this should be made window width adaptive
  (let ((hot-key-string "")
	(hot-keys keys))
    (while hot-keys
      (let ((x (car hot-keys)))
	(setq hot-key-string
	      (concat hot-key-string
		      (concat "" 
			      (nth 3 x)
			      ": "
			      (nth 1 x) "  "))))
      (setq hot-keys (cdr hot-keys)))
    hot-key-string))

(define-key superman-view-mode-map [return] 'superman-hot-return) 
(define-key superman-view-mode-map "m" 'superman-toggle-mark)
(define-key superman-view-mode-map "n" 'superman-next-entry)
(define-key superman-view-mode-map "p" 'superman-previous-entry)
(define-key superman-view-mode-map "r" 'org-agenda-redo)
(define-key superman-view-mode-map "!" 'superman-goto-shell)
(define-key superman-view-mode-map "?" 'superman-view-show-help)
(define-key superman-view-mode-map "I" 'superman-view-git-init)
;;}}}
;;{{{ document view mode

(defvar superman-documents-view-mode-map
  (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-documents-view-mode' commands.")

(define-minor-mode superman-documents-view-mode 
  "Toggle superman documents view mode.
 With argument ARG turn superman-documents-view-mode on if ARG
is positive, otherwise turn it off."
  :lighter " *S*-docs"
  :group 'org
  :keymap 'superman-documents-view-mode-map)

(defun superman-documents-view-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-documents-view-mode t))

(defun superman-documents-view-set-hot-keys ()
  (mapcar
   '(lambda (x)
      (define-key superman-documents-view-mode-map
	(nth 3 x)
	(nth 2 x)))
   superman-view-documents-hot-keys))

(setq superman-view-documents-hot-keys
      '((list "grep" superman-view-git-grep "g")
	(list "history" superman-view-git-history "h")
	(list "grep" superman-view-git-grep "g")
	(list "annotate" superman-view-git-annotate "v")
	(list "Index" superman-view-index "i")
	(list "diff" superman-view-git-diff "d")
	(list "Document" superman-new-document "D")
	(list "Add" superman-view-git-add-all "A")
	(list "add" superman-view-git-add "a")
	(list "Search" superman-view-git-search "S")
	(list "update" superman-view-update "u")
	(list "Update" superman-view-update-all "U")
	(list "commit" superman-view-git-commit "c")
	(list "Commit" superman-view-git-commit-all "C")
	(list "log" superman-view-git-log "l") 
	(list "Log" superman-view-git-log-decorationonly "L")))

(superman-documents-view-set-hot-keys)


;;}}}
;;{{{ project view mode

;; (setq superman-project-view-mode-map
  ;; (copy-keymap superman-view-mode-map))
(defvar superman-project-view-mode-map
  (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-project-view-mode' commands.")

(define-minor-mode superman-project-view-mode 
  "Toggle superman project view mode.
 With argument ARG turn superman-project-view-mode on if ARG
is positive, otherwise turn it off."
  :lighter " *S*-pro"
  :group 'org
  :keymap 'superman-project-view-mode-map)

(defun superman-project-view-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-project-view-mode t))

(defvar superman-view-project-hot-keys nil "Keybindings visible in project view")

(setq superman-view-project-hot-keys
      '("r" "s" "i" "F" "N" "D" "T" "B" "M" "P" "U" "c" "C"))
;; '("i" "f" "F" "N" "P" "u" "U" "m" "M" "c" "C" "B" "v" "V" "D"))

(defun superman-view-project-set-hot-keys ()
  (mapcar
   '(lambda (x)
      (define-key superman-project-view-mode-map
	x
	(intern (concat "superman-hot-" x))))
   superman-view-project-hot-keys))


(defun superman-current-heading ()
  "Safely call `outline-back-to-heading' and return heading. If error return nil."
  (condition-case nil
      (save-excursion
	(car (split-string (org-get-heading) "[ ]+")))
    (error nil)))

(defun superman-view-choose-hot-key (key)
  (let* ((cat (superman-current-heading))
	 (S-call (intern (concat "superman-project-hot-" key)))	 ;; use project key if no hot key is defined in section
	 (hot-cat (if cat (intern (concat "superman-" cat "-hot-" key))
		    (intern (concat "superman-project-hot-" key)))))
    (cond ((and cat (fboundp hot-cat))
	   (funcall hot-cat))
	  ;; (condition-case nil
	  ;; (funcall hot-cat)
	  ;; (error (progn (message
	  ;; (concat "Call to " hot-cat " resulted in an error. ")
	  ;; nil)))))
	  ((fboundp S-call) (funcall S-call))
	  (t (message (concat "Hot-key "key" not defined (in this section)"))))))
  
(defun superman-hot-i () (interactive) (superman-view-choose-hot-key "i"))
(defun superman-hot-f () (interactive) (superman-view-choose-hot-key "f"))
(defun superman-hot-F () (interactive) (superman-view-choose-hot-key "F"))
(defun superman-hot-m () (interactive) (superman-view-choose-hot-key "m"))
(defun superman-hot-M () (interactive) (superman-view-choose-hot-key "M"))
(defun superman-hot-N () (interactive) (superman-view-choose-hot-key "N"))
(defun superman-hot-d () (interactive) (superman-view-choose-hot-key "d"))
(defun superman-hot-D () (interactive) (superman-view-choose-hot-key "D"))
(defun superman-hot-B () (interactive) (superman-view-choose-hot-key "B"))
(defun superman-hot-T () (interactive) (superman-view-choose-hot-key "T"))
(defun superman-hot-s () (interactive) (superman-view-choose-hot-key "s"))
(defun superman-hot-c () (interactive) (superman-view-choose-hot-key "c"))
(defun superman-hot-C () (interactive) (superman-view-choose-hot-key "C"))
(defun superman-hot-v () (interactive) (superman-view-choose-hot-key "v"))
(defun superman-hot-V () (interactive) (superman-view-choose-hot-key "V"))
(defun superman-hot-U () (interactive) (superman-view-choose-hot-key "U"))
(defun superman-hot-P () (interactive) (superman-view-choose-hot-key "P"))
(defun superman-hot-r () (interactive) (superman-view-choose-hot-key "r"))

(fset 'superman-project-hot-i 'superman-view-index)
(setq superman-project-hot-i "index")
(fset 'superman-project-hot-! 'superman-goto-shell)
(setq superman-project-hot-! "shell")
(fset 'superman-project-hot-F 'superman-view-file-list)
(setq superman-project-hot-F "FileList")
(fset 'superman-project-hot-f 'org-agenda-follow-mode)
(setq superman-project-hot-f "follow")
(fset 'superman-project-hot-P 'superman-git-push)
(setq superman-project-hot-P "GitPush")
;; (fset 'superman-project-hot-U 'superman-unison)
;; (setq superman-project-hot-U "Unison")
(fset 'superman-project-hot-U 'superman-update-all)
(setq superman-project-hot-U "Update")
(fset 'superman-project-hot-r 'org-agenda-redo)
(setq superman-project-hot-r "redo")
(fset 'superman-project-hot-N 'superman-new-note)
(setq superman-project-hot-N "Note")
(fset 'superman-project-hot-d 'superman-new-data)
(setq superman-project-hot-d "data")
(fset 'superman-project-hot-D 'superman-new-document)
(setq superman-project-hot-D "Document")
(fset 'superman-project-hot-M 'superman-new-meeting)
(setq superman-project-hot-M "Meeting")
(fset 'superman-project-hot-T 'superman-new-task)
(setq superman-project-hot-T "Task")
(fset 'superman-project-hot-s 'superman-sort-section)
(setq superman-project-hot-s "sort")
(fset 'superman-project-hot-B 'superman-new-bookmark)
(setq superman-project-hot-B "Bookmark")
(fset 'superman-project-hot-V 'superman-toggle-view)
(setq superman-project-hot-V "toggleView")
(fset 'superman-project-hot-c '(lambda () (message "Nothing to commit in this section")))
(fset 'superman-project-hot-C 'superman-view-git-commit-all)
(setq superman-project-hot-C "GitCommit")

(fset 'superman-Documents-hot-c 'superman-view-git-commit)
(setq superman-Documents-hot-c "CommitFile")

(fset 'superman-project-hot-U 'superman-view-update-all)
(setq superman-project-hot-U "UpdateGit")

(fset 'superman-Documents-hot-U 'superman-view-update-all)
(setq superman-Documents-hot-U "UpdateGit")

(fset 'superman-Documents-hot-m 'superman-toggle-mark)
(setq superman-Documents-hot-m "mark-item")

(fset 'superman-Documents-hot-M 'superman-mark-all)
(setq superman-Documents-hot-M "mark-all")

(fset 'superman-Data-hot-c 'superman-view-git-commit)
(setq superman-Data-hot-c "GitCommit")

(superman-view-project-set-hot-keys)


;; (setq superman-view-project-hot-keys
;; (list "Shell" superman-goto-shell "!")
;; (list "Index" superman-view-index "i")
;; (list "File-list" superman-view-file-list "F")
;; (list "Git-push" superman-git-push "P")
;; (list "Unison" superman-unison "U")
;; (list "Note" superman-new-note "N")
;; (list "Document" superman-new-document "D")
;; (list "Meeting" superman-new-meeting "M")
;; (list "Task" superman-new-task "T")
;; (list "ToggleView" superman-toggle-view "V")
;; (list "Bookmark" superman-new-bookmark "B")
;; (list "commit" superman-view-git-commit "c")
;; (list "Commit" superman-view-git-commit-all "C")

;;}}}
;;{{{ sorting

(defun superman-sort-section (&optional field)
  (interactive "P")
  (let* ((buffer-read-only nil)
	 ;; (col (if field (if (numberp field) field nil)))
	 (cc (current-column))
	 (col 1)
	 cols
	 next
	 beg end sec-end)
    (save-excursion
      (when (superman-current-heading)
	(org-back-to-heading)
	(setq cols (get-text-property (point) 'columns))
	(while (> cc (car cols))
	  (setq col (+ col 1))
	  (setq cols (cdr cols)))
	(goto-char (next-single-property-change (point-at-eol) 'org-marker))
	(setq beg (point))
	(if (outline-next-heading)
	    (setq sec-end (point))
	  (setq sec-end (point-max)))
	(goto-char beg)
	(while (not end)
	  (setq next
		(condition-case nil
		    (next-single-property-change (point-at-eol) 'org-marker)
		  (error nil)))
	  (if (or (not next) (> next sec-end))
	      (progn (end-of-line)
		     (setq end (point)))
	    (goto-char next)))
	(if col
	    (sort-fields-1 col beg end
			   (function (lambda ()
				       (sort-skip-fields col)
				       nil))
			   (function (lambda () (skip-chars-forward "^ \t\n"))))
	  (sort-lines nil beg end))))
    (if col (forward-char cc))))
      

(defun superman-sort-by-status (a b)
  (let ((A  (substring-no-properties a 0 12))
	(B  (substring-no-properties b 0 12)))
    (if (string= A B) nil 
      (if (string-lessp A B)
	  1 -1))))
    
;; see org-agenda-manipulate-query
(defun superman-sort-superman ()
  (let* ((options (cadr (cadar (cddr org-agenda-redo-command))))
	 (column 1)
	 (org-agenda-cmp-user-defined 'superman-sort-by-status))
    ;; (new-options
    ;; (append options
    ;; '((org-agenda-sorting-strategy '(user-defined-up))))))
    ;; (setcdr (cadr (cadar (cddr org-agenda-redo-command))) new-options)
    (org-agenda-redo)))

;;}}}

;;{{{ help 

(defun superman-popup-tip (msg)
  (save-excursion
    (goto-char (point-min))
    (popup-tip msg)))

(defvar superman-help-fun 'superman-popup-tip 
  "Function used to display help. Possible values 'tooltip-show or 'popup-tip (depends on popup.el)") 
(defun superman-view-show-help ()
  (interactive)
  (let ((msg
	(concat 
	 "------------------\n"
	 "[return]:\t\t Open file at point\n"
	 "[l]:     \t\t Show git log ([L] tags only. Prefix-arg: limit)\n"
	 "[u]:    \t\t Update git status ([U] updates all files)\n"
	 "[a]:    \t\t Add to git repository ([A] add all files. Prefix-arg: limit)\n"
	 "[c]:    \t\t Commit  ([C] commit all files)\n"
	 "[I]:    \t\t Init git repository\n"
	 "[n]:    \t\t New file (add exisiting or new file to document list)\n"
	 "[S]:    \t\t Search for revision introducing change (Prefix-arg: limit)\n"
	 "[v]:    \t\t View annotated file\n"
	 "[g]:    \t\t Grep in git controlled files (Prefix-arg: fine-tune)\n"
	 "[d]:    \t\t Show difference between revisions ([D] ediff)\n"
	 "[!]:     \t\t Shell\n"
	 "------------------")))
    ;;	"[q]:    \t\t Quit view mode\n"
    (funcall superman-help-fun msg)))

;;}}}

(provide 'superman-views)

;;; superman-summary.el ends here


