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
balls (a list).

A ball can have one of the following alternative forms:

 (key fun args face) : function fun (default is superman-trim-string) is applied
with arguments args to the key, which is either of 'hdr (the header) 'todo (todo-status)
or \"prop\" (a property of the heading) of the current item.
Optionaly face is a face or a function which gets the value of key as argument to
determine the face.")

(setq superman-finalize-cat-alist
      '(("Documents" superman-document-balls superman-document-columns)
	("Data" superman-data-balls)
	("Notes" superman-note-balls)
	("Mail" superman-mail-balls)
	("Tasks" superman-task-balls)
	("Bookmarks" superman-bookmark-balls)
	("Meetings" superman-meeting-balls)))

(setq superman-document-columns
      (list "Description" "GitStatus" "LastCommit" "FileName"))
(setq superman-document-balls
      '((hdr ("trim" nil (23)) ("face" font-lock-function-name-face) ("name" "Description"))
	("GitStatus" ("trim" nil (10)) ("face" superman-get-git-status-face))
	("LastCommit" ("trim" superman-trim-date (13)) ("face" font-lock-string-face))
	("FileName" ("trim" (lambda (x len) x) nil))))
(setq superman-meeting-balls
      '((hdr ("trim" nil (23)) ("face" font-lock-function-name-face))
	("Date" ("trim" superman-trim-date nil)
	 ("face" font-lock-string-face))
	("Participants" ("trim" nil (23)))))
(setq superman-note-balls
      '((todo ("trim" nil (7)))
	("NoteDate" ("trim" superman-trim-date (13))
	 ("face" font-lock-string-face))
	(hdr ("trim" nil (49))
	     ("face" font-lock-function-name-face))))
(setq superman-data-balls
      '(("CaptureDate"
	 ("trim" superman-trim-date (13))
	 ("face" font-lock-string-face))
	(hdr ("trim" nil (23)) ("face" font-lock-function-name-face))
	("DataFileName" ("trim" (lambda (x len) x) nil))))
(setq superman-task-balls
      '((todo ("trim" nil (7)))
	("TaskDate"
	 ("trim" superman-trim-date (13))
	 ("face" font-lock-string-face))
	(hdr ("trim" nil (49)) ("face" font-lock-function-name-face))))
(setq superman-bookmark-balls
      '(("BookmarkDate" ("trim" superman-trim-date (13)) ("face" font-lock-string-face))
	(hdr ("trim" superman-trim-string nil) ("face" font-lock-function-name-face))
	("Link" ("trim" superman-trim-link (48)))))
(setq superman-mail-balls
      '((todo ("trim" nil (7)))
	("EmailDate" superman-trim-date (13) font-lock-string-face)
	(hdr ("trim" nil (23)) ("face" font-lock-function-name-face))
	("Link" ("trim" superman-trim-bracketed-filename (48)))))

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

(defun superman-view-current-project ()
  "Identifies the project associated with the current view buffer
and sets the variable superman-view-current-project."
  (let* ((nick (get-text-property (point-min) 'nickname))
	(pro (when nick (assoc nick superman-project-alist))))
    (if pro
	(setq superman-view-current-project pro)
      (error "Malformed header of project view buffer: cannot identify project"))))

(defun superman-view-control (project)
  "Insert the git repository if project is git controlled
and the keybinding to initialize git control otherwise."
  (let* ((loc (get-text-property (point-min) 'git-dir))
	 (control (if (superman-git-p loc)
		      (concat "Control: Git repository at "
			      ;;FIXME: would like to have current git status
			      (superman-git-toplevel loc))
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



(defun superman-current-cat ()
  (let ((pos (previous-single-property-change (point) 'cat)))
    (when pos 
      (get-text-property
       (save-excursion
	 (goto-char pos)
	 (beginning-of-line)
	 (point))
       'cat))))

(defun superman-current-subcat ()
  (let* ((cat-pos (previous-single-property-change (point) 'cat))
	 (subcat-pos (previous-single-property-change (point) 'subcat))
	 (subp (and subcat-pos (> subcat-pos cat-pos))))
    (when cat-pos
      (get-text-property
       (save-excursion
	 (goto-char (if subp subcat-pos cat-pos))
	 (beginning-of-line)
	 (point))
       (if subp 'subcat 'cat)))))

(defun superman-current-subcat-pos ()
  (let* ((cat-pos (previous-single-property-change (point) 'cat))
	 (subcat-pos (previous-single-property-change (point) 'subcat))
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
;;{{{ Finalizing

(defun superman-finalize-cat (&optional cat)
  (let* ((cat (or cat (org-get-heading t t)))
	 (gear (cdr (assoc cat superman-finalize-cat-alist)))
	 (balls (eval (nth 0 gear)))
	 (names (eval (nth 1 gear)))
	 next
	 line
	 cnames)
    ;; format elements (if any)
    ;; region is narrowed to section
    (goto-char (point-min))
    (while (setq next (next-single-property-change (point-at-eol) 'org-hd-marker))
      (goto-char next)
      (setq line (superman-format-thing (org-get-at-bol 'org-hd-marker) balls))
      (beginning-of-line)
      (looking-at ".*")
      (replace-match line t t))
    (goto-char (point-min))
    (if (next-single-property-change
	 (point-at-eol) 'org-marker)
	;; insert hot keys
	(progn
	  (end-of-line)
	  (let ((hotkeys (superman-view-show-hot-keys cat)))
	    (if (> (length hotkeys) 0)
		(insert "\n\n" hotkeys "\n\n")
	      (insert "\n\n")))
	  ;; insert column names for section
	  (insert (superman-column-names balls))
	  ;; count items and highlight 
	  (goto-char (point-min))
	  (end-of-line)
	  (insert " [" (int-to-string (superman-count-items) ) "]")
	  (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2))
      ;; delete some empty cats
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

(defun superman-column-names (balls)
  (let ((cols
	 (superman-format-thing
	  (list "columns"
		(mapcar '(lambda (b)
			   (cons (car b)
				 (or (cadr (assoc "name" (cdr b)))
				 (cond ((stringp (car b)) (car b))
				       ((eq (car b) 'hdr) "Title")
				       ((eq (car b) 'todo) "Status")))))
			balls)) balls 'no-face)))
    (put-text-property 0 (length cols) 'face 'font-lock-comment-face cols)
    cols))


;; (defun superman-finalize-view (&optional cat)
  ;; (let* ((org-startup-folded nil)
	 ;; (bufferq-read-only nil)
	 ;; (pro (superman-view-current-project))
	 ;; (header (if cat
		     ;; (apply (cdr (assoc (car cat) superman-cat-headers))
			    ;; (list pro))
		   ;; (superman-project-view-header pro))))
    ;; (org-mode)
    ;; (font-lock-mode -1)
    ;; ;; insert header and highlight
    ;; (goto-char (point-min))
    ;; (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
    ;; (end-of-line)
    ;; (when header
      ;; (insert header))
    ;; ;; finalizing cats
    ;; (superman-structure-loop
     ;; 'superman-finalize-cat cat)
    ;; ;; facings
    ;; (save-excursion
      ;; (goto-char (point-min))
      ;; (while (or (org-activate-bracket-links (point-max)) (org-activate-plain-links (point-max)))
	;; (add-text-properties
	 ;; (match-beginning 0) (match-end 0)
	 ;; '(face org-link))))
    ;; ;; default-dir
    ;; (setq default-directory
	  ;; (superman-project-home
	   ;; (superman-view-current-project)))
    ;; ;; minor-mode
    ;; (superman-view-mode-on)))  

;;}}}
;;{{{ Project views

(defun superman-view-project (&optional project)
  "Display the current project in a view buffer."
  (interactive)
  (let* ((pro (if (stringp project)
		  (assoc project superman-project-alist)
		(or project
		    superman-current-project
		    (superman-switch-to-project 'force nil t))))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (vbuf (concat "*Project[" (car pro) "]*"))
	 ;; (org-agenda-window-setup 'current-window)
	 (project-header (concat "Project: " (car pro)))
	 (index (superman-get-index pro))
	 (ibuf (or (get-file-buffer index) (find-file index)))
	 (cats superman-cats)
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
	     (gear (cdr (assoc cat superman-finalize-cat-alist)))
	     (balls (eval (nth 0 gear)))
	     (names (eval (nth 1 gear)))
	     cat-head
	     (count 0)
	     line)
	;; back to vbuf
	(set-buffer vbuf)
	(setq cat-head (point))
	;; move to ibuf
	(when (superman-goto-project pro cat nil)
	  ;; format elements (if any)
	  ;; region is narrowed to section
	  (goto-char (point-min))
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
	      (with-current-buffer vbuf (insert line "\n"))))
	  (widen)
	  (set-buffer vbuf)
	  (goto-char cat-head)
	  (insert "\n** " cat "\n")
	  (forward-line -1)
	  (beginning-of-line)
	  (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
	  (put-text-property (point-at-bol) (point-at-eol) 'cat cat)
	  (put-text-property (point-at-bol) (point-at-eol) 'display (concat "★ " cat))
	  (if (or (member cat superman-views-permanent-cats) (> count 0))
	      (progn (end-of-line)
		     (insert " [" (int-to-string count) "]")
		     ;; insert hot-keys or blank line
		     (end-of-line)
		     (let ((hotkeys (superman-view-show-hot-keys cat)))
		       (if (> (length hotkeys) 0)
			   (insert "\n" hotkeys "\n")
			 (insert "\n")))
		     ;; insert column names 
		     (insert (superman-column-names balls)))
	    (delete-region (point) (point-max)))
	  (goto-char (point-max)))
	(widen)
	(setq cats (cdr cats))))
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
  (superman-view-git-update-status nil nil 'dont)
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
	 (trim-info (assoc "trim" (cdr ball)))
	 (trim-function (or (nth 1 trim-info)
			    'superman-trim-string))
	 (trim-args (or (nth 2 trim-info) '(23)))
	 (trimmed-string
	  (concat "  " ;; column sep
		  (apply trim-function raw-string trim-args)))
	 (face
	  (or (cadr (assoc "face" ball))
	      (get-text-property 0 'face raw-string))))
    ;; remove all existing text-properties
    (set-text-properties 0 (length trimmed-string) nil trimmed-string)
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
    (put-text-property 0 ilen 'balls balls item)
    ;; text property: columns
    (put-text-property 0 ilen 'columns column-widths item)
    item))


(defun superman-project-view-header (pro)
  "Construct extra heading lines for project views."
  (let ((hdr  (concat "\n\n"
		      (superman-view-others pro)
		      (superman-view-control pro)
		      "\n"
		      (superman-view-show-hot-keys))))
    hdr))


(defun superman-documents-view-header (pro)
  "Construct extra heading lines for project views."
  (let ((control (superman-view-control pro))
	(hotkeys (superman-view-hot-keys superman-view-documents-hot-keys)))
    (concat "\n" control (insert "\n\n" hotkeys "\n\n"))) "\n" )

;;}}}
;;{{{ Moving items around

(defun superman-one-up (&optional down)
  (interactive "P")
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (when marker
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
		(org-promote)))))	      
      (superman-redo)
      (forward-line (if down 1 -1)))))

(defun superman-one-down ()
  (interactive)
  (superman-one-up 1))


;;}}}
;;{{{ View commands (including redo and git) 

(defun superman-redo ()
  (interactive)
  (let ((curline (progn
		   (beginning-of-line)
		   (count-lines 1 (point))))
	cmd)
    (goto-char (point-min))
    (setq cmd (get-text-property (point) 'redo-cmd))
    (eval cmd)
    (goto-line (+ 1 curline))))

(defun superman-view-redo-line ()
  (interactive)
  (let* ((buffer-read-only nil)
	 (marker (org-get-at-bol 'org-hd-marker))
	 (balls (org-get-at-bol 'balls))
	 (new-line (superman-format-thing marker balls)))
    (beginning-of-line)
    (if	(looking-at ".*")
	(replace-match new-line))
    (beginning-of-line)
      (while (or (org-activate-bracket-links (point-at-eol)) (org-activate-plain-links (point-at-eol)))
	(add-text-properties
	 (match-beginning 0) (match-end 0)
	 '(face org-link)))
      (beginning-of-line)))


(defun superman-view-toggle-todo ()
  (interactive)
  (let ((marker (org-get-at-bol 'org-hd-marker)))
    (when marker
      (save-excursion
	(org-with-point-at marker
	  (org-todo)))
      (sit-for 0)
      (superman-view-redo-line))))

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
  (superman-redo))
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
  (superman-redo))

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
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
    (vc-annotate (org-link-display-format file) "HEAD")
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
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
    (vc-print-log-internal
     'Git
     (list dir)
     nil nil 2000))))

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
    (when pom (goto-char pom))))

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

;; (defun superman-view-set (&optional dont-redo)
  ;; "Set a property for document at point."
  ;; (interactive)
  ;; (let ((prop "Property"
  ;; (org-entry-put 
  ;; (superman-redo))

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
	 (b (org-get-heading t t))
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
	  (superman-mode
	   (superman-return))
	  ;; (message "Follow-link"))
	  ((setq f (superman-get-property m "filename"))
	   (org-open-link-from-string f))
	  (t (message "Nothing to do here")))))

(defun superman-view-git-log (&optional arg)
  (interactive "p")
  (superman-git-log-at-point (or arg 10)))

(defun superman-view-git-log-decorationonly (&optional arg)
  (interactive "p")
  (superman-git-log-decorationonly-at-point arg))

(defun superman-view-git-search (&optional arg)
  (interactive "p")
  (superman-git-search-at-point arg))

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
      (put-text-property 0 (length filename) 'org-hd-marker (org-get-at-bol 'org-hd-marker) filename)
      filename)))


(defun superman-view-git-update-status (&optional beg end dont-redo)
  "Update git status for project and all entries (that have a filename)."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (if (not dir) (message "Project is not git-controlled.")
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
	      (when (or
		     (string= (downcase status) "modified")
		     (string= (downcase current-status) "modified"))
		(org-entry-put pom "GitStatus" status)))
	    (setq file-list (cdr file-list)))))
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
   '(lambda ()
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
      (superman-view-git-update-status)
      (unless dont-redo (superman-redo)))))

;;}}}
;;{{{ View-mode and hot-keys

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

(defun superman-view-show-hot-keys (&optional cat)
  "Show keybindings in project view header or in section CAT."
  (let ((hot-key-string "")
	(len 0)
	this-key-string
	(key-alist (eval (intern (concat "superman-" (if cat (downcase cat) "global") "-hot-keys")))))
    (while key-alist
      (let* ((x (car key-alist))
	     ;; (f (nth 1 x))
	     (s (nth 2 x)))
	(when s
	  (setq this-key-string (concat (car x) ": " s "  "))
	  (setq hot-key-string (concat hot-key-string this-key-string))
	  (setq len (+ len (length this-key-string)))
	  (when (> len fill-column)
	    (setq hot-key-string (concat hot-key-string "\n       ")
		  len 0)))
	  (setq key-alist (cdr key-alist))))
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


(setq superman-view-hot-keys
'("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
     "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
     "!" "?" "*" "="))
(defun superman-view-set-hot-keys ()
  "Define hot keys for superman-view"
  (mapcar
   '(lambda (x)
      (define-key superman-view-mode-map
	x (intern (concat "superman-hot-" x))))
   superman-view-hot-keys))

(superman-view-set-hot-keys)

(define-key superman-view-mode-map [return] 'superman-hot-return)
(define-key superman-view-mode-map [(shift up)] 'superman-one-up)
(define-key superman-view-mode-map [(shift down)] 'superman-one-down) 

(setq superman-documents-hot-keys '(("c" superman-view-git-commit)))

(defun superman-view-choose-hot-key (key)
  "Find command bound to key in current section. If undefined use global key."
  (let* ((cat (superman-current-cat))
	 (alist (if cat
		    (condition-case nil
			(eval (intern (concat "superman-" (downcase cat) "-hot-keys")))
		      (error nil))
		  superman-global-hot-keys))
	 cmd)
    (unless alist
      (setq alist superman-global-hot-keys))
    (setq cmd (nth 1 (assoc key alist)))
    (unless cmd
      (setq cmd (nth 1 (assoc key superman-global-hot-keys))))
    (cond
     ((not cmd)
      (message (concat "Hot-key "key" not bound (in this section)")))
     ((fboundp cmd)
      (funcall cmd))
     (t (message (concat "Not a function: " (symbol-name cmd)))))))


(defun superman-hot-a () (interactive) (superman-view-choose-hot-key "a"))
(defun superman-hot-b () (interactive) (superman-view-choose-hot-key "b"))
(defun superman-hot-c () (interactive) (superman-view-choose-hot-key "c"))
(defun superman-hot-d () (interactive) (superman-view-choose-hot-key "d"))
(defun superman-hot-e () (interactive) (superman-view-choose-hot-key "e"))
(defun superman-hot-f () (interactive) (superman-view-choose-hot-key "f"))
(defun superman-hot-g () (interactive) (superman-view-choose-hot-key "g"))
(defun superman-hot-h () (interactive) (superman-view-choose-hot-key "h"))
(defun superman-hot-i () (interactive) (superman-view-choose-hot-key "i"))
(defun superman-hot-j () (interactive) (superman-view-choose-hot-key "j"))
(defun superman-hot-k () (interactive) (superman-view-choose-hot-key "k"))
(defun superman-hot-l () (interactive) (superman-view-choose-hot-key "l"))
(defun superman-hot-m () (interactive) (superman-view-choose-hot-key "m"))
(defun superman-hot-n () (interactive) (superman-view-choose-hot-key "n"))
(defun superman-hot-o () (interactive) (superman-view-choose-hot-key "o"))
(defun superman-hot-p () (interactive) (superman-view-choose-hot-key "p"))
(defun superman-hot-q () (interactive) (superman-view-choose-hot-key "q"))
(defun superman-hot-r () (interactive) (superman-view-choose-hot-key "r"))
(defun superman-hot-s () (interactive) (superman-view-choose-hot-key "s"))
(defun superman-hot-t () (interactive) (superman-view-choose-hot-key "t"))
(defun superman-hot-u () (interactive) (superman-view-choose-hot-key "u"))
(defun superman-hot-v () (interactive) (superman-view-choose-hot-key "v"))
(defun superman-hot-w () (interactive) (superman-view-choose-hot-key "w"))
(defun superman-hot-x () (interactive) (superman-view-choose-hot-key "x"))
(defun superman-hot-y () (interactive) (superman-view-choose-hot-key "y"))
(defun superman-hot-z () (interactive) (superman-view-choose-hot-key "z"))
(defun superman-hot-A () (interactive) (superman-view-choose-hot-key "A"))
(defun superman-hot-B () (interactive) (superman-view-choose-hot-key "B"))
(defun superman-hot-C () (interactive) (superman-view-choose-hot-key "C"))
(defun superman-hot-D () (interactive) (superman-view-choose-hot-key "D"))
(defun superman-hot-E () (interactive) (superman-view-choose-hot-key "E"))
(defun superman-hot-F () (interactive) (superman-view-choose-hot-key "F"))
(defun superman-hot-G () (interactive) (superman-view-choose-hot-key "G"))
(defun superman-hot-H () (interactive) (superman-view-choose-hot-key "H"))
(defun superman-hot-I () (interactive) (superman-view-choose-hot-key "I"))
(defun superman-hot-J () (interactive) (superman-view-choose-hot-key "J"))
(defun superman-hot-K () (interactive) (superman-view-choose-hot-key "K"))
(defun superman-hot-L () (interactive) (superman-view-choose-hot-key "L"))
(defun superman-hot-M () (interactive) (superman-view-choose-hot-key "M"))
(defun superman-hot-N () (interactive) (superman-view-choose-hot-key "N"))
(defun superman-hot-O () (interactive) (superman-view-choose-hot-key "O"))
(defun superman-hot-P () (interactive) (superman-view-choose-hot-key "P"))
(defun superman-hot-Q () (interactive) (superman-view-choose-hot-key "Q"))
(defun superman-hot-R () (interactive) (superman-view-choose-hot-key "R"))
(defun superman-hot-S () (interactive) (superman-view-choose-hot-key "S"))
(defun superman-hot-T () (interactive) (superman-view-choose-hot-key "T"))
(defun superman-hot-U () (interactive) (superman-view-choose-hot-key "U"))
(defun superman-hot-V () (interactive) (superman-view-choose-hot-key "V"))
(defun superman-hot-W () (interactive) (superman-view-choose-hot-key "W"))
(defun superman-hot-X () (interactive) (superman-view-choose-hot-key "X"))
(defun superman-hot-Y () (interactive) (superman-view-choose-hot-key "Y"))
(defun superman-hot-Z () (interactive) (superman-view-choose-hot-key "Z"))
(defun superman-hot-! () (interactive) (superman-view-choose-hot-key "!"))
(defun superman-hot-? () (interactive) (superman-view-choose-hot-key "?"))
(defun superman-hot-= () (interactive) (superman-view-choose-hot-key "="))
(defun superman-hot-* () (interactive) (superman-view-choose-hot-key "*"))

(setq superman-global-hot-keys
      '(("a" nil)
	("b" nil)
	("c" nil)
	("d" nil)
	("e" nil)
	("f" org-agenda-follow-mode "follow")
	("g" superman-view-git-grep "grep")
	("h" nil)
	("i" superman-view-index "index")
	("j" nil)
	("k" nil)
	("l" nil)
	("m" superman-toggle-mark "mark")
	("n" superman-next-entry "next")
	("o" nil)
	("p" superman-previous-entry "previous")
	("q" nil)
	("r" superman-view-redo-line "redo")
	("s" nil)
	("t" superman-view-toggle-todo)
	("u" nil)
	("v" nil)
	("w" nil)
	("x" nil)
	("y" nil)
	("z" nil)
	("A" nil)
	("B" nil)
	("C" nil)
	("D" superman-new-document "document")
	("E" nil)
	("F" superman-view-file-list "FileList")
	("G" nil)
	("H" nil)
	("I" superman-view-git-init "GitInit")
	("J" nil)
	("K" nil)
	("L" nil)
	("M" superman-new-meeting "Meeting")
	("N" superman-new-thing "Note")
	("O" nil)
	("P" superman-view-git-push "Push")
	("Q" superman-unison "Unison")
	("R" superman-redo)
	("S" superman-sort-section "sort")
	("T" superman-new-task "Task")
	("U" superman-view-git-update-status "Update")
	("V" superman-switch-config "next-config")
	("W" nil)
	("X" nil)
	("Y" nil)
	("Z" nil)
	("!" superman-goto-shell)
	("?" superman-view-help)
	("=" nil)
	("*" nil)))

(setq superman-documents-hot-keys
      '(( "c" superman-view-git-commit "commit")
	( "C" superman-view-git-commit-all "Commit")
	( "d" superman-view-git-diff "diff")
	( "h" superman-view-git-history "history")
	( "l" superman-view-git-log "log")
	( "u" superman-view-git-update-status "update")
	( "L" superman-view-git-log-decorationonly)
	;; ( "v" superman-view-git-annotate "annotate")
	("M" superman-view-mark-all "Mark")
	("N" superman-new-document "New")
	("=" superman-view-git-version-diff)))

(setq superman-data-hot-keys superman-documents-hot-keys)

(setq superman-meetings-hot-keys
      '(("M" superman-view-mark-all)
	( "N" superman-new-meeting)))

(setq superman-notes-hot-keys
      '(("M" superman-view-mark-all "Mark all")
	( "N" superman-new-note "New note")))

(setq superman-bookmarks-hot-keys
      '(("M" superman-view-mark-all)
	( "N" superman-new-bookmark "New bookmark")))

(setq superman-tasks-hot-keys
      '(("M" superman-view-mark-all)
	( "N" superman-new-task "New Task")))

(setq superman-mail-hot-keys
      '(("M" superman-view-mark-all)))


(defun superman-new-thing ()
  (interactive)
  (let ((thing (completing-read
		"Add thing to project (select): "
		'(("Document")
		  ("Data")
		  ("Bookmark")
		  ("Note")))))
    (funcall (intern (concat "superman-new-" (downcase thing))))))

;;}}}
;;{{{ Sorting

(defun superman-sort-section (&optional reverse)
  (interactive "P")
  (let* ((buffer-read-only nil)
	 (cc (current-column))
	 (col-start 0)
	 col-width
	 (cols (cdr (get-text-property (point-at-bol) 'columns)))
	 (pos (superman-current-subcat-pos))
	 next
	 beg
	 end)
    (when (and pos cols)
	(while (> cc (+ col-start (car cols)))
	  (setq col-start (+ col-start (car cols)))
	  (setq cols (cdr cols)))
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
	(sort-subr reverse 'forward-line 'end-of-line
		   `(lambda () (forward-char ,col-start))
		   `(lambda () (forward-char ,col-width)))
	(widen)
	(forward-char (+ 2 col-start)))))
      

(defun superman-sort-by-status (a b)
  (let ((A  (substring-no-properties a 0 12))
	(B  (substring-no-properties b 0 12)))
    (if (string= A B) nil 
      (if (string-lessp A B)
	  1 -1))))

;;}}}
;;{{{ help 

(defun superman-popup-tip (msg)
  (save-excursion
    (goto-char (point-min))
    (popup-tip msg)))

(defvar superman-help-fun 'superman-popup-tip 
  "Function used to display help. Possible values 'tooltip-show or 'popup-tip (depends on popup.el)") 
(defun superman-view-help ()
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


