;;; superman-summary.el --- Summary of project contents and adding information to projects

;; Copyright (C) 2012  Klaus Kähler Holst, Thomas Alexander Gerds

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

;; Code:

;;{{{ summary views

(defun superman-summary ()
  (interactive)
  (let ((org-agenda-files (superman-get-index superman-current-project)))
    (superman-tags-view-plus nil "filename={.+}" nil)))

(defvar superman-view-marks nil "Marks for items in agenda.")
(make-variable-buffer-local 'superman-view-marks)

(defvar org-agenda-overriding-buffer-name nil)

(defvar superman-view-current-project nil)
(make-variable-buffer-local 'superman-view-current-project)

(defvar org-agenda-property-list nil)
(make-variable-buffer-local 'org-agenda-property-list)

(defvar org-agenda-overriding-agenda-format nil)
(make-variable-buffer-local 'org-agenda-overriding-agenda-format)

(defun superman-trim-string (str len)
  "Trim string STR to a given length by either calling substring
or by adding whitespace characters."
  (let* ((slen (length str))
	 (diff (- len slen)))
    (if (> diff 0)
	(concat str (make-string diff (string-to-char " ")))
      (substring str 0 len))))

;; FIXME: use gnus-user-date-format-alist to trim date
(defun superman-view-documents-format (hdr level category tags-list prop-list)
  (concat " " (superman-trim-string hdr 20)
	  (let ((cprops prop-list)
		(pstring ""))
	    (while cprops
	      (let ((val (cdr (car cprops))))
		(cond ((string= (downcase (caar cprops)) "filename")
		       (setq val (file-name-nondirectory (org-link-display-format val)))))
		(setq pstring (concat pstring "  " (superman-trim-string val  23))))
		(setq cprops (cdr cprops)))
	      pstring) "\t"))

(defun superman-view-current-project ()
  ;; FIXME: may give problems when property "Project" does
  ;; not match currently viewed project
  ;; (or (superman-property-at-point "Project")
  (save-excursion (goto-char (point-min))
		  (if (re-search-forward "^Project:[ \t]*\\(.*\\)[ \t]*$" nil t)
		      (assoc (match-string-no-properties 1) superman-project-alist)
		    (superman-select-project))))
;; )

(defun superman-view-control ()
  ;; FIXME: this is a hack to distinguish between
  ;; the first time agenda and redo's
  (let ((pro (if superman-view-mode (superman-view-current-project) superman-current-project)))
    (if (superman-git-p (concat (superman-get-location pro) (car pro)))
	(concat "Git repository: " (concat (superman-get-location pro) (car pro)))
      "press `I' to initialize git")))

;; (defun superman-view-set-marks ()
;; (superman-loop 'superman-view-set-mark))

;; (defun superman-view-set-mark (&optional m)
;; (let ((m (or m "M"))
;; (buffer-read-only nil))
;; (if (org-get-at-bol 'superman-view-mark) (insert m) )))

(defvar superman-mark-face 'bold  "Face name for marked entries in the view buffers.")

(defun superman-toggle-mark ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((buffer-read-only nil)
	   (cur (get-text-property (point) 'face))
	   (item (progn (looking-at ".*") (match-string 0)))
	   (new (if (eq cur 'default) superman-mark-face 'default)))
      (put-text-property 0 (length item) 'face new item)
      (replace-match item t t))))


(defun superman-make-item (list hdr len)
  (let ((prop-values list)
	(item ""))
    (while prop-values
      (let ((val (cdr (car prop-values))))
	(cond ((string= (downcase (caar prop-values)) "filename")
	       (setq val (file-name-nondirectory (org-link-display-format val))))
	      ((string-match org-bracket-link-regexp val)
	       ;; (string= (downcase (caar prop-values)) "link")
	       (let ((link (org-match-string-no-properties 1 val))
		     (desc (if (match-end 3) (org-match-string-no-properties 3 val) "link")))
		 (setq val (org-make-link-string link (superman-trim-string desc len)))))
	      (t (setq val (superman-trim-string val len))))
	(setq item (concat item "  " val)))
      (setq prop-values (cdr prop-values)))
    (concat " " (superman-trim-string hdr 20) item)))
    
(defun superman-reformat-item (prop-list)
  (let* ((pom (org-get-at-bol 'org-hd-marker))
	 (hdr (org-with-point-at pom
		(org-get-heading 'no-tags 'no-todo)))
	 (prop-values
	  (org-with-point-at pom
	    (mapcar
	     '(lambda (prop)
		(cons prop (or (superman-get-property (point) prop 'inherit)
			       "--"
			       "")))
	     prop-list)))
	 (text-props  (text-properties-at (point)))
	 (new-item (superman-make-item prop-values hdr 23))
	 (buffer-read-only nil))
    (beginning-of-line)
    (looking-at ".*")
    (replace-match new-item)
    (beginning-of-line)
    (add-text-properties (point-at-bol) (point-at-eol) text-props)))

(defvar superman-view-format-alist '(("Documents" . ("GitStatus" "LastCommit" "FileName"))
				     ("Notes" . ("NoteDate"))
				     ("Mail" . ("EmailDate" "Link"))
				     ("Tasks" . ("CaptureDate"))
				     ("Bookmarks" . ("Bookmark"))))

(defun superman-reformat-document-lines ()
  (superman-loop
   'superman-reformat-item (list '("GitStatus" "LastCommit" "FileName"))))

(defun superman-finalize-project-view ()
  (let (cat-head
	cat-tail
	done
	(buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (while (not done)
	;; Either there is a next header or we are almost done
	(unless (setq cat-tail (next-single-property-change (point-at-eol) 'org-agenda-structural-header))
	  (setq cat-tail (point-max))
	  (setq done t))
	(let* ((cat (progn (re-search-forward "^\\[\\(.*\\)\\]" nil t) (match-string-no-properties 1)))
	       (cat-fun (cdr (assoc cat superman-view-format-alist)))
	       header
	       delete
	       (next-item-pos (next-single-property-change (point-at-eol) 'org-marker)))
	  (beginning-of-line)
	  (setq cat-head (point))
	  (if (or (not next-item-pos) (< cat-tail next-item-pos))
	      ;; no items in this block
	      (progn (setq delete t)
		     (delete-region cat-head cat-tail))
	    (setq header (superman-make-item
			  (mapcar '(lambda (cat) (cons cat cat)) cat-fun) "header" 23))
	    (put-text-property 0 (length header) 'face 'org-agenda-structure header)
	    (end-of-line)
	    (insert "\n" header)
	    (superman-loop 'superman-reformat-item
			   (list cat-fun) cat-head cat-tail))
	  (goto-char cat-head)
	  (unless delete
	    (setq cat-head (next-single-property-change (point-at-eol) 'org-agenda-structural-header)))
	  (unless done 
	    (goto-char cat-head)))))
    (superman-view-mode-on)))

(defun superman-view-finalize-documents ()
  (let* ((pro (or (superman-view-current-project)
		  (superman-select-project)))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (header-end (save-excursion
		       (next-single-property-change (point) 'org-marker)))
	 (git-string (concat "Control: "
			     (if (superman-git-p loc)
				 (concat "Git repository at "
					 loc)
			       "press `I' to initialize git"))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^Control:[ \t]*\\(.*\\)[ \t]*$" header-end t)
	  (replace-match git-string)))
    (superman-reformat-document-lines)
    (superman-view-mode-on)))



;; (org-agenda-property-list
	  ;; (quote (,(superman-property 'gitstatus)
		  ;; ,(superman-property 'lastcommit) ,(superman-property 'filename))))
;; (let ((buffer-read-only nil))
;; (superman-view-set-marks))))



(defvar superman-cats '(("Documents" . "FileName") ("Notes" . "NoteDate") ("Tasks" . "TaskDate") ("Mail" . "EmailDate") ("Bookmarks" . "Bookmark")))

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
	 ;; FIXME: either scan index file for existing entries or find a way to have special header
	 ;;        for empty match lists
	 (cats superman-cats)
	 ;; (view-buf (concat "*Project[" (car pro) "]*"))
	 (cat-number-one (car cats))
	 (header-start (concat superman-view-project-helpline
			       "\nProject: " (car pro)
			       "\n" (or (superman-view-control)
					(concat "Control: not set. <> Press `I' to initialize git"))
			       "\n\n"))
	 (shared-header "")
	 (cmd-block
	  (mapcar '(lambda (cat)
		     (list 'tags (concat (cdr cat) "={.+}")
			   (let ((hdr (if (eq (car cat) (car cat-number-one))
					  (concat header-start "[" (car cat) "]" shared-header)
					(concat "[" (car cat) "]"))))
			     `((org-agenda-overriding-header ,hdr)))))
		  cats))
	 (org-agenda-custom-commands
	  `(("p" "view Project"
	     ,cmd-block
	     ((org-agenda-finalize-hook 'superman-finalize-project-view)
	      (org-agenda-block-separator superman-document-category-separator)
	      (org-agenda-view-columns-initially nil)
	      (org-agenda-buffer-name (concat "*Documents[" ,(car pro) "]*"))
	      (org-agenda-files (quote (,(superman-get-index pro))))
	      )))))
    (org-agenda nil "p")))

;; (defun superman-view-project (&optional project)
  ;; "View documents of the current project."
  ;; (interactive)
  ;; (let* ((pro (or project
		  ;; superman-current-project
		  ;; (setq superman-current-project
		  ;; (superman-select-project))))
	 ;; (loc (concat (superman-get-location pro) (car pro)))
	 ;; (org-agenda-buffer-name (concat "*Project[" (car pro) "]*"))
	 ;; (org-agenda-sticky nil)
	 ;; (org-agenda-window-setup 'current-window)
	 ;; (org-agenda-finalize-hook 'superman-view-finalize-documents)
	 ;; (view-buf (concat "*Documents[" (car pro) "]*"))
	 ;; (org-agenda-custom-commands
	  ;; `(("p" "view Project"
	     ;; ((tags (concat ,(superman-property 'filename)  "={.+}")
		    ;; ((org-agenda-files (quote (,(superman-get-index pro))))
		     ;; (org-agenda-finalize-hook 'superman-view-finalize-documents)
		     ;; (org-agenda-overriding-header
		      ;; (concat "?: help, n: new document, a[A]: git add[all], c[C]:commit[all], l: git log, u[U]: update[all]"
			      ;; "\nProject: "  ,(car pro)
			      ;; "\n" (or (superman-view-control) 
				       ;; (concat "\nControl: " "press `I' to initialize git"))
			      ;; "\n\nDocuments: " "\n"
			      ;; (superman-view-documents-format "header" 0 nil nil '
							      ;; (("GitStatus" .  "GitStatus") 
							       ;; ("LastCommit" . "LastCommit") 
							       ;; ("FileName" . "FileName")))))
		     ;; (org-agenda-property-list '("GitStatus" "LastCommit" "FileName"))
		     ;; (org-agenda-overriding-agenda-format 'superman-view-documents-format)
		     ;; (org-agenda-view-columns-initially nil)
		     ;; (org-agenda-overriding-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     ;; (org-agenda-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     ;; ))
	      ;; (tags "NoteDate={.+}"
		    ;; ((org-agenda-files (quote (,(superman-get-index pro))))
		     ;; (org-agenda-finalize-hook 'superman-view-finalize-documents)
		     ;; (org-agenda-overriding-header "")
		     ;; (org-agenda-overriding-header (concat "\n"
							   ;; (superman-view-documents-format "header" 0 nil nil '(("NoteDate" .  "NoteDate")))))
		     ;; (org-agenda-property-list '("NoteDate"))
		     ;; (org-agenda-overriding-agenda-format 'superman-view-documents-format)
		     ;; (org-agenda-overriding-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     ;; (org-agenda-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     ;; (org-agenda-view-columns-initially nil))))))))
    ;; (push ?p unread-command-events)
    ;; (call-interactively 'org-agenda)
    ;; ;; (rename-buffer view-buf)
    ;; ))

(defun superman-parse-document-categories (buf)
  "Parse the file `superman-home' and update `superman-project-categories'."
  (save-excursion
    (set-buffer buf)
    (reverse (superman-get-buffer-props (superman-property 'category)))))

(defvar superman-view-documents-helpline
  "?: help, n: new document, a[A]: git add[all], c[C]:commit[all], l: git log, u[U]: update[all]"
  "First line of document view buffer which -- by default -- is showing some keystrokes")


(defvar superman-view-project-helpline
  "?: help, add document [d], note [n], task [t], bookmark [b]"
  "First line of project view buffer which -- by default -- is showing some keystrokes")

(defvar superman-document-category-separator '32 "Symbol for separating categories in document views.
See `org-agenda-block-separator'. Set to '0 to get a       line.
Can also be set to (string-to-char \"~\") with any string in place of ~.")

(defun superman-view-documents (&optional project)
  "View documents of the current project."
  (interactive)
  (let* ((pro (or project
		  superman-current-project
		  (superman-switch-to-project 'force nil t)))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (org-agenda-buffer-name (concat "*Documents[" (car pro) "]*"))
	 (org-agenda-sticky nil)
	 (org-agenda-window-setup 'current-window)
	 (cats (superman-parse-document-categories
		(find-file-noselect (superman-get-index pro))))
	 (view-buf (concat "*Documents[" (car pro) "]*"))
	 (header-start (concat superman-view-documents-helpline
			       "\nProject: " (car pro)
			       "\n" (or (superman-view-control)
					(concat "Control: not set. <> Press `I' to initialize git"))
			       "\n\n"))
	 (shared-header
	  (superman-view-documents-format
	   "header" 0 nil nil
	   '(("GitStatus" .  "GitStatus") ("LastCommit" . "LastCommit") ("FileName" . "FileName"))))
	 (cats-and-one-dog (append `((,(car pro))) cats))
	 (cmd-block
	  (if cats 
	      (mapcar '(lambda (cat)
			 (list 'tags (concat "FileName={.+}" "+" "CATEGORY=\"" (car cat) "\"")
			       (let ((hdr (if (eq (car cat) (caar cats-and-one-dog))
					      (concat header-start shared-header (concat "\n[" (car cat) "]"))
					    (concat "[" (car cat) "]"))))
				 `((org-agenda-overriding-header ,hdr)))))
		      cats-and-one-dog)
	    `((tags "FileName={.+}" ((org-agenda-overriding-header (concat ,header-start ,shared-header)))))))
	 (org-agenda-custom-commands
	  `(("d" "view Project-DOCUMENTS"
	     ,cmd-block
	     ((org-agenda-finalize-hook 'superman-view-finalize-documents)
	      (org-agenda-block-separator superman-document-category-separator)
	      (org-agenda-view-columns-initially nil)
	      (org-agenda-buffer-name (concat "*Documents[" ,(car pro) "]*"))
	      ;; (org-agenda-overriding-agenda-format 'superman-view-documents-format)
	      (org-agenda-files (quote (,(superman-get-index pro))))
	      ;; (org-agenda-overriding-buffer-name (concat "*Project[" ,(car pro) "]*"))
	      )))))
    (org-agenda nil "d")))
;; (push ?d unread-command-events)
;; (call-interactively 'org-agenda)))


;; (if org-agenda-overriding-agenda-format
    ;; (setq txt (funcall org-agenda-overriding-agenda-format
		       ;; (org-get-heading 'no-tags 'no-todo)
		       ;; level
		       ;; category
		       ;; tags-list
		       ;; prop-list))

;;}}}
;;{{{ summary-view commands
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
  (let ((pro (or (superman-view-current-project) (superman-select-project)))
	(st (read-string "Grep: ")))
    (if arg
	(vc-git-grep)
	(vc-git-grep st "*" (concat (superman-get-location pro) (car pro))))))


(defun superman-view-git-grep (&optional arg)
  (interactive)
  (let ((pro (or (superman-view-current-project) (superman-select-project)))
	(st (read-string "Grep: ")))
    (if arg
	(vc-git-grep)
	(vc-git-grep st "*" (concat (superman-get-location pro) (car pro))))))

(defun superman-view-git-history ()
  (interactive)
  (let ((pro (or (superman-view-current-project) (superman-select-project))))
    (vc-print-log-internal 'Git (list (concat (superman-get-location pro) (car pro))) nil nil 2000)))

(defun superman-view-git-init ()
  (interactive)
  (let ((pro (or (superman-view-current-project) (superman-select-project))))
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

(defun superman-view-return ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker)))
    (org-open-link-from-string (superman-get-property m "filename"))))

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
  (let ((file (superman-filename-at-point))
	(pom  (org-get-at-bol 'org-hd-marker)))
    (superman-git-set-status pom file check)
    (when save (superman-view-save-hd-buffer))
    (when redo (org-agenda-redo))))

(defun superman-view-save-hd-buffer ()
  (save-excursion
    (goto-char (point-min))
    (org-agenda-next-item 1)
    (set-buffer
     (marker-buffer (org-get-at-bol 'org-hd-marker)))
    (save-buffer)))

(defun superman-view-update-all ()
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
	(while (next-single-property-change (point-at-eol) 'org-marker)
	  (goto-char (next-single-property-change (point-at-eol) 'org-marker))
	  (when (or (not marked)
		    (eq (get-text-property (point) (car marked)) (cadr marked)))
	    (setq loop-out (append (list (apply fun args)) loop-out))))
	loop-out))))

(defun superman-view-git-add-all (&optional dont-redo)
  (interactive)
  (superman-loop 'superman-view-git-add (list 'dont) nil nil `(face ,superman-mark-face))
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
;;{{{ summary-view-mode

(defvar superman-view-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
   
(define-minor-mode superman-view-mode 
     "Toggle org projectmanager document view mode.
                   With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
                   turn it off.
                   
                   Enabling superman-view mode electrifies the column view for documents
                   for git and other actions like commit, history search and pretty log-view."
     :lighter "S-view"
     :group 'org
     :keymap 'superman-view-mode-map)

(defun superman-view-mode-on ()
  (interactive)
  (hl-line-mode 1)
  (superman-view-mode t))

(defun superman-start-shell (&optional dir)
  (interactive)
  (let* ((filedir (or dir (file-name-directory (superman-filename-at-point))))
	 (pro (or dir (superman-view-current-project)))
	 (dir (or dir (concat (superman-get-location pro) (car pro))))
	 (default-directory (expand-file-name (or filedir dir))))
    (shell "*shell*superman")
    (comint-send-string (current-buffer) (concat "\ncd " default-directory "\n"))
    ))


   
(define-key superman-view-mode-map [return] 'superman-view-return) ;; Return is not used anyway in column mode
(define-key superman-view-mode-map "l" 'superman-view-git-log) 
(define-key superman-view-mode-map "L" 'superman-view-git-log-decorationonly)
(define-key superman-view-mode-map "n" 'superman-view-register-document)
(define-key superman-view-mode-map "m" 'superman-toggle-mark)
(define-key superman-view-mode-map "r" 'org-agenda-redo)
(define-key superman-view-mode-map "h" 'superman-view-git-history)
(define-key superman-view-mode-map "!" 'superman-start-shell)
(define-key superman-view-mode-map "g" 'superman-view-git-grep)
(define-key superman-view-mode-map "v" 'superman-view-git-annotate)
(define-key superman-view-mode-map "d" 'superman-view-git-diff)
(define-key superman-view-mode-map "D" 'superman-view-git-ediff)
(define-key superman-view-mode-map "a" 'superman-view-git-add)
(define-key superman-view-mode-map "A" 'superman-view-git-add-all)
(define-key superman-view-mode-map "S" 'superman-view-git-search)
(define-key superman-view-mode-map "?" 'superman-view-show-help)
(define-key superman-view-mode-map "u" 'superman-view-update)
(define-key superman-view-mode-map "I" 'superman-view-git-init)
(define-key superman-view-mode-map "U" 'superman-view-update-all)
(define-key superman-view-mode-map "c" 'superman-view-git-commit)
(define-key superman-view-mode-map "C" 'superman-view-git-commit-all)
;; (define-key superman-view-mode-map "c" 'org-agenda-columns)


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
 
;; (split-window-vertically)  
;; (other-window 1)
;; (switch-to-buffer "*superman-help-buffer*")
;; (toggle-read-only -1)
;; (erase-buffer)
;; (goto-char (point-min))
;; (toggle-read-only 1)
;; (other-window -1))

;;}}}
;;{{{ Adding documents from file-list

(defun superman-add-documents (&optional file-list)
  (interactive)
  (let* ((fl (or file-list file-list-current-file-list)))
    ;; FIXME need to write superman-get-documents and filter duplicates
    (superman-goto-project-documents)
    (while fl
      (insert "\n*** " (file-name-nondirectory (file-name-sans-extension (file-list-make-file-name (car fl))))
	      "\n:PROPERTIES:\n:filename: [["(file-list-make-file-name (car fl))"]]\n:CaptureDate: ")
      (org-insert-time-stamp (current-time) t)
      (insert "\n:END:")
      (setq fl (cdr fl)))))

(defun superman-view-register-document (&optional file-list)
  (interactive)
  (let* ((pro (superman-view-current-project))
	 (dir (expand-file-name (concat (superman-get-location pro) (car pro))))
	 (fl (or file-list `(,(read-file-name (concat "Choose: ") (file-name-as-directory dir))))))
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

;;}}}

(provide 'superman-summary)
;;; superman-summary.el ends here



