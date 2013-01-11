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

;; (defun superman-view-project (&optional project)
  ;; (interactive)
  ;; (let ((pro (or project superman-current-project (superman-select-project))))
    ;; (switch-to-buffer (concat "*" (car pro) " view*"))
    ;; (local-set-key "d" 'superman-view-documents)))

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
  (or (superman-property-at-point "Project")
      (save-excursion (goto-char (point-min))
		      (if (re-search-forward "^Project:[ \t]*\\(.*\\)[ \t]*$" nil t)
			  (assoc (match-string-no-properties 1) superman-project-alist)
			(superman-select-project)))))

(defun superman-view-control ()
  ;; FIXME: this is a hack to distinguish between
  ;; the first time agenda and redo's
  (let ((pro (if superman-view-mode (superman-view-current-project) superman-current-project)))
    (if (superman-git-p (concat (superman-get-location pro) (car pro)))
	(concat "Git repository: " (concat (superman-get-location pro) (car pro)))
      "press `I' to initialize git")))

;; (defun superman-view-set-marks ()
  ;; (superman-view-loop 'superman-view-set-mark))

;; (defun superman-view-set-mark (&optional m)
 ;; (let ((m (or m "M"))
       ;; (buffer-read-only nil))
   ;; (if (org-get-at-bol 'superman-view-mark) (insert m) )))

(defun superman-view-finalize-documents ()
  (let* ((pro (or (superman-view-current-project)
		  (superman-select-project)))
	 (loc (concat (superman-get-location pro) (car pro))))
    (superman-view-mode-on)))
    ;; (let ((buffer-read-only nil))
      ;; (superman-view-set-marks))))

(defun superman-view-project (&optional project)
  "View documents of the current project."
  (interactive)
  (let* ((pro (or project superman-current-project (superman-select-project)))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (org-agenda-buffer-name (concat "*Project[" (car pro) "]*"))
	 (org-agenda-overriding-buffer-name (concat "*Project[" (car pro) "]*"))
	 (org-agenda-finalize-hook 'superman-view-finalize-documents)
	 (view-buf (concat "*Documents[" (car pro) "]*"))
	 (org-agenda-custom-commands
	  `(("p" "view Project"
	     ((tags (concat ,(superman-property 'filename)  "={.+}")
		    ((org-agenda-files (quote (,(superman-get-index pro))))
		     (org-agenda-finalize-hook 'superman-view-finalize-documents)
		     (org-agenda-overriding-header
		      (concat "h: help, n: new document, a[A]: git add[all], c[C]:commit[all], l: git log, u[U]: update[all]"
			      "\nProject: "  ,(car pro)
			      "\nControl: " (or (superman-view-control) 
						(concat "press `I' to initialize git"))
			      "\n\nDocuments: " "\n"
			      (superman-view-documents-format "header" 0 nil nil '
							     (("GitStatus" .  "GitStatus") 
							      ("LastCommit" . "LastCommit") 
							      ("FileName" . "FileName")))))
		     (org-agenda-property-list '("GitStatus" "LastCommit" "FileName"))
		     (org-agenda-overriding-agenda-format 'superman-view-documents-format)
		     (org-agenda-view-columns-initially nil)
		     (org-agenda-overriding-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     (org-agenda-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     ))
	      (tags "NoteDate={.+}"
		    ((org-agenda-files (quote (,(superman-get-index pro))))
		     (org-agenda-finalize-hook 'superman-view-finalize-documents)
		     (org-agenda-overriding-header "")
		     (org-agenda-overriding-header (concat "\n"
							   (superman-view-documents-format "header" 0 nil nil '(("NoteDate" .  "NoteDate")))))
		     (org-agenda-property-list '("NoteDate"))
		     (org-agenda-overriding-agenda-format 'superman-view-documents-format)
		     (org-agenda-overriding-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     (org-agenda-buffer-name (concat "*Project[" ,(car pro) "]*"))
		     (org-agenda-view-columns-initially nil))))))))
    (push ?p unread-command-events)
    (call-interactively 'org-agenda)
    ;; (rename-buffer view-buf)
    ))

(defun superman-parse-document-categories (buf)
  "Parse the file `superman-file' and update `superman-project-categories'."
  (save-excursion
    (set-buffer buf)
    (reverse (superman-get-buffer-props (superman-property 'category)))))

(defun superman-view-documents (&optional project)
  "View documents of the current project."
  (interactive)
  (let* ((pro (or project superman-current-project (superman-select-project)))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (org-agenda-overriding-buffer-name (concat "*Project[" (car pro) "]*"))
	 ;; (org-agenda-finalize-hook 'superman-view-finalize-documents)
	 (cats (superman-parse-document-categories (get-file-buffer (superman-get-index pro))))
	 ;; (cats '(("start-me-up") ("nice-to-know")))
	 (cat-number-one (car cats))
	 (view-buf (concat "*Documents[" (car pro) "]*"))
	 (header-start (concat "h: help, n: new document, a[A]: git add[all], c[C]:commit[all], l: git log, u[U]: update[all]"
			       "\nProject: " (car pro)
			       "\nControl: " (or (superman-view-control) (concat "press `I' to initialize git"))
			       "\n\nDocuments: " "\n"))
	 (shared-header
	  (superman-view-documents-format "header" 0 nil nil '(("GitStatus" .  "GitStatus") ("LastCommit" . "LastCommit") ("FileName" . "FileName"))))
	 (cmd-block
	  (if cats
	      (mapcar '(lambda (cat)
			 (list 'tags (concat "FileName={.+}" "+" "CATEGORY=\"" (car cat) "\"")
			       (let ((hdr (if (eq cat cat-number-one)
					      (concat header-start "cat: " (car cat) "\n" shared-header)
					    (concat "cat: " (car cat) "\n" shared-header))))
				 `((org-agenda-overriding-header ,hdr)))))
		      cats)
	    `((tags "FileName={.+}" ((org-agenda-overriding-header (concat ,header-start ,shared-header)))))))
	 (org-agenda-custom-commands
	  `(("d" "view Project-DOCUMENTS"
	     ,cmd-block
	     ((org-agenda-finalize-hook 'superman-view-finalize-documents)
	      (org-agenda-property-list (quote (,(superman-property 'gitstatus) ,(superman-property 'lastcommit) ,(superman-property 'filename)))) (org-agenda-property-list '("GitStatus" "LastCommit" "FileName"))
	      (org-agenda-view-columns-initially nil)
	      (org-agenda-buffer-name (concat "*Documents[" ,(car pro) "]*"))
	      (org-agenda-overriding-agenda-format 'superman-view-documents-format)
	      (org-agenda-files (quote (,(superman-get-index pro))))
	      (org-agenda-overriding-buffer-name (concat "*Project[" ,(car pro) "]*")))))))
    (push ?d unread-command-events)
    (call-interactively 'org-agenda)))

;; (org-let lprops '(superman-tags-view-plus nil "FileName={.+}" '("GitStatus" "LastCommit" "FileName")))
;; (rename-buffer view-buf)
;; (put 'org-agenda-redo-command 'org-lprops lprops)))


;; (defun superman-view-documents-1 (&optional project)
;;   "View documents of the current project."
;;   (interactive)
;;   (let* ((pro (or project superman-current-project (superman-select-project)))
;; 	 (loc (concat (superman-get-location pro) (car pro)))
;; 	 (view-buf (concat "*Documents[" (car pro) "]*")))
;;     (if (get-buffer view-buf)
;; 	(switch-to-buffer view-buf)
;;       (put 'org-agenda-redo-command 'org-lprops nil)
;;       (let ((lprops
;; 	     `((org-agenda-files (quote (,(superman-get-index pro))))
;; 	       (org-agenda-finalize-hook 'superman-view-finalize-documents)
;; 	       (org-agenda-overriding-header
;; 		(concat "h: help, n: new document, a[A]: git add[all], c[C]:commit[all], l: git log, u[U]: update[all]"
;; 			"\nProject: "  ,(car pro)
;; 			"\nControl: " (or (superman-view-control) 
;; 					  (concat "press `I' to initialize git"))
;; 			"\n\nDocuments: " "\n"
;; 			(superman-view-documents-format "header" 0 nil nil '(("GitStatus" .  "GitStatus") ("LastCommit" . "LastCommit") ("FileName" . "FileName")))))
;; 	       (org-agenda-overriding-agenda-format 'superman-view-documents-format)
;; 	       (org-agenda-view-columns-initially nil)
;; ;;	       (org-agenda-buffer-name (concat "*Documents[" ,(car pro) "]*")))))
;; 	(org-let lprops '(superman-tags-view-plus nil (concat (superman-property 'filename) "={.+}") '("GitStatus" "LastCommit" "FileName")))
;; 	(rename-buffer view-buf)
;; 	(put 'org-agenda-redo-command 'org-lprops lprops)))))

;;{{{ copy of org-tags-view

;;;###autoload

(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq todo-only (car org-agenda-overriding-arguments)
	    match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
	  org-tags-match-list-sublevels)
	 (completion-ignore-case t)
	 rtn rtnall files file pos matcher
	 buffer)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (setq matcher (org-make-tags-matcher match)
	  match (car matcher) matcher (cdr matcher))
    (catch 'exit
      (if org-agenda-overriding-buffer-name
	  (setq org-agenda-buffer-name org-agenda-overriding-buffer-name)
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(if (stringp match)
		    (format "*Org Agenda(%s:%s)*"
			    (or org-keys (or (and todo-only "M") "m")) match)
		  (format "*Org Agenda(%s)*" (or (and todo-only "M") "m"))))))
      (org-agenda-prepare (concat "TAGS " match))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
      	    (list 'org-tags-view `(quote ,todo-only)
      		  (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
      (setq files (org-agenda-files nil 'ifmode)
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (if (not buffer)
	      ;; If file does not exist, error message to agenda
	      (setq rtn (list
			 (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		    rtnall (append rtnall rtn))
	    (with-current-buffer buffer
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in `org-mode'" file))
	      (save-excursion
		(save-restriction
		  (if org-agenda-restrict
		      (narrow-to-region org-agenda-restrict-begin
					org-agenda-restrict-end)
		    (widen))
		  (setq rtn (org-scan-tags 'agenda matcher todo-only))
		  (setq rtnall (append rtnall rtn))))))))
      (if org-agenda-overriding-header
	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
		      nil 'face 'org-agenda-structure) "\n")
	(insert "Headlines with TAGS match: ")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-agenda-structure
				   'short-heading
				   (concat "Match: " match)))
	(setq pos (point))
	(insert match "\n")
	(add-text-properties pos (1- (point)) (list 'face 'org-warning))
	(setq pos (point))
	(unless org-agenda-multi
	  (insert "Press `C-u r' to search again with new search string\n"))
	(add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type tags
					     org-last-args (,todo-only ,match)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

;;}}}

;;{{{ copy of org-agenda

(defun org-agenda (&optional arg org-keys restriction)
  "Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
L     Create a timeline for the current buffer.
e     Export views to associated files.
s     Search entries for keywords.
S     Search entries for keywords, only with TODO keywords.
/     Multi occur across all agenda files and also files listed
      in `org-agenda-text-search-extra-files'.
<     Restrict agenda commands to buffer, subtree, or region.
      Press several times to get the desired effect.
>     Remove a previous restriction.
#     List \"stuck\" projects.
!     Configure what \"stuck\" means.
C     Configure custom agenda commands.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org-mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of \\[org-agenda]) restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active)."
  (interactive "P")
  (catch 'exit
    (let* ((prefix-descriptions nil)
	   (org-agenda-buffer-name org-agenda-buffer-name)
	   (org-agenda-window-setup (if (equal (buffer-name)
					       org-agenda-buffer-name)
					'current-window
				      org-agenda-window-setup))
	   (org-agenda-custom-commands-orig org-agenda-custom-commands)
	   (org-agenda-custom-commands
	    ;; normlize different versions
	    (delq nil
		  (mapcar
		   (lambda (x)
		     (cond ((stringp (cdr x))
			    (push x prefix-descriptions)
			    nil)
			   ((stringp (nth 1 x)) x)
			   ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
			   (t (cons (car x) (cons "" (cdr x))))))
		   org-agenda-custom-commands)))
	   (org-agenda-custom-commands
	    (org-contextualize-keys
	     org-agenda-custom-commands org-agenda-custom-commands-contexts))
	   (buf (current-buffer))
	   (bfn (buffer-file-name (buffer-base-buffer)))
	   entry key type org-match lprops ans)
      ;; Turn off restriction unless there is an overriding one,
      (unless org-agenda-overriding-restriction
	(unless (org-bound-and-true-p org-agenda-keep-restricted-file-list)
	  ;; There is a request to keep the file list in place
	  (put 'org-agenda-files 'org-restrict nil))
	(setq org-agenda-restrict nil)
	(move-marker org-agenda-restrict-begin nil)
	(move-marker org-agenda-restrict-end nil))
      ;; Delete old local properties
      (put 'org-agenda-redo-command 'org-lprops nil)
      ;; Delete previously set last-arguments
      (put 'org-agenda-redo-command 'last-args nil)
      ;; Remember where this call originated
      (setq org-agenda-last-dispatch-buffer (current-buffer))
      (unless org-keys
	(setq ans (org-agenda-get-restriction-and-command prefix-descriptions)
	      org-keys (car ans)
	      restriction (cdr ans)))
      ;; If we have sticky agenda buffers, set a name for the buffer,
      ;; depending on the invoking keys.  The user may still set this
      ;; as a command option, which will overwrite what we do here.
      (if org-agenda-overriding-buffer-name
	  (setq org-agenda-buffer-name org-agenda-overriding-buffer-name)
	(if org-agenda-sticky
	    (setq org-agenda-buffer-name
		  (format "*Org Agenda(%s)*" org-keys))))
      ;; Establish the restriction, if any
      (when (and (not org-agenda-overriding-restriction) restriction)
	(put 'org-agenda-files 'org-restrict (list bfn))
	(cond
	 ((eq restriction 'region)
	  (setq org-agenda-restrict t)
	  (move-marker org-agenda-restrict-begin (region-beginning))
	  (move-marker org-agenda-restrict-end (region-end)))
	 ((eq restriction 'subtree)
	  (save-excursion
	    (setq org-agenda-restrict t)
	    (org-back-to-heading t)
	    (move-marker org-agenda-restrict-begin (point))
	    (move-marker org-agenda-restrict-end
			 (progn (org-end-of-subtree t)))))))

      ;; For example the todo list should not need it (but does...)
      (cond
       ((setq entry (assoc org-keys org-agenda-custom-commands))
	(if (or (symbolp (nth 2 entry)) (functionp (nth 2 entry)))
	    (progn
	      (setq type (nth 2 entry) org-match (eval (nth 3 entry))
		    lprops (nth 4 entry))
	      (if org-agenda-sticky
		  (setq org-agenda-buffer-name
			(or (and (stringp org-match) (format "*Org Agenda(%s:%s)*" org-keys org-match))
			    (format "*Org Agenda(%s)*" org-keys))))
	      (put 'org-agenda-redo-command 'org-lprops lprops)
	      (cond
	       ((eq type 'agenda)
		(org-let lprops '(org-agenda-list current-prefix-arg)))
	       ((eq type 'alltodo)
		(org-let lprops '(org-todo-list current-prefix-arg)))
	       ((eq type 'search)
		(org-let lprops '(org-search-view current-prefix-arg org-match nil)))
	       ((eq type 'stuck)
		(org-let lprops '(org-agenda-list-stuck-projects
				  current-prefix-arg)))
	       ((eq type 'tags)
		(org-let lprops '(org-tags-view current-prefix-arg org-match)))
	       ((eq type 'tags-todo)
		(org-let lprops '(org-tags-view '(4) org-match)))
	       ((eq type 'todo)
		(org-let lprops '(org-todo-list org-match)))
	       ((eq type 'tags-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-match-sparse-tree current-prefix-arg org-match)))
	       ((eq type 'todo-tree)
		(org-check-for-org-mode)
		(org-let lprops
		  '(org-occur (concat "^" org-outline-regexp "[ \t]*"
				      (regexp-quote org-match) "\\>"))))
	       ((eq type 'occur-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-occur org-match)))
	       ((functionp type)
		(org-let lprops '(funcall type org-match)))
	       ((fboundp type)
		(org-let lprops '(funcall type org-match)))
	       (t (error "Invalid custom agenda command type %s" type))))
	  (org-agenda-run-series (nth 1 entry) (cddr entry))))
       ((equal org-keys "C")
	(setq org-agenda-custom-commands org-agenda-custom-commands-orig)
	(customize-variable 'org-agenda-custom-commands))
       ((equal org-keys "a") (call-interactively 'org-agenda-list))
       ((equal org-keys "s") (call-interactively 'org-search-view))
       ((equal org-keys "S") (org-call-with-arg 'org-search-view (or arg '(4))))
       ((equal org-keys "t") (call-interactively 'org-todo-list))
       ((equal org-keys "T") (org-call-with-arg 'org-todo-list (or arg '(4))))
       ((equal org-keys "m") (call-interactively 'org-tags-view))
       ((equal org-keys "M") (org-call-with-arg 'org-tags-view (or arg '(4))))
       ((equal org-keys "e") (call-interactively 'org-store-agenda-views))
       ((equal org-keys "?") (org-tags-view nil "+FLAGGED")
	(org-add-hook
	 'post-command-hook
	 (lambda ()
	   (unless (current-message)
	     (let* ((m (org-agenda-get-any-marker))
		    (note (and m (org-entry-get m "THEFLAGGINGNOTE"))))
	       (when note
		 (message (concat
			   "FLAGGING-NOTE ([?] for more info): "
			   (org-add-props
			       (replace-regexp-in-string
				"\\\\n" "//"
				(copy-sequence note))
			       nil 'face 'org-warning)))))))
	 t t))
       ((equal org-keys "L")
	(unless (derived-mode-p 'org-mode)
	  (error "This is not an Org-mode file"))
	(unless restriction
	  (put 'org-agenda-files 'org-restrict (list bfn))
	  (org-call-with-arg 'org-timeline arg)))
       ((equal org-keys "#") (call-interactively 'org-agenda-list-stuck-projects))
       ((equal org-keys "/") (call-interactively 'org-occur-in-agenda-files))
       ((equal org-keys "!") (customize-variable 'org-stuck-projects))
       (t (error "Invalid agenda key"))))))

;;}}}

;; hack of org-tags-view version 7.9.2 (copy: 06 Jan 2013 (10:36))
;; replaces org-scan-tags by superman-scan-tags-plus
;; additional argument: get-properties
;; (defun superman-tags-view-plus (&optional todo-only match get-prop-list)
(defun superman-tags-view-plus (&optional todo-only match get-prop-list)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

Additional properties can be specified as a list of property keywords GET-PROP-LIST."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq todo-only (car org-agenda-overriding-arguments)
	    match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
	  org-tags-match-list-sublevels)
	 (completion-ignore-case t)
	 rtn rtnall files file pos matcher
	 buffer)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (setq matcher (org-make-tags-matcher match)
	  match (car matcher) matcher (cdr matcher))
    (catch 'exit
      (if org-agenda-sticky
	  (setq org-agenda-buffer-name
		(if (stringp match)
		    (format "*Org Agenda(%s:%s)*"
			    (or org-keys (or (and todo-only "M") "m")) match)
		  (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
      (org-agenda-prepare (concat "TAGS " match))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
	    (list 'superman-tags-view-plus `(quote ,todo-only)
		  (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))
		  `(quote ,get-prop-list)))
      ;; (put 'org-agenda-redo-command 'org-lprops)
      (setq files (org-agenda-files nil 'ifmode)
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (if (not buffer)
	      ;; If file does not exist, error message to agenda
	      (setq rtn (list
			 (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		    rtnall (append rtnall rtn))
	    (with-current-buffer buffer
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in `org-mode'" file))
	      (save-excursion
		(save-restriction
		  (if org-agenda-restrict
		      (narrow-to-region org-agenda-restrict-begin
					org-agenda-restrict-end)
		    (widen))
		  (setq rtn (superman-scan-tags-plus 'agenda matcher todo-only nil get-prop-list))
		  (setq rtnall (append rtnall rtn))))))))
      (if org-agenda-overriding-header
	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
		      nil 'face 'org-agenda-structure) "\n")
	(insert "Headlines with TAGS match: ")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-agenda-structure
				   'short-heading
				   (concat "Match: " match)))
	(setq pos (point))
	(insert match "\n")
	(add-text-properties pos (1- (point)) (list 'face 'org-warning))
	(setq pos (point))
	(unless org-agenda-multi
	  (insert "Press `C-u r' to search again with new search string\n"))
	(add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
      (org-agenda-mark-header-line (point-min))
      (when rtnall
	(insert (org-agenda-finalize-entries rtnall) "\n"))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type tags
					     org-last-args (,todo-only ,match)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (org-agenda-finalize)
      (setq buffer-read-only t))))

;; hack of org-scan-tags version 7.9.2 (copy: 06 Jan 2013 (10:28))
;; additional argument: get-prop-list
;; (defun superman-scan-tags-plus (action matcher todo-only &optional start-level get-prop-list)
(defun org-scan-tags (action matcher todo-only &optional start-level)
  "Scan headline tags with inheritance and produce output ACTION.

ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
or `agenda' to produce an entry list for an agenda view.  It can also be
a Lisp form or a function that should be called at each matched headline, in
this case the return value is a list of all return values from these calls.

MATCHER is a Lisp form to be evaluated, testing if a given set of tags
qualifies a headline for inclusion.  When TODO-ONLY is non-nil,
only lines with a not-done TODO keyword are included in the output.
This should be the same variable that was scoped into
and set by `org-make-tags-matcher' when it constructed MATCHER.

START-LEVEL can be a string with asterisks, reducing the scope to
headlines matching this string."
  (require 'org-agenda) 
  (let* ((re (concat "^"
		     (if start-level
			 ;; Get the correct level to match
			 (concat "\\*\\{" (number-to-string start-level) "\\} ")
		       org-outline-regexp)
		     " *\\(\\<\\("
		     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		     (org-re "\\)\\>\\)? *\\(.*?\\)\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*$")))
	 (props (list 'face 'default
		      'done-face 'org-agenda-done
		      'undone-face 'default
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 (case-fold-search nil)
	 (org-map-continue-from nil)
         lspos tags tags-list
	 (tags-alist (list (cons 0 org-file-tags)))
	 (llast 0) rtn rtn1 level category i txt
	 todo prop-list marker entry priority)
    (when (not (or (member action '(agenda sparse-tree)) (functionp action)))
      (setq action (list 'lambda nil action)))
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
	(org-overview)
	(org-remove-occur-highlights))
      (while (re-search-forward re nil t)
	(setq org-map-continue-from nil)
	(catch :skip
	  (setq todo (if (match-end 1) (org-match-string-no-properties 2))
		tags (if (match-end 4) (org-match-string-no-properties 4)))
	  (goto-char (setq lspos (match-beginning 0)))
	  (setq level (org-reduced-level (org-outline-level))
		category (org-get-category))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the next tags
	  (when tags
	    (setq tags (org-split-string tags ":")
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr (reverse tags-alist)))
		  tags)
		org-scanner-tags tags-list)
	  (when org-use-tag-inheritance
	    (setcdr (car tags-alist)
		    (mapcar (lambda (x)
			      (setq x (copy-sequence x))
			      (org-add-prop-inherited x))
			    (cdar tags-alist))))
	  (when (and tags org-use-tag-inheritance
		     (or (not (eq t org-use-tag-inheritance))
			 org-tags-exclude-from-inheritance))
	    ;; selective inheritance, remove uninherited ones
	    (setcdr (car tags-alist)
		    (org-remove-uninherited-tags (cdar tags-alist))))
	  (when (and
		 ;; eval matcher only when the todo condition is OK
		 (and (or (not todo-only) (member todo org-not-done-keywords))
		      (let ((case-fold-search t) (org-trust-scanner-tags t))
			(eval matcher)))
		 ;; Call the skipper, but return t if it does not skip,
		 ;; so that the `and' form continues evaluating
		 (progn
		   (unless (eq action 'sparse-tree) (org-agenda-skip))
		   t)
		 ;; Check if timestamps are deselecting this entry
		 (or (not todo-only)
		     (and (member todo org-not-done-keywords)
			  (or (not org-agenda-tags-todo-honor-ignore-options)
			      (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item)))))
		 ;; Extra check for the archive tag
		 ;; FIXME: Does the skipper already do this????
		 (or
		  (not (member org-archive-tag tags-list))
		  ;; we have an archive tag, should we use this anyway?
		  (or (not org-agenda-skip-archived-trees)
		      (and (eq action 'agenda) org-agenda-archives-mode))))
	    ;; extract properties
	    (if org-agenda-property-list
		;; (if get-prop-list
		;; (setq prop-list
		;; (mapcar
		;; '(lambda (prop)
		;; (cons prop (or (superman-get-property (point) prop 'inherit)
		;; "not set"
		;; ""))) get-prop-list))
		(setq prop-list
		      (mapcar
		       '(lambda (prop)
			  (cons prop (or (superman-get-property (point) prop 'inherit)
					 "not set"
					 ""))) org-agenda-property-list))
	      (setq prop-list ""))
	    ;; select this headline
	    (cond
	     ((eq action 'sparse-tree)
	      (and org-highlight-sparse-tree-matches
		   (org-get-heading) (match-end 0)
		   (org-highlight-new-match
		    (match-beginning 1) (match-end 1)))
	      (org-show-context 'tags-tree))
	     ((eq action 'agenda)
	      (if org-agenda-overriding-agenda-format
		  (setq txt (funcall org-agenda-overriding-agenda-format
				     (org-get-heading 'no-tags 'no-todo)
				     level
				     category
				     tags-list
				     prop-list))
		(setq txt (org-agenda-format-item
			   ""
			   (concat
			    (if (eq org-tags-match-list-sublevels 'indented)
				(make-string (1- level) ?.) "")
			    (org-get-heading))
			   level category
			   tags-list)))
	      (setq priority (org-get-priority txt))
	      (goto-char lspos)
	      (setq marker (org-agenda-new-marker))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker marker 'org-category category
		'todo-state todo
		'priority priority 'type "tagsmatch")
	      (push txt rtn))
	     ((functionp action)
	      (setq org-map-continue-from nil)
	      (save-excursion
		(setq rtn1 (funcall action))
		(push rtn1 rtn)))
	     (t (error "Invalid action")))
	    ;; if we are to skip sublevels, jump to end of subtree
	    (unless org-tags-match-list-sublevels
	      (org-end-of-subtree t)
	      (backward-char 1))))
	;; Get the correct position from where to continue
	(if org-map-continue-from
	    (goto-char org-map-continue-from)
	  (and (= (point) lspos) (end-of-line 1)))))
    (when (and (eq action 'sparse-tree)
	       (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))



;; hack of org-agenda-todo version 7.9.2 (copy: 05 Jan 2013 (10:28))
;; to remote control git status (instead of TODO status)
(defun superman-document-status (&optional arg)
  "Cycle document state of line at point, also in Org-mode file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
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
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(let ((current-prefix-arg arg)
	      (statlist (call-interactively 'superman-git-set-status-at-point)))
	  (and (bolp) (forward-char 1))
	(setq newhead (concat " " (nth 1 statlist) " " (org-get-heading))))
	(when (and (org-bound-and-true-p
		    org-agenda-headline-snapshot-before-repeat)
		   (not (equal org-agenda-headline-snapshot-before-repeat
			       newhead))
		   todayp)
	  (setq newhead org-agenda-headline-snapshot-before-repeat
		just-one t))
	(save-excursion
	  (org-back-to-heading)
	  (move-marker org-last-heading-marker (point))))
      (beginning-of-line 1)
      (save-excursion
	(org-agenda-change-all-lines newhead hdmarker 'fixface just-one))
      (org-move-to-column col))))

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

;;}}}
;;{{{ summary-view commands

;;(require 'superman-git)

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
  (superman-view-loop 'superman-view-git-set-status (list nil nil nil))
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


(defun superman-view-loop (fun args)
  "Call function on all items."
  (let (loop-out (i t))
  (save-excursion
    (goto-char (point-min))
    (org-agenda-next-item 1)
    (while i
      (setq loop-out (append (list (apply fun args)) loop-out))
      (move-end-of-line 1)
      (goto-char (next-single-property-change (point) 'org-marker))
      (setq i (next-single-property-change (point-at-eol) 'org-marker)))
    loop-out)))

(defun superman-view-git-add-all (&optional dont-redo)
  (interactive)
   (superman-view-loop 'superman-view-git-add (list 'dont))
   (unless dont-redo (org-agenda-redo)))

(defun superman-view-git-commit-all (&optional commit dont-redo)
  (interactive)
  (let* ((pro (superman-view-current-project))
	 (dir (concat (superman-get-location pro) (car pro))))
    ;; (files (superman-view-loop 'superman-filename-at-point (list nil))))
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
     :lighter " pro"
     :group 'org
     :keymap 'superman-view-mode-map)

(defun superman-view-mode-on ()
  (interactive)
  (superman-view-mode t))
   
(define-key superman-view-mode-map [return] 'superman-view-return) ;; Return is not used anyway in column mode
(define-key superman-view-mode-map "l" 'superman-view-git-log) 
(define-key superman-view-mode-map "L" 'superman-view-git-log-decorationonly)
(define-key superman-view-mode-map "n" 'superman-view-register-document)
(define-key superman-view-mode-map "r" 'org-agenda-redo)
;; (define-key superman-view-mode-map "R" 'superman-view-git)
(define-key superman-view-mode-map "a" 'superman-view-git-add)
(define-key superman-view-mode-map "A" 'superman-view-git-add-all)
(define-key superman-view-mode-map "S" 'superman-view-git-search)
(define-key superman-view-mode-map "h" 'superman-show-help)
(define-key superman-view-mode-map "u" 'superman-view-update)
(define-key superman-view-mode-map "I" 'superman-view-git-init)
(define-key superman-view-mode-map "U" 'superman-view-update-all)
(define-key superman-view-mode-map "c" 'superman-view-git-commit)
(define-key superman-view-mode-map "C" 'superman-view-git-commit-all)
;; (define-key superman-view-mode-map "c" 'org-agenda-columns)


(defun superman-show-help ()
  (interactive)
  (split-window-vertically)  
  (other-window 1)
  (switch-to-buffer "*superman-help-buffer*")
  (toggle-read-only -1)
  (erase-buffer)
  (insert "'<ret>':\t\t Open file (or revision) at point\n")
  (insert "'l':    \t\t Show git log\n")
  (insert "'L':    \t\t Show git log for tagged revisions\n")
  (insert "'u':    \t\t Update git status\n")
  (insert "'A':    \t\t Add to git repository\n")
  (insert "'a':    \t\t Add file to document list\n")
  (insert "'b':    \t\t Blame\n")
  (insert "'S':    \t\t Search for revision containing a regular expression\n")
  (insert "'D':    \t\t Show difference between revision at point and HEAD\n")
  (insert "'h':    \t\t Open this help window\n")
  (insert "'t':    \t\t Alter tag (empty string to remove)\n")
  (insert "'q':    \t\t Quit view mode\n")
  (goto-char (point-min))
  (toggle-read-only 1)
  (other-window -1))

;;}}}
;;{{{ remote control
;; hack of org-agenda-todo
(defun superman-treat-document (&optional arg)
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
		       (superman-git-revision-at-point)
		       (let ((buffer-file-name (expand-file-name (buffer-name)))) (normal-mode))))
		 (if (re-search-forward ":filename:" nil t)
		     (progn 
		       (widen)
		       (org-open-at-point-global)
		       (widen))))
		((string= prop "GitStatus")
		 (nth 1 (superman-git-get-status-at-point))
		 (org-columns-redo))
		((string= prop "Decoration")
		 (superman-git-tag-at-point)
		 (org-columns-redo))
		((string= prop "filename")    
		 (org-columns-open-link))
		((string= prop "Other")
		 (org-columns-open-link))
		((string= prop "Hash")
		 (superman-git-revision-at-revision))
		((string= prop "LastCommit")
		 (superman-git-commit-at-point)
		 (org-columns-redo))
		(t (org-columns-edit-value)))))
	(save-excursion
	  (org-agenda-change-all-lines newhead hdmarker 'fixface just-one))
	(org-move-to-column col)))
  org-columns-redo)

;;}}}
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
      (org-narrow-to-element)
      (if prop-alist (mapcar (lambda (p)
			       (unless (org-entry-get nil (car p))
				 (org-set-property (car p) (car (cdr p))))) prop-alist))
      (goto-char (point-max)))))
      ;; (forward-line)
      ;; (unless (looking-at "^[ \t]*$") (progn (insert "\n") (forward-line -1))))))

(defun superman-goto-project-workflow ()
  (interactive)
  (or (superman-goto-project nil "WorkFlow" 'create)))

(defun superman-goto-project-notes ()
  (interactive)
  (or (superman-goto-project nil "Notes" 'create)))

(defun superman-goto-project-documents (&optional narrow)
  (interactive)
  (or (superman-goto-project nil "Documents" 'create '(("COLUMNS" "%20ITEM(Title) %GitStatus(Git Status) %50LastCommit(Last Commit) %8TODO(ToDo)")))
      (if narrow (superman-view-mode t))))

(defun superman-insert-list-files (&optional postfix)
  (interactive)
  (let ((files (superman-list-files "." 
				   (or postfix (concat (read-string "Postfix: ") "$")) nil)))
    (insert "\n")
    (loop for x in files
	  do 
	  (insert (concat "*** " (file-relative-name x) "\n:PROPERTIES:\n:filename: [[" (file-relative-name x) "]]\n:END:\n")))))

(defun superman-goto-project-taskpool (&optional arg)
  (interactive)
  (if arg (org-store-link nil))
  (let* ((buf (current-buffer))
	 (pro (car (superman-select-project)))
	 ;;             (pro (ido-completing-read "Select project: " superman-project-alist))
	 (entry (assoc pro superman-project-alist))
	 (index (superman-get-index entry)))
    (if index
	(find-file index)
      (error (concat "Project " pro " does not have an index.")))
    (goto-char (point-min))
    (or (re-search-forward "^[*]+ TaskPool" nil t)
	(progn
	  (goto-char (point-max))
	  (insert "\n\n* TaskPool [0/0]\n")
	  (point))) 
    (org-end-of-line)))


;; Choose a prefix
(setq superman-capture-prefix "P")
(add-to-list 'org-capture-templates `(,superman-capture-prefix "Project management") 'append)
(defun superman-capture() 
  (interactive)
  (push (string-to-char superman-capture-prefix) unread-command-events)
  (call-interactively 'org-capture))
;; Capturing links 
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "l") "Add link" plain 
				      (function (lambda () (superman-goto-project nil "Links" 'yes))) "\n - %x%?") 'append)
;; Capturing tasks
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "t") "Add task" plain
				      (function superman-goto-project-taskpool) "\n*** TODO %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "c") "Add checklist item" plain
				      (function superman-goto-project-taskpool) "\n- [ ] %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing notes
(add-to-list 'org-capture-templates `(,(concat superman-capture-prefix "n") "Add note" plain
				      (function superman-goto-project-notes) "\n*** %? \n:PROPERTIES:\n:NoteDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing documents
(add-to-list 'org-capture-templates
	     `(,(concat superman-capture-prefix "d") "Add document" plain
	       (function superman-goto-project-documents) "\n*** %? \n:PROPERTIES:\n:filename: [[%(read-file-name \"Document file: \")]]\n:CaptureDate: %T\n:END:") 'append)

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
      (superman-goto-project pro "Documents")
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



