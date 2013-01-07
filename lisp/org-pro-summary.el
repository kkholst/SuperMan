;;; org-pro-summary.el --- Summary of project contents and adding information to projects

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

(defun org-pro-view-project (&optional project)
  (interactive)
  (let ((pro (or project org-pro-current-project (org-pro-select-project))))
    (switch-to-buffer (concat "*" (car pro) " view*"))
    (local-set-key "d" 'org-pro-view-documents)))


(defun org-pro-summary ()
  (interactive)
  (let ((org-agenda-files (org-pro-get-index org-pro-current-project)))
    (org-pro-tags-view-plus nil "filename={.+}" nil)))


(defun org-pro-view-documents (&optional project)
  "View documents of the current project"
  (interactive)
  (let* ((pro (or project org-pro-current-project (org-pro-select-project)))
         (lprops
	  `((org-agenda-files (quote (,(org-pro-get-index pro))))
	   (org-agenda-overriding-header (concat "Documents in " ,(car pro) "\th: help, C:commit, l: log, H:history\n\n title, git status, last commit"))
	   ;; (org-agenda-overriding-agenda-format t)
	   (org-agenda-overriding-agenda-format
	    '(lambda (hdr level category tags-list properties)
	       (concat " " hdr
		       (let ((cprops properties)
			     (pstring ""))
			 (while cprops
			   (setq pstring (concat pstring " | " (cdr (car cprops))))
			   (setq cprops (cdr cprops)))
			 pstring))))
	   (org-agenda-view-columns-initially nil))))
    (put 'org-agenda-redo-command 'org-lprops lprops)
    (org-let lprops '(org-pro-tags-view-plus nil "filename={.+}" '("GitStatus" "LastCommit")))
    (org-pro-view-mode t)))



(defun org-pro-view-documents-1 (&optional project)
  "View documents of the current project"
  (interactive)
  (let* ((pro (or project org-pro-current-project (org-pro-select-project)))
	 (index (org-pro-get-index pro))
	 (org-agenda-custom-commands
	  `(("d" "view Project-DOCUMENTS" tags "filename={.+}"
	     ((org-agenda-files `(,index))
	      (org-agenda-overriding-columns-format "%20ITEM(Title) %8TODO(ToDo) %GitStatus %50LastCommit(Last Commit)")
              (org-agenda-overriding-header "Documents:\n")
	      ;; (org-agenda-finalize-hook '(org-pro-show-document-status-in-summary))
	      (org-agenda-view-columns-initially nil))))))
    (push ?d unread-command-events)
    (call-interactively 'org-agenda)
    (org-pro-view-mode t)))

;; hack of org-tags-view version 7.9.2 (copy: 06 Jan 2013 (10:36))
;; replaces org-scan-tags by org-pro-scan-tags-plus
;; additional argument: get-properties
(defun org-pro-tags-view-plus (&optional todo-only match get-properties)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

Additional properties can be specified as a list of property keywords GET-PROPERTIES."
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
      	    (list 'org-pro-tags-view-plus `(quote ,todo-only)
      		  (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))
		  `(quote ,get-properties)))
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
		  (setq rtn (org-pro-scan-tags-plus 'agenda matcher todo-only nil get-properties))
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
;; additional argument: get-properties
(defun org-pro-scan-tags-plus (action matcher todo-only &optional start-level get-properties)
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
headlines matching this string.

GET-PROPERTIES is a list of properties to get from each entry."
  
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
	 todo properties marker entry priority)
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
	    (if get-properties
	      (setq properties
		    (mapcar
		     '(lambda (prop)
			(cons prop (or (org-pro-get-property (point) prop 'inherit)
				       "not set"
				       ""))) get-properties))
	    (setq properties ""))

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
				     properties))
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
(defun org-pro-document-status (&optional arg)
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
	      (statlist (call-interactively 'org-pro-git-set-status-at-point)))
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
(require 'org-pro-git)

(defun org-pro-view-git-return ()
  (interactive)
  (let* ((limit (if (= arg 1) org-pro-git-log-limit (or arg org-pro-git-log-limit)))
	 (m (org-get-at-bol 'org-hd-marker))
	 (file (replace-regexp-in-string "\\]+$" "" (replace-regexp-in-string "^\\[+" "" (org-pro-get-property m "filename")))))
    (org-pro-git-log file nil limit nil nil)))

(defun org-pro-view-git-log (arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) org-pro-git-log-limit (or arg org-pro-git-log-limit)))
	 (m (org-get-at-bol 'org-hd-marker))
	 (file (replace-regexp-in-string "\\]+$" "" (replace-regexp-in-string "^\\[+" "" (org-pro-get-property m "filename")))))
    (org-pro-git-log file nil limit nil nil)))

(defun org-pro-view-git-set-status ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (filename (replace-regexp-in-string "\\]+$" "" (replace-regexp-in-string "^\\[+" "" (org-pro-get-property m "filename")))))
    (org-pro-git-set-status m filename)))

(defun org-pro-view-git-commit-file ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (filename (replace-regexp-in-string "\\]+$" "" (replace-regexp-in-string "^\\[+" "" (org-pro-get-property m "filename"))))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (message (read-string (concat "Commit message for " (file-name-nondirectory file) ": "))))
    (org-pro-git-add-and-commit-file file dir message)
    (org-pro-git-set-status m filename)))



;;}}}
;;{{{ summary-view-mode

(defvar org-pro-view-mode-map (make-sparse-keymap)
  "Keymap used for `org-pro-view-mode' commands.")
   
(define-minor-mode org-pro-view-mode 
     "Toggle org projectmanager document view mode.
                   With argument ARG turn org-pro-docview-mode on if ARG is positive, otherwise
                   turn it off.
                   
                   Enabling org-pro-view mode electrifies the column view for documents
                   for git and other actions like commit, history search and pretty log-view."
     :lighter " pro"
     :group 'org
     :keymap 'org-pro-view-mode-map)
   
(define-key org-pro-view-mode-map "\r" 'org-pro-agenda-find-document) ;; Return is not used anyway in column mode
(define-key org-pro-view-mode-map "l" 'org-pro-agenda-git-log) 
;; (define-key org-pro-view-mode-map "L" (lambda () (interactive) (org-pro-git-log-at-point 1 nil nil nil nil t)))
(define-key org-pro-view-mode-map "S" 'org-pro-git-search-at-point)
(define-key org-pro-view-mode-map "b" 'org-pro-git-blame-at-point)
(define-key org-pro-view-mode-map "t" 'org-pro-git-tag-at-point)
(define-key org-pro-view-mode-map "h" 'org-pro-show-help)
(define-key org-pro-view-mode-map "D" (lambda () (interactive) (org-pro-git-revision-at-point 1)))
(define-key org-pro-view-mode-map "u" 'org-pro-update-git-status)
(define-key org-pro-view-mode-map "C" 'org-pro-agenda-git-commit-file)
(define-key org-pro-view-mode-map "c" 'org-agenda-columns)

(defun org-pro-column-action ()
  (interactive)
  (let* ((prop (get-char-property (point) 'org-columns-key))
	 (tempstr) (tempfrm))
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



;;}}}
;;{{{ showing document properties

(setq org-agenda-document-properties-maxlines 5)

(defun org-agenda-add-document-properties ()
  "Add entry text to agenda lines.
This will add properties following headings shown in the agenda.
"
  (when (not (org-bound-and-true-p org-mobile-creating-agendas))
    (let (m txt)
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (setq m (org-get-at-bol 'org-hd-marker)))
	    (beginning-of-line 2)
	  (setq txt (org-agenda-get-some-document-properties
		     m org-agenda-add-document-properties-maxlines "    > "))
	  (end-of-line 1)
	  (if (string-match "\\S-" txt)
	      (insert "\n" txt)
	    (or (eobp) (forward-char 1))))))))

(defun org-agenda-get-some-document-properties (marker n-lines &optional indent
						       &rest keep)
  "Extract entry text from MARKER, at most N-LINES lines.
This will ignore drawers etc, just get the text.
If INDENT is given, prefix every line with this string.  If KEEP is
given, it is a list of symbols, defining stuff that should not be
removed from the entry content.  Currently only `planning' is allowed here."
  (let (txt drawer-re kwd-time-re ind)
    (save-excursion
      (with-current-buffer (marker-buffer marker)
	(if (not (derived-mode-p 'org-mode))
	    (setq txt "")
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char marker)
	      (setq txt
		    (concat (or (org-pro-get-property nil "GitStatus") "Not git controlled") "\n"
			    (org-pro-get-property nil "CaptureDate"))))))))))

(setq org-agenda-document-properties-mode t)
(defun org-agenda-document-properties-mode (&optional arg)
  "Toggle entry text mode in an agenda buffer."
  (interactive "P")
 (setq org-agenda-document-properties-mode (or (integerp arg)
                                       (not org-agenda-document-properties-mode)))
  (org-agenda-document-properties-hide)
  (and org-agenda-document-properties-mode
       (org-agenda-document-properties-show)))
  ;; (org-agenda-set-mode-name)
  ;; (message "Entry text mode is %s.  Maximum number of lines is %d"
	   ;; (if org-agenda-document-properties-mode "on" "off")
	   ;; (if (integerp arg) arg org-agenda-document-properties-maxlines)))

(defun org-agenda-document-properties-show-here ()
  "Add some text from the entry as context to the current line."
  (let (m txt o)
    (setq m (org-get-at-bol 'org-hd-marker))
    (unless (marker-buffer m)
      (error "No marker points to an entry here"))
    (setq txt (concat "\n" (org-no-properties
			    (org-agenda-get-some-document-properties
			     m org-agenda-document-properties-maxlines "    > "))))
    (when (string-match "\\S-" txt)
      (setq o (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'org-overlay-type 'agenda-entry-content)
      (overlay-put o 'after-string txt))))

(defun org-agenda-document-properties-show ()
  "Add entry context for all agenda lines."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line 1)
    (while (not (bobp))
      (when (org-get-at-bol 'org-hd-marker)
	(org-agenda-document-properties-show-here))
      (beginning-of-line 0))))

(defun org-agenda-document-properties-hide ()
  "Remove any shown entry context."
  (delq nil
	(mapcar (lambda (o)
		  (if (eq (overlay-get o 'org-overlay-type)
			  'agenda-entry-content)
		      (progn (delete-overlay o) t)))
		(overlays-in (point-min) (point-max)))))

;;}}}
;;{{{ remote control

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

;;}}}
;;{{{ capture documents, notes, etc.

(defun org-pro-goto-project (&optional project heading create prop-alist)
  (interactive)
  (let ((pro 
	 (or project
	     (car (org-pro-select-project)))))
    (when (and (not (string-equal pro "")) pro)
      (let* ((entry (assoc pro org-pro-project-alist))
	     (index (org-pro-get-index entry))
	     hiddenp
	     (head (or heading (read-string "Goto heading: "))))
	(if index
	    (find-file index)
	  (error (concat "Project " pro " does not have an index.")))
	(setq hiddenp (not visible-mode))
	(org-columns-quit)
	(if hiddenp (visible-mode t))
	(goto-char (point-min))
	(cond ((re-search-forward
		(format org-complex-heading-regexp-format (regexp-quote head))
		nil t))
	      (create
	       (insert "* " head "\n")
	       (forward-line -1))
	      (t (error (concat "Heading " head " not found in index file of " pro))))
	(if prop-alist (mapcar (lambda (p)
				 (unless (org-entry-get nil (car p))
				   (org-set-property (car p) (car (cdr p))))) prop-alist))
	(re-search-forward ":END:" nil t)
	(forward-line)
	(unless (looking-at "^[ \t]*$") (progn (insert "\n") (forward-line -1)))
	(if hiddenp (visible-mode nil))
        (org-show-subtree)))))

(defun org-pro-goto-project-workflow ()
  (interactive)
  (or (org-pro-goto-project nil "WorkFlow" 'create)))

(defun org-pro-goto-project-notes ()
  (interactive)
  (or (org-pro-goto-project nil "Notes" 'create)))

(defun org-pro-goto-project-documents (&optional narrow)
  (interactive)
  (or (org-pro-goto-project nil "Documents" 'create '(("COLUMNS" "%20ITEM(Title) %GitStatus(Git Status) %50LastCommit(Last Commit) %8TODO(ToDo)")))
      (if narrow (org-pro-view-mode t))))

(defun org-pro-insert-list-files (&optional postfix)
  (interactive)
  (let ((files (org-pro-list-files "." 
				   (or postfix (concat (read-string "Postfix: ") "$")) nil)))
    (insert "\n")
    (loop for x in files
	  do 
	  (insert (concat "*** " (file-relative-name x) "\n:PROPERTIES:\n:filename: [[" (file-relative-name x) "]]\n:END:\n")))))

(defun org-pro-goto-project-taskpool (&optional arg)
  (interactive)
  (if arg (org-store-link nil))
  (let* ((buf (current-buffer))
	 (pro (car (org-pro-select-project)))
	 ;;             (pro (ido-completing-read "Select project: " org-pro-project-alist))
	 (entry (assoc pro org-pro-project-alist))
	 (index (org-pro-get-index entry)))
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

(setq org-pro-capture-prefix "P")
(add-to-list 'org-capture-templates `(,org-pro-capture-prefix "Project management") 'append)
(defun org-pro-capture() 
  (interactive)
  (push (string-to-char org-pro-capture-prefix) unread-command-events)
  (call-interactively 'org-capture))
;; Capturing links 
(add-to-list 'org-capture-templates `(,(concat org-pro-capture-prefix "l") "Add link" plain 
 (function (lambda () (org-pro-goto-project nil "Links" 'yes))) "\n - %x%?") 'append)
;; Capturing tasks
(add-to-list 'org-capture-templates `(,(concat org-pro-capture-prefix "t") "Add task" plain
  (function org-pro-goto-project-taskpool) "\n*** TODO %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
(add-to-list 'org-capture-templates `(,(concat org-pro-capture-prefix "c") "Add checklist item" plain
  (function org-pro-goto-project-taskpool) "\n- [ ] %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing notes
(add-to-list 'org-capture-templates `(,(concat org-pro-capture-prefix "n") "Add note" plain
  (function org-pro-goto-project-notes) "\n*** %? \n:PROPERTIES:\n:CaptureDate: <%<%Y-%m-%d %a>>\n:END:") 'append)
;; Capturing documents
(add-to-list 'org-capture-templates
	     `(,(concat org-pro-capture-prefix "d") "Add document" plain
	       (function org-pro-goto-project-documents) "\n*** %? \n:PROPERTIES:\n:filename: [[%(read-file-name \"Document file: \")]]\n:CaptureDate: %T\n:END:") 'append) 

;;}}}
;;{{{ Adding documents from file-list

(defun org-pro-add-documents (&optional file-list)
  (interactive)
  (let* ((fl (or file-list file-list-current-file-list)))
    ;; FIXME need to write org-pro-get-documents and filter duplicates
    (org-pro-goto-project-documents)
    (while fl
      (insert "\n*** " (file-name-nondirectory (file-name-sans-extension (file-list-make-file-name (car fl))))
	      "\n:PROPERTIES:\n:filename: [["(file-list-make-file-name (car fl))"]]\n:CaptureDate: ")
      (org-insert-time-stamp (current-time) t)
      (insert "\n:END:")
      (setq fl (cdr fl)))))

;;}}}

(provide 'org-pro-summary)
;;; org-pro-summary.el ends here