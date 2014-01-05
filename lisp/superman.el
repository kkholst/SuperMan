;;; superman.el --- org project manager

;; Copyright (C) 2013  Thomas Alexander Gerds

;; Authors:
;; Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Klaus Kaehler Holst <kkho@biostat.ku.dk>
;;
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

;; SuperMan is a project which manages all your other projects
;; Q: Does the super project contain itself?
;; A: Nice question. To find some answers please read logicomix. Get it here: www.logicomix.com

;;; Code:

;;{{{

;; (defvar superman '(("SuperMan" ("location" . (file-name-directory superman-home))
;; ("index" . superman-home)
;; ("category" . nil)
;; ("config" . nil)
;; ("state" . "ACTIVE"))))

(setq superman-balls
      '((todo ("width" 9) ("face" superman-get-todo-face))
	(hdr ("width" 27) ("face" font-lock-function-name-face))
	;; ("Nickname" ("width" 33)
	 ;; ("name" "Nick")
	 ;; ("face" superman-next-project-button-face)
	 ;; ("fun" superman-make-project-button))
	("lastvisit" ("fun" superman-trim-date) ("width" 17) ("face" font-lock-type-face) ("sort-key" t))
	("others" ("width" 66) ("face" font-lock-keyword-face))))

  ;; "Returns a super project for project management"
  ;; `("SuperManager"
    ;; (("location" . ,superman-default-directory)
     ;; ("index" . ,superman-file)
     ;; ("category" . "Super")
     ;; ("state" . "ACTIVE")
     ;; ("config" . "INDEX | AGENDA / TODO"))))

(defun superman-set-property ()
  (interactive)
  (let* ((prop-list '(((superman-property 'location) . nil)
		      ((superman-property 'index) . nil)
		      ((superman-property 'category) . nil)
		      ((superman-property 'others) . nil)
		      ((superman-property 'publishdirectory) . nil)))
	 (prop (completing-read "Set property: " prop-list))
	 (pom (org-get-at-bol 'org-hd-marker))
	 (curval (org-entry-get pom prop))
	  ;; (if  (completing-read (concat "Value for " prop ": ")
	 (val (read-string (concat "Value for " prop ": ") curval)))
    (org-entry-put pom prop val))
  (superman-redo))

(defun superman-return ()
  "Switch to project at point."
  (interactive)
  (let ((pro (assoc
	      (superman-property-at-point
	       (superman-property 'nickname) nil)
	      superman-project-alist)))
    (superman-switch-to-project pro)))

(defun superman-get-priority-face (kwd)
  "Get face for priority symbols"
  (if (< (length kwd) 3)
      'org-priority
    (or (org-face-from-face-or-color
	 'priority 'org-priority
	 (cdr (assoc (string-to-char
		      (substring kwd 2 3))
		     org-priority-faces)))
	'org-priority)))

(defun superman-get-todo-face (kwd)
  "A slight modification of `org-get-tag-face'"
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))


;;}}}
;;{{{ superman 

(defun superman-popup-tip (msg)
  (save-excursion
    (goto-char (point-min))
    (tooltip-show msg)))
;;    (popup-tip msg)))

(defvar superman-help-fun 'superman-popup-tip 
  "Function used to display help. Possible values 'tooltip-show or 'popup-tip (depends on popup.el)") 


(defun superman-make-header ()
  "Insert header into superman project view buffer"
  (goto-char (point-min))
  (insert "SuperMan(ager)")
  (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd '(S))
  (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
  (put-text-property (point-at-bol) (point-at-eol) 'index superman-home)
  (put-text-property (point-at-bol) (point-at-eol) 'nickname "Kal-El")
  (insert "\t" (superman-make-button "Agenda"
				     'superman-agenda
				     'superman-next-project-button-face
				     "Agenda across all projects") "\t")
  (insert "\t" (superman-make-button "Calendar"
				     'superman-calendar
				     'superman-next-project-button-face
				     "Project-wide calendar") "\t")
  (insert (superman-make-button "TodoList"
				'superman-todo
				'superman-next-project-button-face
				"TodoList across all projects"))  
  (insert "\n"))

  ;; (insert "\n\nKeys: \n")
  ;; (forward-line -1)
  ;; (put-text-property (point) (length "Keys: ") 'face 'org-level-2)
  ;; (end-of-line)
  ;; (insert "N: new project RET: select project")
  ;; (put-text-property (point-at-bol) (+ (point-at-bol) (length "Keys: ")) 'face 'org-level-2))


(defun superman-categorize-projects (&optional cats balls)
  "Parse the file `superman-home' and return a categorized project-alist."
  (interactive)
  (save-excursion
    (setq superman-project-alist nil)
    (set-buffer (find-file-noselect superman-home))
    (unless (superman-manager-mode 1))
    (save-buffer)
    (goto-char (point-min))
    (while (superman-forward-project)
      (let* ((loc (or (superman-get-property nil (superman-property 'location) 'inherit) superman-default-directory))
	     (category (superman-get-property nil (superman-property 'category) 'inherit))
	     (others (superman-get-property nil (superman-property 'others) nil))
	     (publish-dir (superman-get-property nil (superman-property 'publish) 'inherit))
	     (name (or (superman-get-property nil (superman-property 'nickname) nil)
		       (nth 4 (org-heading-components))))
	     (marker (copy-marker (point)))
	     (hdr  (org-get-heading t t))
	     (lastvisit (superman-get-property nil "LastVisit" 'inherit))
	     (config (superman-get-property nil (superman-property 'config) 'inherit))
	     (todo (substring-no-properties (or (org-get-todo-state) "")))
	     (index (or (superman-get-property nil (superman-property 'index) nil)
			(let ((default-org-home
				(concat (file-name-as-directory loc)
					name
					superman-org-location)))
			  ;; (make-directory default-org-home t)
			  (concat (file-name-as-directory default-org-home) name ".org")))))
	(set-text-properties 0 (length hdr) nil hdr)
	;; (add-text-properties
	;; 0 (length hdr)
	;; (list 'org-marker marker 'org-hd-marker marker) hdr)
	(unless (file-name-absolute-p index)
	  (setq index
		(expand-file-name (concat (file-name-as-directory loc) name "/" index))))
	(add-to-list 'superman-project-alist
		     (list name
			   (list (cons "location"  loc)
				 (cons "index" index)
				 (cons "category" category)
				 (cons "others" others)
				 (cons 'hdr hdr)
				 (cons "marker" marker)				 
				 (cons "lastvisit" lastvisit)
				 (cons "config" config)
				 (cons 'todo todo)
				 (cons "publish-directory" publish-dir))))))
    superman-project-alist))

(defun superman ()
  (interactive)
  (let* ((cats-buffer "*S*")
	 (cats (superman-parse-project-categories))
	 (cat-alist (mapcar (lambda (x) (list x)) cats))
	 (howmany-cats (length cats))
	 (cat-number-one (car cats))
	 (projects superman-project-alist))
    (switch-to-buffer cats-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (font-lock-mode -1)
    (superman-make-header)
    (goto-char (point-max))
    ;; parse projects by category using superman-balls
    (while projects
      (let* ((pro (car projects))
	     (cat (or (cdr (assoc "category" (cadr pro))) "CatWoman"))
	     (m (- howmany-cats (length (member cat cats))))
	     (tail (cdr (nth m cat-alist))))
	(if tail
	    (setcdr (nth m cat-alist) (append tail (list pro)))
	  (setcdr (nth m cat-alist) (list pro))))
      (setq projects (cdr projects)))
    (insert "\n")
    (superman-view-insert-action-buttons
     '(("New project" . superman-capture-project)
       ("Meeting" . superman-capture-meeting)
       ("Task" . superman-capture-task)))
    (insert "\n")
    ;; loop over categories
    (while cat-alist
      (let* ((cat (car cat-alist))
	     (cat-name (car cat))
	     (cat-fun `(lambda () (interactive)
			 (superman-capture-project nil ,cat-name)))
	     (tail (cdr cat)))
	;; (insert "\n** " cat-name)
	(insert "\n** "
		(superman-make-button
		 cat-name
		 cat-fun
		 'superman-capture-button-face
		 (concat "Add project in category " cat-name)))
	(put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
	(put-text-property (point-at-bol) (point-at-eol) 'cat 'cat-name)
	(put-text-property (point-at-bol) (point-at-eol) 'balls superman-balls)
	(put-text-property (point-at-bol) (point-at-eol) 'display (concat "★ " cat-name))
	(insert " [" (int-to-string (length tail)) "]")
	;; loop over projects (tail) in category
	(insert "\n")
	(superman-format-loop tail superman-balls)
	(put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail cat-name)
	;; column names
	(org-back-to-heading)
	(end-of-line)
	(let ((first-item (next-single-property-change (point-at-eol) 'org-marker)))
	  (when first-item
	    (goto-char first-item)
	    (forward-line -1)
	    (end-of-line)
	    (insert "\n")
	    (insert (superman-column-names superman-balls))
	    ;; sorting
	    (goto-char (next-single-property-change (point) 'org-marker))
	    (when (next-single-property-change (point-at-bol) 'sort-key)
	      (goto-char (+ 2 (next-single-property-change (point-at-bol) 'sort-key)))
	      (superman-sort-section))))
	(goto-char (point-max))
	(setq cat-alist (cdr cat-alist)))))
  (goto-char (point-min))
  ;; (superman-view-mode-on)
  (superman-on)
  (setq buffer-read-only t))


(defun superman-format-loop (list balls)
  "Loop over list and insert all items formatted according to balls."
  (while list
    (let* ((item (superman-format-thing
		  (car list)
		  balls)))
      (insert item)
      (end-of-line)
      (insert "\n")
      (setq list (cdr list)))))

;;}}}
;;{{{ cycle view 

(defvar superman-views nil)
(setq superman-views (list 'S 'S-todo 'S-todo-B 'S-agenda))

(defun superman-change-view  (&optional arg)
  (interactive "p")
  ;; cycle view list
  (when (or (eq major-mode 'org-agenda-mode)
	    superman-view-mode)
    (let ((current  (car superman-views))
	  (rest  (cdr superman-views)))
      (setq superman-views rest)
      (add-to-list 'superman-views current 'append)))
  (eval `(,(car superman-views)))
  (superman-view-mode-on)
  (superman-on))
  
(defalias 'S 'superman)
(defalias 'S-todo 'superman-todo)
;; (defalias 'S-todo-B 'superman-todo-B)
(defalias 'S-agenda 'superman-agenda)


(defvar superman-exclude-from-todo-regexp nil "Regexp to match index-files that should not contribute todo lists")

(defvar superman-todo-tags nil "Regexp to match todo-tags that should popup in the global todo list")


(defun superman-todo (&optional project)
  (interactive)
  (let ((org-agenda-buffer-name (concat "*S-TODO*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands
	 `(("P" "Projects-TODO"
	    ((,(intern (if superman-todo-tags "tags-todo" "alltodo"))
	      ,(if superman-todo-tags superman-todo-tags "")
	      ;; ((tags-todo "PRIORITY<>\"C\"+PRIORITY<>\"B\""
	      ;; ((tags-todo "PRIORITY<>\"C\""
	      ;; ((tags-todo ""
	      ;; ((alltodo ""
	      ((org-agenda-files
		(reverse (superman-index-list nil nil nil nil nil superman-exclude-from-todo-regexp))))))
	    ((org-agenda-window-setup 'current-window)
	     (org-agenda-finalize-hook
	      (lambda ()
		(superman-format-agenda
		 superman-todolist-balls
		 '(superman-todo)
		 "* SupermanTodoList"
		 (concat "\t"
			 (superman-make-button "Agenda"
					       'superman-agenda
					       'superman-next-project-button-face
					       "Agenda across all projects")
			 "\t"
			 (superman-make-button "Calendar"
					       'superman-calendar
					       'superman-next-project-button-face
					       "Project-wide calendar")
			 "\t"
			 (superman-make-button "Projects"
					       'superman
					       'superman-next-project-button-face
					       "List of projects"))))))))))
    (push ?P unread-command-events)
    (call-interactively 'org-agenda)))


(defun superman-make-agenda-title (string face)
  (put-text-property 0 (length string) 'face face string)
  string)


(defun superman-calendar (&optional project)
  (interactive)
  (let ((org-agenda-buffer-name (concat "*SuperMan-Calendar*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands nil))
    (add-to-list 'org-agenda-custom-commands
		 '("C" "Superman calendar"
		   ((agenda ""
			    ((org-agenda-files
			      (superman-index-list)))))
		   ((org-agenda-compact-blocks nil)
		    (org-agenda-show-all-dates t)
		    (org-agenda-span 7)
		    (org-agenda-window-setup 'current-window)
		    (org-agenda-overriding-header
		     (concat (superman-make-agenda-title "Superman calendar" 'org-level-2)
			     "\t"
			     (superman-make-button "TodoList"
						   'superman-todo
						   'superman-next-project-button-face
						   "TodoList across all projects")
			     "\t"
			     (superman-make-button "Agenda"
						   'superman-agenda
						   'superman-next-project-button-face
						   "Project-wide agenda")
			     "\t"
			     (superman-make-button "Projects"
						   'superman
						   'superman-next-project-button-face
						   "List of projects")
			     "\n"
			     (superman-make-button "\nClick here to add a Meeting\n"
						   'superman-capture-meeting
						   'superman-capture-button-face
						   "Add a meeting to calendar")
			     )))))
    (push ?C unread-command-events)
    (call-interactively 'org-agenda)))

    
(defun superman-agenda (&optional project)
  (interactive)
  (let ((org-agenda-buffer-name (concat "*S-agenda*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands nil))
    (add-to-list 'org-agenda-custom-commands
		 '("A" "Superman agenda"
		   ((agenda ""
			    ((org-agenda-files
			      (superman-index-list)))))
		   ((org-agenda-compact-blocks nil)
		    (org-agenda-show-all-dates nil)
		    (org-agenda-window-setup 'current-window)
		    (org-agenda-overriding-header
		     (concat (superman-make-agenda-title "SupermanAgenda" 'org-level-2)
			     "\t"
			     (superman-make-button "TodoList"
						   'superman-todo
						   'superman-next-project-button-face
						   "TodoList across all projects")
			     "\t"
			     (superman-make-button "Calendar"
						   'superman-calendar
						   'superman-next-project-button-face
						   "Project-wide calendar")
			     "\t"
			     (superman-make-button "Projects"
						   'superman
						   'superman-next-project-button-face
						   "List of projects")
			     "\n")))))
    (push ?A unread-command-events)
    (call-interactively 'org-agenda)))

;;}}}
;;{{{ superman-mode-map

(require 'superman-views)

(defvar superman-mode-map (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-mode' commands.")
   
(define-minor-mode superman-mode 
  "Toggle org projectmanager document superman mode.
With argument ARG turn superman-mode on if ARG is positive, otherwise
turn it off.

Enabling superman mode electrifies the superman buffer for project management."
     :lighter " S"
     :group 'org
     :keymap 'superman-mode-map)

(defun superman-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-mode t))


(defun superman-update-project-overview ()
  (save-excursion
    (if (get-buffer "*S*")
	(switch-to-buffer (get-buffer "*S*"))
      (S))
    (superman-redo)))

(fset 'superman-new-project 'superman-capture-project)


(defun superman-clean-up ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^Project category:" nil t)
      (when (progn (forward-line 1) (looking-at "^[ \t]*$"))
	(let ((kill-whole-line t))
	(kill-line 2)
	(kill-line -1))))))

(setq superman-agenda-balls
      '((index ("width" 23) ("face" font-lock-keyword-face) ("name" "File"))
	(todo ("width" 7) ("face" superman-get-todo-face))
	(hdr ("width" 23) ("face" font-lock-function-name-face) ("name" "Description"))
	("DEADLINE" ("fun" superman-trim-date) ("face" superman-warning-face))
	(".*Date" ("fun" superman-trim-date) ("face" font-lock-string-face))
	("FileName" ("fun" superman-dont-trim))))

;; FIXME: It requires some efforts to associate the project with a given index file ...
;;        Making PROJECT-ALIST a hash table may improve efficiency? 
(defun superman-trim-project-attribute (marker attribute &optional dont-trim args)
  (if (markerp marker)
      (let* ((pro-list (mapcar (lambda (p)
				 (cons (expand-file-name (cdr (assoc "index" (cadr p))))
				       (if (string= attribute "nickname") (car p)
					 (cdr (assoc attribute (cadr p))))))
			       superman-project-alist))
	     (ifile (buffer-file-name (marker-buffer marker)))
	     (attr (or (cdr (assoc (expand-file-name ifile) pro-list)) "--")))
	(if dont-trim
	    attr
	    (superman-trim-string attr args)))
    (if dont-trim
	marker
	(superman-trim-string marker args))))

(defun superman-make-project-button (nick &optional args)
  (let* ((nickname (superman-trim-string nick args))
	 (button (superman-make-button
		  nickname
		  `(lambda () (interactive) (superman-switch-to-project ,nick))
		  nil
		  ;; font-lock-string-face
		  (concat "Switch to " nick))))
    button))

(defun superman-trim-project-nickname  (marker &optional args)
  (if (not (markerp marker));; column name
      (superman-trim-string marker args)
    (let* ((nick (superman-trim-project-attribute marker "nickname" 'dont args))
	   (nickname (superman-trim-string nick args))
	   (button       (superman-make-button
			  nickname
			  `(lambda () (interactive) (superman-switch-to-project ,nick))
			  nil
			  ;; font-lock-string-face
			  (concat "Switch to " nick))))
      button)))
				  

(defun superman-trim-project-others  (marker attribute &optional args)
  (superman-trim-project-attribute marker "others" args))

(defun superman-trim-project-cat  (marker attribute &optional args)
  (superman-trim-project-attribute marker "category" args))

(setq superman-todolist-balls
      '((org-hd-marker ("width" 33)
		       ("name" "Nick")
		       ;; ("face" superman-next-project-button-face)
		       ("face" superman-next-project-button-face)
		       ("fun" superman-trim-project-nickname))
	(todo ("width" 7) ("face" superman-get-todo-face))
	(priority ("width" 8) ("face" superman-get-priority-face))
	(hdr ("width" 53) ("face" font-lock-function-name-face) ("name" "Description"))
	("DEADLINE" ("fun" superman-trim-date) ("width" 12) ("face" superman-warning-face))
	(".*Date" ("fun" superman-trim-date) ("width" 12) ("regexp" t) ("face" font-lock-string-face) ("name" "Date"))
	(org-hd-marker ("width" 23) ("name" "Others") ("fun" superman-trim-project-others))
	(org-hd-marker ("width" 23) ("name" "Cat") ("fun" superman-trim-project-cat))
	;; (index ("width" 23) ("face" font-lock-keyword-face) ("name" "File"))
	("FileName" ("fun" superman-dont-trim))))

(defun superman-format-todolist ()
  (superman-format-agenda superman-todolist-balls))

(defun superman-format-agenda (&optional balls redo title buttons)
  (let ((balls (or balls superman-agenda-balls))
	;; (redo-cmd org-agenda-redo-command)
	(count 0)
	agenda-buffers)
    (save-excursion
      (org-mode);; major
      (font-lock-mode -1)
      (font-lock-default-function nil)
      (goto-char (point-min))
      (kill-line)
      (beginning-of-line)
      (insert "\n")
      (goto-char (point-min))
      (insert (or title "* SupermanAgenda"))
      (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
      (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd redo)
      ;; (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd redo-cmd)
      (put-text-property (point-at-bol) (point-at-eol) 'cat t)
      (put-text-property (point-at-bol) (point-at-eol) 'balls balls)
      (if buttons (insert buttons))
      (end-of-line)
      (insert "\n")
      (superman-view-insert-action-buttons
       '(("New project" . superman-capture-project)
	 ("Meeting" . superman-capture-meeting)
	 ("Task" . superman-capture-task)))
      (insert "\n\n" (superman-column-names balls))
      (superman-view-mode-on) ;; minor
      (while (ignore-errors
	       (goto-char (next-single-property-change (point) 'org-hd-marker)))
	(setq count (+ count 1))
	(let* ((buffer-read-only nil)
	       (pom (get-text-property (point-at-bol) 'org-hd-marker))
	       (kill-whole-line t)
	       (line
		(org-with-point-at pom
		  (superman-format-thing pom balls))))
	  (setq agenda-buffers (append (list (marker-buffer pom)) agenda-buffers))
	  (beginning-of-line)
	  (kill-line)
	  (insert line "\n")))
      (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail 'todo-end)
      ;; (goto-char (point-min))
      (goto-char (next-single-property-change (point-min) 'face))
      (insert " [" (int-to-string count) "]"))))
;; (superman-clean-buffer-list agenda-buffers)))

;; FIXME: this still does not make sense,
;;        because different agendas use different
;;        project buffers
(defun superman-clean-buffer-list (list)
  "Kill all project buffers except for those in LIST. This
function is called at the end of `superman-format-agenda' where
LIST includes the buffers that are related to one of the items."
  (let* ((org-files (superman-index-list nil nil nil nil nil superman-exclude-from-todo-regexp))
	 obuf)
    (while org-files
      (when (and (setq obuf (get-file-buffer (car org-files)))
		 (not (member obuf list)))
	(kill-buffer obuf))
      (setq org-files (cdr org-files)))))

(defun superman-visit-project ()
  (interactive)
  (let* ((pom (get-text-property (point-at-bol) 'org-marker))
	(home superman-home)
	(ibuf (if pom (marker-buffer pom)
		(get-file-buffer home)))
	(iwin (when ibuf (get-buffer-window ibuf nil))))
    (if (and ibuf iwin)
	(select-window (get-buffer-window ibuf nil))
      (split-window-vertically)
      (other-window 1)
      (if ibuf (switch-to-buffer ibuf)
	(find-file home)))
    (when pom (goto-char pom))))
		 
(define-key superman-mode-map [return] 'superman-return) 
;; (define-key superman-mode-map "N" 'superman-new-project)
;; (define-key superman-mode-map [(f1)] 'superman-switch-to-project)
;; (define-key superman-mode-map " " 'superman-switch-to-project)
;; (define-key superman-mode-map "S" 'superman-set-property)
(define-key superman-mode-map "i" 'superman-visit-project)
(define-key superman-mode-map "x" 'superman-delete-project)
(define-key superman-mode-map "V" 'superman-change-view)
(define-key superman-mode-map "N" 'superman-new-project)
(define-key superman-mode-map "?" 'supermanual)

;;}}}  
(provide 'superman)
;;; superman.el ends here
