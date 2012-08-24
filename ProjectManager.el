(defvar org-project-manager-include-keywords '("ACTIVE"))
(defvar org-project-manager-current-project nil)
(defvar org-project-manager-org-location "/")
(defvar org-project-manager-project-alist nil)
(defvar org-project-manager-project-org-file-location "project-directory")
(defvar org-project-manager-project-categories nil)
(defvar org-project-manager-directory nil
  "Name of the sub-directory in the project-directory
 where the org-project-manager stores and finds project-org-files.")
(defvar org-project-manager "~/Projects.org")
(defvar org-project-manager-project-level 4)
;; (setq org-project-manager "~/research/ProjectManager.org")
;; (setq org-project-manager "~/Dropbox/ProjectManager/SampleProjectManager.org")
;;(setq org-project-manager "~/research/Projects.org")

;; (add-to-list 'org-capture-templates '("E" "Store email in project" entry
				       ;; (function org-project-manager-goto-project-mailbox)
				       ;; "\n*** Subject: %:subject :email:
;; From: %:from\nDate: %:date\nEmail: %a\n %? %i
;; Saved: %U"))


;; (defun org-project-manager-sync-project-html ()
  ;; (interactive)
  ;; (let* ((pro  org-project-manager-current-project)
;; (prodir (concat (file-name-as-directory (cdr pro)) (car pro)))
	 ;; (pubdir (replace-in-string (replace-in-string (expand-file-name prodir) "public" "") (getenv "HOME") (concat (getenv "HOME") "/public_html")))
	 ;; (source (read-file-name "Copy all files from directory: " (concat prodir "/public") (concat prodir "/public") t ))
	 ;; (target (read-file-name "To directory: " pubdir pubdir nil)))
    ;; (if (yes-or-no-p (concat "send this command: " "rsync -av " (file-name-as-directory source) " " target "? "))
	;; (shell-command (concat "rsync -av " (file-name-as-directory source) " " target)))))


(defun org-project-manager-project-agenda ()
  (interactive)
  (find-file org-project-manager)
  (push ?t unread-command-events)
  (push ?< unread-command-events)
  (call-interactively 'org-agenda))

(defun org-project-manager-project-list ()
  (interactive)
  (delq nil (mapcar '(lambda (x) (let ((f (org-project-manager-get-org x))) (if (file-exists-p f) f))) 
		    (org-project-manager-parse-projects))))

(defun org-project-manager-list-projects (&optional class flag)
  (interactive)
  ;; (org-project-manager-parse-projects)
  (let* ((cl (or class (completing-read "Project class: " (org-project-manager-parse-categories))))
	 (projects org-project-manager-project-alist))
    (delq nil (mapcar '(lambda (x)
			 (let ((org-file (org-project-manager-get-org x))) 
			 (if class
			     (when (and (string= (org-project-manager-get-category x) class)
					org-file
					(file-exists-p org-file))
			       org-file))))
		      projects))))

(defun org-project-manager-agenda ()
  (interactive)
  (let ((org-agenda-files
	 (delq nil (mapcar '(lambda (x) (let ((f (org-project-manager-get-org x))) (if (file-exists-p f) f))) 
			   (org-project-manager-parse-projects))))
	(org-agenda-include-diary nil))
    ;; (setq debug-on-error t)
	;; (message "a")
	(org-agenda-list)))

(defun org-project-manager-return ()
  (interactive)
  (if (org-project-manager-project-at-point)
      (org-project-manager-visit-project)
    (org-return)))

(defun org-project-manager-visit-project ()
  (interactive)
  ;; FIXME: ask user if buffer org-project-manager needs saving
  ;;        and/or parsing
  (when (org-project-manager-project-at-point)
    (find-file (org-project-manager-project-orgfile-at-point))))

(defun org-project-manager-select-project ()
  (interactive)
  (if (and org-project-manager-current-project
	   (not (eq last-command 'org-project-manager-select-project)))
      (let ((index (or
		    (org-project-manager-get-index org-project-manager-current-project)
		    (org-project-manager-get-org org-project-manager-current-project))))
	(find-file index)
	(message "Press same key again to swich project"))
    (org-project-manager-parse-projects)
    (let* ((pro (completing-read "Project: " org-project-manager-project-alist)))
      (setq org-project-manager-current-project (assoc pro org-project-manager-project-alist))
      (find-file (org-project-manager-get-index org-project-manager-current-project)))))

(defun org-project-manager-get-index (project)
  (cdr (assoc "index-file" (cadr project))))

(defun org-project-manager-get-org (project)
  (cdr (assoc "org-file" (cadr project))))

(defun org-project-manager-get-location (project)
  (cdr (assoc "location" (cadr project))))

(defun org-project-manager-get-publish-directory (project)
  (cdr (assoc "publish-directory" (cadr project))))

(defun org-project-manager-get-category (project)
  (cdr (assoc "category" (cadr project))))
  
(defun org-project-manager-goto-project-manager ()
  (interactive)
  (find-file org-project-manager))



(defvar org-project-manager-minor-mode nil)
(make-variable-buffer-local 'org-project-manager-minor-mode)
(defvar org-project-manager-minor-mode-map (make-sparse-keymap)
  "Keymap used for `org-project-manager-minor-mode' commands.")
(or (assq 'org-project-manager-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (append minor-mode-map-alist
		  (list (cons 'org-project-manager-minor-mode org-project-manager-minor-mode-map)))))
(or (assq 'org-project-manager-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(org-project-manager-minor-mode " Project") minor-mode-alist)))
(defun org-project-manager-minor-mode (&optional arg)
  "A minor mode for using org Project Manager."
  (interactive "P")
  (add-to-list 'org-structure-template-alist
	       '("p" "**** New project\n:PROPERTIES:\n:NICKNAME: newProject\n:END:"))
  (add-to-list 'org-structure-template-alist
	       '("t" "**** New project\n:PROPERTIES:\n:NICKNAME: newProject\n:INDEX: talk.tex\n:END:"))
  ;; (make-variable-buffer-local 'hippie-expand-try-functions-list)
  (setq org-project-manager-minor-mode
	(not (or (and (null arg) org-project-manager-minor-mode)
		 (<= (prefix-numeric-value arg) 0)))))
(define-key org-project-manager-minor-mode-map [(meta return)] 'org-project-manager-return)

(add-hook 'find-file-hooks 
          (lambda ()
            (let ((file (buffer-file-name)))
              (when (and file (equal file (expand-file-name org-project-manager)))
                (org-project-manager-minor-mode)))))

;; (define-derived-mode org-project-manager-mode org-mode ;;fundamental-mode
;;   ;; Well, it is basically a subset of org-mode...
;;   "Project_Manager"
;;   "Major mode for editing org-project-manager project file..."
;;   ;; ...
;;   ;; Some default key bindings
;;   (define-key org-project-manager-mode-map [(meta return)] 'org-project-manager-return)
;; )
;; ;; (add-hook 'org-project-manager-mode-hook
;; ;; 	  (lambda ()
;; ;; 	    (org-indent-mode t)
;; ;; 	    ))

;; (add-hook 'org-mode-hook
;;  	  (lambda () (if (string= (buffer-name)
;; 				  (file-name-nondirectory org-project-manager))
;; 			 (org-project-manager-mode))))

(defun org-project-manager-get-buffer-props (property)
  "Get a table of all values of PROPERTY used in the buffer, for completion."
  (let (props)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat ":" property ":") nil t)
	(add-to-list 'props (list
			     (org-entry-get
			      nil property nil)))))
    props))

(defun org-project-manager-parse-categories ()
  (interactive)
  (save-excursion
    (set-buffer (find-file-noselect org-project-manager))
    (setq org-project-manager-project-categories
	  (reverse (org-project-manager-get-buffer-props "CATEGORY")))))

(defun org-project-manager-new-project ()
  (interactive)
  (org-project-manager-parse-projects)
  (org-project-manager-parse-categories)
  (let* ((title (read-string "Project title (long name): "))
	 (nickname (read-string
		    (concat "Nickname (default " title "): ") nil 'minibuffer-history title))
	 (cat (completing-read
	       "Choose a category: "
	       org-project-manager-project-categories nil nil nil "New projects")))
    (find-file org-project-manager)
    (goto-char (point-min))
    (if (or (re-search-forward (concat ":" cat ":") nil t)
	    (re-search-forward (concat "^**[ \t]+" cat) nil t)
	    (re-search-forward "^**[ \t]+ New projects" nil t))
	(let
	    ((org-insert-heading-respect-content t))
	  (org-insert-subheading))
      (insert
       (make-string org-project-manager-project-level
		    (string-to-char "*"))
       " "
       title
       "\n:PROPERTIES:\n:NICKNAME: "
       nickname
       "\n:END:\n")
      (org-set-tags-to "active"))))

(defun org-project-manager-project-include-p ()
  (save-excursion
    (org-back-to-heading t)
    (and (looking-at org-todo-line-regexp)
	 (match-end 2)
	 (member (match-string-no-properties 2) org-project-manager-include-keywords))))

(defun org-project-manager-parse-projects ()
  "Parse file 'project-manager' and update 'org-project-manager-project-list'"
  (interactive)
  (save-excursion
    (setq org-project-manager-project-alist nil)
    (set-buffer (find-file-noselect org-project-manager))
    (save-buffer)
    (goto-char (point-min))
    (while (re-search-forward (format "^\\*\\{%d\\} " org-project-manager-project-level) nil t)
      (when (org-project-manager-project-include-p)
	(let* ((loc (org-entry-get nil "LOCATION" 'inherit))
	     (category (org-entry-get nil "CATEGORY" 'inherit))
	     (publish-dir (org-entry-get nil "PUBLISH" 'inherit))
	     (name (or (org-entry-get nil "NICKNAME" nil)
		       (nth 4 (org-heading-components))))
	     (index (or (org-entry-get nil "INDEX" nil)
			(let ((default-org-home
				(concat (file-name-as-directory loc)
					name
					org-project-manager-org-location)))
			  (make-directory default-org-home t)
			  (concat (file-name-as-directory default-org-home) name ".org"))))
	     (org (or (org-entry-get nil "PROJECTORG")
		      (when (string= (file-name-extension index) "org")
			index))))
	(unless (file-name-absolute-p index)
	  (setq index
		(expand-file-name (concat (file-name-as-directory loc) name "/" index))))
	;; (if loc (concat (file-name-as-directory loc) name "/" index)))))
	(add-to-list 'org-project-manager-project-alist
		     (list name
			   (list (cons "location"  loc)
			   (cons "org-file" org)
			   (cons "index-file" index)
			   (cons "category" category)
			   (cons "publish-directory" publish-dir)))))))
      org-project-manager-project-alist))

(defun org-project-manager-project-at-point ()
  (interactive)
  (if (org-before-first-heading-p)
      (message "No project at point")
    (org-project-manager-project-orgfile-at-point)))

(defun org-project-manager-set-nickname ()
  (interactive)
  (org-set-property
   "NICKNAME"
   (read-string "NickName for project: "
		(nth 4 (org-heading-components)))))

(defun org-project-manager-project-orgfile-at-point ()
  "Construct the orgfile of project at point. Return nil if
a) the point is not at a project
b) the construction of the orgile failed (needs property PROJECTORG, or both LOCATION and NICKNAME)"
  (if (not (eq (nth 0 (org-heading-components)) org-project-manager-project-level))
      (error "No project at point.")
    (let* ((indexfile (org-entry-get nil "INDEX"))
	   (orgfile (org-entry-get nil "PROJECTORG"))
	   (pro (org-entry-get nil "NICKNAME"))
	   (loc (org-entry-get nil "LOCATION" 'inherit)))
      (cond (indexfile
	     (if (file-name-absolute-p indexfile)
		 indexfile
	       (expand-file-name (concat (file-name-as-directory loc) pro "/" indexfile))))
	    (orgfile
	     (if (file-name-absolute-p orgfile)
		 orgfile
	       (expand-file-name (concat (file-name-as-directory loc) pro "/" orgfile))))
	    (t
	     (expand-file-name (concat (file-name-as-directory loc) pro
				       org-project-manager-org-location
				       pro ".org")))))))


(defun org-project-manager-goto-project-taskpool (&optional arg)
  (interactive)
  (if arg (org-store-link))
  (let* ((buf (current-buffer))
	 (pro (completing-read "Select project: " org-project-manager-project-alist))
	 (entry (assoc pro org-project-manager-project-alist))
	 (loc (org-project-manager-get-location entry))
	 (org (org-project-manager-get-org entry)))
    (if org
	(find-file org)
      (error "Project " pro " does not have an org-file."))
    (goto-char (point-min))
    (or (re-search-forward "^[*]+ TaskPool" nil t)
	(progn
	  (goto-char (point-max))
	  (insert "\n\n* TaskPool\n")
	  (point)))))

(defun org-project-manager-goto-project-mailbox (&optional arg)
  (interactive)
  (unless (eq major-mode 'gnus-article-mode)
    (error "Can only capture mails from gnus-article-buffers"))
  (if arg (org-store-link))
  (let* ((buf (current-buffer))
	 (pro (completing-read "Select project: " org-project-manager-project-alist))
	 (entry (assoc pro org-project-manager-project-alist))
	 (loc (org-project-manager-get-location entry))
	 (org (org-project-manager-get-org entry))
	 (mailbox (file-name-as-directory (concat (file-name-as-directory loc) pro "/" "Mailbox"))))
    (org-project-manager-save-attachments pro mailbox buf)
    (if org
	(find-file org)
      (error "Project " pro " does not have an org-file."))
    (goto-char (point-min))
    (if (re-search-forward "^[*]+ Mailbox" nil t)
	(progn
	  (if (re-search-forward (format-time-string "%Y") nil t)
	      (if (re-search-forward "^*** " nil t)
		  (progn (beginning-of-line)
			 (insert "\n"))
		(insert "\n*** Saved mails\n"))
	    (org-insert-subheading 1)
	    (insert " " (format-time-string "%Y") "\n*** Saved mails\n"))
	  (end-of-line)
	  (insert "\n"))
      (goto-char (point-max))
      (insert "\n\n* Mailbox\n** " (format-time-string "%Y") "\n*** Saved mails\n"))))

;; (defun org-project-manager-project-org-file (&optional project)
  ;; (let* ((project-name (or project
			   ;; (if (org-project-manager-project-at-point)
			       ;; (or (org-entry-get nil "NICKNAME" 'inherit)
				   ;; (progn
				   ;; (org-project-manager-set-nickname)
				   ;; (org-entry-get nil "NICKNAME" 'inherit))))))
	 ;; (project-dir (concat
		       ;; (file-name-as-directory
			;; (org-entry-get nil "LOCATION" 'inherit))
		       ;; project-name))
	 ;; (org-dir (cond ((and (string= org-project-manager-project-org-file-location "project-directory")
			      ;; (if (file-exists-p project-dir)
				  ;; project-dir
				;; (make-directory project-dir)
				;; project-dir)))
			;; ((and (string= org-project-manager-project-org-file-location "org-project-manager-directory")
			      ;; (file-exists-p org-project-manager-directory))
			 ;; org-project-manager-directory)
			;; (t org-directory)))
	 ;; (project-org-file
	  ;; (concat (file-name-as-directory org-dir)
		  ;; project-name ".org")))
    ;; project-org-file))
;; (unless (file-exists-p project-org-file)
;; (unless (file-exists-p org-dir)
;; (make-directory org-dir)))
;; (find-file project-org-file)))



(defun org-project-manager-save-attachments (project dir buf)
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
    (while (re-search-forward "\\[[0-9]\\." nil t)
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
			dir nil nil filename))
	    (if (file-directory-p file)
		(message "file not saved")
	      (when (or (not (file-exists-p file))
			(y-or-n-p (concat "File " file " exists, overwright?" )))
		(mm-save-part-to-file data file))
	      (setq mime-line (concat mime-line "\nAttached file: "
				      "[[file:" file "][" (file-name-nondirectory file) "]]")))))))
    (plist-put org-store-link-plist :initial (concat (plist-get org-store-link-plist :initial) mime-line))))


;;{{{ Publishing 

(defvar org-project-manager-export-subdirectory "export")
(defvar org-project-manager-public-directory "~/public_html/")
;; (defvar org-project-manager-publish-subdirectory "public")
(require 'org-publish)
(defun org-project-manager-set-publish-alist ()
  (interactive)
  (let ((p-alist org-project-manager-project-alist))
    (while p-alist
      (let* ((entry  (car p-alist))
	     (nickname (car entry))
	     (base-directory (file-name-as-directory
			      (concat (file-name-as-directory
				       (org-project-manager-get-location entry))
				      nickname)))
	     (export-directory
	      (concat base-directory
		      org-project-manager-export-subdirectory))
	     (public-directory
	      (or (org-project-manager-get-publish-directory entry)
		  (concat (file-name-as-directory org-project-manager-public-directory)
			  nickname))))
	;;(replace-regexp-in-string org-project-manager-public-directory (getenv "HOME") (expand-file-name export-directory))))
	(add-to-list 'org-publish-project-alist
		     `(,(concat nickname "-export")
		       :base-directory
		       ,base-directory
		       :base-extension "org"
		       :publishing-directory
		       ,base-directory
		       :headline-levels 4
		       :auto-preamble t
		       :recursive t
		       :publishing-function
		       org-publish-org-to-html))
	(add-to-list 'org-publish-project-alist
		     `(,(concat nickname "-copy")
		       :base-directory
		       ,export-directory
		       :base-extension
		       "html\\|png\\|jpg\\|org\\|pdf"
		       :publishing-directory
		       ,public-directory
		       :recursive t
		       :publishing-function
		       org-publish-attachment))
	(add-to-list 'org-publish-project-alist
		     `(,nickname
		       :components (,(concat nickname "-export") ,(concat nickname "-copy")))))
      (setq p-alist (cdr p-alist)))))
;;}}} Publishing

;;{{{ Add new project (with project directory skeleton)

(defvar org-project-manager-default-project-content "
** Collaborators
 - Nomen Nescio <nn@foo.bar>
ORG-LIST-END-MARKER
** Files
*** active
*** old
** Meetings
** Documents
** Presentations
** Emails
* TaskPool
")
(defvar org-project-manager-default-project-ignore "
export
*.html
*.pdf
*.png
")     
(defvar org-project-manager-project-directories '("analysis" "org" "data" "email" "export" "misc" "presentation" "manuscript"))

(defun org-project-manager-template (directory projectname)
  "Generate project skeleton"
   (interactive "DDirectory: 
sProject-name (short): ")
   (let* (my-project-cd my-proj-path my-proj-file)
     (setq my-proj-path (concat directory "/" projectname "/"))
     (setq my-proj-cd (concat "cd " my-proj-path ";"))
     (setq my-proj-file (concat my-proj-path projectname ".org"))

     (loop for dir in org-project-manager-project-directories
	   do (make-directory (concat my-proj-path dir) t))
     (if (not (file-exists-p my-proj-file))
         (append-to-file org-project-manager-default-project-content nil my-proj-file)
       )
     (if (not (file-exists-p (concat my-proj-path ".git")))
	 ((lambda ()	   
	   (shell-command (concat my-proj-cd "git init"))      
	   (append-to-file org-project-manager-default-project-ignore nil (concat my-proj-path ".gitignore"))	   
	   (shell-command (concat my-proj-cd "git add *"))
	   ))
       )
     (message "Created new project: " my-proj-path)     	   
     )     
)

(defvar org-project-manager-default-category "Miscellaneous")
(defun org-project-manager-add-project (nickname)
  "Get parameters"
  (interactive ;;"DDirectory: 
   "sProject-name (short): ")
  (org-project-manager-parse-categories)
  
  (let* ((category (completing-read
		    "Choose a category: "
		    org-project-manager-project-categories nil nil nil nil org-project-manager-default-category)) loc directory)
    (save-excursion
      (set-buffer (find-file-noselect org-project-manager))
      (save-buffer)
      (goto-char (point-min))
      (re-search-forward (concat ":CATEGORY: " category))  
      (setq loc (org-entry-get nil "LOCATION" 'inherit))
      )  
    (setq directory (read-directory-name
		     "Choose location: "
		     loc
		     ))
    (let (org-capture-templates)
      (setq org-capture-templates
	    `(("p" "Project" plain (file+headline org-project-manager 
						  ,category)
	       ,(concat (make-string org-project-manager-project-level
				     (string-to-char "*"))
			" ACTIVE %c%?\n:PROPERTIES:\n:NICKNAME: %c\n:END:\n")
	       )))
      (org-project-manager-template directory nickname)
      (kill-new nickname)
      (org-capture nil "p")
      (pop kill-ring)
      )
    )
  )


(setq org-project-manager-default-category "Miscellaneous")

(setq org-refile-targets (quote ((org-project-manager :maxlevel . 3) (nil :maxlevel . 2))))

;;}}} Add new project (with project directory skeleton)

;;{{{ Default key-bindings

;; (global-set-key [(control f2)] 'org-project-manager-add-project)
;; (global-set-key [(f2)] 'org-project-manager-select-project)
;; (global-set-key [(meta f2)] 'org-project-manager-goto-project-manager)

;;}}}

;;{{{adding special project agenda views
;; (add-to-list 'org-agenda-custom-commands
	     ;; `("P" "Projects-TODO"
	       ;; ,(mapcar '(lambda (cat)
			   ;; (list 'todo "TODO"
				 ;; `((org-agenda-overriding-header  (concat "Project category: ",(car cat)))
				   ;; (org-agenda-files (quote ,(org-project-manager-list-projects (car cat)))))))
			;; (org-project-manager-parse-categories))))

;; (add-to-list 'org-agenda-custom-commands
	     ;; `("p" "Projects"
	       ;; ,(mapcar '(lambda (key)
			  ;; (list 'todo key
				;; `((org-agenda-files '("~/research/Projects.org"))
				  ;; (org-agenda-overriding-header (concat ,key " Projects")))))
		       ;; '("ACTIVE" "WAITING" "SLEEPING" "CANCELED" "DONE"))))

;;}}}
(provide 'ProjectManager)

