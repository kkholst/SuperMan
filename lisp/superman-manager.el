;;; superman-manager.el --- org project manager

;; Copyright (C) 2013  Thomas Alexander Gerds

;; Authors:
;; Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Klaus Kähler Holst <kkho@biostat.ku.dk>
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

;; An emacs orgmode based project manager for applied statisticians

;;; Code:

;; External dependencies
(require 'org)  
(require 'deft nil t) ;; http://jblevins.org/git/deft.git
(require 'popup nil t) ;; https://github.com/auto-complete/popup-el.git
(require 'winner) 
(require 'ido)
;; (require 'org-colview)
(require 'org-publish)
(require 'vc)
(require 'cl)

;; Loading extensions
(require 'superman) ;; a project to manage projects
(require 'superman-views)  ;; project views
(require 'superman-capture)  ;; capture information
(require 'superman-git)      ;; git control,
(require 'superman-config)   ;; saving and setting window configurations
(require 'superman-pub)   ;; publication manager
(if (featurep 'deft)
    (require 'superman-deft))     ;; selecting projects via deft

;;{{{ variables and user options

(defvar superman-property-list 
  '((index . "Index")
    (nickname . "NickName")
    (gitstatus . "GitStatus")
    (hash . "Hash")
    (author . "Author")
    (decoration . "Decoration")
    (gitpath . "GitPath")
    (location . "Location")
    (filename . "FileName")
    (others . "Others")
    (category . "Category")
    (git . "Git")
    (date . "Date")
    (lastcommit . "LastCommit")
    (capturedate . "CaptureDate")
    (project . "Project")
    (publish . "Publish")
    (config . "Config")
    (publishdirectory . "PublishDirectory")
    (initialvisit . "InitialVisit"))  
  "Association list with names of all superman properties.
index: Name of the index property
nickname: Name of the nick-name property
gitstatus: Name of the git status property
hash: Name of the git hash property
author: Name of the git author property
decoration: Name of the git decoration property
gitpath: Name of the git path property
location: Name of the location property
filename: Name of the filename property
others: Name of the others (collaborators) property
category: Name of the category property
")

(defun superman-property (label)
  (interactive)
  (cdr (assoc label superman-property-list)))

(defvar superman-default-directory
  (file-name-as-directory org-directory)
  "A place for new projects.")
;;(defvar superman-home (concat (file-name-as-directory org-directory) "projects.org") ;
;;  "File for managing projects. See the manual
;;    for structure and syntax.")
(defvar superman-home (concat (file-name-directory (locate-library "superman")) "../manager/" "Projects.org")
  "File for managing projects. See the manual
    for structure and syntax.")
(defvar supermanual
  (concat (file-name-directory (locate-library "superman")) "../manager/" "Supermanual.org")
  "File with instructions for using superman.")
(defvar superman-default-content "" "Initial contents of org project index file.")
(defvar superman-project-subdirectories nil)
(defvar superman-project-level 4
"Subheading level at which projects are defined in `superman-home'.")
(defvar superman-manager-mode-map (make-sparse-keymap)
  "Keymap used for `superman-manager-mode' commands.")
(defvar superman-project-alist nil
  "Alist of projects associating the nickname of the project
    with information like the location of the project, the index file,
    collaborator names, a category, the publishing directory, etc.")
(defvar superman-current-project nil "The currently selected project.")
(defvar superman-project-categories nil
  "List of categories for sorting projects.")
(defvar superman-org-location "/"
    "Relative to the project location this defines
  the path to the index file of a project. If set to
  'org' then the index file will be placed
  in a subdirectory 'org' of the project directory.
 The project directory is set by a property LOCATION in
the `superman-home'.")
(defvar superman-default-category "Unsorted" "Category for new projects.")
(defvar superman-select-project-summary-format
  "%c/%o/%n"
  "Format of the entries of the completion-list when selecting a project. ")
;; (setq superman-select-project-summary-format "%n %c -- %o")
;; (setq superman-select-project-summary-format "%n %o")
(defvar superman-frame-title-format nil
  "if non-nil add the nickname of the active project to frame-title")
(defvar superman-save-buffers 'save-some-buffers
    "Function to be called to save buffers before switching project.")

(defvar superman-config-alist '(("supermanual" . "PROJECT / SUPERMANUAL")))

;; config

(setq superman-config-action-alist '(("INDEX" . superman-find-index)
				    ("TODO" . superman-project-todo)
				    ("TIMELINE" . superman-timeline)
				    ("LOCATION" . superman-location)
				    ("DOCUMENTS" . superman-view-documents)
				    ("FILELIST" . superman-file-list)
				    ("PROJECT" . superman-view-project)
				    ("SUPERMANUAL" . supermanual)
				    ("magit" . superman-magit)
				    ("recent.org" . superman-recent-org)
				    ("*shell*" . superman-start-shell)
				    ("*S*" . superman)
				    ("*S-todo*" . superman-todo)
				    ("*S-agenda*" . superman-agenda)
                                    ("*ielm*" . 
				     (lambda (project) 
				       (if (get-buffer "*ielm*") 
					   (switch-to-buffer "*ielm*") 
					 (ielm))))
				    ("*R*" . superman-find-R-function)
				    ("" . (lambda (project) ))))

;; TODO Add description
(defvar superman-default-config "PROJECT" "default window configuration") 
(defvar superman-sticky-config nil "sticky window configuration")
;; (setq superman-sticky-config "recent.org / *R* | TODO")

(defvar superman-file-manager "file-list")
(defvar superman-find-R-function
  "Function used to find *R*"
  (lambda (project) (if (get-buffer "*R*") (switch-to-buffer "*R*") (R))))

(defvar superman-switch-always t
  "If nil 'superman-switch-to-project' will
 switch to current project unless the last command also was 'superman-switch-to-project'.
 Setting this variable to non-nil (the default) will force 'superman-switch-to-project'
 to always prompt for new project")
(defvar superman-human-readable-ext "^[^\\.].*\\.org\\|\\.[rR]\\|\\.tex\\|\\.txt\\|\\.el$" "Extensions of human readable files")
(defvar superman-config-cycle-pos 0 "Position in the current window configuration cycle. Starts at 0.")
(defvar superman-export-subdirectory "export")
(defvar superman-public-directory "~/public_html/")
(defvar superman-public-server "" "Place on the web where pages are published.")
(defvar superman-export-base-extension "html\\|png\\|jpg\\|org\\|pdf\\|R")
;; (setq org-agenda-show-inherited-tags (list))

;;}}}
;;{{{ the pro-file in manager-mode

;; The project manager is in org-mode (major-mode). To bind specific
;; keystrokes differently in this file, the current solution is to put
;; a minor-mode on top of it.

(define-minor-mode superman-manager-mode 
  "Toggle org projectmanager document view mode.
                  With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
                  turn it off.
                  
                  Enabling superman-view mode electrifies the column view for documents
                  for git and other actions like commit, history search and pretty log-view."
  :lighter " manager"
  :group 'org
  :keymap 'superman-manager-mode-map
  (setq superman-manager-mode
	(not (or (and (null arg) superman-manager-mode)
		 (<= (prefix-numeric-value arg) 0))))    
  (add-hook 'after-save-hook 'superman-refresh nil 'local))

(define-key superman-manager-mode-map [(meta return)] 'superman-return)
;; (define-key superman-manager-mode-map [(meta n)] 'superman-next-project)
;; (define-key superman-manager-mode-map [(meta p)] 'superman-previous-project)
(define-key superman-manager-mode-map [f1] 'superman-manager)

(defun superman-manager ()
  (interactive)
  (pop-to-buffer "*superman-manager*")
  (local-set-key "d" 'superman-view-documents)
  (local-set-key "D" 'superman-view-all-documents)
  (local-set-key "N" 'superman-new-project)
  (insert "Press 'd' to view documents")
  (insert "Press 'n' to view notes")
  (insert "Press 'N' to add a project"))


;; (defun superman-view-all-documents ()
;;  (interactive)
;;  (org-tags-view nil "LastCommit={.+}&GitStatus<>{Comitted}"))
;; (defun superman-manager-mode (&optional arg)
;;  "A minor mode for using org Project Manager."
;;  (interactive "P")
;;  ;; (make-variable-buffer-local 'hippie-expand-try-functions-list)
;;  (setq superman-manager-mode
;;      (not (or (and (null arg) superman-manager-mode)
;;               (<= (prefix-numeric-value arg) 0))))    
;;  (add-hook 'after-save-hook 'superman-refresh nil 'local))
;; (defvar superman-manager-mode nil)
;; (make-variable-buffer-local 'superman-manager-mode)
;;(or (assq 'superman-manager-mode minor-mode-map-alist)
;;    (setq minor-mode-map-alist
;;        (append minor-mode-map-alist
;;                (list (cons 'superman-manager-mode superman-manager-mode-map)))))
;;(or (assq 'superman-manager-mode minor-mode-alist)
;;    (setq minor-mode-alist
;;        (cons '(superman-manager-mode " Project") minor-mode-alist)))
(add-hook 'find-file-hooks 
        (lambda ()
          (let ((file (buffer-file-name)))
            (when (and file (equal file (expand-file-name superman-home)))
              (superman-manager-mode)))))


(defun superman-goto-project-manager ()
  (interactive)
  (find-file superman-home))

(defun superman-project-at-point (&optional noerror)
  "Check if point is at project heading and return the project,
                      i.e. its entry from the 'superman-project-alist'.
                      Otherwise return error or nil if NOERROR is non-nil. "
  (interactive)
  ;; (org-back-to-heading)
  (if (or (org-before-first-heading-p)
	  (not (org-at-heading-p))
	  (not (= superman-project-level
		  (- (match-end 0) (match-beginning 0) 1))))
      (if noerror nil
	(error "No project at point"))
    (or (org-entry-get nil "NICKNAME")
	(progn (superman-set-nickname)
	       (save-buffer) ;; to update the project-alist
	       (org-entry-get nil "NICKNAME")))))

(defun superman-goto-profile (project)
  (let ((case-fold-search t))
    (find-file superman-home)
    (unless (superman-manager-mode 1))
    (goto-char (point-min))
    (or (re-search-forward (concat "^[ \t]*:NICKNAME:[ \t]*" (car project)) nil t)
	(error (concat "Cannot locate project " (car project))))))

(defun superman-project-at-point (&optional pom)
  (let* ((pom (or pom (org-get-at-bol 'org-hd-marker)))
	 (nickname (superman-get-property pom "NickName"))
	 (pro (assoc nickname superman-project-alist)))
    pro))

(defun superman-return ()
  (interactive)
  (let ((pro (superman-project-at-point)))
    (if pro
	(superman-switch-to-project pro nil))))

(defun superman-forward-project ()
  (interactive)
  (re-search-forward
   (format "^\\*\\{%d\\} " superman-project-level) nil t))

(defun superman-backward-project ()
  (interactive)
  (re-search-backward
   (format "^\\*\\{%d\\} " superman-project-level) nil t))


;;}}}
;;{{{ parsing dynamically updating lists

(defun superman-get-property  (pom property &optional inherit literal-nil)
  "Read property and remove leading and trailing whitespace."
  (let* ((case-fold-search t)
	 (prop
	 (if (not (markerp pom))
	     (org-entry-get pom property inherit literal-nil)
	   (if (marker-buffer pom)
	       ;;FIXME: maybe the following widen is unnecessary?
	       (save-excursion
		 (save-restriction
		   (set-buffer (marker-buffer pom))
		   (widen)
		   (org-entry-get pom property inherit literal-nil)))))))
    (if (stringp prop)
	(replace-regexp-in-string "[ \t]+$" "" prop))))
  
(defun superman-parse-projects ()
  "Parse the file `superman-home' and update `superman-project-alist'."
  (interactive)
  (save-excursion
    (setq superman-project-alist nil)
    (set-buffer (find-file-noselect superman-home))
    (show-all)
    (widen)
    (unless (superman-manager-mode 1))
    (save-buffer)
    (goto-char (point-min))
    (while
	 (superman-forward-project)
      (unless (and (org-get-todo-state) (string= (org-get-todo-state) "ZOMBI"))
	(let* ((loc (or (superman-get-property nil (superman-property 'location) 'inherit) superman-default-directory))
	       (category (or (superman-get-property nil (superman-property 'category) 'inherit) "CatWoman"))
	       (others (superman-get-property nil (superman-property 'others) nil))
	       (publish-dir (superman-get-property nil (superman-property 'publish) 'inherit))
	       (name (or (superman-get-property nil (superman-property 'nickname) nil)
			 (nth 4 (org-heading-components))))
	       (marker (org-agenda-new-marker (match-beginning 0)))
	       (hdr (org-get-heading t t))
	       (lastvisit (superman-get-property nil "LastVisit" 'inherit))
	       (config (superman-get-property nil (superman-property 'config) 'inherit))
	       (todo (or (org-get-todo-state) ""))
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
      superman-project-alist))) 

(defun superman-parse-project-categories ()
  "Parse the file `superman-home' and update `superman-project-categories'."
  (interactive)
  (set-buffer (find-file-noselect superman-home))
  (unless (superman-manager-mode 1))
  (save-restriction
    (widen)
    (show-all)
    (save-excursion
      (reverse
       (superman-property-values "category")))))

(defun superman-property-values (key)
  "Return a list of all values of property KEY in the current buffer or region. This
function is very similar to `org-property-values' with two differences:
1) values are returned without text-properties.
2) The function does not call widen and hence search can be restricted to region."
  (save-excursion
    (save-restriction
      ;; (widen)
      (goto-char (point-min))
      (let ((re (org-re-property key))
	    values)
	(while (re-search-forward re nil t)
	  (add-to-list 'values (org-trim (match-string-no-properties 3))))
	(delete "" values)))))


(defun superman-property-keys (&optional include-specials include-defaults)
  "Get all property keys in the current buffer or region.
This is basically a copy of `org-buffer-property-keys'.

With INCLUDE-SPECIALS, also list the special properties that reflect things
like tags and TODO state.

With INCLUDE-DEFAULTS, also include properties that has special meaning
internally: ARCHIVE, CATEGORY, SUMMARY, DESCRIPTION, LOCATION, and LOGGING
and others."
  (let (rtn range cfmt s p)
    (save-excursion
      (save-restriction
	;; (widen)
	(goto-char (point-min))
	(while (re-search-forward org-property-start-re nil t)
	  (setq range (org-get-property-block))
	  (goto-char (car range))
	  (while (re-search-forward
		  org-property-re
		  ;; (org-re "^[ \t]*:\\([-[:alnum:]_]+\\):")
		  (cdr range) t)
	    (add-to-list 'rtn (org-match-string-no-properties 2)))
	  (outline-next-heading))))
    (when include-specials
      (setq rtn (append org-special-properties rtn)))
    (when include-defaults
      (mapc (lambda (x) (add-to-list 'rtn x)) org-default-properties)
      (add-to-list 'rtn org-effort-property))
    (sort rtn (lambda (a b) (string< (upcase a) (upcase b))))))


(defun superman-refresh ()
  "Parses the categories and projects in file `superman-home' and also
             updates the currently selected project."
  (interactive)
  ;; (superman-parse-project-categories)
  (superman-parse-projects)
  (when superman-current-project
    (setq superman-current-project
	  (assoc (car superman-current-project) superman-project-alist))))





;;}}}
;;{{{ Adding, (re-)moving, projects

;; (add-to-list 'org-structure-template-alist
;; '("P" "**** ACTIVE %?:PROPERTIES:\n:NICKNAME:\n:OTHERS:\n:CaptureDate:\n:END:"))



(defun superman-create-project (project &optional ask)
  "Create the index file, the project directory, and subdirectories if
                                    'superman-project-subdirectories' is set."
  (interactive)
  (let ((pro (if (stringp project)
		 (assoc project superman-project-alist)
	       project)))
    (when pro
      (let ((dir (concat (superman-get-location pro) (car pro)))
	    (index (superman-get-index pro)))
	(when (and index (not (file-exists-p index)))
	  (unless (file-exists-p (file-name-directory index))
	    (make-directory (file-name-directory index) t))
	  (find-file index)
	  (unless (file-exists-p index)
	    (insert "***** Index of project " (car pro) " (this line can be deleted.)\n")
	    (save-buffer)))
	;; (append-to-file superman-default-content nil index)
	(unless (or (not dir) (file-exists-p dir) (not (and ask (y-or-n-p (concat "Create directory (and default sub-directories) " dir "? ")))))
	  (make-directory dir)
	  (loop for subdir in superman-project-subdirectories
		do (unless (file-exists-p subdir) (make-directory (concat path subdir) t))))
	(find-file superman-home)
	(unless (superman-manager-mode 1))
	(goto-char (point-min))
	(re-search-forward (concat (make-string superman-project-level (string-to-char "*")) ".*" (car pro)) nil )))))

(defun superman-move-project (&optional project)
  (interactive)
  (let* ((pro (or project (superman-select-project)))
	 (index (superman-get-index pro))
	 (dir (concat (superman-get-location pro) (car pro)))
	 (target  (read-directory-name (concat "Move all files below " dir " to: " )))
	 (new-index (unless (string-match dir (file-name-directory index))
		      (read-file-name (concat "Move " index " to ")))))
    (if (string= (file-name-as-directory target) target)
	(setq target (concat target (file-name-nondirectory dir))))
    (unless (file-exists-p (file-name-directory target)) (make-directory (file-name-directory target)))
    (when (yes-or-no-p (concat "Move " dir " to " target "? "))
      (rename-file dir target)
      (if (and new-index (yes-or-no-p (concat "Move " index " to " new-index "? ")))
	  (rename-file index new-index))
      (superman-goto-profile pro)
      (org-set-property (superman-property 'location)
			(file-name-directory target))
      (org-set-property (superman-property 'index)
			(or new-index
			    (replace-regexp-in-string
			     (expand-file-name (file-name-directory dir))
			     (expand-file-name (file-name-directory target))
			     (expand-file-name index))))
      (save-buffer))))

(defun superman-delete-project (&optional project)
  (interactive)
  (let* ((marker (org-get-at-bol 'org-hd-marker))
	 (scene (current-window-configuration))
	 (pro (or project (superman-project-at-point)))
	 (dir (concat (superman-get-location pro) (car pro)))
	 (index (superman-get-index pro))
	 (ibuf (get-file-buffer index)))
    (when (and (file-exists-p dir)
	       (yes-or-no-p (concat "Remove project directory tree? " dir " ")))
      (move-file-to-trash dir))
    (when (and (file-exists-p index)
	       (yes-or-no-p (concat "Remove index file? " index)))
      (move-file-to-trash index))
    (when (buffer-live-p ibuf)
	(kill-buffer ibuf))
    (superman-view-index)
    (org-narrow-to-subtree)
    (when (yes-or-no-p "Delete this project from SuperMan control? ")
      (org-cut-subtree))
    (widen)
    (superman-parse-projects)
    (delete (car pro) superman-project-history)
    (when (buffer-live-p (get-buffer (concat "*Project[" (car pro) "]*")))
      (kill-buffer (concat "*Project[" (car pro) "]*")))
    (set-window-configuration scene)
    (when superman-mode (superman-redo))))
      

;;}}}
;;{{{ setting project properties

(defun superman-project-agenda ()
    "Show an agenda of all the projects. Useful, e.g. for toggling
the active status of projects."
    (interactive)
    (find-file superman-home)
    (push ?t unread-command-events)
    (push ?< unread-command-events)
    (call-interactively 'org-agenda))  


(defun superman-set-nickname ()
  (interactive)
  (org-set-property
   (superman-property 'nickname)
   (read-string "NickName for project: "
		(nth 4 (org-heading-components)))))

(defun superman-set-others ()
  (interactive)
  (let* ((pro (assoc (superman-project-at-point t)
		     superman-project-alist)) (superman-forward-project)
	 (others (cdr (assoc (superman-property 'others) (cadr pro))))
	 (init (if others (concat others ", ") "")))
    ;; (org-entry-get nil "others")
    (if pro
	(org-set-property
	 (superman-property 'others)
	 (replace-regexp-in-string
	  "[,\t ]+$" ""     (read-string (concat "Set collaborators for " (car pro) ": ") init))))))

(defun superman-fix-others ()
  "Update the others property (collaborator names) of all projects in `superman-home'."
  (interactive "P")
  (set-buffer (find-file-noselect superman-home))
  (unless (superman-manager-mode 1))
  (goto-char (point-min))
  (while (superman-forward-project)
	(superman-set-others)))

;;}}}
;;{{{ listing projects

(defun superman-index-list (&optional category state extension not-exist-ok update exclude-regexp)
  "Return a list of project specific indexes.
Projects are filtered by CATEGORY unless CATEGORY is nil.
Projects are filtered by the todo-state regexp STATE unless STATE is nil.
Only existing files are returned unless NOT-EXIST-OK is non-nil.
Only files ending on EXTENSION are returned unless EXTENSION is nil.
Only files not matching EXCLUDE-REGEXP are included.a

If UPDATE is non-nil first parse the file superman.
Examples:
 (superman-index-list nil \"ACTIVE\")
 (superman-index-list nil \"DONE\")
"
  (interactive "P")
  (when update
    (superman-refresh))
  (let* ((testfun
	  (lambda (p)
	    (let ((p-cat (superman-get-category p)))
	      (when (and
		     (or (not category)
			 (not p-cat)
			 (string= (downcase category) (downcase p-cat)))
		     (or (not state)
			 (string-match state (superman-get-state p)))) p))))
	 (palist (if (or category state)
		     (delq nil (mapcar testfun superman-project-alist))
		   superman-project-alist))
	 (index-list
	  (delete-dups
	   (delq nil
		 (mapcar
		  #'(lambda (x)
		      (let ((f (superman-get-index x)))
			(when (and (or not-exist-ok (file-exists-p f))
				   (or (not exclude-regexp) (not (string-match exclude-regexp f)))
				   (or (not extension)
				       (string= extension (file-name-extension f))))
			  f)))
		  palist)))))
    index-list))

;;}}}
;;{{{ selecting projects

(defun superman-format-project (entry)
  (let* ((cat (or (superman-get entry "category") ""))
	 (coll (or (superman-get entry "others") ""))
	 (nickname (car entry))
	 (string (replace-regexp-in-string "%c" cat superman-select-project-summary-format))
	 (string (replace-regexp-in-string "%o" coll string))
	 (string (replace-regexp-in-string "%n" (car entry) string)))
    (cons string (car entry))))

(defun superman-select-project ()
  "Select a project from the project alist, 
              which is modified such that 'superman-current-project'
              is the first choice."
  (let* ((plist superman-project-alist)
	 (project-array (mapcar 'superman-format-project
				(if (not superman-current-project)
				    plist
				  (setq plist (append (list superman-current-project)
						      (remove superman-current-project plist))))))
	 (completion-ignore-case t)
	 (key (ido-completing-read "Project: " (mapcar 'car project-array)))
	 (nickname (cdr (assoc key project-array))))
    (assoc nickname superman-project-alist)))

(defun superman-set-frame-title ()
  (let* ((old-format (split-string frame-title-format "Project:[ \t]+[^ \t]+[ \t]+"))
        (keep (if (> (length old-format) 1) (cadr old-format) (car old-format))))
    (setq frame-title-format
          (concat "Project: " (or (car superman-current-project) "No active project") " " keep))))

(defun superman-activate-project (project)
  "Sets the current project.
            Start git, if the project is under git control, and git is not up and running yet."
  (setq superman-current-project project)
  (if superman-frame-title-format (superman-set-frame-title))
  (with-current-buffer (or (find-buffer-visiting superman-home)
			   (find-file-noselect superman-home))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (concat ":NICKNAME:[ \t]?.*" (car project)) nil t)
      (org-entry-put (point) "LastVisit"
		     (format-time-string "<%Y-%m-%d %a %H:%M>")
		     ;; (with-temp-buffer (org-insert-time-stamp (current-time) 'hm))
		     )
      (save-buffer))))

(defun superman-save-project (&optional project)
  (interactive)
  (when (functionp superman-save-buffers)
    (funcall superman-save-buffers))
  (let* ((pro (or project superman-current-project)))))

;;}}}
;;{{{ switching projects (see also superman-config)

(defun superman-switch (&optional arg)
  "If ARG switch project else switch config."
  (interactive "P")
  (if arg
      (superman-switch-to-project)
    (superman-switch-config)))

(defun superman-switch-to-project (&optional project noselect)
  "Select project via `superman-select-project', activate it
 via `superman-activate-project',  find the associated index file.

Unless NOSELECT is nil, set the next window config of project.
If NOSELECT is set return the project."
  (interactive "P")
  (let* ((curpro superman-current-project)
	 (pro (cond (project
		     (if (stringp project)
			 (assoc project superman-project-alist)
		       project))
		    (t (superman-select-project))))
	 (stay (eq pro curpro)))
    (unless stay
      (if (member (car pro) superman-project-history)
	  (progn
	    (setq superman-project-history
		  (cons (car pro) superman-project-history))
	    (delete-dups superman-project-history))
	(setq superman-project-history
	      (cons (car pro) superman-project-history)))
      ;; (add-to-list 'superman-project-history (car pro))
      (superman-save-project curpro)
      (superman-activate-project pro))
    (if noselect
	superman-current-project
      (if stay 
	  (superman-switch-config pro nil)
	;; the next command 
	;; re-sets superman-config-cycle-pos 
	(superman-switch-config pro 0)))))

(defun superman-list-files (dir ext sort-by)
  (if (featurep 'file-list)
      (mapcar 'file-list-make-file-name
	      (file-list-sort-internal
	       (file-list-select-internal nil ext nil nil dir nil 'dont)
	       (or sort-by "time") nil t))
    (directory-files dir nil ext t)))

;;}}}
;;{{{ publishing project contents

(defun superman-browse-this-file (&optional arg)
  "Browse the html version of the current file using `browse-url'. If
        prefix arg is given, then browse the corresponding file on the superman-public-server"
  (interactive "P")
  (let* ((bf (buffer-file-name (current-buffer)))
	 (server-home (if (and arg (not superman-public-server-home))
			  (read-string "Specify address on server: " "http://")
			superman-public-server-home))
         (html-file (if arg
                        (concat (replace-regexp-in-string
                                 (expand-file-name superman-public-directory)
                                 server-home
                                 (file-name-sans-extension bf))
                                ".html")
                      (concat "file:///" (file-name-sans-extension bf) ".html"))))
    ;; fixme superman-browse-file-hook (e.g. to synchronize with public server)
    (message html-file)
    (browse-url html-file)))


(defun superman-set-publish-alist ()
  (interactive)
  (let ((p-alist superman-project-alist))
    (while p-alist
      (let* ((pro  (car p-alist))
	     (nickname (car pro))
	     (base-directory (concat (superman-get-location pro) (car pro)))
	     (export-directory
	      (concat base-directory "/"
		      superman-export-subdirectory))
	     (public-directory
	      (or (superman-get-publish-directory pro)
		  (concat (file-name-as-directory superman-public-directory)
			  nickname))))
	;;(replace-regexp-in-string superman-public-directory (getenv "HOME") (expand-file-name export-directory))))
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
                       ,superman-export-base-extension
		       :publishing-directory
		       ,public-directory
		       :recursive t
		       :publishing-function
		       org-publish-attachment))
	(add-to-list 'org-publish-project-alist
		     `(,nickname
		       :components (,(concat nickname "-export") ,(concat nickname "-copy")))))
      (setq p-alist (cdr p-alist)))))

;;}}}
;;{{{ extracting properties from a project 
(defun superman-get (project el)
  (cdr (assoc el (cadr project))))

(defun superman-get-index (project)
"Extract the index file of PROJECT."
  (cdr (assoc "index" (cadr project))))

(defun superman-get-git (project)
  (or (cdr (assoc "git" (cadr project))) ""))
(defun superman-visit-home (project)
  "Open the supermanager and leave point at PROJECT"
  (S)
  (re-search-forward (car project) nil t))

(defun superman-go-home (&optional heading)
  "Visit the file superman-home and leave point at PROJECT."
  (find-file superman-home)
  (goto-char (point-min))
  (re-search-forward
   (format org-complex-heading-regexp-format (regexp-quote heading))
   nil t))

(defun superman-project-home (project)
  (concat (superman-get-location project) (car project)))

(defun superman-get-location (project)
  "Get the directory associated with PROJECT."
  (file-name-as-directory (cdr (assoc "location" (cadr project)))))
;;  (let ((loc (cdr (assoc "location" (cadr project)))))
;;                (if loc 
;;                                (concat (file-name-as-directory loc)
;;                                        (car project)))))

(defun superman-get-config (project)
  (cdr (assoc "config" (cadr project))))

(defun superman-get-publish-directory (project)
  (cdr (assoc "publish-directory" (cadr project))))

(defun superman-get-category (project)
  (cdr (assoc "category" (cadr project))))

(defun superman-get-others (project)
  (cdr (assoc "others" (cadr project))))

(defun superman-get-lastvisit (project)
  (cdr (assoc "lastvisit" (cadr project))))

(defun superman-get-state (project)
  (cdr (assoc 'todo (cadr project))))
;;}}}

(provide 'superman-manager)
;;; superman-manager.el ends here
