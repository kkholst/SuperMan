;;; org-pro-manager.el --- org project manager

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
(require 'deft)
(require 'winner)
(require 'ido)
(require 'org-colview)
(require 'org-publish)

;; Loading extensions
(require 'org-pro-superman) ;; a project to manage projects
(require 'org-pro-summary)  ;; project summary
(require 'org-pro-git)      ;; git contro,
(require 'org-pro-config)   ;; saving and setting window configurations
(require 'org-pro-deft)     ;; selecting projects via deft

;;{{{ variables and user options

(defvar org-pro-default-directory
  (file-name-as-directory org-directory)
  "A place for new projects.")
(defvar org-pro-file (concat
		      (file-name-as-directory org-directory)
		      "Projects.org")
  "File for managing projects. See the manual
    for structure and syntax.")
(defvar org-pro-default-content "" "Initial contents of org project index file.")
(defvar org-pro-project-subdirectories nil)
(defvar org-pro-project-level 4
"Subheading level at which projects are defined in `org-pro-file'.")
(defvar org-pro-manager-mode-map (make-sparse-keymap)
  "Keymap used for `org-pro-manager-mode' commands.")
(defvar org-pro-project-alist nil
  "Alist of projects associating the nickname of the project
    with information like the location of the project, the index file,
    collaborator names, a category, the publishing directory, etc.")
(defvar org-pro-current-project nil "The currently selected project.")
(defvar org-pro-project-categories nil
  "List of categories for sorting projects.")
(defvar org-pro-org-location "/"
    "Relative to the project location this defines
  the path to the index file of a project. If set to
  'org' then the index file will be placed
  in a subdirectory 'org' of the project directory.
 The project directory is set by a property LOCATION in
the `org-pro-file'.")
(defvar org-pro-default-category "Unsorted" "Category for new projects.")
(defvar org-pro-select-project-summary-format
  "%c/%o/%n"
  "Format of the entries of the completion-list when selecting a project. ")
;; (setq org-pro-select-project-summary-format "%n %c -- %o")
;; (setq org-pro-select-project-summary-format "%n %o")
(defvar org-pro-frame-title-format t
  "If non-nil add the nickname of the active project to frame-title")
(defvar org-pro-save-buffers 'save-some-buffers
    "Function to be called to save buffers before switching project.")

;; config
(setq org-pro-config-action-alist '(("INDEX" . org-pro-find-index)
				    ("TODO" . org-pro-todo)
				    ("TIMELINE" . org-pro-timeline)
				    ("LOCATION" . org-pro-location)
				    ("DOCUMENTS" . org-pro-view-documents)
				    ("FILELIST" . org-pro-file-list)
				    ("magit" . org-pro-magit)
				    ("recent.org" . org-pro-recent-org)
				    ("*shell*" . (lambda (project) (if (get-buffer "*shell*") (switch-to-buffer "*shell*") (shell))))
                                    ("*ielm*" . (lambda (project) (if (get-buffer "*ielm*") (switch-to-buffer "*ielm*") (ielm))))
				    ("*R*" . org-pro-find-R-function)))
(setq org-pro-default-config "INDEX")
(setq org-pro-sticky-config nil)
;; (setq org-pro-sticky-config "recent.org / *R* | TODO")
(defvar org-pro-file-manager "file-list")
(defvar org-pro-find-R-function
  "Function used to find *R*"
  (lambda (project) (if (get-buffer "*R*") (switch-to-buffer "*R*") (R))))

(defvar org-pro-switch-always t
  "If nil 'org-pro-switch-to-project' will
 switch to current project unless the last command also was 'org-pro-switch-to-project'.
 Setting this variable to non-nil (the default) will force 'org-pro-switch-to-project'
 to always prompt for new project")
(defvar org-pro-human-readable-ext "^[^\\.].*\\.org\\|\\.[rR]\\|\\.tex\\|\\.txt\\|\\.el$" "Extensions of human readable files")
(defvar org-pro-config-cycle-pos 0 "Position in the current window configuration cycle. Starts at 0.")

(defvar org-pro-export-subdirectory "export")
(defvar org-pro-public-directory "~/public_html/")
(defvar org-pro-public-server "" "Place on the web where pages are published.")
(defvar org-pro-export-base-extension "html\\|png\\|jpg\\|org\\|pdf\\|R")

;;}}}
;;{{{ the pro-file in manager-mode

;; The project manager is in org-mode (major-mode). To bind specific
;; keystrokes differently in this file, the current solution is to put
;; a minor-mode on top of it.

(define-minor-mode org-pro-manager-mode 
  "Toggle org projectmanager document view mode.
                  With argument ARG turn org-pro-docview-mode on if ARG is positive, otherwise
                  turn it off.
                  
                  Enabling org-pro-view mode electrifies the column view for documents
                  for git and other actions like commit, history search and pretty log-view."
  :lighter " manager"
  :group 'org
  :keymap 'org-pro-manager-mode-map
  (setq org-pro-manager-mode
	(not (or (and (null arg) org-pro-manager-mode)
		 (<= (prefix-numeric-value arg) 0))))    
  (add-hook 'after-save-hook 'org-pro-refresh nil 'local))

(define-key org-pro-manager-mode-map [(meta return)] 'org-pro-return)
(define-key org-pro-manager-mode-map [(meta n)] 'org-pro-next-project)
(define-key org-pro-manager-mode-map [(meta p)] 'org-pro-previous-project)
(define-key org-pro-manager-mode-map [f1] 'org-pro-manager)

(defun org-pro-manager ()
  (interactive)
  (pop-to-buffer "*org-pro-manager*")
  (local-set-key "d" 'org-pro-view-documents)
  (local-set-key "D" 'org-pro-view-all-documents)
  (local-set-key "N" 'org-pro-new-project)
  (insert "Press 'd' to view documents")
  (insert "Press 'n' to view notes")
  (insert "Press 'N' to add a project"))


;; (defun org-pro-view-all-documents ()
;;  (interactive)
;;  (org-tags-view nil "LastCommit={.+}&GitStatus<>{Comitted}"))
;; (defun org-pro-manager-mode (&optional arg)
;;  "A minor mode for using org Project Manager."
;;  (interactive "P")
;;  ;; (make-variable-buffer-local 'hippie-expand-try-functions-list)
;;  (setq org-pro-manager-mode
;;      (not (or (and (null arg) org-pro-manager-mode)
;;               (<= (prefix-numeric-value arg) 0))))    
;;  (add-hook 'after-save-hook 'org-pro-refresh nil 'local))
;; (defvar org-pro-manager-mode nil)
;; (make-variable-buffer-local 'org-pro-manager-mode)
;;(or (assq 'org-pro-manager-mode minor-mode-map-alist)
;;    (setq minor-mode-map-alist
;;        (append minor-mode-map-alist
;;                (list (cons 'org-pro-manager-mode org-pro-manager-mode-map)))))
;;(or (assq 'org-pro-manager-mode minor-mode-alist)
;;    (setq minor-mode-alist
;;        (cons '(org-pro-manager-mode " Project") minor-mode-alist)))
(add-hook 'find-file-hooks 
        (lambda ()
          (let ((file (buffer-file-name)))
            (when (and file (equal file (expand-file-name org-pro-file)))
              (org-pro-manager-mode)))))


(defun org-pro-goto-project-manager ()
  (interactive)
  (find-file org-pro-file))

(defun org-pro-project-at-point (&optional noerror)
  "Check if point is at project heading and return the project,
                      i.e. its entry from the 'org-pro-project-alist'.
                      Otherwise return error or nil if NOERROR is non-nil. "
  (interactive)
  ;; (org-back-to-heading)
  (if (or (org-before-first-heading-p)
	  (not (org-at-heading-p))
	  (not (= org-pro-project-level
		  (- (match-end 0) (match-beginning 0) 1))))
      (if noerror nil
	(error "No project at point"))
    (or (org-entry-get nil "NICKNAME")
	(progn (org-pro-set-nickname)
	       (save-buffer) ;; to update the project-alist
	       (org-entry-get nil "NICKNAME")))))

(defun org-pro-goto-profile (project)
  (let ((case-fold-search t))
    (find-file org-pro-file)
    (unless (org-pro-manager-mode 1))
    (goto-char (point-min))
    (or (re-search-forward (concat "^[ \t]*:NICKNAME:[ \t]*" (car project)) nil t)
	(error (concat "Cannot locate project " (car project))))))

(defun org-pro-return ()
  (interactive)
  (let* ((pro (assoc (org-pro-project-at-point)
		     org-pro-project-alist)))
    (delete-other-windows)
    (split-window-horizontally 25)
    (other-window 1)
    (find-file (org-pro-get-index pro))
    (split-window-vertically 13)
    (switch-to-buffer "*Current project*")
    (erase-buffer)
    (insert (car pro) "\n------------------------------\n")
    (mapc (lambda (x) (insert (car x) ": " (if (cdr x) (cdr x) "")  "\n")) (cadr pro))
    (other-window 1)))

(defun org-pro-forward-project ()
  (interactive)
  (re-search-forward
   (format "^\\*\\{%d\\} " org-pro-project-level) nil t))

(defun org-pro-backward-project ()
  (interactive)
  (re-search-backward
   (format "^\\*\\{%d\\} " org-pro-project-level) nil t))

(defun org-pro-next-project (arg)
  (interactive  "p")
  (org-pro-forward-project)
  (org-pro-return))

(defun org-pro-previous-project (arg)
  (interactive  "p")
  (org-pro-backward-project)
  (org-pro-return))

;;}}}
;;{{{ parsing dynamically updating lists

(defun org-pro-get-property  (pom property &optional inherit literal-nil)
  "Read property and remove leading and trailing whitespace."
  (let ((prop (org-entry-get pom property inherit literal-nil)))
    (if (stringp prop) (replace-regexp-in-string "[ \t]+$" "" prop))))
  
  (defun org-pro-parse-projects (&optional all)
    "Parse the file `org-pro-file' and update `org-pro-project-alist'."
    (interactive)
    (save-excursion
      (setq org-pro-project-alist nil)
      (set-buffer (find-file-noselect org-pro-file))
      (unless (org-pro-manager-mode 1))
      (save-buffer)
      (goto-char (point-min))
      (while (org-pro-forward-project)
        (let* ((loc (or (org-pro-get-property nil "LOCATION" 'inherit) org-pro-default-directory))
               (category (org-pro-get-property nil "CATEGORY" 'inherit))
               (others (org-pro-get-property nil "OTHERS" nil))
               (publish-dir (org-pro-get-property nil "PUBLISH" 'inherit))
               (name (or (org-pro-get-property nil "NICKNAME" nil)
                         (nth 4 (org-heading-components))))
               (git (org-pro-get-property nil "GIT" 'inherit))
               (config (org-pro-get-property nil "config" 'inherit))
               (todo (substring-no-properties (or (org-get-todo-state) "")))
               (index (or (org-pro-get-property nil "INDEX" nil)
                          (let ((default-org-home
                                  (concat (file-name-as-directory loc)
                                          name
                                          org-pro-org-location)))
                            ;; (make-directory default-org-home t)
                            (concat (file-name-as-directory default-org-home) name ".org")))))
          (unless (file-name-absolute-p index)
            (setq index
                  (expand-file-name (concat (file-name-as-directory loc) name "/" index))))
          (add-to-list 'org-pro-project-alist
                       (list name
                             (list (cons "location"  loc)
                                   (cons "index" index)
                                   (cons "category" category)
                                   (cons "others" others)
                                   (cons "git" git)
                                   (cons "config" config)
                                   (cons "state" todo)
                                   (cons "publish-directory" publish-dir))))))
      org-pro-project-alist))
  
  
  (defun org-pro-get-buffer-props (property)
    "Get a table of all values of PROPERTY used in the buffer, for completion."
    (let (props)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat ":" property ":") nil t)
          (add-to-list 'props (list
                               (org-entry-get
                                nil property nil)))))
      props))
  
  (defun org-pro-parse-categories ()
    "Parse the file `org-pro-file' and update `org-pro-project-categories'."
    (interactive)
    (set-buffer (find-file-noselect org-pro-file))
    (unless (org-pro-manager-mode 1))
    (setq org-pro-project-categories
          (reverse (org-pro-get-buffer-props "CATEGORY"))))
  
  (defun org-pro-refresh ()
    "Parses the categories and projects in file `org-pro-file' and also
             updates the currently selected project."
    (interactive)
    (org-pro-parse-categories)
    (org-pro-parse-projects)
    (when org-pro-current-project
      (setq org-pro-current-project
            (assoc (car org-pro-current-project) org-pro-project-alist))))

;;}}}
;;{{{ Adding, (re-)moving, projects

;; (add-to-list 'org-structure-template-alist
;; '("P" "**** ACTIVE %?:PROPERTIES:\n:NICKNAME:\n:OTHERS:\n:CaptureDate:\n:END:"))

(defun org-pro-create-project (&optional project ask)
  "Create the index file, the project directory, and subdirectories if
                                    'org-pro-project-subdirectories' is set."
  (interactive)
  (let ((pro (assoc project org-pro-project-alist)))
    (when pro
      (let ((dir (concat (org-pro-get-location pro) (car pro)))
	    (index (org-pro-get-index pro)))
	(when (and index (not (file-exists-p index)))
	  (unless (file-exists-p (file-name-directory index))
	    (make-directory (file-name-directory index) t))
	  (find-file index))
	;; (append-to-file org-pro-default-content nil index)
	(unless (or (not dir) (file-exists-p dir) (not (and ask (y-or-n-p (concat "Create directory (and default sub-directories) " dir "? ")))))
	  (make-directory dir)
	  (loop for subdir in org-pro-project-subdirectories
		do (unless (file-exists-p subdir) (make-directory (concat path subdir) t))))
	(find-file org-pro-file)
	(unless (org-pro-manager-mode 1))
	(goto-char (point-min))
	(re-search-forward (concat (make-string org-pro-project-level (string-to-char "*")) ".*" (car pro)) nil )))))


(defun org-pro-new-project (&optional nickname category)
  "Create a new project. Prompt for CATEGORY and NICKNAME if necessary.
                  This function modifies the 'org-pro' and creates and visits the index file of the new project.
                  Thus, to undo all this you may want to call 'org-pro-delete-project'. 
                  " 
  (interactive)
  (org-pro-refresh)
  (let* ((nickname (or (and (not (string= nickname "")) nickname) (read-string "Project name (short) ")))
	 category)
    ;; check if nickname exists 
    (while (assoc nickname org-pro-project-alist)
      (setq nickname
	    (read-string (concat "Project " nickname " exists. Please choose a different name (C-g to exit): "))))
    (setq category (or category (completing-read "Category: " (org-pro-parse-categories) nil nil)))
    ;; a local capture command places the new project
    (let ((org-capture-templates
	   `(("p" "Project" plain
	      (file+headline org-pro-file ,category)
	      ,(concat (make-string org-pro-project-level (string-to-char "*"))
		       " ACTIVE " nickname "%?\n:PROPERTIES:\n:NICKNAME: "
		       nickname
		       "\n:LOCATION: \n:CATEGORY: " category "\n:INDEX: \n:GIT: \n:OTHERS: \n:END:\n"))))
	  (org-capture-bookmark nil))
      (add-hook 'org-capture-mode-hook '(lambda () (define-key org-capture-mode-map [(tab)] 'org-pro-complete-property)) nil 'local)
      (add-hook 'org-capture-after-finalize-hook `(lambda () (save-buffer) (org-pro-create-project ,nickname 'ask)) nil 'local)
      ;;(add-hook 'org-capture-mode-hook 'org-pro-show-properties nil 'local)
      (org-capture nil "p"))))


;; (defun org-pro-show-properties ()
  ;; (let ((pop-up-windows t)
	;; (obuf (current-buffer))
	;; (pbuf (get-buffer "*Org project manager properties*")))
    ;; (set-buffer pbuf)
    ;; (erase-buffer)
    ;; (insert "Current project categories:\n\n")
    ;; (mapcar '(lambda (x) (if (car x) (insert (car x) ", "))) org-pro-project-categories)
    ;; (delete-backward-char 2)
    ;; (insert "\n\n")
    ;; (pop-to-buffer pbuf)
    ;; (pop-to-buffer obuf)))

(defun org-pro-complete-property ()
  (interactive)
  (let ((curprop (save-excursion (beginning-of-line) (looking-at ".*:\\(.*\\):") (org-match-string-no-properties 1))))
    (cond ((string= (downcase curprop) "index")
	   (insert (read-file-name (concat "Set " curprop ": "))))
	  ((string= (downcase curprop) "location")
	   (insert (read-directory-name (concat "Set " curprop ": ")))))))

(defun org-pro-move-project (&optional project)
  (interactive)
  (let* ((pro (or project (org-pro-select-project)))
	 (index (org-pro-get-index pro))
	 (dir (concat (org-pro-get-location pro) (car pro)))
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
      (org-pro-goto-profile pro)
      (org-set-property "LOCATION" (file-name-directory target))
      (org-set-property "INDEX" (or new-index (replace-regexp-in-string (file-name-directory dir) (file-name-directory target) index)))
      (save-buffer))))


(defun org-pro-delete-project (&optional project)
  (interactive)
  (let* ((pro (or project (org-pro-select-project)))
	 (dir (concat (org-pro-get-location pro) (car pro)))
	 (index (org-pro-get-index pro)))
    (pop-to-buffer "*Org-project-files*")
    (erase-buffer)
    (insert index "\n" dir "\n")
    (when (yes-or-no-p (concat "Really remove project " (car pro) "? "))
      (when (file-exists-p dir) (move-file-to-trash dir))
      (when (file-exists-p index) (move-file-to-trash index))
      (find-file org-pro-file)
      (unless (org-pro-manager-mode 1))
      (goto-char (point-min))
      (re-search-forward (concat ":NICKNAME:[ \t]?.*" (car pro)) nil t)
      ;; (org-show-subtree)
      ;; (org-mark-element)
      (message "If you delete this entry and then save the buffer, the project will disappear from the project-alist"))))
;; (when (yes-or-no-p (concat "Is this project entry to be deleted " (car pro) "?"))
;; (kill-region (region-beginning) (region-end))))))  

;;}}}
;;{{{ setting project properties

(defun org-pro-project-agenda ()
    "Show an agenda of all the projects. Useful, e.g. for toggling
the active status of projects."
    (interactive)
    (find-file org-pro-file)
    (push ?t unread-command-events)
    (push ?< unread-command-events)
    (call-interactively 'org-agenda))  


(defun org-pro-set-nickname ()
  (interactive)
  (org-set-property
   "NICKNAME"
   (read-string "NickName for project: "
		(nth 4 (org-heading-components)))))

(defun org-pro-set-others ()
  (interactive)
  (let* ((pro (assoc (org-pro-project-at-point t)
		     org-pro-project-alist))
	 (others (cdr (assoc "others" (cadr pro))))
	 (init (if others (concat others ", ") "")))
    ;; (org-entry-get nil "others")
    (if pro
	(org-set-property
	 "others"
	 (replace-regexp-in-string
	  "[,\t ]+$" ""     (read-string (concat "Set collaborators for " (car pro) ": ") init))))))

(defun org-pro-fix-others ()
  "Update the others property (collaborator names) of all projects in `org-pro-file'."
  (interactive "P")
  (set-buffer (find-file-noselect org-pro-file))
  (unless (org-pro-manager-mode 1))
  (goto-char (point-min))
  (while (org-pro-forward-project)
    (org-pro-set-others)))
;;}}}
;;{{{ listing projects

(defun org-pro-index-list (&optional category state extension not-exist-ok update)
  "Return a list of project specific indexes.
              Projects are filtered by CATEGORY unless CATEGORY is nil.
              Projects are filtered by the todo-state regexp STATE unless STATE is nil.
              Only existing files are returned unless NOT-EXIST-OK is non-nil.
              Only files ending on EXTENSION are returned unless EXTENSION is nil.
              If UPDATE is non-nil first parse the file org-pro.
Examples:
(org-pro-index-list nil \"ACTIVE\")
(org-pro-index-list nil \"DONE\")
"
  (interactive "P")
  (when update
    (org-pro-refresh))
  (let* ((testfun (lambda (p) (when (and
				     (or (not category) (string= category (org-pro-get-category p)))
				     (or (not state) (string-match state (org-pro-get-state p)))) p)))
	 (palist (if (or category state)
		     (delq nil (mapcar testfun org-pro-project-alist))
		   org-pro-project-alist)))
    (delete-dups (delq nil (mapcar '(lambda (x)
				      (let ((f (org-pro-get-index x)))
					(when (and (or not-exist-ok (file-exists-p f))
						   (or (not extension)
						       (string= extension (file-name-extension f))))
					  f)))
				   palist)))))

;;}}}
;;{{{ selecting projects

(defun org-pro-format-project (entry)
  (let* ((cat (or (org-pro-get entry "category") ""))
	 (coll (or (org-pro-get entry "others") ""))
	 (nickname (car entry))
	 (string (replace-regexp-in-string "%c" cat org-pro-select-project-summary-format))
	 (string (replace-regexp-in-string "%o" coll string))
	 (string (replace-regexp-in-string "%n" (car entry) string)))
    (cons string (car entry))))

(defun org-pro-select-project ()
  "Select a project from the project alist, 
              which is modified such that 'org-pro-current-project'
              is the first choice."
  (let* ((plist org-pro-project-alist)
	 (project-array (mapcar 'org-pro-format-project
				(if (not org-pro-current-project)
				    plist
				  (setq plist (append (list org-pro-current-project)
						      (remove org-pro-current-project plist))))))
	 (completion-ignore-case t)
	 (key (ido-completing-read "Project: " (mapcar 'car project-array)))
	 (nickname (cdr (assoc key project-array))))
    (assoc nickname org-pro-project-alist)))

(defun org-pro-set-frame-title ()
  (let* ((old-format (split-string frame-title-format "Project:[ \t]+[^ \t]+[ \t]+"))
        (keep (if (> (length old-format) 1) (cadr old-format) (car old-format))))
    (setq frame-title-format
          (concat "Project: " (or (car org-pro-current-project) "No active project") " " keep))))

(defun org-pro-activate-project (project)
  "Sets the current project.
            Start git, if the project is under git control, and git is not up and running yet."
  (setq org-pro-current-project project)
  (if org-pro-frame-title-format (org-pro-set-frame-title))
  (with-current-buffer (or (find-buffer-visiting org-pro-file)
			   (find-file-noselect org-pro-file))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (concat ":NICKNAME:[ \t]?.*" (car project)) nil t)
      (org-entry-put (point) "LastVisit" (with-temp-buffer (org-insert-time-stamp (current-time) 'hm))))))

(defun org-pro-save-project (&optional project)
    (interactive)
    (when (and (object-p org-pro-save-buffers)
               (functionp org-pro-save-buffers))
      (funcall org-pro-save-buffers))
    (let* ((pro (or project org-pro-current-project)))
       (when org-pro-use-git 
      (org-pro-git-update-project pro nil))))

;;}}}
;;{{{ switching projects (see also org-pro-config)

(defun org-pro-switch (&optional arg)
  "If ARG switch project else switch config."
  (interactive "P")
  (if arg
      (org-pro-switch-to-project)
    (org-pro-switch-config)))

(defun org-pro-switch-to-project (&optional force)
  "Select project via 'org-pro-select-project', activate it
 via 'org-pro-activate-project',  find the associated index file."
  (interactive "P")
  (let* ((curpro org-pro-current-project)
	 (change-maybe (or force
			   org-pro-switch-always
			   (not org-pro-current-project)))
	 (pro (if change-maybe (org-pro-select-project) curpro))
	 (stay (eq pro curpro)))
    (unless stay
      (org-pro-save-project curpro)
      (setq org-pro-config-cycle-pos 0)
      (org-pro-activate-project pro))
    (org-pro-find-project pro org-pro-config-cycle-pos)))

(defun org-pro-list-files (dir ext sort-by)
  (if (featurep 'file-list)
      (mapcar 'file-list-make-file-name
	      (file-list-sort-internal
	       (file-list-select-internal nil ext nil nil dir nil 'dont)
	       (or sort-by "time") nil t))
    (directory-files dir nil ext t)))

;;}}}
;;{{{ publishing project contents

(defun org-pro-browse-this-file (&optional arg)
  "Browse the html version of the current file using `browse-url'. If
        prefix arg is given, then browse the corresponding file on the org-pro-public-server"
  (interactive "P")
  (let* ((bf (buffer-file-name (current-buffer)))
	 (server-home (if (and arg (not org-pro-public-server-home))
			  (read-string "Specify address on server: " "http://")
			org-pro-public-server-home))
         (html-file (if arg
                        (concat (replace-regexp-in-string
                                 (expand-file-name org-pro-public-directory)
                                 server-home
                                 (file-name-sans-extension bf))
                                ".html")
                      (concat "file:///" (file-name-sans-extension bf) ".html"))))
    ;; fixme org-pro-browse-file-hook (e.g. to synchronize with public server)
    (message html-file)
    (browse-url html-file)))


(defun org-pro-set-publish-alist ()
  (interactive)
  (let ((p-alist org-pro-project-alist))
    (while p-alist
      (let* ((pro  (car p-alist))
	     (nickname (car pro))
	     (base-directory (concat (org-pro-get-location pro) (car pro)))
	     (export-directory
	      (concat base-directory "/"
		      org-pro-export-subdirectory))
	     (public-directory
	      (or (org-pro-get-publish-directory pro)
		  (concat (file-name-as-directory org-pro-public-directory)
			  nickname))))
	;;(replace-regexp-in-string org-pro-public-directory (getenv "HOME") (expand-file-name export-directory))))
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
                       ,org-pro-export-base-extension
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
(provide 'org-pro-manager)
;;; org-pro-manager.el ends here
