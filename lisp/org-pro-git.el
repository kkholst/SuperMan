;;; org-pro-git.el --- Summary of project contents and adding information to projects

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

;;; Code:


;;{{{ variables

(defvar org-pro-cmd-git "git")
(defvar org-pro-use-git t "Whether to use git to backup projects. Set to nil to completely disable git.")
(setq org-pro-git-ignore "*")
(defvar org-pro-git-ignore "*" "What files to include or not include. See M-x manual-entry RET gitignore.
                                                        
                                                     By default we set this to '*' which means that all files are ignored.
                                                        
                                                     You think this sounds like a stupid idea? Hehe, we can still add files via
                                                     the -f (force) command line switch. And we get not bothered by
                                                     having to filter all the unpredictable names one can give to files
                                                     that never should get git controlled.")

;;}}}
;;{{{ property set functions
;; (setq org-property-set-functions-alist nil)  
;; (add-to-list 'org-property-set-functions-alist
	     ;; `("GitStatus" . org-pro-git-get-status-at-point))
;; (add-to-list 'org-property-set-functions-alist
	     ;; `("LastCommit" . org-pro-git-add-at-point))
;;}}}
;;{{{ helper functions

(defun org-pro-git-p (dir)
  "Test if directory DIR is under git control."
  (eq 0 (shell-command (concat "cd " dir ";" org-pro-cmd-git " rev-parse --is-inside-work-tree "))))

(defun org-pro-git-toplevel (file)
  "Find the toplevel directory DIR is under git control."
  (let ((dir (if (file-directory-p file) file (file-name-directory file))))
    (if (org-pro-git-p dir)
	(replace-regexp-in-string "\n" "" (shell-command-to-string (concat "cd " dir "; git rev-parse --show-toplevel "))))))

(defun org-pro-relative-name (file dir)
  "If filename FILE is absolute return the relative filename w.r.t. dir,
Else return FILE as it is."
  (if (file-name-absolute-p file)
      (file-relative-name (expand-file-name file) (expand-file-name dir))
    file))

(defun org-pro-read-git-date (git-date-string &optional no-time)
  "Transform git date to org-format"
  (with-temp-buffer
    (org-insert-time-stamp 
     (date-to-time git-date-string) (not no-time))))
;;      (set-time-zone-rule t) ;; Use Universal time.
;;      (prog1 (format-time-string "%Y-%m-%d %T UTC" time)
;;        (set-time-zone-rule nil))))

;;}}}
;;{{{ initialize project and directory

(defun org-pro-git-init-project (&optional pro)
  "Put project under git control."
  (interactive)
  (let* ((pro (or pro (org-pro-select-project)))
	 (index (org-pro-get-index pro))
	 (loc (concat (org-pro-get-location pro) (car pro))))
    (if (not index)
	(error (concat "Trying to org-pro-git-init-project: Project " (car pro) " has no index file."))
      (org-pro-git-init-directory loc)
      (if (string-match loc index)
	  (org-pro-git-add
	   index loc
	   'commit (concat "Initial commit of project " (car pro)))))))

(defun org-pro-git-init-directory (dir)
  "Put directory DIR under git control."
  (if (org-pro-git-p dir)
      (message (concat "Directory " dir " is under git control."))
    (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " init"))
    (append-to-file org-pro-git-ignore nil (concat (file-name-as-directory dir) ".gitignore"))))

;;}}}
;;{{{ property and filename at point

(defun org-pro-property-at-point (prop)
  (interactive)
  (let* ((pom (cond ((eq major-mode 'org-mode) (point))
		    ((eq major-mode 'org-agenda-mode) (org-get-at-bol 'org-hd-marker))
		    (t (error "This function works only in org-mode, org-agenda-mode, org-pro-git-log-mode, org-pro-view-mode."))))
	 (propval  (org-pro-get-property pom prop t)))
    propval))

(defun org-pro-filename-at-point ()
  "If property FileName exists at point return its value."
  (let* ((file-or-link (org-pro-property-at-point "FileName")))
    (if (not (stringp file-or-link))
	(error "No proper(ty) FileName at point."))
    (org-link-display-format file-or-link)))

;;}}}
;;{{{ get status and commit from git

(defun org-pro-git-get-commit (arg file &optional dir)
  "Run git log and report the date and message of the n'th commit of
file FILE in directory DIR where n is determined by ARG."
  (let* ((dir (cond (dir)
		    ((file-name-absolute-p file) (file-name-directory file))
		    (t (read-directory-name (concat "Find the git directory of file " file ": ")))))
	 (file (org-pro-relative-name file dir))
	 (date-string
	  (shell-command-to-string
	   (if (string= arg "first")
	       (concat  "cd " dir ";" org-pro-cmd-git " log --date=local --pretty=format:\"%ad\" --reverse -- "
			file "  | head -1")
	     (concat "cd " dir ";" org-pro-cmd-git " log --date=local -" arg " --pretty=format:\"%ad\" -- " file))))
         (date (if (string= date-string "") "" (org-pro-read-git-date date-string)))
	 (mess (shell-command-to-string
		(if (string= arg "first")
		    (concat "cd " dir ";" org-pro-cmd-git " log --reverse --pretty=format:\"%s\" -- " file " | head -1")
		  (concat org-pro-cmd-git " log -" arg " --pretty=format:\"%s\" -- " file)))))
    (concat date " " mess)))

(defun org-pro-git-get-status-at-point ()
  "Report the status of the document-file at point.
Point is either (point), ie. if the current buffer is in org-mode,
or the hdmarker associated with the item at point when the current
buffer is in org-agenda-mode."
  (interactive)
  (org-pro-git-get-status (org-pro-filename-at-point)))

(defun org-pro-git-get-status (file)
  "Determine the git status of file FILE"
  (let* ((file (or file (read-file-name "Get git status for file: ")))
	 (dir (if file (file-name-directory file)))
	 (file (org-pro-relative-name file dir))
	 (git-status (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " status --ignored --porcelain " file)))
	 git-last-commit
	 label)
    (if (not (org-pro-git-p dir))
	(error (concat "Directory " dir " is not git controlled. You may want to start\ngit control of the project via M-x `org-pro-git-init-project'."))
      (if (string= git-status "")
	  (if (file-exists-p (concat dir "/" file))
	      (setq git-status "C")
	    (setq git-status "E"))
	(if (string-match "^fatal" git-status)
	    (setq git-status "")
	  (setq git-status (substring git-status 0 1))))
      (if (string= git-status "!") (setq git-status "?"))
      (if (or  (string= git-status "") (string= git-status "E") (string= git-status "?"))
	  (setq git-last-commit "")
	(setq git-last-commit  (org-pro-git-get-commit "1" file dir)))
      (cond ((string= git-status "?")
	     (setq label "Untracked"))
	    ((string= git-status "E")
	     (setq label "File does not exist"))
	    ((string= git-status "M")
	     (setq label "Modified and staged"))
	    ((string= git-status "A")
	     (setq label "New file"))
	    ((string= git-status " ")
	     (setq label "Modified but unstaged"))
	    ((string= git-status "C")
	     (setq label "Committed"))
	    (t (setq label "Unknown")))
      (list git-status label git-last-commit))))

;;}}}
;;{{{ set and update status

(defun org-pro-git-set-status-at-point ()
  (interactive)
  (let ((file (org-pro-filename-at-point)))
    (org-pro-git-set-status (point) file)))

(defun org-pro-git-set-status (pom file)
  (interactive)
  (let* ((statlist (org-pro-git-get-status file))
	 (last-commit (nth 2 statlist))
	 (git-status (nth 0 statlist))
	 (git-label (nth 1 statlist))
	 (org-property-changed-functions '(lambda (prop val) (save-buffer))))
    (org-entry-put pom "GitStatus" git-label)
    ;; (org-set-property "GitStatus" git-label)
    (unless (or (string= git-status "A") (string= git-status "E") (string= git-status "?"))
      (unless (org-pro-get-property pom "GitInit")
	(org-entry-put pom "GitInit" (org-pro-git-get-commit "first" file)))
      (unless (string= last-commit "")
	(org-entry-put pom "LastCommit" last-commit)))
    statlist))


(defun org-pro-update-git-status ()
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (while (re-search-forward ":FileName:" nil t)
      (org-pro-git-set-status-at-point))))

;;}}}
;;{{{ git add/commit 

(defun org-pro-git-commit (&optional dir query)
  (let* ((dir (or dir
		  (let ((pro (assoc (org-pro-property-at-point "Project") org-pro-project-alist)))
		    (concat (org-pro-get-location pro) (car pro)))))
	 (message (read-string query))
	 (cmd (concat (concat "cd " dir ";" org-pro-cmd-git " commit -m \"" message "\""))))
    (shell-command-to-string cmd)))

(defun org-pro-git-add (file dir &optional commit message)
  "Add file FILE to git repository at DIR. If DIR is nil,
prompt for project and use the associated git repository.
If FILE is nil then read file name below DIR.

If COMMIT is non-nil prompt for commit message and
commit the file to the git repository.

The attempt is made to `git add' the file at the location
of the project. This fails if location is not a git repository,
or if the file is not inside the location."
  (interactive)
  (let* ((dir (or dir
		  (let ((pro (org-pro-select-project)))
		    (concat (org-pro-get-location pro) (car pro)))))
	 (file (or file (read-file-name "Git add file: " dir nil t)))
	 (file (org-pro-relative-name file dir))
	 (cmd (concat "cd " dir ";" org-pro-cmd-git " add -f " file))
	 (message (if commit (or message (read-string (concat "Commit message for " (file-name-nondirectory file) ": "))))))
    (if message (setq cmd (concat cmd  ";" org-pro-cmd-git " commit -m \"" message "\" " file)))
    (shell-command-to-string cmd)))

(defun org-pro-git-add-at-point (&optional commit message)
  "Add or update file FILE to git repository DIR."
  (interactive)
  (let* ((file (org-pro-property-at-point "FileName"))
	 (pro (assoc (org-pro-property-at-point "Project") org-pro-project-alist)))
    (org-pro-git-add (org-link-display-format file) pro commit message)
    (org-pro-git-set-status-at-point)))

;;}}}
;;{{{ updating and pushing projects
;; (defun org-pro-git-push-directory (dir silent)
  ;; "Git push directory DIR."
  ;; (let* ((status (shell-command-to-string  (concat "cd " dir ";" org-pro-cmd-git " status")))
	 ;; (necessary (string-match "Your branch is ahead .*\n" status))
	 ;; (doit (or silent (y-or-n-p (concat "Your branch is ahead ... push git at " dir "? ")))))
    ;; (if doit
	;; (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " push")))))

(defun org-pro-git-update-project (project before)
  "Check if project needs to be put under git control and update.
If BEFORE is set then either initialize or pull. Otherwise, add, commit and/or push."
  (let* ((git-control (downcase (org-pro-get-git project))))
    (unless (or (string= git-control "") (string-match "no\\|never\\|nil" git-control))
      (let ((silent-p (string= git-control "silent"))
	    (dir (org-pro-get-git-location project)))
	(when (file-exists-p dir)
	  (if before
	      (progn
		;; activating project
		(unless (or (org-pro-git-p dir) (string-match "no" git-control) (string= "" git-control))
		  (when (or silent-p
			    (y-or-n-p (concat "Initialize git control at " dir "?")))
		    (org-pro-git-init-directory dir)))
		(when (and (string-match "pull" git-control)
			   (or silent-p (y-or-n-p (concat "Run this command: \"git pull\" at " dir "? "))))
		  (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " pull"))))
	    ;; deactivating project
	    (when (and (org-pro-git-p dir)
		       (string-match "yes\\|silent" git-control)))))))))

;;}}}
;;{{{ log-view

(defvar org-pro-git-log-mode-map (copy-keymap org-pro-view-mode-map)
  "Keymap used for `org-pro-git-log-mode' commands.")

(define-key org-pro-git-log-mode-map [return] 'org-pro-git-revision-at-point)
(define-key org-pro-git-log-mode-map "D" (lambda () (interactive) (org-pro-git-revision-at-point 1)))
(define-key org-pro-git-log-mode-map "t" 'org-pro-git-tag-at-point)
(define-key org-pro-git-log-mode-map "b" 'org-pro-view-git-blame)



(define-minor-mode org-pro-git-log-mode 
  "Toggle org projectmanager document view mode.
                        With argument ARG turn org-pro-docview-mode on if ARG is positive, otherwise
                        turn it off.
                        
                        Enabling org-pro-view mode electrifies the column view for documents
                        for git and other actions like commit, history search and pretty log-view."
  :lighter " git-log"
  :group 'org
  :keymap 'org-pro-git-log-mode-map)

(defvar org-pro-git-log-limit 25)
(defvar org-pro-git-search-limit 500)


(defun org-pro-git-log-mode-on ()
  (interactive)
  (org-pro-git-log-mode t))

(defun org-pro-git-setup-log-buffer (file dir git-switches decorationonly)
  (let* ((file (org-pro-relative-name file dir))
	 (dir dir)
	 (gitlog (shell-command-to-string
		  (concat
		   "cd " dir "; " org-pro-cmd-git git-switches " -- " file)))
	 (logbuf (concat " #" (file-name-nondirectory file) ".org"))
	 (logfile (concat "/tmp/" logbuf)) ;; Use (make-temp-file name-of-application) instead?!
	 item val)
    (if (string= gitlog "")
	(error (concat "No search results in file history or file " file " not (not yet) git controlled."))
      (pop-to-buffer logbuf)
      (erase-buffer)
      (insert (concat "* Git Log ("
                      file
                      ")\n:PROPERTIES:\n:COLUMNS: %40ITEM(Comment) %Date %15Author %15Decoration %8Hash \n:FileName: "
                      file
                      "\n:GitPath: "
                      dir
                      "\n:END:\n"))
      (loop for x in (split-string (substring gitlog 0 -1) "\n")
            do 
            (setq val (delete "" (split-string x ":#:")))
            (setq item (concat "*** " (nth 1 val) "\n:PROPERTIES:\n:Hash: " (car val) "\n:Date: " (nth 2 val) "\n:Author: " (nth 3 val) (when (nth 4 val) (concat "\n:Decoration: " (nth 4 val))) "\n:END:\n"))
            (if (or (not decorationonly) (nth 4 val)) (insert item)))
      ;; FIXME: rather change org-tags-view-plus to accept buffers instead of files
      (write-file logfile nil)
      ;;(delete-buffer logfile)
      (goto-char (point-min))
      (let ((lprops
	     `((org-agenda-files (quote (,logfile)))
	       (org-agenda-finalize-hook 'org-pro-git-log-mode-on)
	       (org-agenda-overriding-header (concat "Git-log of " ,file "\th: help, C:commit, l: log, H:history\n\n"))
	       ;;	       (org-agenda-buffer-name (concat "*org-pro-log-mode[" ,logfile "]*"))
	       (org-agenda-overriding-agenda-format
		'(lambda (hdr level category tags-list properties)
		   (concat  "| " hdr
			   (let ((cprops properties)
				 (pstring ""))
			     (while cprops
			       (setq pstring (concat pstring " | " (cdr (car cprops))))
			       (setq cprops (cdr cprops)))
			     pstring) " |")))
	       (org-agenda-view-columns-initially nil))))
	(put 'org-agenda-redo-command 'org-lprops lprops)
	(org-let lprops '(org-pro-tags-view-plus nil "Hash={.+}" '("Hash" "Date" "Author" "Decoration")))))))


(defun org-pro-git-log (file gitpath limit &optional search-string decorationonly)
  (let* ((file (or file (org-pro-filename-at-point)
		   (org-pro-get-property nil "FileName" t)))
	 (gitsearch (if search-string (concat " -G\"" search-string "\"") ""))
	 (gitpath (or gitpath (or (org-pro-property-at-point "GitPath")
				  (org-pro-git-toplevel file))))
	 (gitcmd (concat " --no-pager log --pretty=\"%h:#:%s:#:%ad:#:%an:#:%d\"--date short "
			 gitsearch  " "
			 (if limit (concat "-n " (int-to-string limit))))))
    (org-pro-git-setup-log-buffer file gitpath gitcmd decorationonly)))

(defun org-pro-git-log-at-point (arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) org-pro-git-log-limit (or arg org-pro-git-log-limit)))
	 (file (org-pro-filename-at-point)))
    (org-pro-git-log file nil limit nil nil)))

(defun org-pro-git-log-decorationonly-at-point (arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) org-pro-git-log-limit (or arg org-pro-git-log-limit)))
	 (file (org-pro-filename-at-point)))
    (org-pro-git-log file nil limit nil t)))

(defun org-pro-git-search-at-point (arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) org-pro-git-log-limit (or arg org-pro-git-log-limit)))
	 (file (org-pro-filename-at-point)))
    (org-pro-git-log file nil limit (read-string "Search string: ")) nil))

(defun org-pro-git-tag-at-point ()
  "Shows git tag "
  (interactive)
  (org-pro-git-tag (org-get-at-bol 'org-hd-marker)))

(defun org-pro-git-tag (pom)
  "Set git tag"
  (interactive)
  (let* ((hash (org-pro-get-property pom "Hash" nil))
	 (oldtag (org-pro-get-property pom "Decoration" nil))
	 (path (org-pro-get-property pom "GitPath" t))
	 (tag (read-string "Tag (empty to clear): ")))
    (if (string-equal tag "")
	(progn 
	  (setq oldtag (replace-regexp-in-string "\)" "" (replace-regexp-in-string "\(" "" oldtag)))
	  (shell-command-to-string (concat "cd " path ";" org-pro-cmd-git " tag -d " oldtag)))
      (shell-command-to-string (concat "cd " path ";" org-pro-cmd-git " tag -a " tag " " Hash " -m \"\"")))) 
;;  (save-excursion
;;    (goto-char (point-min))
;;    (org-pro-view-git-log))
)

(defun org-pro-git-revision-at-point (&optional diff)
  "Shows version of the document at point "
  (interactive)
  (org-pro-git-revision (org-get-at-bol 'org-hd-marker) diff))

(defun org-pro-git-revision (pom &optional diff)
  "Shows version of the document at point "
  (let* ((file (org-pro-get-property pom "FileName" t))
	 (hash (org-pro-get-property pom "Hash" nil))
	 (path (org-pro-get-property pom "GitPath" t))
	 (fileabs (concat path file))
	 (filehash (concat hash "_" file))
	 (str (shell-command-to-string 
	       (concat "cd " path ";" org-pro-cmd-git " show " Hash ":" file))))
    (if diff (find-file fileabs))
    (switch-to-buffer-other-window filehash) ;;set-buffer find-file-noselect fileabs
    (erase-buffer)  
    (insert str)
    (normal-mode) ;; Get default major-mode 
    (if diff (ediff-buffers (file-name-nondirectory file) filehash))))

;;}}}


(provide 'org-pro-git)
;;; org-pro-git.el ends here
