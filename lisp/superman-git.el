;;; superman-git.el --- Summary of project contents and adding information to projects

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

(defvar superman-cmd-git "git")
(defvar superman-use-git t "Whether to use git to backup projects. Set to nil to completely disable git.")
(setq superman-git-ignore "*")
(defvar superman-git-ignore "*" "Decides about which files to include and which to exclude.
See M-x manual-entry RET gitignore.
By default we set this to '*' which means that all files are ignored.
You think this sounds like a stupid idea? Hehe, we can still add files via
the -f (force) command line switch. And we get not bothered by
having to filter all the unpredictable names one can give to files
that never should get git controlled.")

;;}}}
;;{{{ property set functions

;; (setq supermanperty-set-functions-alist nil)  
;; (add-to-list 'supermanperty-set-functions-alist
	     ;; `("GitStatus" . superman-git-get-status-at-point))
;; (add-to-list 'supermanperty-set-functions-alist
	     ;; `("LastCommit" . superman-git-add-at-point))

;;}}}
;;{{{ helper functions

(defun superman-git-p (dir)
  "Test if directory DIR is under git control."
  (string-match
   "^true"
   (shell-command-to-string
    (concat "cd " dir ";"
	    superman-cmd-git " rev-parse --is-inside-work-tree "))))

(defun superman-git-toplevel (file)
  "Find the toplevel directory DIR is under git control."
  (let ((dir (if (file-directory-p file) file (file-name-directory file))))
    (if (superman-git-p dir)
	(replace-regexp-in-string
	 "\n" ""
	 (shell-command-to-string
	  (concat "cd " dir "; git rev-parse --show-toplevel "))))))

(defun superman-relative-name (file dir)
  "If filename FILE is absolute return the relative filename w.r.t. dir,
Else return FILE as it is."
  (if (file-name-absolute-p file)
      (file-relative-name (expand-file-name file) (expand-file-name dir))
    file))

(defun superman-read-git-date (git-date-string &optional no-time)
  "Transform git date to org-format"
  (with-temp-buffer
    (org-insert-time-stamp 
     (date-to-time git-date-string) (not no-time))))
;;      (set-time-zone-rule t) ;; Use Universal time.
;;      (prog1 (format-time-string "%Y-%m-%d %T UTC" time)
;;        (set-time-zone-rule nil))))

;;}}}
;;{{{ initialize project and directory

(defun superman-git-init-project (&optional pro)
  "Put project under git control."
  (interactive)
  (let* ((pro (or pro (superman-select-project)))
	 (index (superman-get-index pro))
	 (loc (concat (superman-get-location pro) (car pro))))
    (if (not index)
	(error (concat "Trying to superman-git-init-project: Project " (car pro) " has no index file."))
      (superman-git-init-directory loc)
      (if (string-match loc index)
	  (superman-git-add
	   (list index) loc
	   'commit (concat "Initial commit of project " (car pro)))))))

(defun superman-git-init-directory (dir)
  "Put directory DIR under git control."
  (if (superman-git-p dir)
      (message (concat "Directory " dir " is under git control."))
    (shell-command-to-string (concat "cd " dir ";" superman-cmd-git " init"))
    (append-to-file superman-git-ignore nil (concat (file-name-as-directory dir) ".gitignore"))))

;;}}}
;;{{{ property and filename at point

(defun superman-property-at-point (prop noerror)
  (interactive)
  (let* ((pom (cond
	       ;; ((or (superman-view-mode) (superman-mode) (superman-git-log-mode))
	       ((org-get-at-bol 'org-hd-marker))
	       ((point))
	       (t (error "Don't know where to look for property."))))
	 ;; ((eq major-mode 'org-mode) (point))
	 ;; ((eq major-mode 'org-agenda-mode) (org-get-at-bol 'org-hd-marker))
	 ;; (t (error (concat  "This function works only in org-mode, org-agenda-mode, superman-git-log-mode, superman-view-mode." (buffer-name (current-buffer)))))))
	 (propval
	  (superman-get-property pom prop t)))
    propval))

(defun superman-filename-at-point (&optional noerror)
  "If property FileName exists at point return its value."
  (let* ((file-or-link
	  (superman-property-at-point
	   (superman-property 'filename) noerror)))
    (if (not (stringp file-or-link))
	(unless noerror
	  (error "No proper(ty) FileName at point."))
      (org-link-display-format file-or-link))))

;;}}}
;;{{{ get status and commit from git

(defun superman-git-get-commit (arg file &optional dir)
  "Run git log and report the date and message of the n'th commit of
file FILE in directory DIR where n is determined by ARG."
  (let* ((dir (cond (dir)
		    ((file-name-absolute-p file) (file-name-directory file))
		    (t (read-directory-name (concat "Find the git directory of file " file ": ")))))
	 (file (superman-relative-name file dir))
	 (date-string
	  (shell-command-to-string
	   (if (string= arg "first")
	       (concat  "cd " dir ";" superman-cmd-git " log --date=local --pretty=format:\"%ad\" --reverse -- "
			file "  | head -1")
	     (concat "cd " dir ";" superman-cmd-git " log --date=local -" arg " --pretty=format:\"%ad\" -- " file))))
         (date (if (string= date-string "") "" (superman-read-git-date date-string)))
	 (mess (shell-command-to-string
		(if (string= arg "first")
		    (concat "cd " dir ";" superman-cmd-git " log --reverse --pretty=format:\"%s\" -- " file " | head -1")
		  (concat "cd " dir ";"  superman-cmd-git " log -" arg " --pretty=format:\"%s\" -- " file)))))
    (concat date " " (replace-regexp-in-string "\n+" "" mess))))

(defun superman-git-get-status-at-point (&optional check)
  "Report the status of the document-file at point.
Point is either (point), ie. if the current buffer is in org-mode,
or the hdmarker associated with the item at point when the current
buffer is in org-agenda-mode."
  (interactive)
  (superman-git-get-status (superman-filename-at-point) check))

(defun superman-git-get-status (file check)
  "Determine the git status of file FILE."
  (let* ((file (or file (read-file-name "Get git status for file: ")))
	 (dir (if file (file-name-directory file)))
	 (file-rel (superman-relative-name file dir))
	 git-status
	 git-last-commit
	 label)
    (if (not (file-exists-p file))
	(list "N" "Not exists" "")
      (setq git-status
	    (shell-command-to-string
	     (concat "cd " dir ";"
		     superman-cmd-git
		     " status --ignored --porcelain "
		     file-rel)))
      (if (and check (not (superman-git-p dir)))
	  (error (concat "Directory " dir " is not git controlled. You may want to start\ngit control of the project via M-x `superman-git-init-project'."))
	(if (string= git-status "")
	    (if (file-exists-p (concat dir "/" file-rel))
		(setq git-status "C")
	      (setq git-status "E"))
	  (if (string-match "^fatal" git-status)
	      (setq git-status "")
	    (setq git-status (substring git-status 0 1))))
	(if (string= git-status "!") (setq git-status "?"))
	(if (or  (string= git-status "") (string= git-status "E") (string= git-status "?"))
	    (setq git-last-commit "")
	  (setq git-last-commit
		(superman-git-get-commit "1" file-rel dir)))
	(setq label (superman-status-label git-status))
	(list git-status label git-last-commit)))))

(defun superman-status-label (status)
  (cond ((string= status "?")
	 "Untracked")
	((string= status "E")
	 "NA")
	((string= status "M")
	 "Modified")
	((string= status "A")
	 "New file")
	((string= status " ")
	 "Modified")
	((string= status "C")
	 "Committed")
	(t "Unknown")))

;;}}}
;;{{{ set and update status

(defun superman-git-set-status-at-point (&optional check)
  (interactive)
  (let ((file (superman-filename-at-point)))
    (superman-git-set-status (point) file check)))

(defun superman-git-set-status (pom file check)
  (interactive)
  (let* ((statlist (superman-git-get-status file check))
	 (last-commit (nth 2 statlist))
	 (git-status (nth 0 statlist))
	 (git-label (nth 1 statlist)))
    ;; (supermanperty-changed-functions '(lambda (prop val) (save-buffer))))
    (org-entry-put pom (superman-property 'gitstatus) git-label)
    ;; (org-set-property "GitStatus" git-label)
    (unless (or (string= git-status "A") (string= git-status "E") (string= git-status "?"))
      (unless (superman-get-property pom "GitInit")
	(org-entry-put pom "GitInit" (superman-git-get-commit "first" file)))
      (unless (string= last-commit "")
	(org-entry-put pom "LastCommit" last-commit)))
    statlist))

(defun superman-update-git-status ()
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (while (re-search-forward (concat ":" (superman-property 'filename) ":") nil t)
      (superman-git-set-status-at-point nil))))

;;}}}
;;{{{ git add/commit 

(defun superman-git-commit (&optional dir query)
  (let* ((dir (or dir
		  (let ((pro (assoc (superman-property-at-point "Project" nil) superman-project-alist)))
		    (concat (superman-get-location pro) (car pro)))))
	 (message (read-string query))
	 (cmd (concat (concat "cd " dir ";" superman-cmd-git " commit -m \"" message "\""))))
    (shell-command-to-string cmd)))

(defun superman-git-add (file-list dir &optional commit message)
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
		  (let ((pro (superman-select-project)))
		    (concat (superman-get-location pro) (car pro)))))
	 (file-list (or file-list
			(list (read-file-name "Git add file: " dir nil t))))
	 (file-list-string
	  (apply 'concat
		 (mapcar `(lambda (f) (concat (superman-relative-name f ,dir) " ")) file-list)))
	 (cmd (concat "cd " dir ";" superman-cmd-git " add -f " file-list-string))
	 (message (if commit (or message (read-string (concat "Commit message for " file-list-string ": "))))))
    (if message (setq cmd (concat cmd  ";" superman-cmd-git " commit -m \"" message "\" " file-list-string)))
    (shell-command-to-string cmd)))

(defun superman-git-add-at-point (&optional commit message check)
  "Add or update file FILE to git repository DIR."
  (interactive)
  (let* ((file (superman-property-at-point (superman-property 'filename) nil))
	 (pro (assoc (superman-property-at-point (superman-property 'project)) superman-project-alist)))
    (superman-git-add (list (org-link-display-format file)) pro commit message)
    (superman-git-set-status-at-point check)))

;;}}}
;;{{{ updating and pushing projects

(defun superman-git-push (&optional project)
  (interactive)
  (let* ((pro (or project (superman-view-current-project) (superman-select-project)))
	 (loc (concat (superman-get-location pro) (car pro)))
	 (cmd (concat "cd " loc ";" superman-cmd-git " push")))
    (superman-goto-shell)
    (insert cmd)))

;;}}}
;;{{{ log-view

(defvar superman-git-log-mode-map (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-git-log-mode' commands.")

(define-key superman-git-log-mode-map [return] 'superman-git-revision-at-point)
(define-key superman-git-log-mode-map "D" (lambda () (interactive) (superman-git-revision-at-point 1)))
(define-key superman-git-log-mode-map "t" 'superman-git-tag)
(define-key superman-git-log-mode-map "?" 'superman-git-show-help)
(define-key superman-git-log-mode-map "q" 'kill-this-buffer)
(define-key superman-git-log-mode-map "r" (lambda () (interactive) (org-agenda-redo) (superman-git-log-mode-on)))
(define-key superman-git-log-mode-map "!" 'superman-start-shell)
(define-key superman-git-log-mode-map " " (lambda () (interactive) (funcall superman-help-fun (superman-git-comment-at-point))))

(defun superman-git-show-help ()
  (interactive)
  (let ((msg
	(concat 
	 "------------------\n"
	"[return]:\t\t Open revision at point\n"
	"[l]:     \t\t Show git log ([L] tags only. Prefix-arg: limit)\n"
	"[S]:    \t\t Search for revision introducing change (Prefix-arg: limit)\n"
	"[v]:    \t\t View annotated file\n"
	"[g]:    \t\t Grep in git controlled files (Prefix-arg: fine-tune)\n"
	"[d]:    \t\t Show difference between revisions ([D] ediff)\n"
	"[space]:\t\t Show full commit message\n"
	"[t]:    \t\t Alter tag (empty string to remove)\n"
	"[!]:     \t\t Shell\n"
	 "------------------\n")))
    (funcall superman-help-fun msg)))


(define-minor-mode superman-git-log-mode 
  "Toggle org projectmanager document view mode.
                        With argument ARG turn superman-docview-mode on if ARG is positive, otherwise
                        turn it off.
                        
                        Enabling superman-view mode electrifies the column view for documents
                        for git and other actions like commit, history search and pretty log-view."
  :lighter " S-log"
  :group 'org
  :keymap 'superman-git-log-mode-map)

(defvar superman-git-log-limit 50)
(defvar superman-git-search-limit 250)


(defun superman-git-log-mode-on ()
  (interactive)
  (hl-line-mode 1)
  (superman-git-log-mode t))

(defun superman-git-log-format (hdr level category tags-list prop-list)
  (concat " " 
	  (let* ((cprops prop-list)
		 (pstring "")
		 (ntrim))
	    (while cprops
	      (let ((val (cdr (car cprops))))
		(cond ((string= (downcase (caar cprops)) (downcase (superman-property 'decoration)))
		       (setq ntrim 22)
		       (if (string= val "not set") (setq val " ")))		      
		      ((string= (downcase (caar cprops)) (downcase (superman-property 'date)))
		       (setq ntrim 10))
		      (t (setq ntrim 7)))
		;; (cond ((string= (downcase (caar cprops)) (down-case)"filename")
		;;        (setq val (file-name-nondirectory (org-link-display-format val)))))
		(setq pstring (concat pstring "  " (superman-trim-string val ntrim))))
	      (setq cprops (cdr cprops)))
	    pstring) "    " (superman-trim-string hdr 70)))


(defun superman-git-setup-log-buffer (file dir git-switches decorationonly arglist)
  (let* ((file (superman-relative-name file dir))
	 (dir dir)
	 (cmd (concat
		   "cd " dir "; " superman-cmd-git git-switches " -- " file))
	 (gitlog (shell-command-to-string cmd))
	 (log-buf  (concat "*Log[" (file-name-nondirectory file) "]*"))
	 log-strings)
    (when (string= gitlog "")
      (error (concat "No search results in file history or file " file " not (not yet) git controlled.")))
    (switch-to-buffer log-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (font-lock-mode -1)
    (insert "* Git Log of " file)
    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
    (put-text-property (point-at-bol) (point-at-eol) 'filename file)
    (put-text-property (point-at-bol) (point-at-eol) 'dir dir)
    ;;    (put-text-property (point-at-bol) (point-at-eol) 'command cmd)
    (put-text-property (point-at-bol) (point-at-eol) 'arglist arglist)
    (insert "\n\n")
    ;; column names
    (insert (superman-column-names superman-log-balls) "\n")
    (setq logstrings (split-string (substring gitlog 0 -1) "\n"))
    (while logstrings
      (let* ((log (split-string (car logstrings) ":#:"))
	     (deco (nth 4 log))
	     (item (superman-format-thing
		    (list (nth 0 log)
			  (list 
			   (cons "Comment" (nth 1 log))
			   (cons "Hash" (nth 0 log))
			   (cons "marker" (nth 0 log))
			   (cons "Tag" deco)
			   (cons "Date" (nth 2 log))
			   (cons "Author"  (nth 3 log))))
		    superman-log-balls)))
	(if (or (not decorationonly) (not (string= deco "")))
	    (progn
	      (put-text-property 0 (length item) 'decoration (nth 4 log) item)
	      (insert item "\n")))
      (setq logstrings (cdr logstrings))))
  (superman-git-log-mode-on)
  (goto-char (point-min))
  (toggle-truncate-lines 1)
  (superman-next-entry)
  (setq buffer-read-only t)))

(setq superman-log-balls
      '(("Date" ("trim" superman-trim-date (12))
	 ("face" font-lock-string-face))
	("Author" ("trim" superman-trim-string (13))
	 ("face" font-lock-function-name-face))
	("Hash"
	 ("trim" superman-trim-string (8))
	 ("face" font-lock-comment-face))
	("Tag"
	 ("trim" superman-trim-string (12))
	 ("face" font-lock-comment-face))
	("Comment"
	 ("trim" superman-trim-string (50))
	 ("face" font-lock-keyword-face))))

(defun superman-git-log (file gitpath limit &optional search-string decorationonly)
  (let* ((file (or file (superman-filename-at-point)
		   (superman-get-property nil (superman-property 'filename) t)))
	 (gitsearch (if search-string (concat " -G\"" search-string "\"") ""))
	 (gitpath (or gitpath (or (superman-property-at-point (superman-property 'gitpath) t)
				  (superman-git-toplevel file))))
	 (gitcmd (concat " --no-pager log --pretty=\"%h:#:%s:#:%ad:#:%an:#:%d\" --date=short "
			 gitsearch  " "
			 (if limit (concat "-n " (int-to-string limit))))))
    (superman-git-setup-log-buffer file gitpath gitcmd decorationonly (list limit search-string decorationonly) )))

(defun superman-git-log-at-point (&optional arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file nil limit nil nil)))

(defun superman-git-log-decorationonly-at-point (arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file nil limit nil t)))

(defun superman-git-search-at-point (arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file nil limit (read-string "Search string: ")) nil))

(defun superman-git-comment-at-point ()
  (interactive)
  (let* ((pom (org-get-at-bol 'org-hd-marker))
	 (path (superman-get-property pom (superman-property 'gitpath) t))
	 (hash (superman-get-property pom (superman-property 'hash) nil)))
    (shell-command-to-string (concat "cd " path ";" superman-cmd-git " log -1 " hash))))

(defun superman-git-tag ()
  "Set git tag"
  (interactive)
  (let* (
         (oldtag (get-text-property (point-at-bol) 'decoration))
         (path (get-text-property (point-min) 'dir))
	 (hash (get-text-property (point-at-bol) 'org-hd-marker))
	 (file (get-text-property (point-min) 'filename))
	 (arglist (get-text-property (point-min) 'arglist)) ;; limit search decorationonly
	 (tag (read-string "Tag (empty to clear): "))
	 (linenum (line-number-at-pos)))
    (if (string-equal tag "")
	(progn 
	  (if oldtag
	      (progn
		(setq oldtag (replace-regexp-in-string "\)" "" (replace-regexp-in-string "\(" "" oldtag)))
		(shell-command-to-string (concat "cd " path ";" superman-cmd-git " tag -d " oldtag)))))
      (shell-command-to-string (concat "cd " path ";" superman-cmd-git " tag -a " tag " " hash " -m \"\"")))
  (superman-git-log file path (nth 0 arglist) (nth 1 arglist) (nth 2 arglist))
  (goto-line linenum)))

(defun superman-git-revision-at-point (&optional diff)
  "Shows version of the document at point "
  (interactive)
  (superman-git-revision (org-get-at-bol 'org-hd-marker) diff))

(defun superman-git-revision (pom &optional diff)
  "Shows version of the document at point "
  (let* ((file (get-text-property (point-min) 'filename))
         (path (get-text-property (point-min) 'dir))
	 (hash (get-text-property (point-at-bol) 'org-hd-marker))
	 (fileabs (concat path file))
	 (filehash (concat hash "_" file))
	 (str (shell-command-to-string 
	       (concat "cd " path ";" superman-cmd-git " show " hash ":" file))))
    (if diff (find-file fileabs))
    (switch-to-buffer-other-window filehash) ;;set-buffer find-file-noselect fileabs
    (erase-buffer)  
    (insert str)
    (normal-mode) ;; Get default major-mode 
    (if diff (ediff-buffers (file-name-nondirectory file) filehash))))

;;}}}


(provide 'superman-git)
;;; superman-git.el ends here
