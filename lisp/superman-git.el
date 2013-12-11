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
(setq superman-git-ignore "*")
(defvar superman-git-ignore "*" "Decides about which files to include and which to exclude.
See M-x manual-entry RET gitignore.
By default we set this to '*' which means that all files are ignored.
You think this sounds like a stupid idea? Hehe, we can still add files via
the -f (force) command line switch. And we get not bothered by
having to filter all the unpredictable names one can give to files
that never should get git controlled.")

;;}}}
;;{{{ superman run cmd

(defun superman-run-cmd (cmd buf &optional intro redo-buf)
  "Execute CMD with `shell-command-to-string' and display
result in buffer BUF. Optional INTRO is shown before the
result."
  (let ((intro (or intro "Superman returns:\n\n"))
	(msg (shell-command-to-string cmd))
	(cbuf (current-buffer)))
    (if redo-buf
	(with-current-buffer redo-buf
	  (superman-redo))
      (when superman-view-mode (superman-redo)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer (get-buffer-create buf))
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (put-text-property 0 1 'scroll-position 1 intro)
      (insert (concat "\n*** " (format-time-string "<%Y-%m-%d %a %H:%M>") " "
		      intro msg))
      (goto-char (previous-single-property-change (point) 'scroll-position))
      (let ((this-scroll-margin
	     (min (max 0 scroll-margin)
		  (truncate (/ (window-body-height) 4.0)))))
	(recenter this-scroll-margin)))))

;;}}}
;;
;;{{{ initialize project and directory

(defun superman-git-init ()
  (interactive)
  (or (get-text-property (point-min) 'git-dir)
    (let ((pro (superman-view-current-project)))
      (superman-git-init-directory (concat (superman-get-location pro) (car pro)))
      (superman-redo))))

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
;;{{{ push, pull, diff, branch and other global git actions 

(defun superman-git-p (dir)
  "Test if directory DIR is under git control."
  (when (and dir (file-exists-p dir))
    (string=
     "true\n"
     (shell-command-to-string
      (concat "cd " dir ";"
	      superman-cmd-git " rev-parse --is-inside-work-tree ")))))

(defun superman-git-action (action &optional dir)
  "Run a git command ACTION in directory DIR and display result."
  (let ((dir (or dir (if superman-view-mode
			 (get-text-property (point-min) 'git-dir)
		       (if superman-current-project
			   (superman-get-location superman-current-project)
			 (read-directory-name "Path to git repository: "))))))
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir "; " superman-cmd-git " " action "\n")
       "*Superman-returns*"
       (concat "git " action " '" dir "' returns:\n\n")))))

(defun superman-git-diff ()
  "Run \"git diff\" on current project."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-action "diff" dir))))

(defun superman-git-status ()
  "Run \"git status\" on current project."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-action "status" dir))))

(defun superman-git-diff (&optional dir)
  "Run git diff."
  (interactive)
  (superman-git-action dir "diff"))

(defun superman-git-push (&optional dir)
  "Pull to remote at DIR."
  (interactive)
  (superman-git-action dir "push"))

(defun superman-git-pull (&optional dir)
  "Pull from remote at DIR."
  (interactive)
  (superman-git-action dir "pull"))

(defun superman-git-toplevel (file)
  "Find the toplevel directory DIR is under git control."
  (let ((dir (if (file-directory-p file) file (file-name-directory file))))
    (if (superman-git-p dir)
	(replace-regexp-in-string
	 "\n" ""
	 (shell-command-to-string
	  (concat "cd " dir "; " superman-cmd-git " rev-parse --show-toplevel "))))))

(defun superman-git-branches (dir)
  (let* ((branch-list
	  (mapcar #'(lambda (x)
		      (replace-regexp-in-string
		       "^[ \t\n]+\\|[ \t\n]+$" "" x nil t))
		  (delete "" (split-string
			      (shell-command-to-string
			       (concat "cd " dir "; " superman-cmd-git " branch -a ")) "\n"))))
	 (current (if branch-list (car (member-if (lambda (x) (string-match "^\\*" x)) branch-list))
		    "master"))
	 (others (when branch-list (delete current branch-list))))
    (cons current others)))


(defun superman-git-merge-branches (dir)
  (let* ((branch-list (superman-git-branches dir))
	(current-branch (car branch-list))
	(other-branches (cdr branch-list))
	(m-branch (completing-read
		   (concat "Merge current branch (" current-branch ") with: ")
		   (mapcar* 'cons other-branches
			    (make-list (length other-branches) `())))))
    (superman-run-cmd
     (concat "cd " dir ";" superman-cmd-git " merge " m-branch  "\n")
     "*Superman-returns*"
     (concat "git merge returns:\n\n"))))


(defun superman-git-new-branch (&optional dir)
  "Create a new branch in git directory DIR. If DIR is nil
use the location of the current project, if no project is current prompt for project."
  (interactive)
  (let ((dir (or dir
		 (superman-project-home
		  (or superman-current-project (superman-select-project))))))
    (message
     (shell-command-to-string
      (concat "cd " dir "; " superman-cmd-git " branch " (read-string "Name of new branch: ") "\n")))
    (when superman-view-mode (superman-redo))))

(defun superman-git-checkout-branch (&optional dir branch)
  "Checkout branch in git directory DIR. If DIR is nil
use the location of the current project, if no project is current prompt for project."
  (interactive)
  (let* ((dir (or dir
		  (superman-project-home
		   (or superman-current-project (superman-select-project)))))
	 (branch (or branch
		     (let ((branches (superman-git-branches dir)))
		       (completing-read "Choose branch to checkout: "
					(mapcar* 'cons branches (make-list (length branches) `()))
					nil t)))))
    (superman-run-cmd 
     (concat "cd " dir "; " superman-cmd-git " checkout " branch "\n")
     "*Superman-returns*"
     (concat "Superman git checkout branch '" branch "' returns:\n\n"))
    (when superman-view-mode (superman-redo))))


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
;; 
;;{{{ get and set git status 

(defun superman-git-status-file ()
  "Show git status of the file at point"
  (interactive)
  (let* ((file (superman-filename-at-point))
	 (dir (file-name-directory file)))
  (superman-run-cmd
   (concat "cd " dir "; " superman-cmd-git " status "
	   (file-name-nondirectory file) "\n")
   "*Superman-returns*"
   (concat "git status '" file "' returns:\n\n"))))


(defun superman-git-set-status (pom file)
  (interactive)
  (let* ((status (superman-git-XY-status file))
	 (label (superman-label-status
		 (concat (nth 1 status) (nth 2 status)))))
    (org-entry-put pom (superman-property 'gitstatus) label)))

(defun superman-git-XY-status (file)
  "Finds the git status of file FILE as XY code, see M-x manual-entry RET git-status RET,
 and returns a list with 3 elements: the file-name relative to the path of the git-repository,
 the index-status (X) and the work-tree-status (Y)."
  (let* ((dir (file-name-directory file))
	 (raw-status (shell-command-to-string
		      (concat
		       "cd " dir ";"
		       superman-cmd-git
		       " status --ignored --porcelain "
		       (file-name-nondirectory file))))
	 (len  (length raw-status))
	 (index-status (if (> len 1)
			   (substring-no-properties raw-status 0 1)
			 ""))
	 (work-tree-status (if (> len 2)
			       (substring-no-properties raw-status 1 2)
			     ""))
	 (fname (if (> len 3)
		    (substring-no-properties raw-status 3 (length raw-status))
		  (file-name-nondirectory file))))
    (list (replace-regexp-in-string "\n" "" fname) index-status work-tree-status)))
  
(defun superman-label-status (XY)
  "Replace git status --  index-status (X) and the work-tree-status (Y) -- by a human readable label."
  (cond ((string= "" XY)
	 "Committed")
	((string= "M " XY)
	 "Modified (staged)")
	((string= " M" XY)
	 "Modified (unstaged)")
	((string= "MM" XY)
	 "Added to index, modified in Worktree")
	((string= "??" XY)
	 "Untracked")
	((string= " D" XY)
	 "Deleted")
	((string= " R" XY)
	 "Renamed")
	((string= " U" XY)
	 "Unmerged")
	((string= "A " XY)
	 "New in git-index")
	((string= "AM" XY)
	 "New in git-index, modified in Worktree")
	((string= "UU" XY)
	 "unmerged, both modified")
	((string= "DD" XY)
	 "unmerged, both deleted")
	((string= "AU" XY)
	 "unmerged, added by us")
	((string= "UD" XY)
	 "unmerged, deleted by them")
	((string= "UA" XY)
	 "unmerged, added by them")
	((string= "DU" XY)
	 "unmerged, deleted by us")
	((string= "AA" XY)
	 "unmerged, both added")
	(t "Unknown")))

;; (defun superman-git-get-commit (arg file &optional dir)
  ;; "Run git log and report the date and message of the n'th commit of
;; file FILE in directory DIR where n is determined by ARG."
  ;; (let* ((dir (cond (dir)
		    ;; ((file-name-absolute-p file) (file-name-directory file))
		    ;; (t (read-directory-name (concat "Find the git directory of file " file ": ")))))
	 ;; (file (superman-relative-name file dir))
	 ;; (date-string
	  ;; (shell-command-to-string
	   ;; (if (string= arg "first")
	       ;; (concat  "cd " dir ";" superman-cmd-git " log --date=local --pretty=format:\"%ad\" --reverse -- "
			;; file "  | head -1")
	     ;; (concat "cd " dir ";" superman-cmd-git " log --date=local -" arg " --pretty=format:\"%ad\" -- " file))))
         ;; (date (if (or (string= date-string "") (string-match "^fatal:" date-string)) "" (superman-read-git-date date-string)))
	 ;; (mess (shell-command-to-string
		;; (if (string= arg "first")
		    ;; (concat "cd " dir ";" superman-cmd-git " log --reverse --pretty=format:\"%s\" -- " file " | head -1")
		  ;; (concat "cd " dir ";"  superman-cmd-git " log -" arg " --pretty=format:\"%s\" -- " file)))))
    ;; (concat date " " (replace-regexp-in-string "\n+" "" mess))))

;;}}}
;;{{{ actions add/commit/delete on file-at-point

(defun superman-git-add-file ()
  "Add the file at point to the git repository."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (fbuf (get-file-buffer file)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " fbuf "?")))
      (with-current-buffer fbuf (save-buffer)))
    (superman-git-add (list file) dir nil nil)
    (superman-git-set-status (org-get-at-bol 'org-hd-marker) filename)
    (superman-view-redo-line)))

(defun superman-git-commit-file ()
  "Add and commit the file given by the filename property
of the item at point."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (fbuf (get-file-buffer file)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " fbuf "?")))
      (with-current-buffer fbuf (save-buffer)))
    (superman-git-add (list file) dir 'commit nil)
    (superman-git-set-status (org-get-at-bol 'org-hd-marker) filename)
    (superman-view-redo-line)))

(defun superman-git-reset-file ()
  "Reset (unstaged) changes via \"git checkout HEAD file\" of the file
given by the filename property of the item at point."
  (interactive)
  (let* ((filename (superman-filename-at-point))
	 (file (file-name-nondirectory filename))
	 (dir (if filename (expand-file-name (file-name-directory filename))))
	 (fbuf (get-file-buffer file)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " fbuf "?")))
      (with-current-buffer fbuf (save-buffer)))
    (save-excursion
      (superman-run-cmd (concat "cd " dir ";" superman-cmd-git " checkout HEAD " file)
			"*Superman-returns*"))
    (superman-view-redo-line)))

(defun superman-git-delete-file ()
  "Delete the file at point by calling `git rm'."
  (interactive)
  (superman-view-delete-entry 'dont 'dont)
  (superman-view-redo-line))

;;}}}
;;{{{ actions add/commit/delete on all marked files

(defun superman-check-if-saved-needed ()
  (member (expand-file-name (buffer-file-name)) (superman-view-marked-files)))

(defun superman-git-commit-marked (&optional commit dont-redo)
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (files
	  (mapcar 'expand-file-name 
		  (superman-view-marked-files))))
    ;; prevent committing unsaved buffers
    (save-some-buffers nil 'superman-check-if-saved-needed)
    (when dir
      (superman-git-add
       files
       dir
       'commit nil)
      (goto-char (previous-single-property-change (point) 'cat))
      (superman-redo-cat))))


(defun superman-git-add (file-list dir &optional commit message)
  "Add files in FILE-LIST to git repository at DIR. If DIR is nil,
prompt for project and use the associated git repository.
If FILE-LIST is nil then read file name below DIR.

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

;;}}}
;;{{{ git diff and annotate

(defun superman-git-diff-file (&optional arg)
  (interactive "p")
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (file (org-link-display-format (superman-get-property m "filename"))))
    (if (= arg 4)
	(progn (find-file file)
	       (vc-diff file "HEAD"))
      (superman-run-cmd (concat "cd " (file-name-directory file)
				";" superman-cmd-git " diff HEAD "
				file  "\n")
			"*Superman-returns*"
			"Superman returns the result of git diff HEAD:"
			nil))))

(defun superman-git-difftool-file ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (file (org-link-display-format (superman-get-property m "filename"))))
    (async-shell-command (concat
			  "cd " (file-name-directory file) "; "
			  superman-cmd-git " difftool "
			  (file-name-nondirectory file)))))

(defun superman-git-ediff-file ()
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
    (file (org-link-display-format (superman-get-property m "filename"))))
    (find-file file)
    (vc-ediff file "HEAD")))

(defun superman-annotate-version (&optional version)
  (interactive)
  (font-lock-mode -1)
  (save-excursion
    (let ((version (or version (buffer-substring (point-at-bol)
						 (progn (goto-char (point-at-bol))
							(forward-word)
							(point)))))
	  (buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward version nil t)
	(put-text-property (point-at-bol) (+ (point-at-bol) (length version))
			   'face 'font-lock-warning-face)
	(put-text-property
	 (progn (skip-chars-forward "^\\)")
		(+ (point) 1))
	 (point-at-eol)
	 'face 'font-lock-warning-face)))))


(defun superman-git-annotate (&optional arg)
  "Annotate file"
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (bufn)
	 (file (org-link-display-format (superman-get-property m "filename"))))
    (save-window-excursion
      (find-file file)
      (vc-annotate (org-link-display-format file) "HEAD")
      (setq bufn (buffer-name)))
    (switch-to-buffer bufn)))

;;}}}
;;
;;{{{ git cycle display

(defvar superman-git-display-cycles nil
  "Keywords to match the elements in superman-git-display-command-list")
(make-variable-buffer-local 'superman-git-display-cycles)
(setq superman-git-display-cycles nil)
(setq superman-git-default-displays '("log" "modified" "files" "untracked"))

(defvar superman-git-display-command-list
  '(("log"
     "log -n 5 --name-status --date=short --pretty=format:\"** %h\n:PROPERTIES:\n:Author: %an\n:Date: %cd\n:Message: %s\n:END:\n\""
     ((hdr ("width" 9) ("face" font-lock-function-name-face) ("name" "Version"))
      ("Author" ("width" 10) ("face" superman-get-git-status-face))
      ("Date" ("width" 13) ("fun" superman-trim-date) ("face" font-lock-string-face))
      ("Message" ("width" 63))))
    ("files"
     "ls-files --full-name"
     (("filename" ("width" 15) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      (hdr ("width" 44) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 39) ("face" superman-get-git-status-face)))
     superman-git-clean-git-ls-files+)
    ("untracked"
     "ls-files --full-name --exclude-standard --others"
     (("filename" ("width" 15) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      (hdr ("width" 44) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 39) ("face" superman-get-git-status-face)))
     superman-git-clean-git-ls-files)
    ("modified"
     "ls-files --full-name -m"
     (("filename" ("width" 15) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      (hdr ("width" 44) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 39) ("face" superman-get-git-status-face)))
     superman-git-clean-git-ls-files+)
    ;; ("date"
    ;; "ls-files | while read file; do git log -n 1 --pretty=\"** $file\n:PROPERTIES:\n:COMMIT: %h\n:DATE: %ad\n:END:\n\" -- $file; done"
    ;; ((hdr ("width" 12) ("face" font-lock-function-name-face) ("name" "Filename"))
    ;; ("DATE" ("fun" superman-trim-date))
    ;; ("COMMIT" ("width" 18))))
    )
  "List of git-views. Each entry has 4 elements: (key git-switches balls cleanup), where key is a string
to identify the element, git-switches are the switches passed to git, balls are used to define the columns and
cleanup is a function which is called before superman plays the balls.")

(defun superman-set-git-cycle (value)
  (org-with-point-at (get-text-property (point-at-bol) 'org-hd-marker)
    (org-set-property "git-display" value))
  (superman-redo-cat))

(defun superman-cycle-git-display ()
  "Cycles to the next value in `superman-git-display-cycles'.
This function should be bound to a key or button."
  (interactive)
  (let* ((pom (get-text-property (point-at-bol) 'org-hd-marker))
	 (cycles (split-string (or (superman-get-property pom "git-cycle")
				   superman-git-default-displays)
			       "[ \t]*,[ \t]*"))
	 (current (superman-get-property pom "git-display"))
	 (rest (member current cycles))
	 (next (if (> (length rest) 1) (cadr rest) (car cycles))))
    ;; (setq superman-git-display-cycles (append (cdr superman-git-display-cycles) (list (car superman-git-display-cycles))))
    (superman-set-git-cycle next)))

(defun superman-redo-git-display ()
  (interactive)
  (when superman-git-mode
    (goto-char (next-single-property-change (point-min) 'cat))
    (superman-redo-cat)))

(defun superman-display-git-cycle ()
  (interactive)
  (unless superman-git-mode
    (let ((repos-point (next-single-property-change (point-min) 'git-repos)))
      (if repos-point
	  (goto-char repos-point)
	(save-window-excursion
	  (find-file (get-text-property (point-min) 'index))
	  (goto-char (point-min))
	  (unless (re-search-forward ":git-cycle:" nil t)
	    (goto-char (point-max))
	    (insert "\n* Git repository\n:PROPERTIES:\n:git-cycle: "
		    (let ((sd (cdr superman-git-default-displays))
			  (dstring (car superman-git-default-displays)))
		      (while sd
			(setq dstring (concat dstring ", " (car sd))
			      sd (cdr sd)))
		      dstring)
		    "\n:git-display: modified\n:END:\n")
	    (save-buffer)))
	(superman-redo)
	(goto-char (next-single-property-change (point-min) 'git-repos))))
    ;; open buffer
    (let* ((pbuf (buffer-name))
	   (ibuf (concat (buffer-name) " :Git-repos"))
	   (git-dir (get-text-property (point-min) 'git-dir))
	   (gbuf (get-buffer ibuf))
	   git-cat)
      ;; (narrow-to-region
      (setq git-cat (buffer-substring
		     (or (previous-single-property-change (point) 'region-start)
			 (point-at-bol))
		     ;; need to add one, otherwise tail is not visible
		     (+ (next-single-property-change (point) 'tail) 1)))
      (if gbuf
	  (switch-to-buffer ibuf)
	;; (make-indirect-buffer (current-buffer) ibuf 'clone)
	(get-buffer-create ibuf)
	(switch-to-buffer ibuf))
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert git-cat)
	(goto-char (point-min))
	(insert (superman-make-button
		 "Back to project (q)"
		 'superman-view-back)
		"\n\n")
	(insert (superman-make-git-marked-keyboard) "\n\n")
	(put-text-property (point-min) (+ (point-min) 1) 'redo-cmd '(superman-redo-git-display))
	(put-text-property (point-min) (+ (point-min) (length "Back to project (q)")) 'region-start t)
	(put-text-property (point-min) (+ (point-min) (length "Back to project (q)")) 'project-buffer pbuf)
	(put-text-property (point-min) (+ (point-min) (length "Back to project (q)")) 'git-dir git-dir))
      (superman-view-mode-on)
      (superman-git-mode-on)
      (setq buffer-read-only t))))

(defun superman-view-back ()
  "Kill current buffer and return to project view."
  (interactive)
  (let ((pbuf (get-text-property (point-min) 'project-buffer)))
    (kill-buffer (current-buffer))
    (switch-to-buffer pbuf)
    (superman-redo)))

;;}}}
;;{{{ formatting git displays

(defun superman-get-git-status-face (str)
  (cond ((string-match "Committed" str ) 'font-lock-type-face)
	((string-match  "Modified" str) 'font-lock-warning-face)
	(t 'font-lock-comment-face)))

(defun superman-git-clean-git-ls-files ()
  (let* ((git-dir (get-text-property (point-min) 'git-dir)))
    (goto-char (point-min))
    (while (re-search-forward "^[^ \t\n]+" nil t)
      (let* ((ff (buffer-substring (point-at-bol) (point-at-eol)))
	     (dname (file-name-directory ff))
	     (fname (file-name-nondirectory ff))
	     (fullname (concat git-dir "/" ff))
	     (status "Untracked"))
	(replace-match
	 (concat "** "
		 fname
		 "\n:PROPERTIES:\n:GitStatus: " status
		 "\n:Directory: " (cond (dname) (t "."))  
		 "\n:FILENAME: [[" fullname "]]\n:END:\n\n") 'fixed)))))


(defun superman-git-clean-git-ls-files+ ()
  (let* ((git-dir (get-text-property (point-min) 'git-dir))
	 (git-status
	  (shell-command-to-string
	   (concat "cd " git-dir ";" superman-cmd-git " status --porcelain -uno")))
	 (status-list
	  (mapcar (lambda (x)
		    (let ((index-status (substring-no-properties x 0 1))
			  (work-tree-status (substring-no-properties x 1 2))
			  (fname  (substring-no-properties x 3 (length x))))
		      (list fname index-status work-tree-status)))
		  (delete-if (lambda (x) (string= x ""))
			     (split-string git-status "\n")))))
    (goto-char (point-min))
    (while (re-search-forward "^[^ \t\n]+" nil t)
      (let* ((ff (buffer-substring (point-at-bol) (point-at-eol)))
	     (dname (file-name-directory ff))
	     (fname (file-name-nondirectory ff))
	     (fullname (concat git-dir "/" ff))
	     (status (assoc ff status-list)))
	(replace-match
	 (concat "** "
		 fname
		 "\n:PROPERTIES:\n:GitStatus: "
		 (cond ((not status) "Committed")
		       (t
			(let* ((X (or (nth 1 status) " "))
			       (Y (or (nth 2 status) " "))
			       (XY (concat X Y)))
			  (superman-label-status XY))))
		 "\n:Directory: " (cond (dname) (t "."))  
		 "\n:FILENAME: [[" fullname "]]\n:END:\n\n")
	 'fixed)))))


(defun superman-format-git-display (view-buf dir props view-point index-buf index-cat-point name)
  (let* ((cycles (split-string (cadr (assoc "git-cycle" props)) "[ \t]*,[ \t]*"))
	 (cycle (or (cadr (assoc "git-display" props)) (car cycles)))
	 (limit (cadr (assoc "limit" props)))
	 (rest (assoc cycle superman-git-display-command-list))
	 (balls (or (nth 2 rest) superman-default-balls))
	 (clean-up (nth 3 rest))
	 (count 0)
	 (cmd (concat "cd " dir ";" superman-cmd-git " " (nth 1 rest))))
    ;; for the first time ... 
    (unless superman-git-display-cycles (setq superman-git-display-cycles cycles))
    ;; limit on number of revisions
    (when limit
      (replace-regexp-in-string "-n [0-9]+ " (concat "-n " limit " ") cmd))
    ;; insert the result of git command
    (insert (shell-command-to-string cmd))
    (goto-char (point-min))
    ;; clean-up if necessary
    (when clean-up (funcall clean-up))
    (goto-char (point-min))
    (while (outline-next-heading)
      (setq count (+ count 1))
      (setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
      (with-current-buffer view-buf (insert line "\n")))
    (set-buffer view-buf)
    (when superman-empty-line-before-cat (insert "\n"))
    (goto-char view-point)
    ;; section names
    (when (and
	   superman-empty-line-before-cat
	   (save-excursion (beginning-of-line 0)
			   (not (looking-at "^[ \t]*$"))))
      (insert "\n"))
    (put-text-property 0 (length name) 'git-repos dir name) 
    (superman-view-insert-section-name
     name count balls
     ;; FIXME: it must be possible to construct the marker based on buf and point
     (with-current-buffer index-buf
       (widen)
       (goto-char index-cat-point) (point-marker))
     'superman-cycle-git-display)
    (end-of-line 0)
    (let ((cycle-strings cycles))
      (while cycle-strings
	(let ((cstring (car cycle-strings)))
	  (set-text-properties 0 (length cstring) nil cstring)
	  (insert " >> ")
	  (insert (superman-make-button
		   cstring
		   `(lambda () (interactive) (superman-set-git-cycle ,cstring))
		   (if (string= cycle cstring)
		       'superman-next-project-button-face nil)
		   (concat "Cycle display to git " cstring)))
	  (setq  cycle-strings (cdr cycle-strings)))))
    (forward-line 1)
    ;; insert the column names
    (when superman-empty-line-after-cat (insert "\n"))
    (insert (superman-column-names balls))
    (goto-char (1- (or (next-single-property-change (point) 'cat) (point-max))))
    (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)))

;;}}}
;;{{{ superman-git-mode

(defvar superman-git-mode-map (make-sparse-keymap)
  "Keymap used for `superman-git-mode' commands.")
   
(define-minor-mode superman-git-mode
     "Toggle superman git mode.
With argument ARG turn superman-git-mode on if ARG is positive, otherwise
turn it off.
                   
Enabling superman-git mode enables the git keyboard to control single files."
     :lighter " *SG*"
     :group 'org
     :keymap 'superman-git-mode-map)

(defun superman-git-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-git-mode t))

(define-key superman-git-mode-map "q" 'superman-view-back)
(define-key superman-git-mode-map "c" 'superman-git-commit-file)
(define-key superman-git-mode-map "a" 'superman-git-add-file)
(define-key superman-git-mode-map "s" 'superman-git-status-file)
(define-key superman-git-mode-map "x" 'superman-git-delete-file)
(define-key superman-git-mode-map "d" 'superman-git-diff-file)
(define-key superman-git-mode-map "r" 'superman-git-reset-file)

;;}}}
;;{{{ superman-git-keyboard

(defface superman-git-keyboard-face-d
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :background "orange")))
  "Face used for git-diff."
  :group 'superman)
(defface superman-git-keyboard-face-a
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :background "yellow")))
  "Face used for git-add."
  :group 'superman)

(defface superman-git-keyboard-face-l
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :background "blue")))
  "Face used for git-log."
  :group 'superman)

(defface superman-git-keyboard-face-c
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :background "green")))
  "Face used for git-commit."
  :group 'superman)

(defface superman-git-keyboard-face-x
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :background "black")))
  "Face used for git-rm."
  :group 'superman)

(defface superman-git-keyboard-face-r
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :background "violet")))
  "Face used for git-stash."
  :group 'superman)

(defface superman-git-keyboard-face-s
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :background "red")))
  "Face used for git-stash."
  :group 'superman)

(defun superman-make-git-keyboard (f &rest args)
  (if (string-match org-bracket-link-regexp f)
      (let ((diff (superman-make-button "d"
					'superman-git-diff-file
					'superman-git-keyboard-face-d
					"git diff file"))
	    (log (superman-make-button "l"
				       'superman-git-log-at-point
				       'superman-git-keyboard-face-l
				       "git log file"))
	    (add (superman-make-button "a"
				       'superman-git-add-file
				       'superman-git-keyboard-face-a
				       "git add file"))
	    (commit (superman-make-button "c"
					  'superman-git-commit-file
					  'superman-git-keyboard-face-c
					  "git commit file"))
	    (reset (superman-make-button "r"
					 'superman-git-reset-file
					 'superman-git-keyboard-face-r
					 "git checkout (reset) file"))
	    (delete (superman-make-button "x"
					  'superman-git-delete-file
					  'superman-git-keyboard-face-x
					  "git rm file")))
	(concat diff " " log  " " add  " " delete " " reset " " commit " " " " " "))
    ;; for the column name
    (superman-trim-string f (car args))))

(defun superman-make-git-marked-keyboard ()
  (let ((diff (superman-make-button "Diff project"
				    'superman-git-diff
				    'superman-git-keyboard-face-d
				    "git diff"))
	(commit (superman-make-button "Commit marked"
				      'superman-git-commit-marked
				      'superman-git-keyboard-face-c
				      "git commit"))
	(status (superman-make-button "Status (project)"
				      'superman-git-status-project
				      'superman-git-keyboard-face-s
				      "git commit"))
	(delete (superman-make-button "Delete marked"
				      'superman-view-delete-marked
				      'superman-git-keyboard-face-x
				      "Delete marked files")))
    (concat diff  " " delete " " status  " " commit " ")))

;;}}}
;;
;;{{{ git search and history

(defun superman-git-search (&optional arg)
  (interactive "p")
  (superman-git-search-at-point (or arg superman-git-search-limit)))


(defun superman-git-history (&optional arg)
  (interactive)
  (let* ((m (org-get-at-bol 'org-hd-marker))
	 (file
	  (or (if m (org-link-display-format (superman-get-property m "filename")))
	      (get-text-property (point-min) 'git-dir)
	      (buffer-file-name)))
	 (dir (if m (file-name-directory file) (file-name-as-directory file)))
	 (curdir default-directory)
	 (bufn (concat "*history: " file "*"))
	 )
    (when dir
      (save-window-excursion
	(setq default-directory dir)
	(vc-git-print-log file bufn t nil (or arg superman-git-log-limit))
	)
      (setq default-directory curdir)
      (switch-to-buffer bufn)
      (vc-git-log-view-mode))))


;;}}}
;;{{{ git log-view

(defvar superman-git-log-mode-map (copy-keymap superman-view-mode-map)
  "Keymap used for `superman-git-log-mode' commands.")

(define-key superman-git-log-mode-map [return] 'superman-git-revision-at-point)
(define-key superman-git-log-mode-map "D" (lambda () (interactive) (superman-git-revision-at-point 1)))
(define-key superman-git-log-mode-map "t" 'superman-git-tag)
(define-key superman-git-log-mode-map "?" 'superman-git-show-help)
(define-key superman-git-log-mode-map "q" 'kill-this-buffer)
(define-key superman-git-log-mode-map "r" (lambda () (interactive) (org-agenda-redo) (superman-git-log-mode-on)))
(define-key superman-git-log-mode-map "!" 'superman-start-shell)
(define-key superman-git-log-mode-map [(down)] 'next-line)
(define-key superman-git-log-mode-map [(up)] 'previous-line)
(define-key superman-git-log-mode-map " " (lambda () (interactive) (funcall superman-help-fun (superman-git-comment-at-point))))

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


(defun superman-git-setup-log-buffer (file dir git-switches decoration-only arglist)
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
    (setq log-strings (split-string (substring gitlog 0 -1) "\n"))
    (while log-strings
      (let* ((log (split-string (car log-strings) ":#:"))
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
	(if (or (not decoration-only) (not (string= deco "")))
	    (progn
	      (put-text-property 0 (length item) 'decoration (nth 4 log) item)
	      (insert item "\n")))
      (setq log-strings (cdr log-strings))))
  (superman-git-log-mode-on)
  (goto-char (point-min))
  (toggle-truncate-lines 1)
  (superman-next-entry)
  (setq buffer-read-only t)))

(setq superman-log-balls
      '(("Date" ("width" 10) ("face" font-lock-string-face))
	("Hash" ("width" 10) ("face" font-lock-comment-face))
	("Author" ("width" 20) ("face" font-lock-function-name-face))
	("Tag" ("width" 10) ("face" font-lock-comment-face))
	("Comment" ("fun" superman-dont-trim) ("face" font-lock-keyword-face))))

(defun superman-git-log (file gitpath limit &optional search-string decoration-only)
  (let* ((file (or file (superman-filename-at-point)
		   (superman-get-property nil (superman-property 'filename) t)))
	 (gitsearch (if search-string (concat " -G\"" search-string "\"") ""))
	 (gitpath (or gitpath (or (superman-property-at-point (superman-property 'gitpath) t)
				  (superman-git-toplevel file))))
	 (gitcmd (concat " --no-pager log --pretty=\"%h:#:%s:#:%ad:#:%an:#:%d\" --date=short "
			 gitsearch  " "
			 (if limit (concat "-n " (int-to-string limit))))))
    (superman-git-setup-log-buffer file gitpath gitcmd decoration-only (list limit search-string decoration-only) )))


(defun superman-git-log-at-point (&optional arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file nil limit nil nil)))


(defun superman-git-log-decoration-only-at-point (arg)
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
  (let* ((hash (org-get-at-bol 'org-hd-marker))
	 (path (get-text-property (point-min) 'dir))
	 ;;(path (superman-get-property pom (superman-property 'gitpath) t))
	 ;;(hash (superman-get-property pom (superman-property 'hash) nil))))
	 )
    (shell-command-to-string (concat "cd " path ";" superman-cmd-git " log -1 " hash))))

(defun superman-git-tag ()
  "Set git tag"
  (interactive)
  (let* ((oldtag (get-text-property (point-at-bol) 'decoration))
         (path (get-text-property (point-min) 'dir))
	 (hash (get-text-property (point-at-bol) 'org-hd-marker))
	 (file (get-text-property (point-min) 'filename))
	 (arglist (get-text-property (point-min) 'arglist)) ;; limit search decoration-only
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
    (goto-char (point-min))
    (forward-line (1- linenum))))

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
	 (ext (file-name-extension file))
	 (filehash (concat (file-name-sans-extension (file-name-nondirectory file)) "_" hash (if ext (concat "." ext))))
	 (str (shell-command-to-string 
	       (concat "cd " path ";" superman-cmd-git " show " hash ":" file))))
    (if diff (find-file fileabs))
    (switch-to-buffer-other-window filehash) ;;set-buffer find-file-noselect fileabs
    (setq buffer-file-name filehash)
    (normal-mode) ;; Get default major-mode 
    (erase-buffer)  
    (insert str)
    (setq buffer-file-name nil)
    (goto-char (point-min))
    (if diff (ediff-buffers (file-name-nondirectory file) filehash))))

;;}}}
;;{{{ git grep

(defun superman-git-grep (&optional arg)
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (if arg
	(vc-git-grep (read-string "Grep: "))
	(vc-git-grep (read-string "Grep: ") "*" dir)))))
;;}}}

(provide 'superman-git)
;;; superman-git.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End: 
