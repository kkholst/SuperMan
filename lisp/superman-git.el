;;; superman-git.el --- Summary of project contents and adding information to projects

;; Copyright (C) 2012-2013  Klaus Kähler Holst, Thomas Alexander Gerds

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
(defvar superman-git-ignore "" "Decides about which files to include and which to exclude.
See M-x manual-entry RET gitignore.
By default we set this to '' which means that no files are ignored.")

;;}}}
;;{{{ superman run cmd

(defun superman-run-cmd (cmd buf &optional intro redo-buf pre-hook post-hook)
  "Execute CMD with `shell-command-to-string' and display
result in buffer BUF. Optional INTRO is shown before the
result. PRE-HOOK and POST-HOOK are functions that are called before and after CMD, respectively."
  (let ((intro (or intro "Superman returns:\n\n"))
	(msg (shell-command-to-string cmd))
	(cur-point (point)))
    (if redo-buf
	(with-current-buffer redo-buf
	  (superman-redo)))
    ;; (when superman-view-mode (superman-redo)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer (get-buffer-create buf))
    (unless (eq major-mode 'diff-mode)
      (diff-mode))
    ;;(unless (assoc 'orgstruct-mode minor-mode-alist)
    (orgstruct-mode t)
    (font-lock-mode 1)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (when pre-hook (funcall pre-hook))
      (goto-char (point-max))
      (put-text-property 0 1 'scroll-position 1 intro)
      (insert (concat "\n*** " (format-time-string "<%Y-%m-%d %a %H:%M>") " "
		      intro msg))
      (goto-char (previous-single-property-change (point) 'scroll-position))
      (let ((this-scroll-margin
	     (min (max 0 scroll-margin)
		  (truncate (/ (window-body-height) 4.0)))))
	(recenter this-scroll-margin))
      (when post-hook (funcall post-hook)))
    (other-window 1)
    (goto-char cur-point)))

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

(defun superman-git-action (action &optional dir buf)
  "Run a git command ACTION in directory DIR and display result. Optional argument BUF is
passed to `superman-run-cmd'."
  (let ((dir (or dir (if superman-view-mode
			 (get-text-property (point-min) 'git-dir)
		       (if superman-current-project
			   (superman-get-location superman-current-project)
			 (read-directory-name "Path to git repository: "))))))
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir "; " superman-cmd-git " " action "\n")
       (or buf "*Superman-returns*")
       (concat "git " action " '" dir "' returns:\n\n")))))


(defun superman-git-status ()
  "Run \"git status\" on current project."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-action "status" dir))))

(defun superman-git-commit-project ()
  "Run \"git commit\" on current project."
  (interactive)
  (superman-git-status)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (message
	  (read-string 
	   (concat "Commit message"  ": ")))
	 (action (concat " commit -m \"" message "\" ")))
    (when dir
      (superman-git-action action dir)
      (superman-redo))))

(defun superman-git-diff (&optional dir hash ref config)
  (interactive)
  (let* ((ref (or ref (if hash (concat hash "^") "HEAD")))
	 (hash (or hash ""))
	 (dir (or dir (get-text-property (point-min) 'git-dir)))
	 (cmd (concat "cd " dir
		      ";" superman-cmd-git " diff "
		      (unless (string= hash "") (concat hash "^ " hash " "))
		      (when superman-git-diff-context-lines (concat "-U" superman-git-diff-context-lines " "))
		      "\n"))
	 (msg (concat
	       "diff "
	       (if hash (concat ref " " hash " ") "HEAD")"\n")))
    (superman-run-cmd cmd "*Superman:Git-diff*" msg nil 'erase-buffer 'superman-prepare-git-diff-buffer)
    (when config (superman-switch-config nil 0 config))))

(defun superman-prepare-git-diff-buffer ()
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (while (re-search-forward "\\-\\-\\-" nil t)
      (let ((file (progn (looking-at ".*$")
			 (match-string-no-properties 0))))
	(beginning-of-line)
	(insert "*** " file "\n")
	(forward-line 2)))
    (goto-char (point-min))))

(defun superman-git-push (&optional dir)
  "Pull to remote at DIR."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (when dir
      (superman-git-action "push"  dir))))

(defun superman-git-pull (&optional dir)
  "Pull from remote at DIR."
  (interactive)
  (let ((dir (get-text-property (point-min) 'git-dir)))
    (save-some-buffers nil)
    (when dir
      (superman-git-action "pull" dir))))

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
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir ";" superman-cmd-git " merge " m-branch  "\n")
       "*Superman-returns*"
       (concat "git merge returns:\n\n")))))


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
    (save-excursion
      (superman-run-cmd 
       (concat "cd " dir "; " superman-cmd-git " checkout " branch "\n")
       "*Superman-returns*"
       (concat "Superman git checkout branch '" branch "' returns:\n\n")))
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
    (save-excursion
      (superman-run-cmd
       (concat "cd " dir "; " superman-cmd-git " status "
	       (file-name-nondirectory file) "\n")
       "*Superman-returns*"
       (concat "git status '" file "' returns:\n\n")))))

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
		       "\"" (file-name-nondirectory file) "\"")))
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
;;{{{ actions log/search/add/commit/delete on file-at-point

(defun superman-git-log-decoration-file (&optional arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file limit nil t)))

(defun superman-git-search-log-of-file (&optional arg)
  (interactive "p")
  (let* ((limit (if (= arg 1) superman-git-log-limit (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file limit (read-string "Search string: ")) nil))

(defun superman-git-log-file (&optional arg)
  (interactive "p")
  (let* ((limit (if (= arg 1)
		    superman-git-log-limit
		  (or arg superman-git-log-limit)))
	 (file (superman-filename-at-point)))
    (superman-git-log file limit nil nil)))

(defun superman-git-last-log-file (&optional arg)
  "Retrieves last commit message(s) of file"
  (interactive "p")
  (let*
      ((filename (superman-filename-at-point))
       (file (file-name-nondirectory filename))
       (dir (if filename (expand-file-name (file-name-directory filename))))
       (n (or arg 1))
       (cmd (concat superman-cmd-git " log -n" (number-to-string n) " -- " filename)))
    (superman-run-cmd (concat "cd " dir ";" cmd) 
		      "*Superman-returns*" (concat "log -- " file "\n"))))

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
	 (fbuf (get-file-buffer filename)))
    (when (and fbuf
	       (with-current-buffer fbuf (buffer-modified-p))
	       (y-or-n-p (concat "Save buffer " (buffer-name fbuf) "?")))
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
    (when (y-or-n-p (concat "Really reset file " file "? "))
      (save-excursion
	(superman-run-cmd (concat "cd " dir ";" superman-cmd-git " checkout HEAD -- " file)
			  "*Superman-returns*"))
      (superman-view-redo-line))))


(defun superman-git-ignore-file ()
  "Add the file at point to .gitignore."
  (interactive)
  (let* 
      ((dir (get-text-property (point-min) 'git-dir))
       (filename (expand-file-name (superman-filename-at-point)))
       (file (replace-regexp-in-string (concat dir "/") "" filename)))
    (append-to-file (concat file "\n") nil (concat dir "/.gitignore")))
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (kill-whole-line)))

(defun superman-git-delete-file ()
  "Delete the file at point by calling `git rm'."
  (interactive)
  (when (superman-view-delete-entry 'dont 'dont)
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (kill-line))))

;;}}}
;;{{{ actions add/commit/delete on all marked files

(defun superman-check-if-saved-needed ()
  (member (expand-file-name (buffer-file-name))
	  (superman-view-marked-files)))

(defun superman-git-add-marked ()
  "Call git add on the list of marked files."
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (files
	  (mapcar 'expand-file-name 
		  (superman-view-marked-files))))
    (if (not files)
	(message "Apparently, no files are marked.")
      ;; prevent committing unsaved buffers
      ;; (save-some-buffers nil 'superman-check-if-saved-needed)
      (save-some-buffers nil)
      (when dir
	(superman-git-add files dir nil nil)
	;; move point inside cat to the first marked entry
	;; FIXME: it would be safer to have a property 'marked
	(goto-char (next-single-property-change (point) 'type)) ;; org-marked-entry-overlay
	(goto-char (previous-single-property-change (point) 'cat))
	(superman-redo-cat)))))

(defun superman-git-commit-marked (&optional commit)
  "Call git commit on the list of marked files."
  (interactive)
  (let* ((dir (get-text-property (point-min) 'git-dir))
	 (files
	  (mapcar 'expand-file-name 
		  (superman-view-marked-files))))
    (if (not files)
	(message "Apparently, no files are marked.")
      ;; prevent committing unsaved buffers
      ;; (save-some-buffers nil 'superman-check-if-saved-needed)
      (save-some-buffers nil)
      (when dir (superman-git-add files dir 'commit nil)
	    ;; move point inside cat to the first marked entry
	    ;; FIXME: it would be safer to have a property 'marked
	    (goto-char (next-single-property-change (point) 'type)) ;; org-marked-entry-overlay
	    (goto-char (previous-single-property-change (point) 'cat))
	    (superman-redo-cat)))))

(defun superman-git-add (file-list dir &optional commit message)
  "Call git add on the files given by FILE-LIST. If DIR is nil,
prompt for project and use the associated git repository.
If FILE-LIST is nil then read file name below DIR.

If COMMIT is non-nil prompt for commit message and
commit the file to the git repository."
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
    (superman-run-cmd cmd "*Superman-returns*" cmd)))

;;}}}
;;{{{ git diff and annotate

(defvar superman-git-diff-context-lines nil
  "Number of context lines for diff. Passed to -U flag of git diff,
see M-x manual-entry RET git-diff RET.")

(defun superman-git-diff-file (&optional hash ref config)
  (interactive)
  (let* ((file (superman-filename-at-point))
	 (hash (or hash (get-text-property (point-at-bol) 'hash)))
	 (ref (if hash (or ref (concat hash "^")) "HEAD"))
	 (cmd (concat "cd " (file-name-directory file)
		      ";" superman-cmd-git " diff "
		      ref " " hash " "
		      (when superman-git-diff-context-lines (concat "-U" superman-git-diff-context-lines " "))
		      "./" (file-name-nondirectory file) "\n"))
	 (msg (concat
	       "diff "
	       (if hash (concat ref " " hash " ") "HEAD")
	       (file-relative-name file (superman-git-toplevel file)) "\n")))
    (superman-run-cmd cmd "*Superman:Git-diff*" msg nil 'erase-buffer 'superman-prepare-git-diff-buffer)
    (when config (superman-switch-config nil 0 config))))


(defun superman-annotate-version (&optional version)
  (interactive)
  (font-lock-mode -1)
  (save-excursion
    (let ((version
	   (or version
	       (buffer-substring
		(point-at-bol)
		(progn (goto-char (point-at-bol))
		       (forward-word)
		       (point)))))
	  (buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward version nil t)
	(put-text-property (point-at-bol) (+ (point-at-bol) (length version))
			   'face 'superman-warning-face)
	(put-text-property
	 (progn (skip-chars-forward "^\\)")
		(+ (point) 1))
	 (point-at-eol)
	 'face 'superman-warning-face)))))


(defun superman-git-annotate (&optional arg)
  "Annotate file"
  (interactive)
  (let ((file (superman-filename-at-point)) (bufn))
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

(defun superman-trim-hash (hash &rest args)
 (superman-make-button
  (superman-trim-string hash (car args))
  'superman-choose-entry nil "Run git diff"))

(defvar superman-git-display-command-list
  '(("log"
     "log -n 18 --name-status --date=short --pretty=format:\"** %h\n:PROPERTIES:\n:Commit: %h\n:Author: %an\n:Date: %cd\n:Message: %s\n:END:\n\""
     ((hdr ("width" 9) ("face" font-lock-function-name-face) ("name" "Commit") ("fun" superman-trim-hash))
      ("Author" ("width" 10) ("face" superman-get-git-status-face))
      ("Date" ("width" 13) ("fun" superman-trim-date) ("face" font-lock-string-face))
      ("Message" ("width" 63)))
     superman-git-log-pre-display-hook
     superman-git-log-post-display-hook)
    ("files"
     "ls-files --full-name"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face)))
     superman-git-files-pre-display-hook)
    ("untracked"
     "ls-files --full-name --exclude-standard --others"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face)))
     superman-git-untracked-pre-display-hook)
    ("modified"
     "ls-files --full-name -m"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face)))
     superman-git-files-pre-display-hook)
    ("diff"
     "diff --name-status"
     (("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
      ("GitStatus" ("width" 20) ("face" superman-get-git-status-face))
      (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
      ("Directory" ("width" 25) ("face" superman-subheader-face)))
     superman-git-diff-pre-display-hook))
  "List of git-views. Each entry has 4 elements: (key git-switches balls cleanup), where key is a string
to identify the element, git-switches are the switches passed to git, balls are used to define the columns and
cleanup is a function which is called before superman plays the balls.")

(defvar superman-git-display-diff-balls
  '(
    ;; ("filename" ("width" 14) ("fun" superman-make-git-keyboard) ("name" "git-keyboard") ("face" "no-face"))
    ("GitStatus" ("width" 20) ("face" superman-get-git-status-face) ("name" "What happened"))
    (hdr ("width" 34) ("face" font-lock-function-name-face) ("name" "Filename"))
    ("Directory" ("width" 25) ("face" superman-subheader-face)))
  "Balls to format git diff views.")

(defun superman-set-git-cycle (value)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'git-display value)))
;; (org-with-point-at (get-text-property (point-at-bol) 'org-hd-marker)
;; (org-set-property "git-display" value))

(defun superman-cycle-git-display ()
  "Cycles to the next value in `superman-git-display-cycles'.
This function should be bound to a key or button."
  (interactive)
  (let* ((pom (get-text-property (point-at-bol) 'org-hd-marker))
	 (cycles (split-string
		  (or (and pom (superman-get-property pom "git-cycle"))
		      superman-git-default-displays)
		  "[ \t]*,[ \t]*"))
	 (current (or (get-text-property (point-min) 'git-display)
		      (car cycles)))
	 (rest (member current cycles))
	 (next (if (> (length rest) 1) (cadr rest) (car cycles))))
    ;; (setq superman-git-display-cycles (append (cdr superman-git-display-cycles) (list (car superman-git-display-cycles))))
    (superman-set-git-cycle next)
    (superman-redo-cat)))

(defun superman-redo-git-display ()
  (interactive)
  (when superman-git-mode
    (goto-char (next-single-property-change (point-min) 'cat))
    (superman-redo-cat)))

(defun superman-display-git-cycle ()
  (interactive)
  (unless (or superman-git-mode (not (get-text-property (point-min) 'git-dir)))
    (let* ((index (get-text-property (point-min) 'index))
	   ;; (repos-point (next-single-property-change (point-min) 'git-repos))
	   index-marker
	   ;; props
	   (tempp (bufferp index)))
      ;; (if repos-point
      ;; (goto-char repos-point)
      (save-window-excursion
	(if tempp (switch-to-buffer index)
	  (find-file (get-text-property (point-min) 'index)))
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
		  "\n:hidden: superman-git-mode"
		  "\n:git-display: log\n:END:\n")
	  (unless tempp (save-buffer)))
	;; now in index buffer at git repos heading
	(outline-back-to-heading)
	;; (setq props (superman-parse-props
	;; (get-text-property (point-at-bol) 'org-hd-marker)
	;; 'p 'h))
	(setq index-marker (point-marker)))
      ;; open git view buffer
      (let* ((pbuf (buffer-name))
	     (nickname (get-text-property (point-min) 'nickname))
	     (ibuf (concat (buffer-name) " :Git-repository"))
	     (git-dir (get-text-property (point-min) 'git-dir))
	     (gbuf (get-buffer ibuf)))
	(if gbuf (switch-to-buffer ibuf)
	  (get-buffer-create ibuf)
	  (switch-to-buffer ibuf))
	(let ((buffer-read-only nil))
	  (if (get-text-property (point-min) 'git-display)
	      nil ;; buffer already showing git-display
	    (erase-buffer) ;; probably unnecessary
	    (goto-char (point-min))
	    (insert (superman-make-button
		     (concat "* Git: " nickname)
		     'superman-redo 'superman-project-button-face)
		    "\n\n")
	    (insert (superman-view-control nickname git-dir))
	    (superman-view-insert-git-branches git-dir)
	    (superman-view-insert-action-buttons
	     '(("Diff project" superman-git-diff 'superman-git-keyboard-face-D "Git diff")
	       ("Commit project" superman-git-commit-project 'superman-git-keyboard-face-P "Git all project")
	       ("Commit marked" superman-git-commit-marked 'superman-git-keyboard-face-C "Git commit marked files")
	       ("Status" superman-git-status 'superman-git-keyboard-face-S "Git status")
	       ("Delete marked" superman-view-delete-marked 'superman-git-keyboard-face-X "Delete marked files")))
	    (insert "\n\n")
	    (put-text-property (point-min) (1+ (point-min)) 'redo-cmd '(superman-redo-git-display))
	    (put-text-property (point-min) (1+ (point-min)) 'region-start t)
	    (put-text-property (point-min) (1+ (point-min)) 'project-buffer pbuf)
	    (put-text-property (point-min) (1+ (point-min)) 'nickname nickname)
	    (put-text-property (point-min) (1+ (point-min)) 'git-dir git-dir)
	    (put-text-property (point-min) (1+ (point-min)) 'git-display "log")
	    (superman-view-mode-on)
	    (superman-git-mode-on)
	    (goto-char (point-max))
	    (insert "** Git")
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'cat 'git)
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'org-hd-marker index-marker)
	    ;; (superman-redo-cat props)
	    (superman-redo-cat)
	    (setq buffer-read-only t)))))))

(defun superman-format-git-display (view-buf dir props view-point index-buf index-cat-point name)
  "Called by `superman-format-cat' to format git displays."
  (let* ((cycles (split-string (cadr (assoc "git-cycle" props)) "[ \t]*,[ \t]*"))
	 (cycle (or
		 (with-current-buffer view-buf
		   (get-text-property (point-min) 'git-display))
		 (cadr (assoc "git-display" props))
		 (car cycles)))
	 (limit (cadr (assoc "limit" props)))
	 (rest (assoc cycle superman-git-display-command-list))
	 (balls (or (nth 2 rest) superman-default-balls))
	 (pre-hook (nth 3 rest))
	 (post-hook (nth 4 rest))
	 (count 0)
	 (cmd (concat "cd " dir ";" superman-cmd-git " " (nth 1 rest))))
    ;; for the first time ...
    (with-current-buffer view-buf
    (unless (get-text-property (point-min) 'git-display)
      (put-text-property (point-min) (1+ (point-min)) 'git-display cycle)))
    (unless superman-git-display-cycles (setq superman-git-display-cycles cycles))
    ;; limit on number of revisions
    (when limit
      (replace-regexp-in-string "-n [0-9]+ " (concat "-n " limit " ") cmd))
    ;; insert the result of git command
    (insert (shell-command-to-string cmd))
    (goto-char (point-min))
    ;; prepare buffer if necessary
    (when pre-hook (funcall pre-hook))
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
		   `(lambda () (interactive)
		      (superman-set-git-cycle ,cstring)
		      (superman-redo-cat))
		   (if (string= cycle cstring)
		       'superman-next-project-button-face nil)
		   (concat "Cycle display to git " cstring)))
	  (setq  cycle-strings (cdr cycle-strings)))))
    (forward-line 1)
    ;; insert the column names
    (when superman-empty-line-after-cat (insert "\n"))
    (insert (superman-column-names balls))
    (goto-char (1- (or (next-single-property-change (point) 'cat) (point-max))))
    (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)
    (goto-char (previous-single-property-change (point) 'cat))
    (beginning-of-line)
    (put-text-property (point) (+ (point) 1) 'git-cycle cycle)
    (when post-hook 
      (goto-char (point-max))
      (funcall post-hook))))


(defun superman-git-display-diff (commit ref dir project)
  "Display differences between the versions COMMIT and REF of the git
repository of PROJECT which is located at DIR."
  (let* ((balls superman-git-display-diff-balls)
	 (count 0)
	 (name "Git-diff")
	 (nickname (get-text-property (point-min) 'nickname))
	 (git-dir (get-text-property (point-min) 'git-dir))
	 next-item
	 (ref (cond ((string= commit "Workspace") "HEAD") (t ref)))
	 (commit (cond ((string= commit "Workspace") "") (t commit)))
	 (cmd (concat "cd " dir ";" superman-cmd-git " diff " commit " " ref " --name-status"))
	 (log-buf (current-buffer))
	 (commit-string (superman-make-button
			 (if (string= commit "") "Workspace (now)"
			   (concat commit
				   " (" (superman-get-property
					 (get-text-property (point-at-bol) 'superman-item-marker)
					 "date") ")"))
			 'superman-git-diff-switch-commit
			 'superman-capture-button-face
			 "Change active version."))
	 (ref-string (superman-make-button
		      (concat ref
			      " (" (save-excursion
				     (forward-line 1)
				     (or (superman-get-property
					  (get-text-property (point-at-bol) 'superman-item-marker)
					  "date") "unknown")) ")")
		      'superman-git-diff-switch-ref
		      'superman-capture-button-face "Change reference version."))
	 (header (concat "Changes in " commit-string " since " ref-string))
	 (index-buf (get-buffer-create (concat "*["project ": index" "]*")))
	 (view-buf (get-buffer-create (concat "*["project ": diff" "]*")))
	 (config (concat (buffer-name log-buf) " / " (buffer-name view-buf) " | *Superman:Git-diff*")))
    (set-buffer view-buf)
    (org-mode)
    (font-lock-mode -1)
    (superman-view-mode-on)
    (superman-git-mode-on)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      ;; insert the result of git command
      (set-buffer index-buf)
      (org-mode)
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (goto-char (point-min))
      (insert "git-output\n")
      (put-text-property (point-min) (1+ (point-min)) 'git-dir git-dir)
      ;; prepare buffer if necessary
      (funcall 'superman-git-diff-pre-display-hook)
      (goto-char (point-min))
      (while (outline-next-heading)
	(setq count (+ count 1))
	(setq line (superman-format-thing (copy-marker (point-at-bol)) balls))
	(with-current-buffer view-buf (insert line "\n")))
      (set-buffer view-buf)
      (when superman-empty-line-before-cat (insert "\n"))
      (goto-char (point-min))
      ;; section names
      (when (and
	     superman-empty-line-before-cat
	     (save-excursion (beginning-of-line 0)
			     (not (looking-at "^[ \t]*$"))))
	(insert "\n"))
      (put-text-property 0 (length name) 'git-repos dir name) 
      (superman-view-insert-section-name
       name count balls
       nil
       nil)
      (end-of-line 0)
      (forward-line 1)
      ;; insert the column names
      (when superman-empty-line-after-cat (insert "\n"))
      (insert (superman-column-names balls))
      (goto-char (1- (or (next-single-property-change (point) 'cat) (point-max))))
      (put-text-property (- (point-at-eol) 1) (point-at-eol) 'tail name)
      ;; (goto-char (previous-single-property-change (point) 'cat))
      (goto-char (point-min))
      (insert header "\n\n")
      (put-text-property (point-min) (+ (point-min) 1) 'redo-cmd
			 `(lambda () (superman-git-display-diff ,commit ,ref ,dir ,project)))
      (put-text-property (point-min) (+ (point-min) (length header)) 'region-start t)
      (put-text-property (point-min) (+ (point-min) (length header)) 'nickname nickname)
      (put-text-property (point-min) (+ (point-min) (length header)) 'git-dir git-dir)
      ;; now prepare the per file diffs
      (switch-to-buffer view-buf)
      (goto-char (point-min))
      (set-text-properties 0 (length commit) nil commit)
      (set-text-properties 0 (length ref) nil ref)
      (let (next)
	(while (setq next (next-single-property-change (point-at-eol) 'superman-item-marker))
	  (goto-char next)
	  (let* ((cmd `(lambda () (superman-git-diff-file ,commit ,ref ,config))))
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-choice cmd))))
      (setq next-item (next-single-property-change (point-min) 'superman-item-marker))
      (when next-item   ;; might be nil in case nothing changed between workspace and HEAD
	(goto-char next-item)
	(previous-line))
      (superman-git-diff git-dir commit ref)
      (superman-switch-config nil 0 config))))

(defun superman-list-to-alist (list)
  (mapcar* 'cons list (make-list (length list) `())))

(defun superman-git-diff-switch-commit ()
  (interactive)
  (let* ((redo-cmd (get-text-property (point-min) 'redo-cmd))
	 (dir (nth 3 (caddr redo-cmd)))
	 ;; (commit (nth 1 (caddr redo-cmd)))
	 (hash-list (split-string
		     (shell-command-to-string
		      (concat "cd " dir ";" superman-cmd-git " --no-pager log --full-history --pretty=\"%h\""))
		     "\n" t))
	 (hash-alist (superman-list-to-alist hash-list))
	 (commit (completing-read "Choose commit" hast-alist))
	 (ref (nth 2 (caddr redo-cmd)))
	 (nick (nth 4 (caddr redo-cmd))))
    (superman-git-display-diff commit ref dir nick)))

(defun superman-view-back ()
  "Kill current buffer and return to project view."
  (interactive)
  (let ((pbuf (get-text-property (point-min) 'project-buffer)))
    (kill-buffer (current-buffer))
    (switch-to-buffer pbuf)
    (superman-redo)))

;;}}}
;;{{{ preparing git displays

(defun superman-get-git-status-face (str)
  (cond ((string-match "Committed" str ) 'font-lock-type-face)
	((string-match  "Modified" str) 'superman-warning-face)
	(t 'font-lock-comment-face)))

(defun superman-git-untracked-pre-display-hook ()
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


(defun superman-git-files-pre-display-hook ()
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

(defvar superman-git-diff-status-letters
  '(("A" "addition" "addition of a file")
    ("C" "copy" "copy of a file into a new one")
    ("D" "deletion" "deletion of a file")
    ("M" "modification" "modification of the contents or mode of a file")
    ("R" "renaming" "renaming of a file")
    ("T" "type-change" "change in the type of the file")
    ("U" "unmerged" "file is unmerged (you must complete the merge before it can be committed)")
    ("X" "unknown" "unknown change type (most probably a bug, please report it)"))
  "Explanation of status letters copied from man page, see M-x manual-entry RET git-diff RET.")

(defun superman-git-diff-pre-display-hook ()
  (let* ((git-dir (get-text-property (point-min) 'git-dir)))
    (goto-char (point-min))
    (while (re-search-forward "^\\([ACDMRTUX]\\)[ \t\n]+\\(.*\\)\n" nil t)
      (let* ((status (match-string-no-properties 1))
	     (ff (match-string-no-properties 2))
	     (dname (file-name-directory ff))
	     (fname (file-name-nondirectory ff))
	     (fullname (concat git-dir "/" ff)))
	(replace-match
	 (concat "** "
		 fname
		 "\n:PROPERTIES:\n:GitStatus: "
		 (cadr (assoc status superman-git-diff-status-letters))
		 "\n:Directory: " (cond (dname) (t "."))  
		 "\n:FILENAME: [[" fullname "]]\n:END:\n\n")
	 'fixed)))))


(defun superman-git-log-pre-display-hook ()
  (goto-char (point-min))
  (end-of-line)
  (insert (concat "\n\n** Workspace\n:PROPERTIES:\n:Commit: Workspace\n:Author: "
		  (user-full-name)
		  "\n:Date:"
		  "\n:Message: current state on disk\n:END:\n\n")))

;;}}}
;;{{{ git display post-hooks

(defun superman-git-log-post-display-hook ()
  (let* ((tail (point))
	 (cat-point (superman-cat-point))
	 (column-point (previous-single-property-change cat-point 'column-names))
	 (dir (get-text-property (point-min) 'git-dir))
	 (nickname (get-text-property (point-min) 'nickname))
	 next)
    (goto-char cat-point)
    (while (setq next (next-single-property-change (point-at-eol) 'superman-item-marker))
      (goto-char next)
      (let* ((commit (superman-get-property (get-text-property (point-at-bol) 'superman-item-marker) "commit"))
	     (ref (if (string= commit "Workspace") "HEAD" (concat commit "^")))
	     (cmd `(lambda () (superman-git-display-diff ,commit ,ref ,dir ,nickname))))
	(put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-choice cmd)))))
;;}}}
;;
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
(define-key superman-git-mode-map "l" 'superman-git-log-file)
(define-key superman-git-mode-map "#" 'superman-git-ignore-file)
(define-key superman-git-mode-map " " 'superman-git-last-log-file) 

;;}}}
;;{{{ superman-git-keyboard

(defface superman-git-keyboard-face-d
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "orange"
		 )))
  "Face used for git-diff."
  :group 'superman)
(defface superman-git-keyboard-face-D
  '((t (:inherit superman-git-keyboard-face-d :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-diff (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-a
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "yellow")))
  "Face used for git-add."
  :group 'superman)
(defface superman-git-keyboard-face-A
  '((t (:inherit superman-git-keyboard-face-a :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-add (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-l
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :height 0.8
		 :background "blue")))
  "Face used for git-log."
  :group 'superman)
(defface superman-git-keyboard-face-L
  '((t (:inherit superman-git-keyboard-face-l :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-log (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-c
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "green")))
  "Face used for git-commit."
  :group 'superman)

(defface superman-git-keyboard-face-p
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "#00FFFF")))
  "Face used for git-commit."
  :group 'superman)

(defface superman-git-keyboard-face-C
  '((t (:inherit superman-git-keyboard-face-c :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-commit (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-P
  '((t (:inherit superman-git-keyboard-face-p :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-commit (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-x
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :height 0.8
		 :background "black")))
  "Face used for git-rm."
  :group 'superman)
(defface superman-git-keyboard-face-X
  '((t (:inherit superman-git-keyboard-face-x :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-rm (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-r
  '((t (:inherit superman-default-button-face
		 :foreground "white"
		 :height 0.8
		 :background "violet")))
  "Face used for git-stash."
  :group 'superman)
(defface superman-git-keyboard-face-R
  '((t (:inherit superman-git-keyboard-facr-l :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-stash (larger box)."
  :group 'superman)

(defface superman-git-keyboard-face-s
  '((t (:inherit superman-default-button-face
		 :foreground "black"
		 :height 0.8
		 :background "red")))
  "Face used for git-status."
  :group 'superman)
(defface superman-git-keyboard-face-S
  '((t (:inherit superman-git-keyboard-face-s :height 1.0 :box (:line-width 1 :color "gray88" :style released-button))))
  "Face used for git-status (larger box)."
  :group 'superman)

(defun superman-make-git-keyboard (f &rest args)
  (if (string-match org-bracket-link-regexp f)
      (let ((diff (superman-make-button "d"
					'superman-git-diff-file
					'superman-git-keyboard-face-d
					"git diff file"))
	    (log (superman-make-button "l"
				       'superman-git-log-file
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
					  "git rm file"))
	    (ignore (superman-make-button "#"
	     				  'superman-git-ignore-file
	     				  'superman-git-keyboard-face-i
	     				  "add to .gitignore"))
	    )
	(concat diff " " log  " " add  " " delete " " reset " " commit " " ignore " " " "))
    ;; for the column name
    (superman-trim-string f (car args))))

;;}}}
;;
;;{{{ git search and history

(defun superman-git-search (&optional arg)
  (interactive "p")
  (superman-git-search-file (or arg superman-git-search-limit)))


(defun superman-git-history (&optional arg)
  (interactive)
  (let* ((file (or (superman-filename-at-point t)
		   (get-text-property (point-min) 'git-dir)
		   (buffer-file-name)))
	 (dir (if (file-directory-p file) (file-name-as-directory file) (file-name-directory file)))
	 (curdir default-directory)
	 (bufn (concat "*history: " file "*")))
    (when dir
      (save-window-excursion
	(setq default-directory dir)
	(vc-git-print-log file bufn t nil (or arg superman-git-log-limit)))
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
  (let* ((rel-file (superman-relative-name file dir))
	 (dir dir)
	 (cmd (concat
	       "cd " dir "; " superman-cmd-git git-switches " -- " rel-file))
	 (gitlog (shell-command-to-string cmd))
	 (log-buf  (concat "*Log[" (file-name-nondirectory file) "]*"))
	 log-strings)
    (when (string= gitlog "")
      (error (concat "No search results in file history or file " rel-file " not (not yet) git controlled.")))
    (switch-to-buffer log-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (font-lock-mode -1)
    (insert "* Git Log of " (file-name-nondirectory file))
    (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
    (put-text-property (point-at-bol) (point-at-eol) 'filename file)
    ;; (put-text-property (point-at-bol) (point-at-eol) 'dir dir)
    ;; (put-text-property (point-at-bol) (point-at-eol) 'command cmd)
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
			   (cons "Tag" deco)
			   (cons "Date" (nth 2 log))
			   (cons "Author"  (nth 3 log))))
		    superman-log-balls)))
	(put-text-property 0 (length item) 'hash (nth 0 log) item)
	(if (or (not decoration-only) (not (string= deco "")))
	    (progn
	      (put-text-property 0 (length item) 'decoration (nth 4 log) item)
	      (insert item "\n")))
	(setq log-strings (cdr log-strings))))
    (superman-git-log-mode-on)
    (goto-char (point-min))
    ;; (setq truncate-lines t
    ;; truncate-partial-width-windows nil)
    (superman-next-entry)
    (setq buffer-read-only t)))

(setq superman-log-balls
      '(("Date" ("width" 10) ("face" font-lock-string-face))
	("Hash" ("width" 10) ("face" font-lock-comment-face))
	("Author" ("width" 20) ("face" font-lock-function-name-face))
	("Tag" ("width" 10) ("face" font-lock-comment-face))
	("Comment" ("fun" superman-dont-trim) ("face" font-lock-keyword-face))))

(defun superman-git-log (file limit &optional search-string decoration-only)
  (let* ((file (or file (superman-filename-at-point)
		   (superman-get-property nil (superman-property 'filename) t)))
	 (gitsearch (if search-string (concat " -G\"" search-string "\"") ""))
	 (gitpath (get-text-property (point-min) 'git-dir))
	 (gitcmd (concat " --no-pager log --full-history --pretty=\"%h:#:%s:#:%ad:#:%an:#:%d\" --date=short "
			 gitsearch  " "
			 (if limit (concat "-n " (int-to-string limit))))))
    (superman-git-setup-log-buffer file gitpath gitcmd decoration-only (list limit search-string decoration-only) )))


;; (defun superman-git-comment-file ()
  ;; (interactive)
  ;; (let* ((hash (org-get-at-bol 'org-hd-marker))
	 ;; (path (get-text-property (point-min) 'dir))
	 ;; ;;(path (superman-get-property pom (superman-property 'gitpath) t))
	 ;; ;;(hash (superman-get-property pom (superman-property 'hash) nil))))
	 ;; )
    ;; (shell-command-to-string (concat "cd " path ";" superman-cmd-git " log -1 " hash))))

(defun superman-git-tag ()
  "Set git tag"
  (interactive)
  (let* ((oldtag (get-text-property (point-at-bol) 'decoration))
	 (hash (get-text-property (point-at-bol) 'hash))
	 (file (get-text-property (point-min) 'filename))
	 (arglist (get-text-property (point-min) 'arglist)) ;; limit search decoration-only
	 (tag (read-string "Tag (empty to clear): "))
	 (linenum (line-number-at-pos)))
    (if (string-equal tag "")
	(progn 
	  (if oldtag
	      (progn
		(setq oldtag (replace-regexp-in-string "\)" "" (replace-regexp-in-string "\(" "" oldtag)))
		(shell-command-to-string (concat "cd " (file-name-directory file) ";" superman-cmd-git " tag -d " oldtag)))))
      (shell-command-to-string (concat "cd " (file-name-directory file) ";" superman-cmd-git " tag -a " tag " " hash " -m \"\"")))
    (superman-git-log file (nth 0 arglist) (nth 1 arglist) (nth 2 arglist))
    (goto-char (point-min))
    (forward-line (1- linenum))))

(defun superman-git-revision-at-point (&optional diff)
  "Shows version of the document at point "
  (interactive)
  (superman-git-revision (get-text-property (point-at-bol) 'hash) diff))

(defun superman-git-revision (pom &optional diff)
  "Shows version of the document at point "
  (let* ((file (get-text-property (point-min) 'filename))
	 (hash (get-text-property (point-at-bol) 'hash))
	 (ext (file-name-extension file))
	 (filehash (concat (file-name-sans-extension (file-name-nondirectory file)) "_" hash (if ext (concat "." ext))))
	 (str (shell-command-to-string 
	       (concat "cd " (file-name-directory file) ";" superman-cmd-git " show " hash ":./" (file-name-nondirectory file)))))
    (if diff (find-file file))
    (switch-to-buffer-other-window filehash) 
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
;;      (if arg
      (compilation-start (concat "cd " dir "; git grep -n -e " (read-string "Grep: ") " -- *") 'grep-mode))))
;;	(vc-git-grep (read-string "Grep: "))
;;	(vc-git-grep (read-string "Grep: ") "*" dir)))))
;;}}}

(provide 'superman-git)
;;; superman-git.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End: 
