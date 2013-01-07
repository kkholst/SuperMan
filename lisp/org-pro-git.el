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

;;{{{ control

(defvar org-pro-cmd-git "git")

(defun org-pro-show-help ()
  (interactive)
  (split-window-vertically)  
  (other-window 1)
  (switch-to-buffer "*org-pro-help-buffer*")
  (toggle-read-only -1)
  (erase-buffer)
  (insert "'<ret>':\t\t Open file (or revision) at point\n")
  (insert "'l':    \t\t Show git log\n")
  (insert "'L':    \t\t Show git log for tagged revisions\n")
  (insert "'u':    \t\t Update git status\n")
  (insert "'b':    \t\t Blame\n")
  (insert "'S':    \t\t Search for revision containing a regular expression\n")
  (insert "'D':    \t\t Show difference between revision at point and HEAD\n")
  (insert "'h':    \t\t Open this help window\n")
  (insert "'t':    \t\t Alter tag (empty string to remove)\n")
  (insert "'q':    \t\t Quit view mode\n")
  (goto-char (point-min))
  (toggle-read-only 1)
  (other-window -1))


;; (setq org-property-set-functions-alist nil)  
(add-to-list 'org-property-set-functions-alist
	     `("GitStatus" . org-pro-git-status-at-point))
(add-to-list 'org-property-set-functions-alist
	     `("LastCommit" . org-pro-git-commit-at-point))

(defvar org-pro-use-git t "Whether to use git to backup projects. Set to nil to completely disable git.
                                                                     If non-nil, git is controlled on per project basis using properties set in `org-pro'.")

(setq org-pro-git-ignore "*")
(defvar org-pro-git-ignore "*" "What files to include or not include. See M-x manual-entry RET gitignore.
                                                        
                                                     By default we set this to '*' which means that all files are ignored.
                                                        
                                                     You think this sounds like a stupid idea? Hehe, we can still add files via
                                                     the -f (force) command line switch. And we get not bothered by
                                                     having to filter all the unpredictable names one can give to files
                                                     that never should get git controlled.")

(defun org-pro-git-p (dir)
  "Test if directory DIR is under git control."
  (eq 0 (shell-command (concat "cd " dir ";" org-pro-cmd-git " rev-parse --is-inside-work-tree "))))

(defun org-pro-git-toplevel (file)
  "Find the toplevel directory DIR is under git control."
  (let ((dir (if (file-directory-p file) file (file-name-directory file))))
    (if (org-pro-git-p dir)
	(replace-regexp-in-string "\n" "" (shell-command-to-string (concat "cd " dir "; git rev-parse --show-toplevel "))))))

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
	  (org-pro-git-add-and-commit-file
	   index loc (concat "Initial commit of project " (car pro)))))))

(defun org-pro-git-init-directory (dir)
  "Put directory DIR under git control."
  (if (org-pro-git-p dir)
      (message (concat "Directory " dir " is under git control."))
    (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " init"))
    (append-to-file org-pro-git-ignore nil (concat (file-name-as-directory dir) ".gitignore"))))

(defun org-pro-filename-at-point ()
  (let* ((pom (cond ((eq major-mode 'org-mode) (point))
		    ((eq major-mode 'org-agenda-mode) (org-get-at-bol 'org-hd-marker))
		    (t (error "This function works only in org-mode, org-agenda-mode, org-pro-git-log-mode, org-pro-view-mode."))))
	 (file-or-link  (org-pro-get-property pom "filename" t)))
   (if (not (stringp file-or-link))
       (error "No proper(ty) filename at point.")
    (if (string-match org-bracket-link-regexp file-or-link)
	(expand-file-name
	 (org-extract-attributes
	  (org-link-unescape (org-match-string-no-properties 1 file-or-link))))
      (if (file-exists-p file-or-link)
	  (expand-file-name file-or-link))))))

(defun org-pro-read-git-date (git-date-string &optional no-time)
  "Transform git date to org-format"
  (with-temp-buffer
    (org-insert-time-stamp 
     (date-to-time git-date-string) (not no-time))))
;;      (set-time-zone-rule t) ;; Use Universal time.
;;      (prog1 (format-time-string "%Y-%m-%d %T UTC" time)
;;        (set-time-zone-rule nil))))

(defun org-pro-git-get-commit (arg file &optional dir)
  (interactive)
  (let* ((dir (cond (dir) ((file-name-absolute-p file) (file-name-directory file))
		    (t (read-directory-name (concat "Find the git directory of file " file ": ")))))
	 (date-string
		(shell-command-to-string
		 (if (string= arg "first")
		     (concat  "cd " dir ";" org-pro-cmd-git " log --date=local --pretty=format:\"%ad\" --reverse -- " file "  | head -1")
		   (concat org-pro-cmd-git " log --date=local -" arg " --pretty=format:\"%ad\" -- " file))))
         (date (if (string= date-string "") "" (org-pro-read-git-date date-string)))
	 (mess (shell-command-to-string
		(if (string= arg "first")
		    (concat "cd " dir ";" org-pro-cmd-git " log --reverse --pretty=format:\"%s\" -- " file " | head -1")
		  (concat org-pro-cmd-git " log -" arg " --pretty=format:\"%s\" -- " file)))))
    (concat date " " mess)))

(defun org-pro-git-status-at-point ()
  (interactive)
  (org-pro-git-get-status (org-pro-filename-at-point)))

(defun org-pro-git-get-status (file)
  "Determine the git status of file FILE"
  (interactive)
  (let* ((file (or file (read-file-name "Get git status for file: ")))
	 (dir (if file (file-name-directory file)))
	 (git-status (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " status --ignored --porcelain " file)))
	 git-last-commit
	 label)
    (if (not (org-pro-git-p dir))
	(error (concat "Directory " dir " is not git controlled. You may want to start\ngit control of the project via M-x `org-pro-git-init-project'."))
      (if (string= git-status "")
	  (if (file-exists-p file)
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

(defun org-pro-git-set-status-at-point ()
  (interactive)
  (let ((file (org-pro-filename-at-point)))
    (org-pro-git-set-status (point) file)))

(defun org-pro-git-set-status (pom file)
  (interactive)
  (let* ((statlist (org-pro-git-get-status file))
	 (last-commit (nth 2 statlist))
	 (git-status (nth 0 statlist))
	 (git-label (nth 1 statlist)))
    (org-entry-put pom "GitStatus" git-label)
    ;; (org-set-property "GitStatus" git-label)
    (unless (or (string= git-status "E") (string= git-status "?"))
      (unless (org-pro-get-property pom "GitInit")
	(org-entry-put pom "GitInit" (org-pro-git-get-commit "first" file)))
      (unless (string= last-commit "")
	(org-entry-put pom "LastCommit" last-commit)))
    statlist))


(defun org-pro-update-git-status ()
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (while (re-search-forward ":filename:" nil t)
      (org-pro-git-set-status-at-point))))

(defun org-pro-git-add-file (file project)
  (interactive)
  (let* ((pro (or project (org-pro-select-project)))
	 (dir (concat (org-pro-get-location pro) (car pro)))
	 (file (or file (read-file-name "Git add file: " dir nil t))))
    (shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " add -f " file))))

(defun org-pro-git-add-and-commit-file (file dir &optional message)
  (shell-command-to-string (concat "cd " dir
			 ";" org-pro-cmd-git " add -f " file ";" org-pro-cmd-git " commit -m\""
			 (or message 
			     (read-string (concat "Commit message for " (file-name-nondirectory file) ": ")))
			 "\" " file)))


(defun org-pro-git-add-at-point ()
  "Add or update file FILE to git repository DIR."
  (interactive)
  (let* ((file (org-pro-filename-at-point))
	 (dir (if file (file-name-directory file))))
    (org-pro-git-add-file file
			  (if (string= (expand-file-name (buffer-file-name))
				       (expand-file-name (org-pro-get-index org-pro-current-project)))
			      org-pro-current-project
			    nil))
    (org-pro-git-set-status-at-point)))

(defun org-pro-git-commit-at-point (&rest args)
  "Add or update file FILE to git repository DIR."
  (interactive)
  (let* ((file (org-pro-filename-at-point))
	 (dir (if file (file-name-directory file)))
	 (message (read-string (concat "Commit message for " (file-name-nondirectory file) ": "))))
    (org-pro-git-add-and-commit-file file dir message)
    (org-pro-git-set-status-at-point)))

(defun org-pro-git-push-directory (dir silent)
  "Git push directory DIR."
  (let* ((status (shell-command-to-string  (concat "cd " dir ";" org-pro-cmd-git " status")))
	 (necessary (string-match "Your branch is ahead .*\n" status))
	 (doit (or silent (y-or-n-p (concat "Your branch is ahead ... push git at " dir "? ")))))
    (if doit
	(shell-command-to-string (concat "cd " dir ";" org-pro-cmd-git " push")))))


(defun org-pro-git-update-project (project before)
  "Check if project needs to be put under git control and update.
                                                     If BEFORE is set then either initialize or pull. Otherwise, add, commit and/or push.
                                                    "
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

(define-key org-pro-git-log-mode-map "D" (lambda () (interactive) (org-pro-git-revision-at-point 1)))
(define-key org-pro-git-log-mode-map "t" 'org-pro-view-git-tag)
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

(defvar org-pro-git-log-limit 5)
(defvar org-pro-git-search-limit 500)


(defun org-pro-git-log-mode-on ()
  (interactive)
  (org-pro-git-log-mode t))

(defun org-pro-git-setup-log-buffer (file path git-switches decorationonly)
  (let* ((file-rel (file-relative-name (expand-file-name file) (expand-file-name gitpath)))
	 (gitlog (shell-command-to-string
		  (concat
		   "cd " gitpath "; " org-pro-cmd-git git-switches " -- " file-rel)))
	 (logbuf (concat "#" (file-name-nondirectory file) ".org"))
	 (logfile (concat "/tmp/" logbuf))
	 item val)
    (if (string= gitlog "")
	(error (concat "No search results in file history or file " file-rel " not (not yet) git controlled."))
      (pop-to-buffer logbuf)
      (erase-buffer)
      (insert (concat "* Git Log ("
                      file-rel
                      ")\n:PROPERTIES:\n:COLUMNS: %40ITEM(Comment) %Date %15Author %15Decoration %8Hash \n:filename: "
                      file-rel
                      "\n:GitPath: "
                      gitpath
                      "\n:END:\n"))
      (loop for x in (split-string (substring gitlog 0 -1) "\n")
            do 
            (setq val (delete "" (split-string x ":#:")))
            (setq item (concat "*** " (nth 1 val) "\n:PROPERTIES:\n:Hash: " (car val) "\n:Date: " (nth 2 val) "\n:Author: " (nth 3 val) (when (nth 4 val) (concat "\n:Decoration: " (nth 4 val))) "\n:END:\n"))
            (if (or (not decorationonly) (nth 4 val)) (insert item)))
      ;; FIXME: rather change org-tags-view-plus to accept buffers instead of files
      (write-file logfile nil)
      (goto-char (point-min))
      (let ((lprops
	     `((org-agenda-files (quote (,logfile)))
	       (org-agenda-finalize-hook 'org-pro-git-log-mode-on)
	       (org-agenda-overriding-header (concat "Git-log of " ,file-rel "\th: help, C:commit, l: log, H:history\n\n"))
	       (org-agenda-overriding-agenda-format
		'(lambda (hdr level category tags-list properties)
		   (concat "| " hdr
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
		   (org-pro-get-property nil "filename" t)))
	 (gitsearch (if search-string (concat " -G\"" search-string "\"") ""))
	 (gitpath (or gitpath (org-pro-git-toplevel file)))
	 (gitcmd (concat " --no-pager log --pretty=\"%h:#:%s:#:%ad:#:%an:#:%d\" --date short " gitsearch  " " (if limit (concat "-n " (int-to-string limit))))))
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

(defun org-pro-git-tag-at-point (&optional tag)
  "Set git tag"
  (interactive)
  (let ((hash (org-pro-get-property nil "hash" nil))
	(oldtag (org-pro-get-property nil "decoration" nil))
	(path (org-pro-get-property nil "gitpath" t))
	(tag (read-string "Tag (empty to clear): ")))
    (if (string-equal tag "")
	(progn 
	  (setq oldtag (replace-regexp-in-string "\)" "" (replace-regexp-in-string "\(" "" oldtag)))
	  (shell-command-to-string (concat "cd " path ";" org-pro-cmd-git " tag -d " oldtag)))
      (shell-command-to-string (concat "cd " path ";" org-pro-cmd-git " tag -a " tag " " hash " -m \"\"")))) 
  (save-excursion
    (goto-char (point-min))
    (org-pro-git-log-at-point 1)))

(defun org-pro-git-revision-at-point (&optional diff)
  "Shows version of the document at point "
  (interactive)
  (org-pro-git-revision (org-get-at-bol 'org-hd-marker) diff))

(defun org-pro-git-revision (pom &optional diff)
  "Shows version of the document at point "
  (let* ((file (org-pro-get-property pom "filename" t))
	 (hash (org-pro-get-property pom "hash" nil))
	 (path (org-pro-get-property pom "gitpath" t))
	 (fileabs (concat path file))
	 (filehash (concat hash "_" file))
	 (str (shell-command-to-string 
	       (concat "cd " path ";" org-pro-cmd-git " show " hash ":" file))))
    (if diff (find-file fileabs))
    (switch-to-buffer-other-window filehash) ;;set-buffer find-file-noselect fileabs
    (erase-buffer)  
    (insert str)
    (normal-mode) ;; Get default major-mode 
    (if diff (ediff-buffers (file-name-nondirectory file) filehash))))
;;}}}


(provide 'org-pro-git)
;;; org-pro-git.el ends here
