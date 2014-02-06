;;; superman-display-file-list.el --- List files in projects

;; Copyright (C) 2014  Thomas Alexander Gerds

;; Author: Thomas Alexander Gerds <tag@biostat.ku.dk>
;; Keywords: convenience

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

;; 

;;; Code:

(defun superman-view-file-list (&optional arg)
  (interactive "p") 
  (let* ((pro (superman-view-current-project))
	 (dir (if pro (superman-project-home pro)
		(or (get-text-property (point-min) 'git-dir)
		    (read-directory-name "View file-list below directory: ")))))
    (if (and arg (> arg 1))
	;; old-style file-list display 
	(progn
	  (split-window-vertically)
	  (other-window 1p)
	  (superman-file-list pro))
      (superman-display-file-list dir))))

(defun superman-file-list (project &optional ext)
  "List files in project's location that match extension EXT"
  (if (featurep 'file-list)
      (let ((loc (superman-project-home project)))
	(cond ((file-list-select-internal nil (or ext ".")
					  nil nil loc (concat "*File-list-" (car project) "*")))
	      (t
	       (switch-to-buffer (concat "*File-list-" (car project) "*"))
	       (toggle-read-only -1)
	       (erase-buffer)
	       (insert "FILE-LIST: No files in project"))))
    (error "file-list.el not loaded.")))

(defun superman-write-file-list (list buf)
  "Write file-list LIST to temporary buffer BUF."
  (get-buffer-create buf)
  (switch-to-buffer buf)
  (erase-buffer)
  (while list
    (insert "* " (caar list) "\n"
	    ":PROPERTIES:\n:PATH: "
	    (cadar list) "\n:END:\n")
    (setq list (cdr list))))

(defvar superman-file-list-balls
  '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" font-lock-variable-name-face))
    ("Path" ("fun" superman-dont-trim) ("face" font-lock-keyword-face)))
  "balls used to display file-lists.")

(defun superman-display-file-list (dir &optional list balls ext view-buffer refresh)
  "Format file-list displays."
  (let* ((project (superman-view-current-project))
	 (nick (if project (car project) (get-text-property (point-min) 'nickname)))
	 (pbuf (if file-list-mode
		   (get-text-property (point-min) 'project-buffer)
		 (buffer-name)))
	 (loc (or dir (get-text-property (point-min) 'loc)
		  (when project
		    (superman-project-home project))
		  (error "Missing location")))
	 (balls (or balls superman-default-balls))
	 (view-buf (or view-buffer (get-buffer-create (concat "*FileList: " nick "*"))))
	 (list
	  (with-current-buffer view-buf
	    (if refresh
		(file-list-select-internal
		 nil (or ext ".")
		 nil nil loc (concat "*File-list-" (or nick "nix") "*") 'dont)
	      (or list
		  file-list-current-file-list
		  (file-list-select-internal
		   nil (or ext ".")
		   nil nil loc (concat "*File-list-" (or nick "nix") "*") 'dont)))))
	 (balls superman-file-list-balls)
	 (count 0)
	 ;; (cycle file-list-display-level)
	 (maxfile (apply 'max (mapcar #'(lambda (x) (length (car x))) list)))
	 cycle)
    (setcdr (assoc "width" (assoc "FileName" balls)) `(,maxfile))
    (switch-to-buffer view-buf)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (unless file-list-mode
	(org-mode)
	(font-lock-mode -1)
	(file-list-mode))
      (setq file-list-current-file-list list)
      (setq display-level (get-text-property (point-min) 'file-list-display))
      (if (and (not refresh) display-level)
	  nil ;; buffer exists and needs no refreshment
	;; (unless display-level
	;; (put-text-property (point-min) (1+ (point-min)) 'file-list-display cycle))
	;; insert the file-list 
	(goto-char (point-min))
	(erase-buffer)
	;; prepare buffer if necessary
	(run-hooks 'superman-file-list-pre-display-hook)
	(goto-char (point-min))
	(insert (superman-make-button
		 (concat "FileList"
			 (when nick (concat ": " nick)))
		 'file-list-redisplay
		 'superman-project-button-face
		 "Refresh project view"))
	(put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
	(put-text-property (point-min) (1+ (point-min)) 'project-buffer pbuf)
	(put-text-property (point-at-bol) (point-at-eol) 'loc loc)
	(insert "\n")
	(superman-view-insert-action-buttons
	 `((,(concat (char-to-string #x2245) " file-name") file-list-by-name
	    nil "Select files by matching file-name")
	   (,(concat (char-to-string #x21AF) " file-name") (lambda () (interactive) (file-list-by-name 1))
	    nil "Select files by matching file-name")
	   (,(concat (char-to-string #x2245) " dir-name") file-list-by-path
	    nil "Select files by matching directory name")
	   (,(concat (char-to-string #x21AF) " dir-name") (lambda () (interactive) (file-list-by-path 1))
	    nil "Select files by matching directory name")
	   ("< time" file-list-by-time nil "Select files based on modification time")
	   ("> time" (lambda () (interactive) (file-list-by-time 1)) nil "Select files based on modification time")
	   ("< size" (lambda () (interactive) (file-list-by-size 1)) nil "Select files based on size")
	   ("> size" file-list-by-size nil "Select files based on size"))
	 nil "Filter:")
	(superman-view-insert-action-buttons
	 `((,(concat (char-to-string #x25b3) " file-name")
	    file-list-sort-by-name nil "Sort by increasing file-name")
	   (,(concat (char-to-string #x25bd) " file-name")
	    (lambda () (interactive) (file-list-sort-by-name 1))
	    nil "Sort by decreasing file-name")
	   (,(concat (char-to-string #x25b3) " dir-name")
	    file-list-sort-by-path nil "Sort by increasing path")
	   (,(concat (char-to-string #x25bd) " dir-name")
	    (lambda () (interactive) (file-list-sort-by-path 1))
	    nil "Sort by decreasing path")
	   (,(concat (char-to-string #x25b3) " time")
	    (lambda () (interactive) (file-list-sort-by-time 1))
	    nil "Sort by increasing time")
	   (,(concat (char-to-string #x25bd) " time")
	    file-list-sort-by-time
	    nil "Sort by decreasing time")
	   (,(concat (char-to-string #x25b3) " size")
	    (lambda () (interactive) (file-list-sort-by-size 1))
	    nil "Sort by increasing size")
	   (,(concat (char-to-string #x25bd) " size")
	    file-list-sort-by-size
	    nil "Sort by decreasing size"))
	 ;; ("time" file-list-sort-by-time)
	 ;; ("time" (lambda () (interactive) (file-list-sort-by-time 1)))
	 ;; ("size" file-list-sort-by-size)
	 ;; ("size" (lambda () (interactive) (file-list-sort-by-size 1))))
	 nil "  Sort:")
	(superman-view-insert-action-buttons
	 '(("attributies" file-list-ls)
	   ("grep" file-list-grep)
	   ("copy" file-list-copy)
	   ("query-replace" file-list-query-replace)
	   ("delete" file-list-remove)
	   ("clear display" file-list-clear-display)) nil "Action:")
	(insert "\n* ")
	(put-text-property (point-at-bol) (point-at-eol) 'file-list-start t)
	(insert "\n")
	(while list
	  (let* ((el (car list))
		 (file (car el))
		 (path (cadr el))
		 (rest (caddr el))
		 appendix)
	    (insert (superman-format-thing `(list (("FileName" . ,file) ("Path" . ,path))) balls))
	    (setq count (1+ count))
	    (while rest
	      (let ((key (caar rest))
		    (val (cdar rest)))
		(put-text-property 0 (length key) 'face 'font-lock-warning-face key)
		;; (put-text-property 0 (length val) 'face 'custom-state val)
		(setq appendix
		      (concat appendix
			      "\n"
			      (format "%13s" (caar rest))
			      " : " (cdar rest)))
		(setq rest (cdr rest))))
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'filename (file-list-make-file-name el))
	    (put-text-property (point-at-bol) (1+ (point-at-bol)) 'superman-item-marker t)
	    (when appendix (insert appendix))
	    (insert "\n")
	    (setq list (cdr list))))
	(goto-char (next-single-property-change (point-min) 'file-list-start))
	(goto-char (point-at-bol))
	(kill-line)
	(insert "\n\n")
	;; insert the section name		
	(superman-view-insert-section-name
	 loc
	 count
	 balls
	 nil
	 'file-list-redisplay
	 "Refresh view")
	(when (= file-list-display-level 2)
	  (insert "\t")
	  (insert (superman-make-button "Clear display"
					'file-list-clear-display
					'superman-next-project-button-face
					"Return to clean file-list")))
	;; insert filters		
	(when file-list-filter
	  (dolist (x file-list-filter nil)
	    (insert "\t" (superman-make-button
			  (car x)
			  `(lambda () (interactive)
			     (file-list-remove-filter ,(car x)))
			  'superman-next-project-button-face
			  "Unfilter list"))))
	;; insert the column names
	(insert "\n")
	(when superman-empty-line-after-cat (insert "\n"))
	(insert (superman-column-names balls))
	(beginning-of-line)
	(run-hooks 'superman-file-list-pre-display-hook)))))


(defun file-list-remove-filter (filter)
  (let ((ff (cdr (assoc filter file-list-filter))))
    (setq file-list-current-file-list
	  (append file-list-current-file-list (car ff)))
    (setq file-list-filter
	  (delete-if '(lambda (x) (string= (car x) filter)) file-list-filter))
    (superman-display-file-list (get-text-property (point-min) 'loc))))

  
(defun file-list-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map file-list-mode-map)
  (setq major-mode 'file-list-mode)
  (setq mode-name "file-list")
  (make-local-variable 'file-list-current-file-list)
  (make-local-variable 'file-list-filter)
  (make-local-variable 'file-list-mode)
  (make-local-variable 'file-list-match-history)
  (setq file-list-mode t)
  (run-hooks 'file-list-completion-mode-hook))


(defvar file-list-display-help-string
  (concat ""
	  ;; (substitute-command-keys "list of matching files. here are some commands and keys. M-x `describe-bindings' shows all key-bindings\n\n`\\<file-list-mode-map>\\[file-list-choose-file]' find file `\\<file-list-mode-map>\\[file-list-choose-file-other-window]' in other window;`\\<file-list-mode-map>\\[file-list-choose-magic]' open, i.e. find with external program\n`S f', `S p', `S t', `S s' sorting and `/f' `/p' `/s' `/t' sub-selection by name, path, size, time\n `g' grep; `x' shell command; `d' dired; `t' toggle display\n\n")))
	  (substitute-command-keys "finding files: RET, SPACE, M-RET; sorting list: S f, S p, S t, S s; selecting from list: /f /p /s /t (by name, path, size, time)\nother commands: g grep; x shell command; d dired; t toggle display ...\n\n")))


(defun file-list-current-display-buffer ()
  (if file-list-mode
      (buffer-name (current-buffer))
    nil))

(defun file-list-display-match-list* (&optional file-list match-info display-buffer)
  "This function shows file-list in display-buffer.
Sets the value of file-list-current-file-list in display-buffer."
  (let ((file-list (or file-list file-list-current-file-list))
	(display-buffer (or display-buffer
			    (file-list-current-display-buffer)
			    file-list-display-buffer))
	(active (file-list-current-display-buffer))
	match-info-string
	display-list)
    (switch-to-buffer display-buffer)
    ;; sort list
    (setq file-list
	  (sort file-list
		(lambda (e f)
		  (> (length (caddr e)) (length (caddr f))))))
    (setq display-list
	  (cond ((= file-list-display-level 2)
		 (mapcar
		  (lambda (entry)
		    (let ((file-name
			   (if (featurep 'xemacs)
			       (file-list-replace-in-string
				(file-list-make-file-name entry) " " "\\\ " 'literal)
			     (file-list-replace-in-string
			      (file-list-make-file-name entry) " " "\\ " 'literal)))
			  (rest (caddr entry))
			  rest-string)
		      (while rest
			(setq rest-string
			      (concat rest-string "\n" (format "%13s" (caar rest))
				      " : " (cdar rest)))
			(setq rest (cdr rest)))
		      (concat (when rest-string "\n") file-name rest-string)))
		  file-list))
		((= file-list-display-level 1)
		 (mapcar
		  (lambda (entry)
		    (if (featurep 'xemacs)
			(file-list-replace-in-string
			 (file-list-make-file-name entry) " " "\\\ " 'literal)
		      (file-list-replace-in-string
		       (file-list-make-file-name entry) " " "\\ " 'literal)))
		  file-list))
		((= file-list-display-level 3)
		 (mapcar
		  (lambda (entry)
		    (org-make-link-string
		     (file-list-replace-in-string
		      (file-list-make-file-name~ entry) " " "\\ " 'literal)))
		  file-list))
		(t (mapcar
		    (lambda (entry)
		      (file-list-replace-in-string
		       (car entry) " " "\\\ " 'literal)) file-list))))
    ;; match history
    (if match-info
	(if active
	    (setq file-list-match-history
		  (concat file-list-match-history match-info))
	  (setq file-list-match-history match-info))
      (setq file-list-match-history nil))
    ;; (message "bla:" file-list-match-history)
    (setq match-info-string
	  (format "Match-info: %i files %s\n\ncurrent file-list:\n"
		  (length display-list)
		  (or file-list-match-history "match")))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert match-info-string)
      (while display-list
	(insert (car display-list) "\n")
	(setq display-list (cdr display-list)))
      ;; (replace-match match-info-string nil t))))
      (switch-to-buffer display-buffer)
      ;; (when (= file-list-display-level 1))
      ;; (file-list-completion-mode)
      (setq file-list-current-file-list file-list)
      (file-list-beginning-of-file-list))))

(defun file-list-display-match-list (&optional file-list match-info display-buffer)
  "This function shows file-list in display-buffer.
Sets the value of file-list-current-file-list in display-buffer."
  (if (and file-list-mode (not file-list-completion-mode))
      (superman-display-file-list nil)
    (let ((file-list (or file-list file-list-current-file-list))
	  (display-buffer (or display-buffer
			      (file-list-current-display-buffer)
			      file-list-display-buffer))
	  (active (file-list-current-display-buffer))
	  match-info-string
	  display-list)
      (switch-to-buffer display-buffer)
      ;; sort list
      (setq file-list
	    (sort file-list
		  (lambda (e f)
		    (> (length (caddr e)) (length (caddr f))))))
      (setq display-list
	    (cond ((= file-list-display-level 2)
		   (mapcar
		    (lambda (entry)
		      (let ((file-name
			     (if (featurep 'xemacs)
				 (file-list-replace-in-string
				  (file-list-make-file-name entry) " " "\\\ " 'literal)
			       (file-list-replace-in-string
				(file-list-make-file-name entry) " " "\\ " 'literal)))
			    (rest (caddr entry))
			    rest-string)
			(while rest
			  (setq rest-string
				(concat rest-string "\n" (format "%13s" (caar rest))
					" : " (cdar rest)))
			  (setq rest (cdr rest)))
			(concat (when rest-string "\n") file-name rest-string)))
		    file-list))
		  ((= file-list-display-level 1)
		   (mapcar
		    (lambda (entry)
		      (if (featurep 'xemacs)
			  (file-list-replace-in-string
			   (file-list-make-file-name entry) " " "\\\ " 'literal)
			(file-list-replace-in-string
			 (file-list-make-file-name entry) " " "\\ " 'literal)))
		    file-list))
		  ((= file-list-display-level 3)
		   (mapcar
		    (lambda (entry)
		      (org-make-link-string
		       (file-list-replace-in-string
			(file-list-make-file-name~ entry) " " "\\ " 'literal)))
		    file-list))
		  (t (mapcar
		      (lambda (entry)
			(file-list-replace-in-string
			 (car entry) " " "\\\ " 'literal)) file-list))))
      ;; match history
      (if match-info
	  (if active
	      (setq file-list-match-history
		    (concat file-list-match-history match-info))
	    (setq file-list-match-history match-info))
	(setq file-list-match-history nil))
      ;; (message "bla:" file-list-match-history)
      (setq match-info-string
	    (format "Match-info: %i files %s\n\ncurrent file-list:\n"
		    (length display-list)
		    (or file-list-match-history "match")))
      (if (featurep 'xemacs)
	  ;; xemacs
	  (with-output-to-temp-buffer
	      display-buffer 
	    (display-completion-list
	     display-list
	     ;; :activate-callback 'file-list-choose-file
	     :help-string file-list-display-help-string
	     :completion-string
	     match-info-string))
	;; emacs
	(with-output-to-temp-buffer
	    display-buffer 
	  (display-completion-list display-list))
	(let ((buffer-read-only nil))
	  (when (re-search-forward "\\(Possible completions are:\\)" nil t)
	    (delete-region (point-min) (point)))
	  (insert match-info-string)))
      ;; (replace-match match-info-string nil t))))
      (switch-to-buffer display-buffer)
      ;; (when (= file-list-display-level 1))
      (file-list-completion-mode)
      (setq file-list-current-file-list file-list)
      (file-list-beginning-of-file-list))))

;; (file-list-switch-to-file-list)))))


(defun file-list-quit (&optional force)
  "Close file-list window and switch to another buffer."
  (interactive)
  (cond (file-list-completion-mode
	 (when (or force (not (one-window-p)))
	   (delete-window))	  
	 (switch-to-buffer (other-buffer)))
	(file-list-mode
	 (superman-view-back))
	(t (error "Not in file-list"))))

; completition
; --------------------------------------------------------------------

(defun file-list-completion-mode ()
  (interactive)
  ;; (when (string= file-list-display-buffer (buffer-name))
  ;; (when (string-match "file-list" (buffer-name))
  (kill-all-local-variables)
  (use-local-map file-list-mode-map)
  (setq major-mode 'file-list-completion-mode)
  (setq mode-name "file-list-display")
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'file-list-mode)
  (make-local-variable 'file-list-completion-mode)
  (make-local-variable 'file-list-current-file-list)
  (make-local-variable 'file-list-filter)
  (make-local-variable 'file-list-match-history)
  (setq font-lock-defaults
	'(file-list-font-lock-keywords t nil ((?' . ".")))
	file-list-completion-mode t
	file-list-mode t)
  (turn-on-font-lock)
  (run-hooks 'file-list-completion-mode-hook))

(defvar file-list-completion-mode nil "Variable to indicate file-list-completion-mode")
(defvar file-list-mode nil "Variable to indicate file-list-mode")

;; this was not a good idea:
;; (add-hook 'completion-setup-hook 'file-list-completion-mode)

(defun file-list-switch-to-file-list ()
  (interactive)
  (let (w)
    (when (setq w (get-buffer-window file-list-display-buffer t))
      (select-window w)
      (file-list-beginning-of-file-list)
      (file-list-completion-mode))))
	
(defun file-list-toggle-display-mode ()
  "Toggles display between the 4 stages
 (1) filename
 (2) /path/to/filename
 (3) /path/to/filename with attributes and font-lock
 (4) [[~/path/to/filename]] i.e. as orgmode link
"
  (interactive)
    (setq file-list-display-level
	  (cond ((= file-list-display-level 3) 2)
		((= file-list-display-level 2) 1)
		((= file-list-display-level 1) 0)
		((= file-list-display-level 0) 3)))
    (file-list-display-match-list file-list-current-file-list))


(defun file-list-clear-display ()
  (interactive)
  (when (file-list-current-display-buffer)
    (dolist (entry file-list-current-file-list)
      (when (> (length (cdr entry)) 1)
	(setcdr entry (list (car (cdr entry))))))
    (file-list-display-match-list file-list-current-file-list)))
    

(defun file-list-beginning-of-file-name ()
  "Find the beginning of the file-name at point"
  (unless (and (looking-at "[^ \t\n]")
	       (save-excursion
		 (and
		  (progn (backward-char 1) (looking-at "[ \t\n]"))
		  (progn (backward-char 1) (not (looking-at "\\\\"))))))
    (let ((found nil)
	  (pmin (- (save-excursion
		     (file-list-beginning-of-file-list)) 1)))
      (if (and (= file-list-display-level 2)
	       (save-excursion (beginning-of-line) (looking-at "[ \t\n]+")))
	  (re-search-backward "^[^ \t\n]" pmin t)
	(skip-chars-backward " \t\n")
	(re-search-backward "[ \t\n]" pmin t)
	(while (and (not found) (not (bobp)))
	  (backward-char 1)
	  (if (looking-at "\\\\")
	      (re-search-backward "[ \t\n]" pmin t)
	    (forward-char 1)
	    (setq found 'yes)))
	(skip-chars-forward "\t\n ")))))

(defun file-list-find-end-of-file-name ()
  "Find the end of file-name.
Works only if the point is at the beginning of a file-name.
Returns the point at the end of the file-name."
  (let (found)
    (skip-chars-forward "^ \t\n")
    (while (and (not found) (not (eobp)))
      (if (save-excursion
	    (backward-char 1)
	    (looking-at "\\\\"))
	  (progn
	    (skip-chars-forward " \t\n")
	    (skip-chars-forward "^ \t\n"))
	(setq found 'yes)))
    (point)))

(defun file-list-end-of-file-name ()
  "Goto end of file-name. Works only if the point is at the beginning of a file-name."
  (file-list-beginning-of-file-name)
  (goto-char (file-list-find-end-of-file-name)))


(defun file-list-file-at-point (&optional exists-p)
  "Return the absolute file-name at point and error if there is none."
  (let (fname)
    (cond (file-list-completion-mode
	   (if (= file-list-display-level 0)
	       (error "No absolute filename at point! Command needs absolute file names (toggle is on `t' or M-x file-list-toggle-display-mode)"))
	   (save-excursion
	     (file-list-beginning-of-file-name)
	     (let ((fbeg (point))
		   (fend (file-list-find-end-of-file-name)))
	       (if (featurep 'xemacs)
		   (setq fname (buffer-substring fbeg fend))
		 (setq fname (buffer-substring-no-properties fbeg fend)))
	       (setq fname (file-list-replace-in-string fname "\\\\" "")))))
	  (file-list-mode
	   (or (setq fname (get-text-property (point-at-bol) 'filename))
	       (let ((pos (previous-single-property-change (point)  'filename)))
		 (when pos
		   (setq fname (get-text-property (previous-single-property-change pos 'filename) 'filename))))))
	  (t (error (format "Works only in %s buffers." file-list-display-buffer))))
    (if (not fname) (error "No absolute filename at point!")
      (if (or (not exists-p)
	      (file-exists-p fname))
	  fname
	(error "file %s does not exist on disk" fname)))))
	

(defun file-list-end-of-file-list ()
  "Return the point of the end of displayed file-list."
  (when file-list-mode
    ;; (set-buffer file-list-display-buffer)
    (goto-char (point-max))
    (re-search-backward "^[^ \t\n]" nil t)
    (end-of-line)
    (point)))

(defun file-list-beginning-of-file-list ()
  "Return the point of the beginning of displayed file-list."
  (interactive)
  (when file-list-mode
    (goto-char (point-min))
    (re-search-forward "^current file-list:[\n]+" nil t)
    (re-search-forward "^[/a-zA-Z]+" nil t)
    (beginning-of-line)
    (point)))

(defun file-list-next-file (arg)
  "Move to the next item in the file list.
With prefix argument arg, move arg items (negative arg means move backward)."
  (interactive "p")
  (let ((beg (point-min)) (end (point-max)))
    (while (and (> arg 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'filename)
	(goto-char (next-single-property-change (point) 'filename nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'filename)
	(goto-char (next-single-property-change (point) 'filename nil end)))
      (setq arg (1- arg)))
    (while (and (< arg 0) (not (bobp)))
      (let ((prop (get-text-property (1- (point)) 'filename)))
	;; If in a completion, move to the start of it.
	(when (and prop (eq prop (get-text-property (point) 'filename)))
	  (goto-char (previous-single-property-change
		      (point) 'filename nil beg)))
	;; Move to end of the previous completion.
	(unless (or (bobp) (get-text-property (1- (point)) 'filename))
	  (goto-char (previous-single-property-change
		      (point) 'filename nil beg)))
	;; Move to the start of that one.
	(goto-char (previous-single-property-change
		    (point) 'filename nil beg))
	(setq arg (1+ arg))))))


(defun file-list-previous-file (arg)
  "Move to the previous item in file-list.
With prefix argument arg, move arg items backward."
  (interactive "p")
  (file-list-next-file (- arg)))
  
(defun file-list-nth-in-list (entry file-list)
  "Return the position of entry in file-list."
  (let ((ename (if (stringp entry) entry (file-list-make-file-name entry))))
    (if (string= ename (file-list-make-file-name (car file-list)))
	0
      (let ((rest (cdr file-list))
	    (pos 1))
	(while 
	    (not
	     (cond ((string= (file-list-make-file-name (car rest)) ename)
		    pos)
		   (rest (setq rest (cdr rest)
			       pos (+ pos 1))
			 nil)
		   (t (setq pos nil)
		      t))))
	pos))))


;; high-lighting 
;; --------------------------------------------------------------------
(defvar file-list-font-lock-keywords
  '(("\\(/[^\t\n ]+/\\)\\([^/\t\n]+\\)" ;; file-list-file-name-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    ("^ +\\(.*\\) : \\(.*\\)$";;file-list-info-regexp
     (1 font-lock-reference-face t)
     (2 font-lock-string-face t)))
  "Font Lock regexp for file-list-completion-list.")

(provide 'superman-display-file-list)
;;; superman-display-file-list.el ends here

