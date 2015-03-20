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
      (superman-display-file-list dir
				  nil nil nil
				  nil nil nil
				  nil nil
				  ))))

(defun superman-file-list (project &optional ext)
  "List files in project's location that match extension EXT"
  (if (featurep 'superman-file-list)
      (let ((dir (superman-project-home project)))
	(cond ((file-list-select-internal nil (or ext ".")
					  nil nil dir (concat "*File-list-" (car project) "*")))
	      (t
	       (switch-to-buffer (concat "*File-list-" (car project) "*"))
	       (toggle-read-only -1)
	       (erase-buffer)
	       (insert "FILE-LIST: No files in project"))))
    (error "superman-file-list.el not loaded.")))

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

(defun superman-file-capture-button (c &rest args)
  (superman-make-button "track"
			'superman-capture-file-at-point
			'superman-next-project-button-face
			"Capture file for project control"))

(defvar superman-file-list-balls
  '(("FileName" ("fun" superman-trim-string) ("width" 88) ("face" font-lock-variable-name-face))
    ("Path" ("fun" superman-dont-trim) ("face" font-lock-keyword-face)))
    ;; ("Capture" ("fun" superman-file-capture-button) ("width" 7) ("face" "no-face") ("face" superman-next-project-button-face))
  "balls used to display file-lists.")

  
(defun superman-display-file-list (dir &optional list filter sort balls ext view-buffer refresh no-project)
  "Format file-list displays."
  (let* ((project (if no-project nil
		    (ignore-errors (superman-view-current-project))))
	 (nick (if project (car project) (or (get-text-property (point-min) 'nickname) "ff")))
	 (pbuf (if file-list-mode
		   (get-text-property (point-min) 'project-buffer)
		 (buffer-name)))
	 (dir (or dir (get-text-property (point-min) 'dir)
		  (when project
		    (superman-project-home project))
		  (error "Missing location")))
	 ;; (balls (or balls superman-default-balls))
	 (view-buf (or view-buffer (get-buffer-create (concat "*FileList: " nick "*"))))
	 (list (or list
		   (with-current-buffer view-buf
		     (if refresh
			 (file-list-select-internal
			  nil (or ext ".")
			  nil nil dir
			  (concat "*File-list-" (or nick "nix") "*") 'dont)
		       (or list
			   file-list-current-file-list
			   (file-list-select-internal
			    nil (or ext ".")
			    nil nil dir (concat "*File-list-" (or nick "nix") "*") 'dont))))))
	 (balls (or balls superman-file-list-balls))
	 (count 0)
	 ;; (cycle file-list-display-level)
	 cycle)
    (when (= file-list-display-level 0)
      (let ((maxfile (apply 'max (mapcar #'(lambda (x) (length (car x))) list))))
	(setcdr (assoc "width" (assoc "FileName" balls)) `(,maxfile))))
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
			 (concat ": " dir))
		 'file-list-redisplay
		 'superman-project-button-face
		 "Refresh file-list"))
	(unless no-project
	  (put-text-property (point-at-bol) (point-at-eol) 'nickname nick)
	  (put-text-property (point-min) (1+ (point-min)) 'project-buffer pbuf)
	  (put-text-property (point-at-bol) (point-at-eol) 'git-dir (superman-git-toplevel dir)))
	(put-text-property (point-at-bol) (point-at-eol) 'dir dir)
	(put-text-property (point-at-bol) (point-at-eol) 'filter filter)
	(put-text-property (point-at-bol) (point-at-eol) 'sort sort)
	(unless no-project
	  (put-text-property (point-at-bol) (point-at-eol) 'index (superman-get-index project))
	  (insert "  " (superman-make-button "Project view" 'superman-view-back 'superman-next-project-button-face  "Back to project view.")
		  "  " (superman-make-button "Git" 'superman-display-git-cycle 'superman-next-project-button-face "Control project's git repository.")
		  "  " (superman-make-button "Todo" 'superman-project-todo 'superman-next-project-button-face "View project's todo list.")
		  "  " (superman-make-button "Time-line" 'superman-project-timeline 'superman-next-project-button-face "View project's timeline.")))
	(insert "\n")
	(superman-view-insert-action-buttons
	 `(("name"  (lambda () (interactive)
		      (if superman-reverse-filter (file-list-by-path 1) (file-list-by-path)
			  nil "Filter files based on matching against name")))
	   ("path"  (lambda () (interactive)
		      (if superman-reverse-filter (file-list-by-path 1) (file-list-by-path)
			  nil "Filter files based on matching against directory name")))
	   ("time"  (lambda () (interactive)
		      (if superman-reverse-filter (file-list-by-time 1) (file-list-by-time)
			  nil "Filter files based on last modification time")))
	   ("size" (lambda () 
		     (if superman-reverse-filter (file-list-by-size 1) (file-list-by-size)
			 "Filter files based on size in bytes"))))
	 nil
	 (superman-make-button "Filter:" 'superman-reverse-filter 'superman-filter-face))
	(superman-view-insert-action-buttons
	 `(("name" (lambda () (interactive) 
		     (if superman-reverse-sort
			 (file-list-sort-by-name 1) (file-list-sort-by-name) nil "Sort by file-name")))
	   ("path" (lambda () (interactive) 
		     (if superman-reverse-sort
			 (file-list-sort-by-path 1) (file-list-sort-by-path) nil "Sort by path")))
	   ("time" (lambda () (interactive)
		     (if superman-reverse-sort
			 (file-list-sort-by-time 1) (file-list-sort-by-time) nil "Sort by time")))
	   ("size" (lambda () (interactive)
		     (if superman-reverse-sort
			 (file-list-sort-by-size 1) (file-list-sort-by-size) nil "Sort by size"))))
	 nil
	 (superman-make-button " Sort :" 'superman-reverse-sort 'superman-sort-face))
	(superman-view-insert-action-buttons
	 '(("attributes" file-list-ls nil "Show file attributes")
	   ("grep" file-list-grep nil "Run grep on all files")
	   ("shell-command" file-list-shell-command nil "Run the same shell command on all files")
	   ("copy" file-list-copy nil "Copy all files to a new directory")
	   ("remove" file-list-remove nil "Delete all files")
	   ("query-replace" file-list-query-replace nil "Run interactive query replace through all files")
	   ("delete" file-list-remove nil "Remove all files")
	   ("update" file-list-redisplay
	    nil "Update file-list")) nil "Action:")
	;; ("clear display" file-list-clear-display)) 
	(insert "\n* ")
	(put-text-property (point-at-bol) (point-at-eol) 'file-list-start t)
	(insert "\n")
	(while list
	  (let* ((el (car list))
		 (file (car el))
		 (path (cadr el))
		 (rest (caddr el))
		 (line1 (cond ((= file-list-display-level 1)
			       (put-text-property 0 (length file) 'face font-lock-variable-name-face file)
			       (put-text-property 0 (length path) 'face font-lock-keyword-face path)
			       (superman-format-thing
				`("el" (("File" . ,(concat path file))))
				'(("File" ("fun" superman-dont-trim))) ;; balls
				'no-face))
			      ((= file-list-display-level 2)
			       (put-text-property 0 (length file) 'face font-lock-variable-name-face file)
			       (put-text-property 0 (length path) 'face font-lock-keyword-face path)
			       (superman-format-thing
				`("el" (("File" . ,(concat "[[" path file "]]"))))
				'(("File" ("fun" superman-dont-trim))) ;; balls
				'no-face))
			      (t
			       (superman-format-thing
				`(list (("FileName" . ,file)
					("Path" . ,path))) balls))))
		 appendix)
	    (insert line1)
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
	 dir
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
			  "Press button to remove this filter"))))
	;; insert the column names
	(insert "\n")
	(when superman-empty-line-after-cat (insert "\n"))
	(if (member file-list-display-level (list 1 2))
	    (insert (superman-column-names '(("File" ("fun" superman-dont-trim)))))
	  (insert (superman-column-names balls)))
	(beginning-of-line)
	(run-hooks 'superman-file-list-pre-display-hook)))))

(defvar superman-reverse-filter t "If true negate filter")
(defun superman-reverse-filter ()
  (interactive)
  (setq superman-reverse-filter (not superman-reverse-filter))
  (if superman-reverse-filter
      (progn
	(set-face-background 'superman-filter-face "DarkOrange1")
	(set-face-foreground 'superman-filter-face "black"))
    (set-face-background 'superman-filter-face "gray33")
    (set-face-foreground 'superman-filter-face "DarkOrange1")))
(defface superman-filter-face
  '((t (:inherit superman-default-button-face
		 :foreground "DarkOrange1"
		 :height 1.0
		 :background "gray33"
		 )))
  "Face used for reverse filter button."
  :group 'superman)
(defvar superman-reverse-sort t "If true negate sort")
(defun superman-reverse-sort ()
  (interactive)
  (setq superman-reverse-sort (not superman-reverse-sort))
  (if superman-reverse-sort
      (progn
	(set-face-background 'superman-sort-face "orange")
	(set-face-foreground 'superman-sort-face "black"))
    (set-face-background 'superman-sort-face "gray")
    (set-face-foreground 'superman-sort-face "orange")))
(defface superman-sort-face
  '((t (:inherit superman-default-button-face
		 :foreground "orange"
		 :height 1.0
		 :background "gray"
		 )))
  "Face used for reverse sort button."
  :group 'superman)

(defun file-list-remove-filter (filter)
  (let ((ff (cdr (assoc filter file-list-filter))))
    (setq file-list-current-file-list
	  (append file-list-current-file-list (car ff)))
    (setq file-list-filter
	  (delete-if '(lambda (x) (string= (car x) filter)) file-list-filter))
    (superman-display-file-list
     (get-text-property (point-min) 'dir)
     file-list-current-file-list
     (get-text-property (point-min) 'filter)
     (get-text-property (point-min) 'sort)
     (get-text-property (point-min) 'balls)
     nil
     (current-buffer)
     'refresh
     (not (get-text-property (point-min) 'project-buffer)))))

  
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
      (set-text-properties 0 (length fname) nil fname)
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

