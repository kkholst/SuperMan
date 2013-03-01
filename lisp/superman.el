;;; superman.el --- org project manager

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

;; A super project to manage all the other projects
;; Q: Does the super project contain itself?
;; A: Nice question. To find some answers please read logicomix. Get it here: www.logicomix.com

;;; Code:

;;{{{

(defvar superman '(("SuperMan" ("location" . (file-name-directory superman-home))
		   ("index" . superman-home)
		   ("category" . nil)
		   ("config" . nil)
		   ("state" . "ACTIVE"))))

;; (defun superman-super-config ()
  ;; (superman-save-config

;; (defun superman ()
  ;; "Returns a super project for project management"
  ;; `("SuperManager"
    ;; (("location" . ,superman-default-directory)
     ;; ("index" . ,superman-file)
     ;; ("category" . "Super")
     ;; ("state" . "ACTIVE")
     ;; ("config" . "INDEX | AGENDA / TODO"))))

(defun superman-set-property ()
  (interactive)
  (let* ((prop-list '(((superman-property 'location) . nil) ((superman-property 'index) . nil) ((superman-property 'category) . nil) ((superman-property 'others) . nil) ((superman-property 'publishdirectory) . nil)))
	 (prop (completing-read "Set property: " prop-list))
	 (pom (org-get-at-bol 'org-hd-marker))
	 (curval (org-entry-get pom prop))
	  ;; (if  (completing-read (concat "Value for " prop ": ")
	 (val (read-string (concat "Value for " prop ": ") curval)))
    (org-entry-put pom prop val))
  (org-agenda-redo))

(defun org-superman-return ()
  (interactive)
  (let ((pro (assoc
	      (superman-property-at-point
	       (superman-property 'nickname) nil)
	      superman-project-alist)))
    (superman-switch-to-project 'force pro)))

(defun superman-get-todo-face (str)
  (cond ((string-match "TODO" str ) 'org-todo)
	(t 'org-done)))

(defun superman-finalize-superman ()
  (save-excursion
    (let* ((superman-balls
	    '((todo nil (9) superman-get-todo-face nil (27))
	      (hdr nil (19))
	      ("LastVisit" superman-trim-date nil)
	      ("Others" superman-trim-string (30))
	      ;; ("Location" superman-trim-filename 23)
	      ))
	   (start (progn (goto-char (point-min))
			 (next-single-property-change
			  (point-at-eol) 'org-marker)))
	   (buffer-read-only nil))
      (when start 
	(goto-char start)
	(superman-loop 'superman-format-item superman-balls)
	;; Title, columns and highlight
	(goto-char (point-min))
	(end-of-line)
	(if (next-single-property-change
	     (point-at-eol) 'org-marker)
	    (let ((cols
		   (apply
		    'superman-column-names
		    (list (list "Status" "Title" "LastVisit" "Others" "Location")
			  superman-balls))))
	      (progn
		;; (beginning-of-line)
		(insert "\n")
		(insert (car cols))
		(insert "\n")
		(beginning-of-line 0)
		(put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-comment-face)
		(goto-char (point-min))
		(put-text-property (point-at-bol) (point-at-eol) 'columns (cadr cols))
		(put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)))))
      ;; keys
      (end-of-line)
      ;; (point)
      (insert "\n\nKeys: ")
      ;; (point)
      (put-text-property (point) (length "Keys: ") 'face 'org-level-2)
      (end-of-line)
      (insert "N: new project RET: select project\n")
      ;;
      (insert "\n** Projects:\n")
      (put-text-property (point) (length "Keys: ") 'face 'org-level-2)      
      ;; facings
      (save-excursion
	(goto-char (point-min))
	(while (or (org-activate-bracket-links (point-max)) (org-activate-plain-links (point-max)))
	  (add-text-properties
	   (match-beginning 0) (match-end 0)
	   '(face org-link)))))
    (superman-on)))


(defun superman-count-projects-in-bloke (beg end)
  (save-excursion
    (let ((n 0))
      (goto-char beg)
      (while (and (< (point) end) (next-single-property-change (point-at-eol) 'org-marker))
	(goto-char (next-single-property-change (point-at-eol) 'org-marker))
	(end-of-line)
	(setq n (+ 1 n)))
      n)))

(defun superman-count-projects ()
  (superman-structure-loop
   'superman-count-projects-in-bloke nil))

;;}}}
;;{{{ superman 

;; FIXME: use gnus-user-date-format-alist to trim date
;; (defun superman-format (hdr level category tags-list prop-list)
  ;; (concat " " (superman-trim-string hdr 20)
	  ;; (let ((cprops prop-list)
		;; (pstring ""))
	    ;; (while cprops
	      ;; (let ((val (cdr (car cprops))))
		;; (cond ((string= (downcase (caar cprops)) "filename")
		       ;; (setq val (file-name-nondirectory (org-link-display-format val)))))
		;; (setq pstring (concat pstring "  " (superman-trim-string val  23))))
		;; (setq cprops (cdr cprops)))
	      ;; pstring) "\t"))


(setq org-agenda-show-inherited-tags (list))

(defun superman ()
  "Manage projects."
  (interactive)
  (let* ((org-agenda-window-setup 'current-window)
	 (org-agenda-buffer-name (concat "*S*"))
	 (org-agenda-sticky nil)
	 (org-agenda-custom-commands
	  `(("S" "Superman"
	     ;; ((tags "NickName={.+}"
	     ((search "NickName"
		      ((org-agenda-files (quote (,superman-home)))
		       (org-agenda-property-list '("NickName" "LastVisit" "Location" "Others"))
		       (org-agenda-overriding-header
			(concat "SuperMan(ager)"))
		       (org-agenda-window-setup 'current-window)
		       (org-agenda-view-columns-initially nil)
		       (org-agenda-buffer-name "*S*"))))
	     ((org-agenda-finalize-hook 'superman-finalize-superman)
	      ;; (org-agenda-cmp-user-defined (lambda (a b) (if  ))
	      (org-agenda-sorting-strategy '(user-defined-up))
	      (org-agenda-buffer-name "*S*"))))))
    (push ?S unread-command-events)
    (call-interactively 'org-agenda)))



(defun superman-bloke-view ()
  "Manage projects."
  (interactive)
  (let* ((blokes (mapcar 'car
			 (save-window-excursion
			   (find-file superman-home)
			   org-todo-kwd-alist)))
	 (bloke-number-one (car blokes))
	 (org-agenda-finalize-hook 'superman-finalize-superman)
	 (org-agenda-window-setup 'current-window)
	 (org-agenda-buffer-name (concat "*S*"))
	 (org-agenda-sticky nil)
	 (header-start 
	  (concat "?: help, n: new project, RET: choose project"
		  "\n\nProjects: " "\n"))
	 (shared-header "")
	 (cmd-block
	  (mapcar '(lambda (bloke)
		     (list 'todo bloke
			   (let ((hdr (if (eq bloke bloke-number-one)
					  (concat header-start "\n* " bloke "" shared-header)
					(concat "* " bloke ""))))
			     `((org-agenda-overriding-header ,hdr)))))
		  blokes))
	 (org-agenda-custom-commands
	  `(("S" "Superman"
	     ,cmd-block
	     ;; commands for all blokes
	     ((org-agenda-finalize-hook 'superman-finalize-superman)
	      (org-agenda-block-separator superman-document-category-separator)
	      (org-agenda-view-columns-initially nil)
	      (org-agenda-buffer-name (concat "*Superman*"))
	      (org-agenda-files (quote (,superman-home))))
	     (org-agenda-buffer-name "*Superman*")))))
    (push ?S unread-command-events)
    (call-interactively 'org-agenda)))


;;}}}

;;{{{ cycle view 

(defvar superman-views nil)
(setq superman-views (list 'S 'S-todo 'S-todo-B 'S-todo-C 'S-agenda))

(defun superman-change-view  (&optional arg)
  (interactive "p")
  ;; cycle view list
  (when (eq major-mode 'org-agenda-mode)
    (let ((current  (car superman-views))
	  (rest  (cdr superman-views)))
      (setq superman-views rest)
      (add-to-list 'superman-views current 'append)))
  (eval `(,(car superman-views)))
  (superman-on))
  
(defalias 'S 'superman)

(defun S-todo ()
  (let ((org-agenda-buffer-name (concat "*S-todo*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands
	 `(("P" "Projects-TODO neither B or C"
	    ,(mapcar '(lambda (cat)
			;; (list 'todo "TODO"
			(list 'tags-todo "PRIORITY<>\"C\"+PRIORITY<>\"B\""
			      `((org-agenda-overriding-header  (concat "Project category: ",cat))
				(org-agenda-files (quote ,(superman-index-list cat))))))
		     (superman-parse-project-categories))
	    ((org-agenda-window-setup 'current-window)
	     (org-agenda-finalize-hook '(lambda () (superman-clean-up) (superman-on))))))))
    (push ?P unread-command-events)
    (call-interactively 'org-agenda)))
;; (when (get-buffer "*S-todo*")
;; (kill-buffer "*S-todo*"))
;; (rename-buffer "*S-todo*"))

(defun S-agenda ()
  (let ((org-agenda-buffer-name (concat "*S-agenda*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands nil))
    (add-to-list 'org-agenda-custom-commands
		 '("A" "Superman agenda"
		   ((agenda ""
			    ((org-agenda-files
			      (superman-index-list)))))
		   ((org-agenda-compact-blocks nil)
		    (org-agenda-show-all-dates nil)
		    (org-agenda-window-setup 'current-window)
		    (org-agenda-overriding-header "Superman agenda"))))
    (push ?A unread-command-events)
    (call-interactively 'org-agenda)))

(defun S-todo-B ()
  (let ((org-agenda-buffer-name (concat "*S-todo-B*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands
	 `(("B" "Projects-TODO class B"
	    ,(mapcar '(lambda (cat)
			;; (list 'todo "TODO"
			(list 'tags-todo "PRIORITY=\"B\""
			      `((org-agenda-overriding-header  (concat "Project category: ",cat))
				(org-agenda-files (quote ,(superman-index-list cat))))))
		     (superman-parse-project-categories))
	    ((org-agenda-window-setup 'current-window)
	     (org-agenda-finalize-hook 'superman-clean-up))))))
    (push ?B unread-command-events)
    (call-interactively 'org-agenda)))

(defun S-todo-C ()
  (let ((org-agenda-buffer-name (concat "*S-todo-C*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands
	 `(("C" "Projects-TODO class C"
	    ,(mapcar '(lambda (cat)
			;; (list 'todo "TODO"
			(list 'tags-todo "PRIORITY=\"C\""
			      `((org-agenda-overriding-header  (concat "Project category: ",cat))
				(org-agenda-files (quote ,(superman-index-list cat))))))
		     (superman-parse-project-categories))
	    ((org-agenda-window-setup 'current-window)
	     (org-agenda-finalize-hook 'superman-clean-up))))))
    (push ?C unread-command-events)
    (call-interactively 'org-agenda)))

;;}}}
;;{{{ superman-mode-map

(defvar superman-mode-map (make-sparse-keymap)
  "Keymap used for `superman-mode' commands.")
   
(define-minor-mode superman-mode 
  "Toggle org projectmanager document superman mode.
With argument ARG turn superman-mode on if ARG is positive, otherwise
turn it off.

Enabling superman mode electrifies the superman buffer for project management."
     :lighter " S"
     :group 'org
     :keymap 'superman-mode-map)

(defun superman-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-mode t))


(defun superman-new-project ()
  (interactive)
  (save-window-excursion
    (superman-new-project))
  (org-agenda-redo))

(defun superman-clean-up ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^Project category:" nil t)
      (when (progn (forward-line 1) (looking-at "^[ \t]*$"))
	(kill-line 2)
	(kill-line -1)))))

(define-key superman-mode-map [return] 'org-superman-return) ;; Return is not used anyway in column mode
(define-key superman-mode-map "N" 'superman-new-project)
;; (define-key superman-mode-map [(f1)] 'superman-switch-to-project)
;; (define-key superman-mode-map " " 'superman-switch-to-project)
(define-key superman-mode-map "S" 'superman-set-property)
(define-key superman-mode-map "V" 'superman-change-view)
(define-key superman-mode-map "?" 'superman-show-help)

(defun superman-show-help ()
  (interactive)
  (let ((msg
	(concat 
	 "------------------\n"
	"[return]:\t\t Open project at point\n"
	"[n]:     \t\t New project\n"
	"[space]: \t\t Switch to project\n"
	 "------------------\n")))
    (funcall superman-help-fun msg)))

;;}}}  
(provide 'superman)
;;; superman.el ends here

