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

(setq superman-balls
      '((todo ("trim" nil (9)) ("face" superman-get-todo-face))
	(hdr ("trim" nil (27)) ("face" font-lock-function-name-face))
	("lastvisit" ("trim" superman-trim-date nil)
	 ("face" font-lock-type-face))
	("others"
	 ("trim" superman-trim-string (30))
	 ("face" font-lock-keyword-face))))

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

(defun superman-return ()
  (interactive)
  (let ((pro (assoc
	      (superman-property-at-point
	       (superman-property 'nickname) nil)
	      superman-project-alist)))
    (superman-switch-to-project 'force pro)))

(defun superman-get-todo-face (kwd)
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

;; (:foreground "blue" :weight bold :underline t)
;; (if (member kwd org-done-keywords-for-agenda) 'org-done
;; 'org-todo))

;;}}}
;;{{{ superman 

(defun superman-make-header ()
  "Insert header into superman project view buffer"
  (goto-char (point-min))
  (insert "SuperMan(ager)")
  (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd '(S))
  (put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-1)
  (put-text-property (point-at-bol) (point-at-eol) 'index superman-home)
  (put-text-property (point-at-bol) (point-at-eol) 'nickname "Kal-El")
  (insert "\n\nKeys: \n")
  (forward-line -1)
  ;; (put-text-property (point) (length "Keys: ") 'face 'org-level-2)
  (end-of-line)
  (insert "N: new project RET: select project")
  (put-text-property (point-at-bol) (+ (point-at-bol) (length "Keys: ")) 'face 'org-level-2))


(defun superman-categorize-projects (&optional cats balls)
  "Parse the file `superman-home' and return a categorized project-alist."
  (interactive)
  (save-excursion
    (setq superman-project-alist nil)
    (set-buffer (find-file-noselect superman-home))
    (unless (superman-manager-mode 1))
    (save-buffer)
    (goto-char (point-min))
    (while (superman-forward-project)
      (let* ((loc (or (superman-get-property nil (superman-property 'location) 'inherit) superman-default-directory))
	     (category (superman-get-property nil (superman-property 'category) 'inherit))
	     (others (superman-get-property nil (superman-property 'others) nil))
	     (publish-dir (superman-get-property nil (superman-property 'publish) 'inherit))
	     (name (or (superman-get-property nil (superman-property 'nickname) nil)
		       (nth 4 (org-heading-components))))
	     (marker (copy-marker (point)))
	     (hdr  (org-get-heading t t))
	     (lastvisit (superman-get-property nil "LastVisit" 'inherit))
	     (config (superman-get-property nil (superman-property 'config) 'inherit))
	     (todo (substring-no-properties (or (org-get-todo-state) "")))
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
    superman-project-alist))

(defun superman ()
  (interactive)
  (let* ((cats-buffer "*S*")
	 (cats (superman-parse-project-categories))
	 (cat-alist (mapcar (lambda (x) (list x)) cats))
	 (howmany-cats (length cats))
	 (cat-number-one (car cats))
	 (projects superman-project-alist))
    (switch-to-buffer cats-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (org-mode)
    (font-lock-mode -1)
    (superman-make-header)
    (goto-char (point-max))
    ;; parse projects by category using superman-balls
    (while projects
      (let* ((pro (car projects))
	     (cat (cdr (assoc "category" (cadr pro))))
	     (m (- howmany-cats (length (member cat cats))))
	     (tail (cdr (nth m cat-alist))))
	(if tail
	    (setcdr (nth m cat-alist) (append tail (list pro)))
	  (setcdr (nth m cat-alist) (list pro))))
      (setq projects (cdr projects)))
    ;; loop over categories
    (while cat-alist
      (let* ((cat (car cat-alist))
	     (cat-name (car cat))
	     (tail (cdr cat)))
	;; sort projects by lastvisit date
	;; see http://emacswiki.org/emacs/DestructiveOperations
	(setq tail
	      (sort tail
		    (lambda (p q)
		      (org-time<=
		       (or (superman-get-lastvisit p) "<1971-09-13 Mon 08:55>")
		       (or (superman-get-lastvisit q) "<1971-09-13 Mon 08:55>")))))
	(insert "\n** " cat-name " [" (int-to-string (length tail)) "]")
	(put-text-property (point-at-bol) (point-at-eol) 'face 'org-level-2)
	(put-text-property (point-at-bol) (point-at-eol) 'cat 'cat-name)
	(put-text-property (point-at-bol) (point-at-eol) 'display (concat "★ " cat-name))
	;; loop over projects (tail) in category
	(insert "\n")
	(superman-format-loop tail superman-balls)
	;; column names
	(org-back-to-heading)
	(end-of-line)
	(when (next-single-property-change (point-at-eol) 'org-marker)
	  (goto-char (next-single-property-change (point-at-eol) 'org-marker))
	  (forward-line -1)
	  (end-of-line)
	  (insert "\n")
	  (let ((cols (superman-format-thing
		       '("columns"
			 (("others" . "Others")
			  (hdr . "Title")
			  (todo . "State")
			  ("marker" . nil)
			  ("lastvisit" . "LastVisit")))
		       superman-balls)))
	    (insert cols)
	    (set-text-properties (point-at-bol) (point-at-eol) 'face nil)
	    (put-text-property (point-at-bol) (point-at-eol) 'face 'font-lock-comment-face)))
	(org-back-to-heading)
	(goto-char (point-max))
	(setq cat-alist (cdr cat-alist)))))
  (goto-char (point-min))
  (superman-view-mode-on)
  (superman-on)
  (setq buffer-read-only t))


(defun superman-format-loop (list balls)
  "Loop over list and insert all items formatted according to balls."
  (while list
    (let* ((item (superman-format-thing
		  (car list)
		  balls)))
      (insert item)
      (end-of-line)
      (insert "\n")
      (setq list (cdr list)))))

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
  (superman-view-mode-on)
  (superman-on))
  
(defalias 'S 'superman)

(defun S-todo ()
  (let ((org-agenda-buffer-name (concat "*S-todo*"))
	(org-agenda-sticky nil)
	(org-agenda-custom-commands
	 `(("P" "Projects-TODO neither B or C"
	    ,(mapcar #'(lambda (cat)
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
	    ,(mapcar #'(lambda (cat)
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
	    ,(mapcar #'(lambda (cat)
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

(defun superman-visit-project ()
  (interactive)
  (let* ((pom (get-text-property (point-at-bol) 'org-marker))
	(home superman-home)
	(ibuf (if pom (marker-buffer pom)
		(get-file-buffer home)))
	(iwin (when ibuf (get-buffer-window ibuf nil))))
    (if (and ibuf iwin)
	(select-window (get-buffer-window ibuf nil))
      (split-window-vertically)
      (other-window 1)
      (if ibuf (switch-to-buffer ibuf)
	(find-file home)))
    (when pom (goto-char pom))))
		 
(define-key superman-mode-map [return] 'superman-return) ;; Return is not used anyway in column mode
;; (define-key superman-mode-map "N" 'superman-new-project)
;; (define-key superman-mode-map [(f1)] 'superman-switch-to-project)
;; (define-key superman-mode-map " " 'superman-switch-to-project)
;; (define-key superman-mode-map "S" 'superman-set-property)
(define-key superman-mode-map "i" 'superman-visit-project)
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

