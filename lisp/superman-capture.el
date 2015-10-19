;;; superman-capture.el --- superman captures stuff

;; Copyright (C) 2013-2014  Klaus Kähler Holst, Thomas Alexander Gerds

;; Authors: Klaus Kähler Holst <kkho@biostat.ku.dk>
;;          Thomas Alexander Gerds <tag@biostat.ku.dk>
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

;; 

;;; Code:

;;{{{ variables and hooks

(defvar superman-setup-scene-hook nil
  "Hook run by `superman-capture-whatever'
just before the capture buffer is given to the user.")

(defvar superman-capture-mode-map (make-sparse-keymap)
  "Keymap used for `superman-view-mode' commands.")
(define-key superman-capture-mode-map  "\C-c\C-c" 'superman-clean-scene)
(define-key superman-capture-mode-map  "q" 'superman-quit-scene)
(define-key superman-capture-mode-map  "\C-x\C-s" 'superman-clean-scene)
(define-key superman-capture-mode-map  "\C-c\C-q" 'superman-quit-scene)

(defvar superman-unison-switches "-ignore 'Regex .*(~|te?mp|rda)$' -ignore 'Regex ^(\\.|#).*'")
      ;; "-ignore 'Regex .*' -ignorenot 'Regexp *.(org|R|tex|Rd)$'")
(defvar superman-unison-cmd "unison-gtk")

;;}}}


;;{{{ superman goto project

(defun superman-goto-project (&optional project heading create end-of leave-narrowed jabber propertystring)
  "Goto project index file call `widen' and then search for HEADING
and narrow the buffer to this subtree. 

If HEADING is not found and CREATE is non-nil create the HEADING.

In both cases it returns the `point-marker' at the beginning of the heading.

If END-OF is non-nil, leave point at the end of the section,
otherwise at the beginning.

If JABBER is non-nil (and CREATE is nil) be talkative about non-existing headings.
"
  (interactive)
  (let* ((pro (superman-get-project project))
	 (index (superman-get-index pro))
	 (head (or heading (read-string "Goto heading: ")))
	 value)
    (cond ((bufferp index)
	   (switch-to-buffer index))
	  (index
	   (progn
	     (unless (file-exists-p (file-name-directory index))
	       (make-directory (file-name-directory index) 'with-parents))
	     (find-file index)))
	  (t (error (concat "Project " (car pro) " does not have an index."))))
    (widen)
    (show-all)
    (goto-char (point-min))
    (setq value
	  (cond ((re-search-forward (format org-complex-heading-regexp-format (regexp-quote head)) nil t)
		 (point-marker))
		(create
		 (let (marker)
		   (goto-char (point-max))
		   (insert "\n")
		   (setq marker (point-marker))
		   (insert "* " head "\n")
		   (when propertystring  
		     (insert ":PROPERTIES:\n")
		     (insert propertystring)
		     (insert "\n:END:\n\n")
		     (forward-line -1))
		   (point-marker)))
		(t (when jabber (message (concat "Heading " head " not found in index file of " (car pro))))
		   nil)))
    (when value 
      (org-narrow-to-subtree)
      (if end-of (goto-char (point-max))
	;; leave point at the first entry or at the end of this section
	(end-of-line)
	(if (outline-next-heading)
	    (beginning-of-line)
	  (goto-char (point-max)))))
    (unless leave-narrowed
      (widen)
      (show-all))
    value))

;;}}}
;;{{{ superman capture

(defun superman-capture (project heading-or-marker object &optional body plist level scene quit-scene clean-hook quit-hook)
  "Superman captures entries, i.e., the contents of an outline-headings,
to be added to the index file of a PROJECT at a given HEADING-OR-MARKER.

HEADING-OR-MARKER can be the name of the outline heading-or-marker which is found
in the project view buffer or a marker pointing to the project index file.

If OBJECT is a string it is used to construct the first line of the capture buffer.

If LEVEL is given it is the level of the new heading (default is `superman-item-level').
LEVEL can be 0 in which case no heading is created.

As a special case, this function is used to capture a new project
for the superman(ager).

If SCENE is a function it is applied by `superman-clean-scene' and
`superman-quit-scene', i.e., at the end of the capture.
If SCENE is nil then the current window configuration is restored.
If QUIT-SCENE is a function or window configuration then it is handled
by `superman-quit-scene' instead of SCENE.

CLEAN-HOOK and QUIT-HOOK are functions that are stored in the capture buffer
in form of text-properties at the point-min and called by `superman-clean-scene'
just before `superman-capture-before-clean-scene-hook'
and by `superman-quit-scene' just before killing the buffer.

See also `superman-capture-whatever' for the other arguments."
  (interactive)
  (let* ((scene (or scene (current-window-configuration)))
	 (what (or object "thing"))
	 (title (superman-make-button
		 (concat "Superman captures "
			 what
			 " for project "
			 (car project))
		 nil 'superman-capture-button-face))
	 (destination (if heading-or-marker
			  (cond ((stringp heading-or-marker)
				 (superman-goto-project
				  project
				  heading-or-marker 'create nil nil nil))
				((markerp heading-or-marker)
				 (progn (switch-to-buffer
					 (marker-buffer heading-or-marker))
					(goto-char heading-or-marker)
					(if (ignore-errors (org-narrow-to-subtree))
					    (progn
					      (end-of-line)
					      (if (outline-next-heading)
						  (beginning-of-line)
						(goto-char (point-max)))
					      (widen)
					      (point-marker))
					  (point-marker)))))
			;; append item to the end of index file
			(find-file (superman-get-index project))
			(widen)
			(show-all)
			(goto-char (point-max))
			(point-marker))))
    (superman-capture-whatever
     destination title level body plist nil scene nil
     quit-scene clean-hook quit-hook)))

(defun superman-capture-whatever (destination title level body plist edit scene read-only &optional quit-scene clean-hook quit-hook)
"This function is the work-horse of `superman-capture' and `superman-view-edit-item'.

DESTINATION is a marker which indicates where to place the result.

TITLE is a string which is shown in the first line of the capture buffer.

PLIST is an alist which contains elements are transformed into property fields. The elements
have the form (key value required) where key can be:

 - a string: the name of a property for the new entry in which case value
   is a inserted as the default value.
 - equal 'body: then value is inserted after the properties. 
 - equal 'fun: then value is called via `funcall'
 - equal 'hdr: then value is used as heading for this capture

If required is non-nil, then the capture will force a value for the field.

If EDIT is non-nil then saving the capture will erase the subtree at
destination before saving the capture.

If READ-ONLY is non-nil then the capture will neither be editable nor savable.

Runs `superman-setup-scene-hook' just before the capture
buffer is handed over. The capture buffer can be left in two
differnent ways:

 - save calls `superman-clean-scene' 
 - quit calls `superman-quit-scene'

See `superman-capture' for the other arguments."
 (let* ((level (or level superman-item-level))
       (kill-whole-line t)
       body-start
       (body (or body ""))
       (S-buf (generate-new-buffer-name "*Capture of SuperMan*"))
       dest-heading)
  (switch-to-buffer S-buf)
  ;; (make-indirect-buffer (current-buffer) S-buf) 'clone)
  (delete-other-windows)
  (org-mode)
  (font-lock-mode -1) 
  (show-all)
  (when (> level 0) (progn
		      (insert "\n"
			      (make-string level (string-to-char "*"))
			      " NIX \n")
		      (forward-line -1))
	(org-narrow-to-subtree)
	(unless (= level 0) (progn 
			      (skip-chars-forward "[* ]")
			      (kill-line))))
  (goto-char (point-min))
  (insert title)
  (when scene  
    (put-text-property (point-min) (1+ (point-min)) 'scene scene))
  (when quit-scene
    (put-text-property (point-min) (1+ (point-min)) 'quit-scene quit-scene))
  (when edit
    (put-text-property (point-min) (1+ (point-min)) 'edit t))
  (put-text-property (point-min) (1+ (point-min)) 'type 'capture)
  (insert "\n\n")
  (unless read-only
    (insert (superman-make-button
	     "Save (C-c C-c)"
	     'superman-clean-scene
	     'superman-next-project-button-face
	     "Save the current body add the destination")
	    "\t"))
  (insert (superman-make-button
	   (if read-only
	       "back (q)"
	     "Cancel (C-c C-q)")
	   'superman-quit-scene
	   'superman-next-project-button-face
	   (if read-only "Back" "Cancel")))
  (insert "\n\n")
  (org-with-point-at destination
    (ignore-errors (setq dest-heading (org-get-heading t t))))
  (if (> (length dest-heading) 25)
      (setq dest-heading (concat (substring dest-heading 0 22) "...")))
  (insert (superman-make-button
	   "Destination:"
	   'superman-change-destination
	   'superman-header-button-face "Change destination")
	  (concat " Line " (int-to-string
			    (org-with-point-at destination
			      (count-lines 1 (point))))
		  (when (stringp dest-heading) (concat " just below " dest-heading))
		  " in buffer " (buffer-name (marker-buffer destination))))
  (put-text-property (point-at-bol) (1+ (point-at-bol)) 'destination destination)
  (insert "\t" (superman-make-button
		"Show context"
		'superman-capture-show-context
		'superman-header-button-face "Show some context around destination"))
  ;; end of non-editable header
  (insert "\n\n")
  (forward-line -1)    
  (put-text-property (point-min) (point-at-eol) 'read-only t)
  (forward-line 1)
  (end-of-line)
  (insert "\n")
  (if (= level 0)
      (goto-char (point-max))
    (end-of-line))
  ;; 
  (setq body-start (point))
  (when (and (> level 0) plist)
    (insert ":PROPERTIES:")
    (while plist
      (let* ((el (car plist))
	     (key (car el))
	     (val (ignore-errors (nth 1 el)))
	     (req (nth 2 el)))
	(cond ((stringp key)
	       (ignore-errors
		 (insert "\n:" key ": ")
		 ;; (put-text-property (point-at-bol) (- (point) 1) 'read-only t)
		 (put-text-property (- (point) 1) (point) 'prop-marker (point))
		 (if req 
		     (put-text-property (- (point) 1) (point) 'required "required-field"))
		 (when val (insert (superman-make-value val)))))
	      ((eq key 'fun) (ignore-errors (funcall (cadr el))))
	      ((eq key 'hdr) (ignore-errors
			       (save-excursion
				 (org-back-to-heading)
				 (end-of-line)
				 (insert (superman-make-value val)))))
	      ((eq key 'body) (setq body (concat body (superman-make-value val)))))
	(setq plist (cdr plist))))
    (insert "\n:END:\n"))
  (when (stringp body) (insert body))
  ;; (put-text-property (point-at-bol) (point-at-eol) 'read-only t)
  (unless (= level 0) (insert "\n"))
  (goto-char body-start) 
  (superman-capture-mode)
  (when read-only
    (setq buffer-read-only t))
  (run-hooks 'superman-setup-scene-hook)
  (let ((inhibit-read-only t))
    (when clean-hook
      (put-text-property (point-min) (1+ (point-min)) 'clean-hook clean-hook))
    (when quit-hook
      (put-text-property (point-min) (1+ (point-min)) 'quit-hook quit-hook)))))


(defun superman-capture-show-context ()
  "Show the destination buffer and position
in other window for this superman-capture."
  (interactive)
  (let ((capture-buffer (buffer-name (current-buffer)))
	(context-marker (get-text-property (point-at-bol)
					   'destination)))
    (switch-to-buffer (marker-buffer context-marker))
    (goto-char (marker-position context-marker))
    (superman-set-config (concat capture-buffer
				 " / " 
				 (buffer-name
				  (marker-buffer context-marker))
				 ))))

(defun superman-change-destination ()
  (interactive)
  (let* ((dest-marker-pos (next-single-property-change (point-min)
						       'destination))
	 dest-heading
	 (inhibit-read-only t)
	 (buffer-read-only nil)
	 (old-marker (when dest-marker-pos
		       (get-text-property dest-marker-pos 'destination))))
    (if (not dest-marker-pos)
	(error "Cannot find current destination.")
      (goto-char dest-marker-pos)
      (let* ((old-file (buffer-file-name (marker-buffer old-marker)))
	     (new-file (read-file-name "New destination in file: "
				       (file-name-directory old-file)
				       nil t nil nil))
	     (new-pos
	      (save-window-excursion
		(let* ((cats (progn (find-file new-file)
				    (when (eq major-mode 'org-mode)
				      (append (superman-parse-cats (current-buffer) 1)
					      (superman-parse-cats (current-buffer) 2)))))
		       (heading (when cats (completing-read (concat
							     "Select target heading in "
							     new-file " (tab to complete): ")
							    cats)))
		       (pos (when heading (car (cdaadr (assoc heading cats))))))
		  (if pos 
		      (goto-char pos)
		    (goto-char (point-max)))
		  (point-marker)))))
	(org-with-point-at new-pos
	  (ignore-errors (setq dest-heading (org-get-heading t t))))
	(if (> (length dest-heading) 25)
	    (setq dest-heading (concat (substring dest-heading 0 22) "...")))
	(goto-char dest-marker-pos)
	(beginning-of-line)
	(kill-whole-line)
	(insert (superman-make-button
		 "Destination:"
		 'superman-change-destination
		 'superman-header-button-face "Change destination")
		(concat " Line " (int-to-string
				  (org-with-point-at new-pos
				    (count-lines 1 (point))))
			(when (stringp dest-heading) (concat " just below " dest-heading))
			" in buffer " (buffer-name (marker-buffer new-pos))))
	(put-text-property (point-at-bol) (1+ (point-at-bol)) 'destination new-pos)
	(insert "\t" (superman-make-button
		      "Show context"
		      'superman-capture-show-context
		      'superman-header-button-face "Show some context around destination"))
	;; end of non-editable header
	(insert "\n")
	(forward-line -1)
	(put-text-property (point-min) (point-at-eol) 'read-only t)))))
      

(define-minor-mode superman-capture-mode
  "Toggle superman capture mode.
With argument ARG turn superman-capture-mode on if ARG is positive, otherwise
turn it off."
  :lighter " *S*-Capture"
  :group 'org
  :keymap 'superman-capture-mode-map)

(defun superman-capture-mode-on ()
  (interactive)
  (when superman-hl-line (hl-line-mode 1))
  (superman-capture-mode t))

(defun superman-make-value (val)
  (cond ((stringp val) val)
	((functionp val) (funcall val))
	((listp val)
	 (let ((thing (car val)))
	   (cond ((stringp thing)
		  (add-text-properties 0 (length thing) (cdr val) thing))
		 ((functionp thing)
		  (funcall thing (cdr val))))))))

(defun superman-clean-scene ()
  "Cleaning the capture scene. First save the capture
at the requested destination and then reset the window configuration."
  (interactive)
  (if buffer-read-only
      (error "Cannot save in read-only mode.")
    (let* ((scene (get-text-property (point-min) 'scene))
	   (kill-whole-line t)
	   (edit (get-text-property (point-min) 'edit))
	   req
	   next
	   catch
	   hidden-hook
	   ;; (capture-start (previous-single-property-change (point-max) 'read-only))
	   (pos-dest (next-single-property-change (point-min) 'destination))
	   (dest (when pos-dest (get-text-property pos-dest 'destination))))
      (goto-char (point-min))
      (while (setq next (next-single-property-change
			 (point-at-eol)
			 'prop-marker))
	(goto-char next)
	;;
	(if (looking-at "[ \t]*\n")
	    (if (setq req (get-text-property (point) 'required))
		(progn
		  (put-text-property (point-at-bol) (point-at-eol) 'face 'superman-warning-face)
		  (error (concat (or req "This is a required field"))))
	      (beginning-of-line)
	      (let ((kill-whole-line t))
		(kill-line))
	      (forward-line -1))
	  (end-of-line)))
      (when (superman-get-property (point) "GoogleCalendar" t nil)
	(save-restriction
	  (superman-google-export-appointment)))
      ;; run hidden hook 
      (when (setq hidden-hook (get-text-property (point-min) 'clean-hook))
	(funcall hidden-hook))
      (goto-char (next-single-property-change (point-min) 'read-only))
      (skip-chars-forward "\n\t ")
      (setq catch (buffer-substring (point-at-bol)
				    (point-max)))
      ;; say by by to capture buffer
      (kill-buffer (current-buffer))
      (set-buffer (marker-buffer dest))
      (goto-char (marker-position dest))
      (if edit
	  (progn (org-narrow-to-subtree)
		 ;; if this is an edit replace the heading at destination
		 (delete-region (point-min) (point-max)))
	(ignore-errors (org-narrow-to-subtree))
	(goto-char (point-max))
	(insert "\n"))
      (insert catch)
      (save-buffer)
      (widen)
      (cond ((window-configuration-p scene)
	     ;; set window-configuration
	     (set-window-configuration scene))
	    ;; call function
	    ((functionp scene) (funcall scene))
	    (t nil))
      (when (or superman-view-mode superman-mode) (superman-redo)))))

(defun superman-quit-scene ()
  "Cancel `superman-capture'
or `superman-view-edit-item' and restore the
window-configuration saved in text-property scene at
point-min.

If a file is associated with the current-buffer save it.
"
  (interactive)
  (let ((scene (or (get-text-property (point-min) 'quit-scene)
		   (get-text-property (point-min) 'scene))))
    (when (setq hidden-hook (get-text-property (point-min) 'quit-hook))
      (funcall hidden-hook))
    (kill-buffer (current-buffer))
    (when (window-configuration-p scene)
      (set-window-configuration scene))))


;;}}}
;;{{{ capture documents, notes, etc.

(defun superman-capture-thing (&optional project)
  "Associate a thing (file, note, task, link, meeting, etc.) with a project.
The default place for the new item is at the cursor in superman-view-mode
and in the first cat otherwise."
  (interactive)
  (let ((pro (superman-get-project project nil))
	(scene (current-window-configuration))
	(cat (superman-current-cat))
	(marker (get-text-property (point-at-bol) 'org-hd-marker))
	(superman-setup-scene-hook
	 #'(lambda ()
	     (define-key
	       superman-capture-mode-map
	       [(tab)]
	       'superman-complete-project-property)))
	(defaults `((hdr " TODO [A] New item")
		    ("Link" . nil)
		    ("FileName")
		    ("AppointmentDate")
		    ("Location")))
		    ;; ("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))))
	props keys)
    (if superman-view-mode
	(when (and cat (superman-get-property (superman-cat-point) "freetext"))
	  (error "Cannot add items in freetext area"))
      ;; activate project view
      (superman-switch-config pro nil "PROJECT"))
    ;; now we are in project view
    (unless cat
      (superman-next-cat)
      (setq cat (or (superman-current-cat)
		    (progn
		      (save-excursion
			(find-file (superman-get-index pro))
			(end-of-buffer)
			(insert "\n* NewCat\n")
			(save-buffer))
		      "NewCat"))))
    ;; supplement list of existing properties
    ;; with default properties
    (setq keys
	  (mapcar 'list
		  (superman-view-property-keys)))
    (if (assoc "CaptureDate" keys)
	(setq keys
	      (append `(("CaptureDate" ,(format-time-string "[%Y-%m-%d %a %R]")))
		      (delete (assoc "CaptureDate" keys) keys)))
      (setq keys `(("CaptureDate" ,(format-time-string "[%Y-%m-%d %a %R]")))))
    ;; (mapcar #'(lambda (x) (list x nil)) keys)))
    ;; add defaults
    (setq props (append keys defaults))
    (superman-capture pro
		      (or marker cat)
		      "item"
		      nil
		      props nil scene)))

;; FIXME: this should be an edit of the project entry 
(defun superman-capture-others (&optional project)
  "Set the names of the OTHERS, i.e. the collaborators, for project PROJECT."
  (interactive)
  (let* ((nick (if (stringp project) project (car project)))
	 (pro (superman-get-project nick))
	 (others (superman-get-others pro))
	 (new-others (read-string (concat "Set others for project " nick ": ")
				  others)))
    (unless (string= others new-others)
      (save-window-excursion
	(superman-goto-profile (car pro))
	(org-set-property (superman-property 'others)
			  (replace-regexp-in-string
			   "[,\t ]+$" ""
			   new-others)))
      (if superman-view-mode
	  (superman-redo)))))

(fset 'superman-capture-file 'superman-capture-document)
(defun superman-capture-document (&optional project marker ask)
  "Register a file in your project-manager. At this point
file does not need to exist."
  (interactive)
  (let* ((pro (superman-get-project project ask))
	 (marker (or marker (get-text-property (point-at-bol) 'org-hd-marker)))
	 (cat-name (get-text-property (point-at-bol) 'cat))
	 (heading (cond (cat-name)
			((and marker (superman-current-cat))
			 marker)
			(t "Documents")))
	 (dir (expand-file-name (concat (superman-get-location pro) (car pro))))
	 ;; FIXME: to circumvent a bug in ido-read-file-name
	 (read-file-name-function 'read-file-name-default)
	 (file (read-file-name (concat "Add document to " (car pro) ": ") (file-name-as-directory dir))))
    (superman-capture
     pro
     heading
     "Document"
     nil
     `(("FileName" ,(concat "[["  (abbreviate-file-name file) "]]"))
       ("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))
       (hdr ,(file-name-nondirectory file)))
     nil nil)))

(defun superman-view-new-project-hook ()
  "Hook to be run when a new project is captured.
Creates the project directory and index file."
  (let* ((case-fold-search t)
	 (nick
	  (progn
	    ;; (outline-next-heading)
	    (org-back-to-heading)
	    (superman-get-property (point) "nickname")))
	 (pro (progn
		(save-buffer)
		(assoc nick superman-project-alist))))
    (superman-create-project pro)
    (superman-update-project-overview)
    (superman-switch-to-project pro)
    (superman-switch-config pro nil "PROJECT")))

(defun superman-capture-superman ()
  "Set up superman profile."
  (unless superman-profile ;; user set the name but file does not exist yet.
    (stop "You need to set the variable superman-profile to a file-name in your .emacs file."))
  (find-file superman-profile)
  (goto-char (point-min))
  (let* ((profile-buffer (buffer-name (current-buffer)))
	 (marker (point-marker))
	 (quit-scene (current-window-configuration))
	 (welcome
	  (concat
	   (superman-make-button "Initialize the SuperMan(ager)" nil 'superman-project-button-face)
	   "\n\nCheck and adjust the setup below, then press the Save button or press C-c C-c.\nThe profile will be saved in the file:\n\n"
	   (superman-make-button superman-profile nil 'superman-capture-button-face)))
	 (clean-hook
	  `(lambda ()
	     (goto-char (point-min))
	     (re-search-forward "*[ ]+SupermanSetup" nil t)
	     ;; read-off value of superman-home
	     ;; (superman-parse-setup (point) (superman-defaults) nil)
	     ;; create directory if necessary
	     (unless (file-exists-p (file-name-directory superman-profile))
	       (make-directory (file-name-directory superman-profile)  't))
	     ;; save profile
	     (save-excursion
	       (find-file superman-profile)
	       (save-buffer)))))
    (superman-capture-whatever
     marker
     welcome
     1
     (concat "\n" (superman-set-up-defaults))
     `(("InitialVisit" ,(format-time-string "<%Y-%m-%d %a>"))
       (hdr "Superman(ager)"))
     nil ;; this is not an edit
     'superman ;; scene
     nil ;; read-only
     quit-scene ;; quit-scene
     clean-hook ;; clean-hook
     nil ;; quit-hook
     )))

(defun superman-capture-project (&optional nickname category loc)
  "Create a new project. If CATEGORY is nil prompt for project category
with completion in existing categories. If NICKNAME is nil prompt for nickname.
If LOC is given it is the mother directory of the directory which
defines the project. 

The following steps are performed:

Step (1) a new entry is added to the file `superman-profile' and the latter is saved.
Step (2) The project directory is created (unless it exists).
Step (3) The index file is initialized (unless it exists).
Step (4) The new project is visited.

Note that saving the superman-profile alters your current `superman-project-alist'.

To undo all this, enter the supermanager (shortcut: M-x `superman'), navigate to
the new project and call `superman-delete-project' (shortcut: x)
"
  (interactive)
  ;; (superman-refresh)
  (let* ((nickname (or (and (not (string= nickname "")) nickname) (read-string "Project name (short) ")))
	 (category (or category
		       (let ((cat
			      (completing-read
			       "Category: "
			       (mapcar (lambda (x)
					 (list x))
				       (superman-parse-project-categories))
			       nil nil)))
			 (if (string= cat "") "Krypton" cat))))
	 (marker (get-text-property (point-at-bol) 'org-hd-marker))
	 (loc (or loc
		  (save-excursion
		    (superman-go-home category t)
		    (superman-get-property (point) "location" 'inherit))
		  superman-default-directory))
	 (scene (current-window-configuration))
	 (superman-setup-scene-hook
	  #'(lambda ()
	      (define-key
		superman-capture-mode-map
		[(tab)]
		'superman-complete-project-property))))
    ;; category)
    ;; check if nickname exists 
    (while (assoc nickname superman-project-alist)
      (setq nickname
	    (read-string (concat "Project " nickname " exists. Please choose a different name (C-g to exit): "))))
    (superman-capture
     `("*S*" (("index" . ,superman-profile)))
     (or marker category)
     "Project"
     nil
     `(("Nickname" ,nickname)
       ("InitialVisit" ,(format-time-string "<%Y-%m-%d %a>"))
       ("LastVisit" ,(format-time-string "<%Y-%m-%d %a>"))
       ("Others" "")
       ;; ("Location" ("" (required t)))
       ("Location" ,loc)
       (hdr ,(concat "ACTIVE " nickname))
       ("Index" "")
       ("Category" ,category)) 
     superman-project-level
     'superman-view-new-project-hook ;; sets the scene to PROJECT 
     scene ;; when capture is canceled
     )))


(defun superman-capture-file-at-point ()
  (interactive)
  (if (not (eq major-mode 'file-list-mode))
      (error "Works only in file-list-mode")
    (let ((nick (get-text-property (point-min) 'nickname))
	  (file-list (list (file-list-make-entry (file-list-file-at-point)))))
      (superman-capture-file-list nick file-list nil 'ask))))
       
(defun superman-capture-file-list (&optional project file-list level ask)
  "Capture a FILE-LIST of files in PROJECT. Add them all to the project
index file as LEVEL headings. Then show the updated project view buffer."
  (interactive)
  (let* ((pro (superman-get-project project ask))
	 (gitp (superman-git-toplevel (concat
				       (superman-get-location pro)
				       (car pro))))
	 (marker (get-text-property (point-at-bol) 'org-hd-marker))
	 (heading (if (and marker (superman-current-cat))
		      marker
		    "Documents"))
	 (pro-file-list-buffer (concat "*File-list-" (car pro) "*"))
	 (level (or level superman-item-level))
	 (file-list (cond
		     (file-list)
		     ((or major-mode 'file-list-mode)
		      (eq major-mode 'file-list-completion-mode)
		      file-list-current-file-list)
		     ((and (buffer-live-p pro-file-list-buffer)
			   (progn (switch-to-buffer pro-file-list-buffer)
				  file-list-current-file-list))
		      (superman-file-list pro)))))
    ;; goto index file
    (if heading
	(cond ((stringp heading)
	       (superman-goto-project pro heading 'create nil nil nil))
	      ((markerp heading)
	       (progn (switch-to-buffer (marker-buffer heading))
		      (goto-char heading))))
      (find-file (superman-get-index pro))
      (widen)
      (show-all)
      (goto-char (point-max)))
    (while file-list
      (let* ((el (car file-list))
	     (fname (file-list-make-file-name~ el)))
	(message (concat "adding " fname))
	(insert (make-string level (string-to-char "*"))
		" "
		(car el)
		"\n:PROPERTIES:"
		"\n:FileName: [["  (abbreviate-file-name fname) "]]"
		"\n:END:\n\n"))
      (setq file-list (cdr file-list)))
    (superman-view-project pro)
    (superman-redo)))

(defun superman-complete-project-property ()
  (interactive)
  (save-excursion
    (let ((curprop (progn (beginning-of-line) (looking-at ".*:\\(.*\\):") (org-match-string-no-properties 1))))
      (cond
       ((string= (downcase curprop) "filename")
	(goto-char (+ (point) (length curprop) 2))
	(delete-region (point) (point-at-eol))
	(insert " [[" (read-file-name (concat curprop ": ")) "]]"))
       ((string= (downcase curprop) (downcase (superman-property 'index)))
	(goto-char (+ (point) (length curprop) 2))
	(delete-region (point) (point-at-eol))
	(insert " [[" (read-file-name (concat "Set " curprop ": ")) "]]"))
       ((string= (downcase curprop) (downcase (superman-property 'location)))
	(goto-char (+ (point) (length curprop) 2))
	(delete-region (point) (point-at-eol))
	(insert " [[" (read-directory-name (concat "Set " curprop ": ")) "]]"))))))
    
(defun superman-capture-note (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     (or marker "Notes")
     "Note"
     nil
     `(("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))))))

(defun superman-capture-text (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (save-excursion
      (superman-goto-project project "Text" 'create nil nil nil ":FreeText: t"))
    (superman-capture
     pro
     (or marker "Text")
     "free text"
     nil nil 0)))


(defun superman-capture-bookmark (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     (or marker "Bookmarks")
     "Bookmark"
     nil
     `(("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))
       ("Link" nil 'required)))))

(fset 'superman-capture-todo 'superman-capture-task)
(defun superman-capture-task (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     (or marker "Tasks")
     "Task"
     nil
     `(("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))
       (fun
	(lambda ()
	  (save-excursion
	    (org-todo)
	    (org-back-to-heading)
	    (org-shiftup))))))))


;; Capturing meetings
;; Note: inactive time stamp for CaptureDate

(defun superman-capture-calendar (&optional project marker ask)
  (interactive)  
  (superman-capture-meeting "Calendar" marker ask))
  
(defun superman-capture-meeting (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker)))
	;; (date (format-time-string  "<%Y-%m-%d %a %H:%M>" (org-read-date t t))))
	(date (with-temp-buffer
		(org-time-stamp t)
		(buffer-string))))
    ;; (org-read-date t t)))
    (superman-capture
     pro
     (or marker "Calendar")
     "Meeting"
     nil
     `(("MeetingDate" ,date)
       ("Participants" nil)
       ("Location" nil)
       (when superman-google-default-calendar
	 ("GoogleReminderMinutes" nil)
	 )
       ,(when superman-google-default-calendar
	  `("GoogleCalendar" ,superman-google-default-calendar)
	  )
       ("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))))))

;;}}}
;;{{{ capture synchronization commands

(defun superman-capture-unison (&optional project ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(root-1 (read-directory-name "Unison root directory 1: "))
	(root-2 (read-directory-name "Unison root directory 2: ")))
    (superman-capture
     pro
     "Configuration"
     "Unison"
     nil
     `(("UNISON" "superman-unison-cmd")
       ;; (hdr "CHANGEME")
       ("SWITCHES" "-ignore 'Path .git' -ignore 'Regex ^(\\.|#).*' -ignore 'Regex .*~$' -perms 0")
       ;; :SWITCHES: -ignore 'Regex .*(~|te?mp|rda)$' -ignore 'Regex ^(\\.|#).*' -perms 0
       ("ROOT-1" ,root-1)
       ("ROOT-2" ,root-2)
       ("CaptureDate" ,(format-time-string "[%Y-%m-%d %a]"))))))

(defun superman-unison-insert-switches ()
  (interactive)
  (insert ":SWITCHES: -ignore 'Path .git' -ignore 'Regex ^(\\.|#).*' -ignore 'Regex .*~$' -perms 0"))

(defun superman-run-unison (&optional project)
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((pro (or project
		      superman-current-project
		      (superman-switch-to-project nil t)))
	     (unison-list (superman-view-read-unison pro))
	     (ulen (length unison-list)))
	;; prevent synchronizing unsaved buffers
	(save-some-buffers nil t)
	(cond ((= ulen 1)
	       (async-shell-command (cdar unison-list)))
	      ((> ulen 1)
	       (let ((u (completing-read
			 "Choose unison command: " unison-list)))
		 (when u (async-shell-command
			  (cdr (assoc u unison-list)))))))))))

;;}}}
;;{{{ capture mails

(defun superman-capture-mail (&optional project)
  ;; (defun superman-gnus-goto-project-mailbox (project &optional arg)
  (interactive)
  (unless (or (eq major-mode 'gnus-article-mode)
	      (eq major-mode 'gnus-summary-mode))
    (error "Can only capture mails from either gnus-article or gnus-summary buffers"))
  ;; activate the connection with summary
  (when (eq major-mode 'gnus-article-mode)
    (gnus-article-show-summary))
  (gnus-summary-select-article-buffer)
  (unless (use-region-p)
    (mark-whole-buffer))
  (let* ((buf (current-buffer))
	 (link (org-store-link 1))
	 (entry (superman-get-project project 'ask))
	 (pro (car entry))
	 (loc (expand-file-name
	       (file-name-directory (replace-regexp-in-string
				     "/$" ""
				     (file-name-directory
				      (superman-get-index entry))))))
	 (index (superman-get-index entry))
	 (region (buffer-substring (region-beginning) (region-end)))
	 (mailbox (file-name-as-directory
		   (concat (file-name-as-directory loc) "Mailbox")))
	 (from (message-fetch-field "from"))
	 (subject (message-fetch-field "subject"))
	 (date (message-fetch-field "date"))
	 (attachments (superman-save-attachments pro mailbox (current-buffer) date)))
    (superman-capture
     entry
     "Mail"
     "Mail"
     nil
     `(("CaptureDate"  ,(format-time-string "<%Y-%m-%d %a>"))
       (body ,(concat "----\n" region "\n----\n"))
       (body ,attachments)
       ("EmailDate" ,date)
       (hdr ,(concat "Mail from " from " " subject ))
       ("Link" ,link)))))

(defun superman-save-attachments (project dir buf date)
  "Interactively save mail contents in project org file
and MIME parts in sub-directory 'mailAttachments' of the project."
  (interactive)
  (gnus-article-check-buffer)
  (let* ((mime-line ""))
    (unless (file-exists-p dir)
      (if (featurep 'xemacs)
          (make-directory-path dir)
        (make-directory dir t)))
    (save-excursion
      (switch-to-buffer buf)
      (gnus-summary-display-buttonized 't))
    ;; (goto-char (point-min))
    (message-goto-body)
    (while (re-search-forward "\\[[0-9]+\\." nil t)
      ;; modified code from `mm-save-data'
      (save-excursion
        (let* ((data (get-text-property (point) 'gnus-data))
               (filename (or (mail-content-type-get
                              (mm-handle-disposition data) 'filename)
                             (mail-content-type-get
                              (mm-handle-type data) 'name)))
               file)
          (when filename
            (setq filename (gnus-map-function
                            mm-file-name-rewrite-functions
                            (file-name-nondirectory filename))))
          (when (and data filename)
            (setq file (read-file-name
                        "Save attached file to: "
                        dir nil nil (replace-regexp-in-string "[ ]+" "" filename)))
            (if (file-directory-p file)
                (message "file not saved")
              (when (or (not (file-exists-p file))
                        (y-or-n-p (concat "File " file " exists, overwrite?" )))
                (mm-save-part-to-file data file))
              (setq mime-line (concat "\n**** " (file-name-nondirectory file)
				      "\n:PROPERTIES:\n:CaptureDate: " (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:EmailDate: " date 
				      ;; (format-time-string (car org-time-stamp-formats) (org-capture-get :default-time))
				      "\n:Link:" " [[file:"
				      (replace-regexp-in-string
				       (expand-file-name "~")
				       "~"
				       file)
				      "][" (file-name-nondirectory file) "]]"
                                      "\n:END:\n"
                                      mime-line)))))))
    mime-line))

;;}}}
;;{{{ capture cats
(defun superman-capture-cat (&optional project marker ask)
  (interactive)
  (let ((pro (superman-get-project project ask))
	(marker (or marker (get-text-property (point-at-bol) 'org-hd-marker))))
    (superman-capture
     pro
     marker 
     "Heading"
     nil
     `(("Ball1" "todo :face superman-get-todo-face")
       ("Ball2" "hdr :name Title :width 13 :face font-lock-function-name-face")
       ("Ball3" ""))
     1)))

;;}}}

;;{{{ capture VC documents

(defun superman-capture-git-section (&optional project git-dir level ask)
  "Capture files under version control. Delete and recreate section 'GitFiles' "
  (interactive)
  (let* ((pro (superman-get-project project ask))
	 (gitp (superman-git-toplevel (concat
				       (superman-get-location pro)
				       (car pro))))
	 (gitdir (or git-dir
		     (read-directory-name (concat "Directory : "))))
	 (gittop (superman-git-toplevel gitdir))
	 ;; (headingfound (superman-goto-project pro "GitFiles"))
	 (level (or level superman-item-level))
	 (file-list (delete ""
			    (split-string
			     (shell-command-to-string
			      (concat "cd " gitdir ";"
				      superman-cmd-git
				      " ls-files --full-name")) "\n")))
	 (file-hash (make-hash-table :test 'equal))
	 (probuf (get-buffer-create (concat
				     "*"
				     (superman-get-index pro)
				     (car pro)
				     "-git-profile*")))
	 (profile (concat (file-name-directory
			   (superman-get-index pro))
			  (car pro)
			  "-git-profile.org")))
    (set-buffer probuf)
    (erase-buffer)
    ;; goto index file
    (while file-list
      (let* ((el (car file-list))
	     (elsplit (split-string el "/"))
	     (filename (car (last elsplit)))
	     (direl (reverse (cdr (reverse elsplit))))
	     (dir (if (> (length elsplit) 1)
		      (mapconcat 'identity direl "/" )
		    "/")))
	(puthash dir (push filename (gethash dir file-hash)) file-hash)
	(setq file-list (cdr file-list))))
    (find-file profile)
    (erase-buffer)
    (goto-char (point-max))  
    (maphash (lambda (keys vv) 	       
	       (insert (concat "** " keys "\n\n"))
	       (while vv
		 (let ((filename (abbreviate-file-name
				  (expand-file-name
				   (concat (if (string= keys "/") "." keys) "/" (car vv)) gittop))))
		   (insert (make-string level (string-to-char "*"))
			   " "
			   (car vv)
			   "\n:PROPERTIES:"
			   "\n:FileName: [[" filename "]]"
			   "\n:GitStatus: " (if (file-exists-p filename) "Unknown" "Removed")
			   ;; (when gitp (concat "\n:GitStatus: " (nth 1 (superman-git-get-status fname nil))))
			   "\n:END:\n\n"))
		 (setq vv (cdr vv)))) file-hash)
    (goto-char (point-min))
    (superman-format-section superman-document-balls probuf)
    (switch-to-buffer probuf)
    (goto-char (point-min))
    (insert (superman-make-button
	     (concat "Git repository for project: " (car pro))
	     `(superman-capture-git-section ,(car pro) ,gitdir)
	     'superman-project-button-face
	     "Refresh"))
    (insert "\n\n")
    (goto-char (point-min))
    (put-text-property (point-at-bol) (point-at-eol) 'git-dir gitdir)
    (put-text-property (point-at-bol) (point-at-eol) 'nickname (car pro))
    (put-text-property (point-at-bol) (point-at-eol) 'index profile)
    (put-text-property (point-at-bol) (point-at-eol) 'redo-cmd `(superman-capture-git-section ,(car pro) ,gitdir))
    (superman-view-mode)))


;;}}}

(provide 'superman-capture)
;;; superman-capture.el ends here
