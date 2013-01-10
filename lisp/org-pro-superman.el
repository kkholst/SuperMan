;;; org-pro-superman.el --- org project manager

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
;; (defun org-pro-superman ()
  ;; "Returns a super project for project management"
  ;; `("SuperManager"
    ;; (("location" . ,org-pro-default-directory)
     ;; ("index" . ,org-pro-file)
     ;; ("category" . "Super")
     ;; ("state" . "ACTIVE")
     ;; ("config" . "INDEX | AGENDA / TODO"))))

(defun org-pro-set-property ()
  (interactive)
  (let* ((prop-list '(("Location" . nil) ("Index" . nil) ("Category" . nil) ("Others" . nil) ("PublishDirectory" . nil)))
	 (prop (completing-read "Set property: " prop-list))
	 (pom (org-get-at-bol 'org-hd-marker))
	 (curval (org-entry-get pom prop))
	  ;; (if  (completing-read (concat "Value for " prop ": ")
	 (val (read-string (concat "Value for " prop ": ") curval)))
    (org-entry-put pom prop val))
  (org-agenda-redo))

(defun org-superman-return ()
  (interactive)
  (let ((pro (assoc (org-pro-property-at-point "NickName") org-pro-project-alist)))
    (org-pro-switch-to-project 'force pro)))

(defun org-pro-finalize-superman ()
  (org-pro-superman-on))
;;}}}
;;{{{ superman 

;; FIXME: use gnus-user-date-format-alist to trim date
(defun org-pro-superman-format (hdr level category tags-list prop-list)
  (concat " " (org-pro-trim-string hdr 20)
	  (let ((cprops prop-list)
		(pstring ""))
	    (while cprops
	      (let ((val (cdr (car cprops))))
		(cond ((string= (downcase (caar cprops)) "filename")
		       (setq val (file-name-nondirectory (org-link-display-format val)))))
		(setq pstring (concat pstring "  " (org-pro-trim-string val  23))))
		(setq cprops (cdr cprops)))
	      pstring) "\t"))

(defun superman (&optional project)
  "Manage projects."
  (interactive)
  (let* ((view-buf-name (concat "*Superman*")))
    (if (get-buffer view-buf-name)
	(switch-to-buffer view-buf-name)
      (let ((lprops
	     `((org-agenda-files (quote (,org-pro-file)))
	       (org-agenda-finalize-hook 'org-pro-finalize-superman)
	       (org-agenda-overriding-header
		(concat "h: help, n: new project, s[S]: set property[all]"
			"\n\nProjects: " "\n"
			(org-pro-view-documents-format "header" 0 nil nil '(("NickName" . "NickName") ("LastVisit" . "LastVisit") ("Location" . "Location") ("Others" . "Others")))))
	       (org-agenda-overriding-agenda-format 'org-pro-view-documents-format)
	       (org-agenda-view-columns-initially nil)
	       (org-agenda-buffer-name "*Superman*"))))
	(org-let lprops '(org-pro-tags-view-plus nil "InitialVisit={.+}" '("NickName" "LastVisit" "Location" "Others")))
	(rename-buffer view-buf-name)
	(put 'org-agenda-redo-command 'org-lprops lprops)))))
;;}}}
;;{{{

(defvar org-pro-superman-mode-map (make-sparse-keymap)
  "Keymap used for `org-pro-superman-mode' commands.")
   
(define-minor-mode org-pro-superman-mode 
  "Toggle org projectmanager document superman mode.
With argument ARG turn org-pro-superman-mode on if ARG is positive, otherwise
turn it off.

Enabling org-pro-superman mode electrifies the superman buffer for project management."
     :lighter " S"
     :group 'org
     :keymap 'org-pro-superman-mode-map)

(defun org-pro-superman-on ()
  (interactive)
  (org-pro-superman-mode t))


(defun org-pro-superman-new-project ()
  (interactive)
  (save-window-excursion
    (org-pro-new-project))
  (org-agenda-redo))

(define-key org-pro-superman-mode-map [return] 'org-superman-return) ;; Return is not used anyway in column mode
(define-key org-pro-superman-mode-map "n" 'org-pro-superman-new-project)
(define-key org-pro-superman-mode-map "S" 'org-pro-set-property)
;;}}}  

(provide 'org-pro-superman)
;;; org-pro-superman.el ends here
