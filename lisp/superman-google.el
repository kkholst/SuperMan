;;; superman-google.el --- superman has google

;; Copyright (C) 2013  Thomas Alexander Gerds

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
;; If you capture a meeting in this form
;;
;; ,----
;; |  *** iconic Batgirl
;; |  :PROPERTIES:
;; |  :Date:   <1973-01-18 Thu 11:00>
;; |  :Participants: Robin, WonderWoman
;; |  :CaptureDate: <2013-01-16 Wed>
;; |  :GoogleCalendar: Bat
;; |  :Status: confirmed
;; |  :END:
;; `----
;;
;; and you also have
;;
;; (add-hook
;; 'org-capture-before-finalize-hook
;; 'org-google-export-appointment)
;; 
;; then finalizing the capture \C-c \C-c
;; will prompt you for exporting the appointment
;; to your calendar "Bat"
;;
;; If property GoogleCalendar is not set,
;; the prompt uses your
;; superman-google-default-calendar
;;
;; ADVARSEL: non-standard letters like ø or ä
;;           can easily destroy the transporter
;;
;;; Code:




(defvar superman-google-cmd
  "google"
  "Full path to google command line utility")

(defvar superman-google-calendars
  nil
  "List of google calendar names. E.g. '(\"Work\" \"Family\").")

(defvar superman-google-default-calendar
  nil
  "Name of your default google calendar.")

(defun superman-google-export-appointment ()
  (interactive)
  (org-back-to-heading t)
  (org-narrow-to-subtree)
  (let ((g-cal (org-entry-get nil "GoogleCalendar")))
    (if (or g-cal
	    (and
	     (org-entry-get nil "MeetingDate")
	     (progn
	       (setq g-cal
		     (completing-read
		      (concat "Choose google calendar (default: " superman-google-default-calendar "): ")
		      (mapcar #'(lambda (entry) (cons entry nil)) superman-google-calendars)
		      nil t nil nil
		      superman-google-default-calendar))
	       (org-set-property "GoogleCalendar" g-cal)
	       g-cal)))
	;; search for time-stamp
	(if (and (not (org-at-timestamp-p nil))
		 (progn
		   (re-search-forward ":Schedule:" nil t)
		   (re-search-forward ">" nil t)
		   (not (org-at-timestamp-p nil))))
	    (message "Cursor not at time-stamp")
	  (save-excursion
	    (beginning-of-line)
	    (re-search-forward "<" nil t)
	    (let (g-date g-time g-start g-stop g-string)
	      (if (looking-at "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*>\\)--<\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)>")
		  ;; from -- to format, e.g., <2013-02-10 Sun>--<2013-02-16 Sat>
		  ;; NOTE: google seems to substract one day from the first and two days from the last date
		  (setq g-date (format "%s-%s-%s,%s-%s-%s" 
				       (match-string-no-properties 1)
				       (match-string-no-properties 2)
				       (match-string-no-properties 3)
				       (match-string-no-properties 5)
				       (match-string-no-properties 6)
				       (match-string-no-properties 7))
			g-time nil)
		(or (looking-at "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?-?--?\\(\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)>\\)")
		    (looking-at "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)>\\)")
		    (looking-at "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)>\\)"))
		(setq g-date (format "%s/%s/%s" 
				     (match-string-no-properties 2)
				     (match-string-no-properties 3)
				     (match-string-no-properties 4))
		      g-start (match-string-no-properties 6)
		      g-stop (match-string-no-properties 9)
		      g-time
		      (if g-start
			  (if g-stop
			      (format " from %s to %s"
				      (org-agenda-time-of-day-to-ampm g-start)
				      (org-agenda-time-of-day-to-ampm g-stop))
			    (format " from %s to %s"
				    (org-agenda-time-of-day-to-ampm g-start)
				    (org-agenda-time-of-day-to-ampm
				     (concat (number-to-string
					      (+ 1 (string-to-number
						    (match-string-no-properties 7))))
					     ":"
					     (match-string-no-properties 8)))))
			"")))
	      (outline-previous-heading)
	      (looking-at org-complex-heading-regexp)
	      (setq g-string (match-string-no-properties 4))
	      (let* ((pre-command
		      (if g-time
			  (concat superman-google-cmd " calendar add --cal \""
				  g-cal "\" \"" g-string
				  " on " g-date g-time "\"")
			(concat superman-google-cmd " calendar add --cal \""
				g-cal "\" \"" g-string "\""
				" --date \"" g-date g-time "\"")))
		     (g-command
		      (read-string "Google calendar entry: " pre-command)))
		(when (> (length g-command) 0)
		  (superman-run-cmd g-command
				    "*Superman-google-calendar*"
				    (concat "Running\n" g-command " returned:\n\n"))
		  (sit-for 3)))
	      (widen)))))))
;; (shell-command-to-string g-command))))))))

(provide 'superman-google)
;;; superman-google.el ends here


